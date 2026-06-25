#include "ry/module/module_loader.hpp"
#include "ry/lexer/lexer.hpp"
#include "ry/parser/parser.hpp"
#include "ry/diagnostic/diagnostic.hpp"
#include "ry/project/project_config.hpp"
#include "ry/stdlib_registry.hpp"
#include "ry/trace/trace.hpp"
#include <algorithm>
#include <array>
#include <cstdlib>
#include <filesystem>
#include <fstream>
#include <optional>
#include <sstream>
#include <stdexcept>
#include <string_view>


namespace ry {

namespace fs = std::filesystem;
using ry::TraceField;
using ry::emitTraceDiagnostic;
using ry::emitTraceEvent;

// #2297: 公開 stdlib モジュール許可リスト。#1769 で `ry.*` の public surface
// として列挙された 13 モジュールのみ受理し、それ以外は internal とみなして
// reject する。`StdlibRegistry` は codegen dispatch table で「dispatcher を持つ
// package」を列挙するもので意図が異なるため、ここで独立に policy を維持する
// (例: `runtime_internal` は dispatch を持つが internal、逆に `lang` / `regex` /
// `filesystem` / `testing` は dispatcher を持たないが public)。v0.0.30 で
// `net` / `json5` を追加 (#2309)。
static constexpr std::array<std::string_view, 13> kPublicRyModules = {
    "lang", "math", "io", "path", "filesystem", "json",
    "http", "thread", "regex", "testing", "base64",
    "net", "json5",
};

static bool isPublicRyModule(std::string_view first_segment) {
    return std::find(kPublicRyModules.begin(), kPublicRyModules.end(),
                     first_segment) != kPublicRyModules.end();
}

// #2297: allowlist エラー文の "available: ..." リストを `kPublicRyModules` から
// 一度だけ組み立てる。文字列をハードコードすると set/array との drift を招くため。
static const std::string &publicRyModulesListForError() {
    static const std::string kList = [] {
        std::string s;
        bool first = true;
        for (auto m : kPublicRyModules) {
            if (!first) s += ", ";
            s += "ry.";
            s += m;
            first = false;
        }
        return s;
    }();
    return kList;
}

// #2297: `ry/foo/bar` を user-facing `ry.foo.bar` に変換する。loader は内部
// 表現としてスラッシュ区切りを使うが、エラーメッセージはユーザーが書いた
// dot 表記に揃える。
static std::string toRyDotForm(const std::string &slash_path) {
    std::string result = slash_path;
    std::replace(result.begin(), result.end(), '/', '.');
    return result;
}

// #2297: user-facing パス表記の正規化。`ry/*` (予約 namespace) はドット形式に
// 変換し、それ以外 (legacy bare `math` / 相対 `./submod` / 通常パッケージ) は
// loader 内部表現と一致させたまま返す。エラーメッセージで `module_path` を
// 直接埋め込んでいる箇所をこのヘルパーに通すことで、ユーザーが書いたスペリ
// ングに揃った診断を出す。
static std::string toUserFacingPath(const std::string &module_path) {
    return ModuleLoader::isRyPath(module_path) ? toRyDotForm(module_path)
                                                : module_path;
}

// #2350 / #2351: legacy stdlib import (`from math import …` / `from std.math
// import …` / `from std import …` / `import math`) を canonical な user-facing
// `ry.<…>` 形式に変換する。canonical な公開 ry モジュールが存在しない場合
// (例: `std/runtime_internal` — `ry.runtime_internal` は非公開) は nullopt を
// 返し、legacy 検出から外す。これにより「reject + 提示した代替も拒否」という
// 矛盾した案内 (#2357 review) が発生せず、非公開モジュールは従来どおり
// `std/<internal>` / 裸 `<internal>` 形式で解決可能のまま残る。
// 前提: caller は rp.from_stdlib==true でゲートする — bogus 形式は resolve() が
// 例外を投げて detector に到達しないため、ここに来る時点で実モジュールに解決済み。
//   "std"             -> "ry.lang"   (flat std → explicit prelude)
//   "std/math"        -> "ry.math"   (std.dotted → public submodule)
//   "std/runtime_internal" -> nullopt  (non-public; legacy form still accepted)
//   "math"            -> "ry.math"   (bare flat → public submodule)
static std::optional<std::string>
tryLegacyToCanonical(const std::string &resolve_path) {
    if (ModuleLoader::isRyPath(resolve_path)) return std::nullopt;
    // "std" → "ry.lang"。canonical 物理パス定数を dot 形式に変換して経由
    // することで、`kRyLangPreludePath` / `canonicalStdlibName` と同期する。
    if (resolve_path == "std")
        return toRyDotForm(ModuleLoader::kRyLangPreludePath);
    if (resolve_path.size() > 4 && resolve_path.compare(0, 4, "std/") == 0) {
        auto suffix = resolve_path.substr(4);
        if (isPublicRyModule(suffix)) return "ry." + suffix;
        return std::nullopt;
    }
    if (isPublicRyModule(resolve_path)) return "ry." + resolve_path;
    return std::nullopt;
}

// Check if a statement is an exportable definition
static bool isExportable(const StmtNode &stmt) {
    return std::holds_alternative<std::unique_ptr<FnStmt>>(stmt) ||
           std::holds_alternative<RecordStmt>(stmt) ||
           std::holds_alternative<EnumStmt>(stmt) ||
           std::holds_alternative<TypeAliasStmt>(stmt) ||
           std::holds_alternative<AssignStmt>(stmt) ||
           std::holds_alternative<DirectiveDefStmt>(stmt);
}

// #2317: file_id of an exportable definition's source location. Used to
// register stdlib / cross-package definitions as "external" so CodeGen
// can gate any-usage lint warnings.
static int getStmtFileId(const StmtNode &stmt) {
    if (std::holds_alternative<std::unique_ptr<FnStmt>>(stmt))
        return std::get<std::unique_ptr<FnStmt>>(stmt)->loc.file_id;
    if (std::holds_alternative<RecordStmt>(stmt))
        return std::get<RecordStmt>(stmt).loc.file_id;
    if (std::holds_alternative<EnumStmt>(stmt))
        return std::get<EnumStmt>(stmt).loc.file_id;
    if (std::holds_alternative<TypeAliasStmt>(stmt))
        return std::get<TypeAliasStmt>(stmt).loc.file_id;
    if (std::holds_alternative<AssignStmt>(stmt))
        return std::get<AssignStmt>(stmt).loc.file_id;
    if (std::holds_alternative<DirectiveDefStmt>(stmt))
        return std::get<DirectiveDefStmt>(stmt).loc.file_id;
    return -1;
}

// Get the name of an exportable definition
static std::string getExportName(const StmtNode &stmt) {
    if (std::holds_alternative<std::unique_ptr<FnStmt>>(stmt))
        return std::get<std::unique_ptr<FnStmt>>(stmt)->name;
    if (std::holds_alternative<RecordStmt>(stmt))
        return std::get<RecordStmt>(stmt).name;
    if (std::holds_alternative<EnumStmt>(stmt))
        return std::get<EnumStmt>(stmt).name;
    if (std::holds_alternative<TypeAliasStmt>(stmt))
        return std::get<TypeAliasStmt>(stmt).name;
    if (std::holds_alternative<AssignStmt>(stmt))
        return std::get<AssignStmt>(stmt).name;
    if (std::holds_alternative<DirectiveDefStmt>(stmt))
        return std::get<DirectiveDefStmt>(stmt).name;
    return "";
}

// Builds the `line N: ...` prefix used by every import-error throw site so the
// four call sites stay in sync.
static std::string makeImportError(int line, const std::string &detail) {
    std::string msg = "line ";
    msg += std::to_string(line);
    msg += ": ";
    msg += detail;
    return msg;
}

// Compiler intrinsics exposed by the testing module (#712). The names listed
// here either produce no AST node (`expect <expr> <matcher>` is a parser-level
// statement form; `mock` / `spy` / `verifyCalledWith` are recognized at
// codegen) or are emitted directly as IR via a callee-name intercept (`fail`
// — `codegen_test.cpp:emitFailCall` calls `__ry_test_fail(line, msg)`
// directly, with the call-site line number embedded as a constant; #1690).
// Listing them here lets `from testing import expect / mock / fail` resolve
// without producing AST nodes — the allow-list bypass below skips the
// `'<name>' not found` throw without altering anything else.
//
// Note: `it` / `describe` are NOT listed here. They are declared as regular
// `@directive` statements in `share/std/testing/testing.ry` and flow through
// the standard import mechanism into `CodeGen::user_directive_registry_`
// (#721). Validation of unimported usage is handled by `validateDirectives`
// in the general `@directive` machinery, which raises
// `unknown directive '@it'` / `unknown directive '@describe'`. As of #722,
// `verify` is also a regular `@public fn verify` in testing.ry — it flows
// through the same standard mechanism and is NOT a compiler intrinsic.
static const std::unordered_set<std::string> &testingIntrinsics() {
    static const std::unordered_set<std::string> kSet = {
        "expect", "mock", "mockReturnValueOnce", "spy", "fail",
        "verifyCalledWith", "calledWith", "calledTimes", "lastCalledWith"};
    return kSet;
}

// Gate the intrinsic bypass on `from_stdlib` so a project-local `testing.ry`
// shadow (resolved via `referrer_dir`) does NOT silently absorb the
// intrinsic names. Only the stdlib testing module gets the bypass; a local
// shadow continues to fail with the existing `'<name>' not found` diagnostic.
// #1769: callers may pass either the legacy bare spelling (`testing`) or the
// canonical reserved-namespace spelling (`ry/testing`); compare on the
// canonicalized bare name so both paths agree.
static bool isTestingIntrinsic(bool from_stdlib,
                               const std::string &module_path,
                               const std::string &name) {
    return from_stdlib &&
           ModuleLoader::canonicalStdlibName(module_path) == "testing" &&
           testingIntrinsics().count(name) > 0;
}

// #1888: C++-registered resource kinds (File / TcpListener / Thread / ...) are
// declared programmatically via `ResourceKindRegistry::registerKind` at static
// init, not as `record` statements in the stdlib `.ry` files, so they are
// invisible to `extractDefinitions`'s AST scan. The registry stores the owning
// module in `Info::library`; if it matches `import_path` we treat the name as
// a resolvable export. Gated on `from_stdlib` for the same reason as
// `isTestingIntrinsic`: a project-local shadow must not silently absorb a
// stdlib-only registered type name.
static bool isCppResourceKind(bool from_stdlib,
                              const std::string &module_path,
                              const std::string &name) {
    if (!from_stdlib) return false;
    int id = ResourceKindRegistry::instance().lookupByTypeName(name);
    if (id == ResourceKindRegistry::NONE) return false;
    const auto *info = ResourceKindRegistry::instance().getInfo(id);
    if (!info || !info->library) return false;
    return ModuleLoader::canonicalStdlibName(module_path) == info->library;
}

// #1888: Builtin record types registered in `CodeGen` constructor (not in the
// stdlib `.ry` file). Only `Match` qualifies today — see truth source
// `src/codegen.cpp:52` (`record_types_["Match"]` registration). If a new
// builtin record is added there, sync this map. `Error` is intentionally
// excluded: it is a global builtin not associated with any module.
static bool isCppBuiltinRecord(bool from_stdlib,
                               const std::string &module_path,
                               const std::string &name) {
    if (!from_stdlib) return false;
    static const std::unordered_map<std::string,
                                    std::unordered_set<std::string>>
        kModuleToRecords = {
            {"regex", {"Match"}},
        };
    auto it = kModuleToRecords.find(ModuleLoader::canonicalStdlibName(module_path));
    if (it == kModuleToRecords.end()) return false;
    return it->second.count(name) > 0;
}

static ExportableKind getExportableKind(const StmtNode &stmt) {
    if (std::holds_alternative<std::unique_ptr<FnStmt>>(stmt)) return ExportableKind::Fn;
    if (std::holds_alternative<AssignStmt>(stmt)) {
        const auto &a = std::get<AssignStmt>(stmt);
        return hasDirective(a.directives, "const") ? ExportableKind::Const
                                                   : ExportableKind::Value;
    }
    if (std::holds_alternative<RecordStmt>(stmt)) return ExportableKind::Record;
    if (std::holds_alternative<EnumStmt>(stmt)) return ExportableKind::Enum;
    if (std::holds_alternative<TypeAliasStmt>(stmt)) return ExportableKind::TypeAlias;
    if (std::holds_alternative<DirectiveDefStmt>(stmt)) return ExportableKind::Directive;
    return ExportableKind::Fn; // unreachable: caller already filtered via isExportable
}

// Build a synthesized binding for `from m import orig as alias`.
//   - `@const` aliases: emit `AssignStmt{alias = orig}` so the existing
//     module-global / const-folding paths see `alias` as an additional name
//     pointing at the same underlying value.
//   - fn / record / enum / type-alias aliases (#1725): emit an
//     `ImportAliasStmt` whose codegen handler registers `alias_name` as a
//     fresh key into the corresponding codegen table
//     (`functions_` / `record_types_` / `enum_types_` / `type_aliases_`).
// Value (non-@const) and Directive aliases are rejected by
// `rejectUnsupportedAlias` upstream and must not reach this function.
static StmtNode makeAliasBinding(ExportableKind kind,
                                  const std::string &orig_name,
                                  const std::string &alias_name,
                                  SourceLocation loc) {
    switch (kind) {
        case ExportableKind::Const: {
            auto var = std::make_unique<ExprNode>();
            var->data = VariableExpr{orig_name};
            var->loc = loc;
            AssignStmt a;
            a.name = alias_name;
            a.value = std::move(var);
            a.loc = loc;
            return a;
        }
        case ExportableKind::Fn:
            return ImportAliasStmt{alias_name, orig_name,
                                   ImportAliasStmt::Kind::Fn, loc};
        case ExportableKind::Record:
            return ImportAliasStmt{alias_name, orig_name,
                                   ImportAliasStmt::Kind::Record, loc};
        case ExportableKind::Enum:
            return ImportAliasStmt{alias_name, orig_name,
                                   ImportAliasStmt::Kind::Enum, loc};
        case ExportableKind::TypeAlias:
            return ImportAliasStmt{alias_name, orig_name,
                                   ImportAliasStmt::Kind::TypeAlias, loc};
        case ExportableKind::Value:
        case ExportableKind::Directive:
            // Unreachable: rejectUnsupportedAlias filters these before
            // we reach the alias-emit path.
            throw std::runtime_error(
                "internal: makeAliasBinding invoked with unsupported kind for '" +
                alias_name + "'");
    }
    // Unreachable under a well-formed ExportableKind switch.
    throw std::runtime_error(
        "internal: makeAliasBinding fallthrough for '" + alias_name + "'");
}

// Validate that an `as` alias is only requested for a supported kind.
// Since #1725, alias is functional for `@const`, fn, record, enum, and
// type-alias kinds; only Value (non-`@const` assignment) and Directive remain
// rejected, both for semantic reasons documented in the issue scope.
static void rejectUnsupportedAlias(ExportableKind kind,
                                    const std::string &name,
                                    const std::string &import_path,
                                    int line) {
    switch (kind) {
        case ExportableKind::Const:
        case ExportableKind::Fn:
        case ExportableKind::Record:
        case ExportableKind::Enum:
        case ExportableKind::TypeAlias:
            return;
        case ExportableKind::Value:
        case ExportableKind::Directive: break;
    }
    std::string detail = "alias not supported for ";
    if (kind == ExportableKind::Value)
        detail += "non-@const assignment";
    else
        detail += "directive";
    detail += " '";
    detail += name;
    detail += "' from module '";
    detail += toUserFacingPath(import_path);
    detail += "': mutable globals and directives cannot be re-bound via 'as'";
    throw std::runtime_error(makeImportError(line, detail));
}

// Returns true when the definition carries an `@public` directive.
static bool isPublicDefinition(const StmtNode &stmt) {
    auto check = [](const std::vector<Directive> &directives) -> bool {
        for (const auto &d : directives)
            if (d.name == "public") return true;
        return false;
    };
    if (std::holds_alternative<std::unique_ptr<FnStmt>>(stmt))
        return check(std::get<std::unique_ptr<FnStmt>>(stmt)->directives);
    if (std::holds_alternative<RecordStmt>(stmt))
        return check(std::get<RecordStmt>(stmt).directives);
    if (std::holds_alternative<AssignStmt>(stmt))
        return check(std::get<AssignStmt>(stmt).directives);
    if (std::holds_alternative<DirectiveDefStmt>(stmt))
        return check(std::get<DirectiveDefStmt>(stmt).directives);
    if (std::holds_alternative<EnumStmt>(stmt))
        return check(std::get<EnumStmt>(stmt).directives);
    if (std::holds_alternative<TypeAliasStmt>(stmt))
        return check(std::get<TypeAliasStmt>(stmt).directives);
    return false;
}

// Extract exportable definitions from a program for codegen.
//
// Per #1560 (REQ-B3): every exportable definition is copied to `dest`
// regardless of `cross_package` or whether the name was requested, so a
// `@public` facade can transitively call non-`@public` helpers in the same
// JIT linkage unit.
//
// `cross_package` only narrows the named-import validation: when the importer
// writes `from foo import name`, a non-`@public` `name` from a different
// package is rejected. Wildcard imports (no requested names) emit no name
// validation.
//
// If `out_names` is non-null, every exportable name is recorded with its
// is_public flag for subsequent already-loaded import validation against the
// cache without re-parsing.
static void extractDefinitions(Program &source, Program &dest,
                                const std::vector<ImportName> &requested_items,
                                const std::string &import_path, int line,
                                bool cross_package, bool from_stdlib,
                                std::unordered_map<std::string, bool> *out_names = nullptr,
                                std::unordered_map<std::string, ExportableKind> *out_kinds = nullptr,
                                int file_id = 0,
                                std::unordered_set<int> *out_external_file_ids = nullptr) {
    // REQ-B3 (#1560): code availability is decoupled from name visibility.
    // Every exportable definition is copied to the importer's program so a
    // `@public` facade can call its non-`@public` helpers in the same JIT
    // linkage unit. Cross-package `@public` enforcement only narrows what
    // names the importer may write in `from foo import <name>` (validated
    // below), not what the codegen layer can resolve.
    std::unordered_set<std::string> requested;
    requested.reserve(requested_items.size());
    for (const auto &item : requested_items)
        requested.insert(item.name);
    std::unordered_map<std::string, bool> found;
    std::unordered_map<std::string, ExportableKind> found_kinds;
    // #2317: stdlib and cross-package definitions are inlined into the
    // importer's program with their original file_id. Track those so
    // CodeGen can suppress any-usage lint warnings on code the importer
    // cannot act on. Intra-package imports propagate file_ids unchanged
    // (no marking) so the user's multi-file project still gets warnings.
    const bool mark_external =
        out_external_file_ids != nullptr && (cross_package || from_stdlib);
    for (auto &stmt : source) {
        if (!isExportable(stmt)) continue;
        std::string name = getExportName(stmt);
        if (name.empty()) continue;
        bool is_pub = isPublicDefinition(stmt);
        ExportableKind kind = getExportableKind(stmt);
        if (out_names) (*out_names)[name] = is_pub;
        if (out_kinds) (*out_kinds)[name] = kind;
        if (requested.count(name)) {
            found[name] = is_pub;
            found_kinds[name] = kind;
        }
        if (mark_external) {
            int fid = getStmtFileId(stmt);
            if (fid >= 0) out_external_file_ids->insert(fid);
        }
        dest.push_back(std::move(stmt));
    }
    for (const auto &item : requested_items) {
        const std::string &name = item.name;
        auto it = found.find(name);
        if (it == found.end()) {
            if (isTestingIntrinsic(from_stdlib, import_path, name)) {
                if (item.alias.has_value()) {
                    std::string detail = "alias not supported for testing intrinsic '";
                    detail += name;
                    detail += "' from module '";
                    detail += toUserFacingPath(import_path);
                    detail += "': intrinsics cannot be re-bound (tracked in #1725)";
                    throw std::runtime_error(makeImportError(line, detail));
                }
                continue;
            }
            // #1888: C++-registered resource kind (File / TcpListener / ...).
            if (isCppResourceKind(from_stdlib, import_path, name)) {
                if (item.alias.has_value()) {
                    SourceLocation alias_loc{line, 1, file_id};
                    dest.push_back(makeAliasBinding(ExportableKind::TypeAlias,
                                                    name, *item.alias,
                                                    alias_loc));
                }
                continue;
            }
            // #1888: C++-registered builtin record (Match — see codegen.cpp:52).
            if (isCppBuiltinRecord(from_stdlib, import_path, name)) {
                if (item.alias.has_value()) {
                    SourceLocation alias_loc{line, 1, file_id};
                    dest.push_back(makeAliasBinding(ExportableKind::Record,
                                                    name, *item.alias,
                                                    alias_loc));
                }
                continue;
            }
            std::string detail = "'";
            detail += name;
            detail += "' not found in module '";
            detail += toUserFacingPath(import_path);
            detail += "'";
            throw std::runtime_error(makeImportError(line, detail));
        }
        if (cross_package && !it->second) {
            std::string detail = "cannot import '";
            detail += name;
            detail += "' from module '";
            detail += toUserFacingPath(import_path);
            detail += "': symbol is not @public";
            throw std::runtime_error(makeImportError(line, detail));
        }
        if (item.alias.has_value()) {
            ExportableKind kind = found_kinds[name];
            rejectUnsupportedAlias(kind, name, import_path, line);
            SourceLocation alias_loc{line, 1, file_id};
            dest.push_back(makeAliasBinding(kind, name, *item.alias, alias_loc));
        }
    }
}

ModuleLoader::ModuleLoader(const std::vector<std::string> &search_paths,
                           SourceManager *sm)
    : search_paths_(search_paths), sm_(sm) {
    if (const char *ry_path = std::getenv("RY_PATH")) {
        std::istringstream ss(ry_path);
        std::string path;
        while (std::getline(ss, path, ':')) {
            if (!path.empty())
                search_paths_.push_back(path);
        }
    }
}

bool ModuleLoader::isRyPath(const std::string &module_path) {
    // #2297: bare "ry" (length 2) is also a reserved-namespace reference.
    // The original predicate required `[2] == '/'` and silently let bare
    // forms (`import ry`, `from ry import X`) fall through to user-path
    // search, bypassing the namespace guard entirely.
    return module_path == "ry" ||
           (module_path.size() >= 3 && module_path[0] == 'r' &&
            module_path[1] == 'y' && module_path[2] == '/');
}

std::string ModuleLoader::canonicalStdlibName(const std::string &module_path) {
    // Non-`ry/` paths (legacy bare spellings: "math", "testing", "regex", ...)
    // are returned unchanged so the gating predicates remain backward
    // compatible with the existing `from math import` / `from testing import`
    // resolution paths.
    if (!isRyPath(module_path)) return module_path;
    // #2297: bare "ry" is rejected by `resolve()` before reaching here, but
    // guard substr(3) defensively — gating predicates call this from
    // outside the resolve flow and `"ry".substr(3)` would throw out_of_range.
    if (module_path == "ry") return "";
    std::string tail = module_path.substr(3);
    if (tail == "lang") return "std";  // ry.lang -> the prelude directory
    return tail;                       // ry.<sub> -> <sub>
}

std::string ModuleLoader::cachedCanonical(const std::string &raw, std::error_code &ec) {
    auto it = canonical_cache_.find(raw);
    if (it != canonical_cache_.end()) {
        ec = it->second.ec;
        return it->second.path;
    }
    std::string result = fs::canonical(fs::path(raw), ec).string();
    if (ec) result.clear();
    canonical_cache_[raw] = {result, ec};
    return result;
}

std::string ModuleLoader::cachedCanonical(const std::string &raw) {
    std::error_code ec;
    std::string result = cachedCanonical(raw, ec);
    if (ec)
        throw fs::filesystem_error("cachedCanonical", fs::path(raw), ec);
    return result;
}

std::string ModuleLoader::cachedCanonical(const fs::path &p, std::error_code &ec) {
    return cachedCanonical(p.string(), ec);
}

std::string ModuleLoader::cachedCanonical(const fs::path &p) {
    return cachedCanonical(p.string());
}

ModuleLoader::ResolvedPath ModuleLoader::resolve(const std::string &module_path,
                                    const std::string &referrer_dir) {
    SourceLocation loc{1, 1, 0};
    if (ry::traceEnabled())
        emitTraceEvent("import.resolve.start", "compile", &loc,
                       {TraceField("module_path", module_path),
                        TraceField("referrer_dir", referrer_dir)});
    // Reject path traversal and absolute paths
    if (module_path.find("..") != std::string::npos)
        throw std::runtime_error("invalid module path (path traversal): " + module_path);
    if (!module_path.empty() && module_path[0] == '/')
        throw std::runtime_error("invalid module path (absolute): " + module_path);

    // Check resolve cache
    std::string cache_key = module_path + '\0' + referrer_dir;
    auto rc_it = resolve_cache_.find(cache_key);
    if (rc_it != resolve_cache_.end()) {
        if (ry::traceEnabled())
            emitTraceEvent("import.resolve.cache_hit", "compile", &loc,
                           {TraceField("module_path", module_path),
                            TraceField("resolved_path", rc_it->second.path)});
        return rc_it->second;
    }

    auto try_resolve = [&](const std::string &dir,
                           const std::string &rel_path,
                           bool from_stdlib) -> ResolvedPath {
        std::error_code ec;
        std::string base_str = cachedCanonical(dir, ec);
        if (ec) return {};

        auto is_within_base = [&](const std::string &resolved) -> bool {
            if (resolved.compare(0, base_str.size(), base_str) != 0)
                return false;
            return resolved.size() == base_str.size() ||
                   resolved[base_str.size()] == '/';
        };

        // 1. Directory (module)
        fs::path dir_candidate = fs::path(dir) / rel_path;
        if (fs::is_directory(dir_candidate)) {
            std::string resolved = cachedCanonical(dir_candidate);
            if (resolved.empty() || !is_within_base(resolved)) return {};
            return {resolved, /*is_directory=*/true, from_stdlib};
        }

        // 2. Single file (backward compatibility)
        fs::path file_candidate = fs::path(dir) / (rel_path + ".ry");
        if (fs::is_regular_file(file_candidate)) {
            std::string resolved = cachedCanonical(file_candidate);
            if (resolved.empty() || !is_within_base(resolved)) return {};
            return {resolved, /*is_directory=*/false, from_stdlib};
        }

        return {};
    };

    // #1769: `ry/*` reserved-namespace intercept. The path is remapped to the
    // physical stdlib name (`ry/lang` -> `std`, `ry/<sub>` -> `<sub>`) and
    // resolved against `search_paths_` ONLY — never `referrer_dir`. This
    // guarantees that a project-local `ry/` directory cannot shadow the
    // bundled stdlib. The original spelling is preserved in the cache key
    // and in any error message. If the referrer directory contains a local
    // `ry/` directory or `ry.ry` file, record a one-time warning so the
    // user is alerted to the future reservation conflict; the probe itself
    // is cached per-referrer so a file with many `ry.*` imports does not
    // re-stat the same directory.
    if (isRyPath(module_path)) {
        // Shadow probe: bare "ry" 形式でも発火させる (#2297) — 一貫した警告で
        // ユーザーに将来の予約衝突を知らせる。bare reject よりも前に push する
        // ことで、jit_runner の catch flush 経由で stderr に出る。
        if (!referrer_dir.empty() &&
            probed_ry_shadow_dirs_.insert(referrer_dir).second) {
            std::error_code shadow_ec;
            const fs::path local_ry_dir = fs::path(referrer_dir) / "ry";
            const fs::path local_ry_file = fs::path(referrer_dir) / "ry.ry";
            if (fs::is_directory(local_ry_dir, shadow_ec) ||
                fs::is_regular_file(local_ry_file, shadow_ec)) {
                loader_warnings_.push_back(
                    "warning: 'ry' is a reserved stdlib namespace; "
                    "the local 'ry' module in '" + referrer_dir +
                    "' is ignored and will become an error in a future release");
            }
        }

        // #2297: bare `import ry` / `from ry import X` は無効 — `ry` は予約
        // namespace で top-level export を持たないため、`ry.<module>` 形式を
        // 要求する rejection で失敗させる。
        if (module_path == "ry") {
            if (ry::traceEnabled())
                emitTraceEvent("import.resolve.error", "compile", &loc,
                               {TraceField("module_path", module_path),
                                TraceField("detail", "bare 'ry' rejected")});
            throw std::runtime_error(
                "'ry' is the reserved stdlib namespace; "
                "use 'ry.<module>' (e.g. 'ry.lang', 'ry.math')");
        }

        // #2297: stdlib allowlist — `ry/<sub>` の `<sub>` が #1769 列挙の 13
        // module のいずれでなければ reject。`ry.builtins` / `ry.gc` / `ry.core`
        // 等の internal module だけでなく、`ry.math.internal` のようなネスト
        // パスも reject する (public surface は **完全な enumeration** であり、
        // public module の下に深い public path は存在しない)。
        const std::string public_module = module_path.substr(3);
        if (public_module.find('/') != std::string::npos ||
            !isPublicRyModule(public_module)) {
            if (ry::traceEnabled())
                emitTraceEvent("import.resolve.error", "compile", &loc,
                               {TraceField("module_path", module_path),
                                TraceField("detail", "internal stdlib module rejected")});
            throw std::runtime_error(
                "'" + toRyDotForm(module_path) +
                "' is not a public stdlib module; "
                "available: " + publicRyModulesListForError());
        }

        const std::string remapped = canonicalStdlibName(module_path);
        for (const auto &dir : search_paths_) {
            ResolvedPath rp = try_resolve(dir, remapped, /*from_stdlib=*/true);
            if (!rp.path.empty()) {
                if (ry::traceEnabled())
                    emitTraceEvent("import.resolve.hit", "compile", &loc,
                                   {TraceField("module_path", module_path),
                                    TraceField("resolved_path", rp.path)});
                resolve_cache_[cache_key] = rp;
                return rp;
            }
        }
        if (ry::traceEnabled())
            emitTraceEvent("import.resolve.error", "compile", &loc,
                           {TraceField("module_path", module_path),
                            TraceField("detail", "module not found")});
        // #2297: ry/* の "module not found" はユーザーが書いた dot 表記で返す。
        throw std::runtime_error("module not found: " + toRyDotForm(module_path));
    }

    // Relative import: "." or "./submod" — resolve only against referrer_dir
    bool is_relative = (module_path == ".") ||
                       (module_path.size() >= 2 &&
                        module_path[0] == '.' && module_path[1] == '/');
    if (is_relative) {
        if (referrer_dir.empty())
            throw std::runtime_error("relative import requires a referrer directory");
        if (module_path == ".") {
            std::error_code ec;
            std::string resolved = cachedCanonical(referrer_dir, ec);
            if (ec)
                throw std::runtime_error("relative import directory not found: " +
                                         referrer_dir);
            if (ry::traceEnabled()) emitTraceEvent("import.resolve.hit", "compile", &loc,
                           {TraceField("module_path", module_path),
                            TraceField("resolved_path", resolved)});
            ResolvedPath rp{resolved, true};
            resolve_cache_[cache_key] = rp;
            return rp;
        }
        std::string rel = module_path.substr(2);
        ResolvedPath result = try_resolve(referrer_dir, rel, /*from_stdlib=*/false);
        if (!result.path.empty()) {
            if (ry::traceEnabled()) emitTraceEvent("import.resolve.hit", "compile", &loc,
                           {TraceField("module_path", module_path),
                            TraceField("resolved_path", result.path)});
            resolve_cache_[cache_key] = result;
            return result;
        }
        if (ry::traceEnabled()) emitTraceEvent("import.resolve.error", "compile", &loc,
                       {TraceField("module_path", module_path),
                        TraceField("detail", "module not found")});
        throw std::runtime_error("module not found: " + module_path);
    }

    // Absolute import: search referrer_dir first, then search_paths
    ResolvedPath result = try_resolve(referrer_dir, module_path, /*from_stdlib=*/false);
    if (!result.path.empty()) {
        if (ry::traceEnabled()) emitTraceEvent("import.resolve.hit", "compile", &loc,
                       {TraceField("module_path", module_path),
                        TraceField("resolved_path", result.path)});
        resolve_cache_[cache_key] = result;
        return result;
    }

    for (const auto &dir : search_paths_) {
        result = try_resolve(dir, module_path, /*from_stdlib=*/true);
        if (!result.path.empty()) {
            if (ry::traceEnabled()) emitTraceEvent("import.resolve.hit", "compile", &loc,
                           {TraceField("module_path", module_path),
                            TraceField("resolved_path", result.path)});
            resolve_cache_[cache_key] = result;
            return result;
        }
    }

    if (ry::traceEnabled()) emitTraceEvent("import.resolve.error", "compile", &loc,
                   {TraceField("module_path", module_path),
                    TraceField("detail", "module not found")});
    throw std::runtime_error("module not found: " + module_path);
}

Program loadAndParse(const std::string &abs_path, SourceManager *sm) {
    std::ifstream file(abs_path);
    if (!file.is_open())
        throw std::runtime_error("cannot open file: " + abs_path);

    std::ostringstream ss;
    ss << file.rdbuf();
    std::string src = ss.str();

    SourceLocation loc{1, 1, 0};
    if (ry::traceEnabled()) emitTraceEvent("import.load.file", "compile", &loc,
                   {TraceField("resolved_path", abs_path),
                    TraceField("size_bytes", static_cast<int64_t>(src.size()))});

    int fileId = 0;
    if (sm) {
        fileId = sm->addSource(abs_path, src);
        loc.file_id = fileId;
    }

    Lexer lex(src);
    Parser parser(lex, sm, fileId);
    if (ry::traceEnabled()) emitTraceEvent("parse.start", "compile", &loc,
                   {TraceField("file", abs_path)});
    try {
        Program prog = parser.parseProgram();
        if (ry::traceEnabled()) emitTraceEvent("parse.done", "compile", &loc,
                       {TraceField("file", abs_path)});
        return prog;
    } catch (const DiagnosticError &e) {
        emitTraceDiagnostic("parse.error", "compile", &e.diagnostic().loc, e.diagnostic().message);
        throw;
    }
}

std::optional<std::string> ModuleLoader::packageRootOfDir(const std::string &dir) {
    if (dir.empty()) return std::nullopt;
    auto it = pkg_root_cache_.find(dir);
    if (it != pkg_root_cache_.end()) return it->second;
    // Canonicalize before comparing — macOS resolves /tmp via /private, so the
    // referrer directory and the importee's directory may otherwise share a
    // root by inode but differ by string. fs::canonical() resolves symlinks.
    std::error_code ec;
    std::string canon_dir = cachedCanonical(dir, ec);
    std::optional<std::string> result;
    if (!ec && !canon_dir.empty()) {
        if (auto root = findProjectRoot(canon_dir); root.has_value()) {
            std::error_code ec2;
            std::string canon_root = cachedCanonical(*root, ec2);
            result = (!ec2 && !canon_root.empty()) ? canon_root : *root;
        }
    }
    pkg_root_cache_[dir] = result;
    return result;
}

bool ModuleLoader::isCrossPackage(const std::string &caller_dir,
                                  const std::string &target_dir) {
    auto caller_root = packageRootOfDir(caller_dir);
    auto target_root = packageRootOfDir(target_dir);
    if (!caller_root.has_value() && !target_root.has_value())
        return false; // both unrooted ⇒ same anonymous package
    if (caller_root.has_value() != target_root.has_value())
        return true;
    return *caller_root != *target_root;
}

void ModuleLoader::invalidateLoaded(const std::string &abs_path, bool is_directory) {
    loaded_.erase(abs_path);
    qualified_only_user_loaded_.erase(abs_path);
    if (!is_directory) return;
    // `loadModuleDir` inserts each contained `.ry` file's canonical path
    // into `loaded_` independently from the directory path itself. If we
    // only erased the directory entry, the next `loadModuleDir` would
    // observe the per-file `loaded_.count(file_path)` guard at line 546
    // and silently skip every file — leaving the re-extract destination
    // empty. Walk the directory and clear those entries too.
    std::error_code dec;
    fs::directory_iterator dit(abs_path, dec);
    if (dec) return;
    for (const auto &entry : dit) {
        if (!entry.is_regular_file()) continue;
        auto filename = entry.path().filename().string();
        if (filename.size() < 3 || filename.compare(filename.size() - 3, 3, ".ry") != 0) continue;
        if (filename.size() >= 8 && filename.compare(filename.size() - 8, 8, ".test.ry") == 0) continue;
        std::error_code ec;
        std::string canonical = cachedCanonical(entry.path(), ec);
        if (canonical.empty()) continue;
        loaded_.erase(canonical);
    }
}

Program ModuleLoader::loadModuleDir(const std::string &abs_dir_path) {
    Program collected;
    std::vector<std::string> ry_files;
    SourceLocation loc{1, 1, 0};
    if (ry::traceEnabled()) emitTraceEvent("import.load.dir", "compile", &loc,
                   {TraceField("resolved_path", abs_dir_path)});

    for (const auto &entry : fs::directory_iterator(abs_dir_path)) {
        if (!entry.is_regular_file()) continue;
        auto filename = entry.path().filename().string();
        if (filename.size() < 3 || filename.compare(filename.size() - 3, 3, ".ry") != 0) continue;
        // Exclude test files from module loading
        if (filename.size() >= 8 && filename.compare(filename.size() - 8, 8, ".test.ry") == 0) continue;
        std::string canonical = cachedCanonical(entry.path());
        if (!canonical.empty())
            ry_files.push_back(std::move(canonical));
    }

    std::sort(ry_files.begin(), ry_files.end());

    for (const auto &file_path : ry_files) {
        if (loaded_.count(file_path)) continue;

        loading_.insert(file_path);
        auto sub_prog = loadAndParse(file_path, sm_);
        std::string sub_dir = fs::path(file_path).parent_path().string();
        sub_prog = resolveImports(sub_prog, sub_dir);
        loading_.erase(file_path);
        loaded_.insert(file_path);

        // Internal merge of files within a directory module: every file in the
        // directory belongs to the same package by construction, so no
        // visibility filtering is applied here. The per-file cache records
        // each definition's is_public flag for later same-vs-cross-package
        // decisions made at the import site.
        extractDefinitions(sub_prog, collected, {}, "", 0, /*cross_package=*/false,
                           /*from_stdlib=*/false,
                           &exports_cache_[file_path],
                           &exports_kinds_cache_[file_path],
                           /*file_id=*/0);
    }

    return collected;
}

Program ModuleLoader::resolveImports(Program &prog, const std::string &referrer_dir) {
    Program result;
    // Track canonical resolved paths (`abs_path` from ResolvedPath) whose
    // `QualifiedImportStmt::definitions` have already been populated in this
    // resolveImports run. The codegen-side `module_namespaces_` bucket is
    // keyed by canonical name (#1730 alias indirection), so only the FIRST
    // qimp per canonical may carry decls — any subsequent qimp for the same
    // canonical (whether bare or aliased) must leave `definitions` empty,
    // otherwise `forwardDeclareFunctions` walks both and `declareFunction`
    // rejects the second emit with "fn 'X' is already defined with the same
    // signature". Mixed-form scenarios that hit this: `import M` → `import M
    // as X` → `from M import y` → `import M as Z` — step 3's inline
    // invalidates qualified_only_user_loaded_, which then re-enables step
    // 4's force-populate branch and re-extracts every decl into the second
    // qimp's definitions. Keyed by `abs_path` (not raw `qimp.module_name`)
    // so equivalent imports of the same module via different spelling
    // (relative vs absolute, symlink) share dedup state.
    std::unordered_set<std::string> canonicals_with_populated_qimp;

    for (auto &stmt : prog) {
        if (std::holds_alternative<std::unique_ptr<QualifiedImportStmt>>(stmt)) {
            auto &qimp = *std::get<std::unique_ptr<QualifiedImportStmt>>(stmt);

            // #1769: dotted qualified imports (`import ry.math`) carry the full
            // slash-separated path in `import_path`; bare imports leave it empty.
            const std::string &resolve_path = qimp.import_path.empty()
                                                  ? qimp.module_name
                                                  : qimp.import_path;
            ResolvedPath rp = resolve(resolve_path, referrer_dir);
            const std::string &abs_path = rp.path;

            // #2351: legacy `import math` 形式は hard error。`rp.from_stdlib` の
            // 確認後に検出するため、user-defined module (referrer dir に同名
            // ファイルが存在し `from_stdlib=false` となるケース) は影響を受けない。
            if (rp.from_stdlib) {
                if (auto canon = tryLegacyToCanonical(resolve_path)) {
                    throw std::runtime_error(makeImportError(
                        qimp.loc.line,
                        "'import " + toRyDotForm(resolve_path) +
                        "' is a legacy stdlib import form and is no longer "
                        "supported; use 'import " + *canon + "' instead"));
                }
            }

            // Propagate stdlib origin to codegen — Task 9 uses this to gate
            // qualified-call routing (stdlib → existing dispatch chain;
            // user-defined → "not yet supported" error in v0.0.23).
            qimp.is_stdlib = rp.from_stdlib;

            std::string target_dir = rp.is_directory ? abs_path
                                                     : fs::path(abs_path).parent_path().string();
            bool cross_package = isCrossPackage(referrer_dir, target_dir);

            if (loading_.count(abs_path))
                throw std::runtime_error("circular import detected: " + abs_path);

            // Cache-hit for user-defined modules that were previously loaded
            // ONLY via `from <mod> import x` (i.e. the module's decls landed
            // in the top-level `result`, not in any prior
            // `QualifiedImportStmt::definitions`). Without re-extracting,
            // this `import <mod>` would leave `qimp.definitions` empty and
            // codegen would create an empty `module_namespaces_[...]` bucket
            // — the subsequent `<mod>.foo()` call would then fail with
            // "module '<mod>' has no function or record 'foo'" (#1730
            // CodeRabbit Bug 2). Invalidate to force the cache-miss path
            // below to repopulate `qimp.definitions`. Stdlib is excluded
            // because its decls remain valid in `result` from the prior
            // `from <stdlib> import` and codegen routes stdlib qualified
            // calls through the regular dispatch chain.
            //
            // For user-defined modules previously loaded via `import <mod>`
            // (qualified-only), `qualified_only_user_loaded_` is set; codegen
            // reuses the bucket keyed by canonical module name (#1730 codegen
            // alias indirection) so a second `import <mod> [as <alias>]`
            // does NOT need its own `qimp.definitions` — fast cache-hit path.
            // If a prior qimp in this Program already populated the canonical
            // bucket, suppress the force-populate path — codegen will register
            // any alias on this stmt via `effective_to_canonical_` and reuse
            // the already-populated bucket. Without this guard, mixed-form
            // input would re-extract the same decls into a second
            // `qimp.definitions`, producing duplicate-signature errors at
            // forward-declare time.
            bool canonical_already_populated = !rp.from_stdlib &&
                canonicals_with_populated_qimp.count(abs_path) > 0;

            if (!rp.from_stdlib && loaded_.count(abs_path) &&
                !qualified_only_user_loaded_.count(abs_path) &&
                !canonical_already_populated) {
                invalidateLoaded(abs_path, rp.is_directory);
                // exports_cache_ / exports_kinds_cache_ remain populated;
                // they will be refreshed by the upcoming
                // extractDefinitions call below.
            }

            if (loaded_.count(abs_path)) {
                // exports_cache_ already populated by an earlier import of the
                // same module (AC2: `import math` + `from math import PI`).
                // Just push the QualifiedImportStmt so codegen can register the
                // module in module_namespaces_.
                result.push_back(std::move(stmt));
                continue;
            }

            // First-time load: inline all exportable definitions (same path as
            // `from xxx import` with empty names) so that:
            //   1. @native fn signatures get registered in native_fn_sigs_
            //      (required for stdlib qualified-call dispatch).
            //   2. exports_cache_ / exports_kinds_cache_ get populated for any
            //      subsequent `from xxx import` referencing the same module.
            //
            // For user-defined modules (`!rp.from_stdlib`, #1730), route
            // extracted definitions to `qimp.definitions` so codegen can emit
            // them under a namespace bucket keyed by the effective alias
            // rather than the top-level Program. Stdlib path stays unchanged:
            // native-fn registration in CodeGen still relies on inlining.
            Program &push_target = rp.from_stdlib ? result : qimp.definitions;
            if (!rp.from_stdlib) {
                qualified_only_user_loaded_.insert(abs_path);
                canonicals_with_populated_qimp.insert(abs_path);
            }
            if (rp.is_directory) {
                loading_.insert(abs_path);
                Program dir_prog = loadModuleDir(abs_path);
                loading_.erase(abs_path);
                loaded_.insert(abs_path);

                extractDefinitions(dir_prog, push_target, /*requested_items=*/{},
                                   qimp.module_name, qimp.loc.line, cross_package,
                                   rp.from_stdlib,
                                   &exports_cache_[abs_path],
                                   &exports_kinds_cache_[abs_path],
                                   qimp.loc.file_id,
                                   &external_file_ids_);
            } else {
                loading_.insert(abs_path);
                auto sub_prog = loadAndParse(abs_path, sm_);
                std::string sub_dir = fs::path(abs_path).parent_path().string();
                sub_prog = resolveImports(sub_prog, sub_dir);
                loading_.erase(abs_path);
                loaded_.insert(abs_path);

                extractDefinitions(sub_prog, push_target, /*requested_items=*/{},
                                   qimp.module_name, qimp.loc.line, cross_package,
                                   rp.from_stdlib,
                                   &exports_cache_[abs_path],
                                   &exports_kinds_cache_[abs_path],
                                   qimp.loc.file_id,
                                   &external_file_ids_);
            }

            result.push_back(std::move(stmt));
            continue;
        }

        if (!std::holds_alternative<ImportStmt>(stmt)) {
            result.push_back(std::move(stmt));
            continue;
        }

        auto &imp = std::get<ImportStmt>(stmt);

        ResolvedPath rp = resolve(imp.module_path, referrer_dir);
        const std::string &abs_path = rp.path;

        // #2351: legacy `from math import …` / `from std.math import …` /
        // `from std import …` 形式は hard error。`rp.from_stdlib` の確認後に
        // 検出するため、user-defined module (referrer dir に同名ファイルが
        // 存在し `from_stdlib=false` となるケース) は影響を受けない。
        if (rp.from_stdlib) {
            if (auto canon = tryLegacyToCanonical(imp.module_path)) {
                throw std::runtime_error(makeImportError(
                    imp.loc.line,
                    "'from " + toRyDotForm(imp.module_path) +
                    " import …' is a legacy stdlib import form and is no longer "
                    "supported; use 'from " + *canon + " import …' instead"));
            }
        }

        // Record testing-module intrinsics observed in the import statement
        // ONLY when the import resolved to the stdlib testing module. A
        // project-local `testing.ry` shadow must continue to fail with the
        // existing `'<name>' not found in module 'testing'` diagnostic — its
        // intrinsic names are not implicitly imported here either.
        if (rp.from_stdlib &&
            ModuleLoader::canonicalStdlibName(imp.module_path) == "testing") {
            if (imp.names.empty()) {
                for (const auto &name : testingIntrinsics())
                    imported_testing_intrinsics_.insert(name);
            } else {
                for (const auto &item : imp.names) {
                    if (testingIntrinsics().count(item.name) > 0)
                        imported_testing_intrinsics_.insert(item.name);
                }
            }
        }

        std::string target_dir = rp.is_directory ? abs_path
                                                 : fs::path(abs_path).parent_path().string();
        bool cross_package = isCrossPackage(referrer_dir, target_dir);

        if (loading_.count(abs_path))
            throw std::runtime_error("circular import detected: " + abs_path);

        // If the module was previously loaded only via `import <mod>` to a
        // user-defined module (#1730), its decls reside in the prior
        // QualifiedImportStmt::definitions and were not inlined to the
        // top-level Program. To honor a subsequent `from <mod> import x`
        // (AC3), re-parse and re-extract so the requested names reach
        // `result`. `invalidateLoaded` walks directory modules too so the
        // per-file `loaded_` entries that `loadModuleDir` adds are also
        // dropped — otherwise the directory-walk would skip every file.
        if (qualified_only_user_loaded_.count(abs_path) && !imp.names.empty()) {
            invalidateLoaded(abs_path, rp.is_directory);
            // exports_cache_ / exports_kinds_cache_ remain populated; they
            // get refreshed by the upcoming extractDefinitions call below
            // (cache-miss path).
        }

        if (loaded_.count(abs_path)) {
            if (!imp.names.empty()) {
                auto &exports = exports_cache_[abs_path];
                auto &kinds = exports_kinds_cache_[abs_path];
                for (const auto &item : imp.names) {
                    const std::string &name = item.name;
                    auto it = exports.find(name);
                    if (it == exports.end()) {
                        if (isTestingIntrinsic(rp.from_stdlib, imp.module_path, name)) {
                            if (item.alias.has_value()) {
                                std::string detail = "alias not supported for testing intrinsic '";
                                detail += name;
                                detail += "' from module '";
                                detail += toUserFacingPath(imp.module_path);
                                detail += "': intrinsics cannot be re-bound (tracked in #1725)";
                                throw std::runtime_error(makeImportError(imp.loc.line, detail));
                            }
                            continue;
                        }
                        // #1888: C++-registered resource kind cache-hit branch.
                        if (isCppResourceKind(rp.from_stdlib, imp.module_path, name)) {
                            if (item.alias.has_value()) {
                                SourceLocation alias_loc{imp.loc.line, 1, imp.loc.file_id};
                                result.push_back(makeAliasBinding(
                                    ExportableKind::TypeAlias, name,
                                    *item.alias, alias_loc));
                            }
                            continue;
                        }
                        // #1888: C++-registered builtin record cache-hit branch.
                        if (isCppBuiltinRecord(rp.from_stdlib, imp.module_path, name)) {
                            if (item.alias.has_value()) {
                                SourceLocation alias_loc{imp.loc.line, 1, imp.loc.file_id};
                                result.push_back(makeAliasBinding(
                                    ExportableKind::Record, name,
                                    *item.alias, alias_loc));
                            }
                            continue;
                        }
                        std::string detail = "'";
                        detail += name;
                        detail += "' not found in module '";
                        detail += toUserFacingPath(imp.module_path);
                        detail += "'";
                        throw std::runtime_error(makeImportError(imp.loc.line, detail));
                    }
                    if (cross_package && !it->second) {
                        std::string detail = "cannot import '";
                        detail += name;
                        detail += "' from module '";
                        detail += toUserFacingPath(imp.module_path);
                        detail += "': symbol is not @public";
                        throw std::runtime_error(makeImportError(imp.loc.line, detail));
                    }
                    if (item.alias.has_value()) {
                        auto k_it = kinds.find(name);
                        ExportableKind kind = (k_it != kinds.end()) ? k_it->second
                                                                    : ExportableKind::Fn;
                        rejectUnsupportedAlias(kind, name, imp.module_path, imp.loc.line);
                        SourceLocation alias_loc{imp.loc.line, 1, imp.loc.file_id};
                        result.push_back(makeAliasBinding(kind, name, *item.alias, alias_loc));
                    }
                }
            }
            continue;
        }

        if (rp.is_directory) {
            loading_.insert(abs_path);
            Program dir_prog = loadModuleDir(abs_path);
            loading_.erase(abs_path);
            loaded_.insert(abs_path);

            extractDefinitions(dir_prog, result, imp.names, imp.module_path,
                               imp.loc.line, cross_package, rp.from_stdlib,
                               &exports_cache_[abs_path],
                               &exports_kinds_cache_[abs_path],
                               imp.loc.file_id,
                               &external_file_ids_);
        } else {
            loading_.insert(abs_path);
            auto sub_prog = loadAndParse(abs_path, sm_);
            std::string sub_dir = fs::path(abs_path).parent_path().string();
            sub_prog = resolveImports(sub_prog, sub_dir);
            loading_.erase(abs_path);
            loaded_.insert(abs_path);

            extractDefinitions(sub_prog, result, imp.names, imp.module_path,
                               imp.loc.line, cross_package, rp.from_stdlib,
                               &exports_cache_[abs_path],
                               &exports_kinds_cache_[abs_path],
                               imp.loc.file_id,
                               &external_file_ids_);
        }
    }

    return result;
}

} // namespace ry
