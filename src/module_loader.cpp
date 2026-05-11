#include "ry/module_loader.hpp"
#include "ry/lexer.hpp"
#include "ry/parser.hpp"
#include "ry/diagnostic.hpp"
#include "ry/project_config.hpp"
#include "ry/trace.hpp"
#include <algorithm>
#include <cstdlib>
#include <filesystem>
#include <fstream>
#include <sstream>
#include <stdexcept>


namespace ry {

namespace fs = std::filesystem;
using ry::TraceField;
using ry::emitTraceDiagnostic;
using ry::emitTraceEvent;

// Check if a statement is an exportable definition
static bool isExportable(const StmtNode &stmt) {
    return std::holds_alternative<std::unique_ptr<FnStmt>>(stmt) ||
           std::holds_alternative<RecordStmt>(stmt) ||
           std::holds_alternative<EnumStmt>(stmt) ||
           std::holds_alternative<TypeAliasStmt>(stmt) ||
           std::holds_alternative<AssignStmt>(stmt) ||
           std::holds_alternative<DirectiveDefStmt>(stmt);
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
// statement form; `mock` is recognized at codegen) or are wrapped by codegen
// synth (`fail` — `codegen_call_user.cpp:435` synthesizes the __LINE__-
// equivalent argument before delegating to the user-fn `fail` defined in
// `share/std/testing/testing.ry`). Listing them here lets
// `from testing import expect / mock / fail` resolve without producing AST
// nodes — the allow-list bypass below skips the `'<name>' not found` throw
// without altering anything else.
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
        "expect", "mock", "fail", "verifyCalledWith"};
    return kSet;
}

// Gate the intrinsic bypass on `from_stdlib` so a project-local `testing.ry`
// shadow (resolved via `referrer_dir`) does NOT silently absorb the
// intrinsic names. Only the stdlib testing module gets the bypass; a local
// shadow continues to fail with the existing `'<name>' not found` diagnostic.
static bool isTestingIntrinsic(bool from_stdlib,
                               const std::string &module_path,
                               const std::string &name) {
    return from_stdlib && module_path == "testing" &&
           testingIntrinsics().count(name) > 0;
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

// Build a synthesized binding `alias = orig` for `@const` symbols so the
// importer's scope sees `alias` as an additional name pointing at the same
// underlying value. Only `@const` alias is supported in this release:
// fn / record / enum / type-alias alias requires codegen-side name-resolution
// fallback that is not yet implemented (tracked in follow-up issue #1725).
// The caller MUST reject non-Const kinds before reaching here; this function
// asserts that contract via the switch default.
static StmtNode makeAliasBinding(ExportableKind kind,
                                  const std::string &orig_name,
                                  const std::string &alias_name,
                                  SourceLocation loc) {
    (void)kind; // contract: caller guarantees ExportableKind::Const
    auto var = std::make_unique<ExprNode>();
    var->data = VariableExpr{orig_name};
    var->loc = loc;
    AssignStmt a;
    a.name = alias_name;
    a.value = std::move(var);
    a.loc = loc;
    return a;
}

// Validate that an `as` alias is only requested for a supported kind.
// In v0.0.23 (#1721) only `@const` alias is functional; the other kinds
// (fn / record / enum / type alias) parse and reach the loader, but emitting
// a binding that survives codegen requires the follow-up work tracked in
// #1725. Throw a descriptive error so the user understands what to do.
static void rejectUnsupportedAlias(ExportableKind kind,
                                    const std::string &name,
                                    const std::string &import_path,
                                    int line) {
    if (kind == ExportableKind::Const) return;
    std::string detail = "alias not supported for ";
    switch (kind) {
        case ExportableKind::Fn:        detail += "function";              break;
        case ExportableKind::Value:     detail += "non-@const assignment"; break;
        case ExportableKind::Record:    detail += "record";                break;
        case ExportableKind::Enum:      detail += "enum";                  break;
        case ExportableKind::TypeAlias: detail += "type alias";            break;
        case ExportableKind::Directive: detail += "directive";             break;
        case ExportableKind::Const:     return;
    }
    detail += " '";
    detail += name;
    detail += "' from module '";
    detail += import_path;
    detail += "': only @const aliases are supported in v0.0.23 (tracked in #1725)";
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
                                int file_id = 0) {
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
                    detail += import_path;
                    detail += "': intrinsics cannot be re-bound (tracked in #1725)";
                    throw std::runtime_error(makeImportError(line, detail));
                }
                continue;
            }
            std::string detail = "'";
            detail += name;
            detail += "' not found in module '";
            detail += import_path;
            detail += "'";
            throw std::runtime_error(makeImportError(line, detail));
        }
        if (cross_package && !it->second) {
            std::string detail = "cannot import '";
            detail += name;
            detail += "' from module '";
            detail += import_path;
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

    for (auto &stmt : prog) {
        if (std::holds_alternative<QualifiedImportStmt>(stmt)) {
            auto &qimp = std::get<QualifiedImportStmt>(stmt);

            ResolvedPath rp = resolve(qimp.module_name, referrer_dir);
            const std::string &abs_path = rp.path;

            // Propagate stdlib origin to codegen — Task 9 uses this to gate
            // qualified-call routing (stdlib → existing dispatch chain;
            // user-defined → "not yet supported" error in v0.0.23).
            qimp.is_stdlib = rp.from_stdlib;

            std::string target_dir = rp.is_directory ? abs_path
                                                     : fs::path(abs_path).parent_path().string();
            bool cross_package = isCrossPackage(referrer_dir, target_dir);

            if (loading_.count(abs_path))
                throw std::runtime_error("circular import detected: " + abs_path);

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
            //      (required for stdlib qualified-call dispatch in Task 9).
            //   2. exports_cache_ / exports_kinds_cache_ get populated for any
            //      subsequent `from xxx import` referencing the same module.
            if (rp.is_directory) {
                loading_.insert(abs_path);
                Program dir_prog = loadModuleDir(abs_path);
                loading_.erase(abs_path);
                loaded_.insert(abs_path);

                extractDefinitions(dir_prog, result, /*requested_items=*/{},
                                   qimp.module_name, qimp.loc.line, cross_package,
                                   rp.from_stdlib,
                                   &exports_cache_[abs_path],
                                   &exports_kinds_cache_[abs_path],
                                   qimp.loc.file_id);
            } else {
                loading_.insert(abs_path);
                auto sub_prog = loadAndParse(abs_path, sm_);
                std::string sub_dir = fs::path(abs_path).parent_path().string();
                sub_prog = resolveImports(sub_prog, sub_dir);
                loading_.erase(abs_path);
                loaded_.insert(abs_path);

                extractDefinitions(sub_prog, result, /*requested_items=*/{},
                                   qimp.module_name, qimp.loc.line, cross_package,
                                   rp.from_stdlib,
                                   &exports_cache_[abs_path],
                                   &exports_kinds_cache_[abs_path],
                                   qimp.loc.file_id);
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

        // Record testing-module intrinsics observed in the import statement
        // ONLY when the import resolved to the stdlib testing module. A
        // project-local `testing.ry` shadow must continue to fail with the
        // existing `'<name>' not found in module 'testing'` diagnostic — its
        // intrinsic names are not implicitly imported here either.
        if (rp.from_stdlib && imp.module_path == "testing") {
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
                                detail += imp.module_path;
                                detail += "': intrinsics cannot be re-bound (tracked in #1725)";
                                throw std::runtime_error(makeImportError(imp.loc.line, detail));
                            }
                            continue;
                        }
                        std::string detail = "'";
                        detail += name;
                        detail += "' not found in module '";
                        detail += imp.module_path;
                        detail += "'";
                        throw std::runtime_error(makeImportError(imp.loc.line, detail));
                    }
                    if (cross_package && !it->second) {
                        std::string detail = "cannot import '";
                        detail += name;
                        detail += "' from module '";
                        detail += imp.module_path;
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
                               imp.loc.file_id);
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
                               imp.loc.file_id);
        }
    }

    return result;
}

} // namespace ry
