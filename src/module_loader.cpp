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
                                const std::vector<std::string> &requested_names,
                                const std::string &import_path, int line,
                                bool cross_package,
                                std::unordered_map<std::string, bool> *out_names = nullptr) {
    // REQ-B3 (#1560): code availability is decoupled from name visibility.
    // Every exportable definition is copied to the importer's program so a
    // `@public` facade can call its non-`@public` helpers in the same JIT
    // linkage unit. Cross-package `@public` enforcement only narrows what
    // names the importer may write in `from foo import <name>` (validated
    // below), not what the codegen layer can resolve.
    std::unordered_set<std::string> requested(requested_names.begin(), requested_names.end());
    std::unordered_map<std::string, bool> found;
    for (auto &stmt : source) {
        if (!isExportable(stmt)) continue;
        std::string name = getExportName(stmt);
        if (name.empty()) continue;
        bool is_pub = isPublicDefinition(stmt);
        if (out_names) (*out_names)[name] = is_pub;
        if (requested.count(name)) found[name] = is_pub;
        dest.push_back(std::move(stmt));
    }
    for (const auto &name : requested_names) {
        auto it = found.find(name);
        if (it == found.end()) {
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
                           const std::string &rel_path) -> ResolvedPath {
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
            return {resolved, true};
        }

        // 2. Single file (backward compatibility)
        fs::path file_candidate = fs::path(dir) / (rel_path + ".ry");
        if (fs::is_regular_file(file_candidate)) {
            std::string resolved = cachedCanonical(file_candidate);
            if (resolved.empty() || !is_within_base(resolved)) return {};
            return {resolved, false};
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
        ResolvedPath result = try_resolve(referrer_dir, rel);
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
    ResolvedPath result = try_resolve(referrer_dir, module_path);
    if (!result.path.empty()) {
        if (ry::traceEnabled()) emitTraceEvent("import.resolve.hit", "compile", &loc,
                       {TraceField("module_path", module_path),
                        TraceField("resolved_path", result.path)});
        resolve_cache_[cache_key] = result;
        return result;
    }

    for (const auto &dir : search_paths_) {
        result = try_resolve(dir, module_path);
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
                           &exports_cache_[file_path]);
    }

    return collected;
}

Program ModuleLoader::resolveImports(Program &prog, const std::string &referrer_dir) {
    Program result;

    for (auto &stmt : prog) {
        if (!std::holds_alternative<ImportStmt>(stmt)) {
            result.push_back(std::move(stmt));
            continue;
        }

        auto &imp = std::get<ImportStmt>(stmt);
        ResolvedPath rp = resolve(imp.module_path, referrer_dir);
        const std::string &abs_path = rp.path;

        std::string target_dir = rp.is_directory ? abs_path
                                                 : fs::path(abs_path).parent_path().string();
        bool cross_package = isCrossPackage(referrer_dir, target_dir);

        if (loading_.count(abs_path))
            throw std::runtime_error("circular import detected: " + abs_path);

        if (loaded_.count(abs_path)) {
            if (!imp.names.empty()) {
                auto &exports = exports_cache_[abs_path];
                for (const auto &name : imp.names) {
                    auto it = exports.find(name);
                    if (it == exports.end()) {
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
                               imp.loc.line, cross_package, &exports_cache_[abs_path]);
        } else {
            loading_.insert(abs_path);
            auto sub_prog = loadAndParse(abs_path, sm_);
            std::string sub_dir = fs::path(abs_path).parent_path().string();
            sub_prog = resolveImports(sub_prog, sub_dir);
            loading_.erase(abs_path);
            loaded_.insert(abs_path);

            extractDefinitions(sub_prog, result, imp.names, imp.module_path,
                               imp.loc.line, cross_package, &exports_cache_[abs_path]);
        }
    }

    return result;
}

} // namespace ry
