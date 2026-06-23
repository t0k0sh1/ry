#pragma once

#include "ry/ast/ast.hpp"
#include "ry/source_manager/source_manager.hpp"
#include <filesystem>
#include <optional>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <vector>


namespace ry {

Program loadAndParse(const std::string &abs_path, SourceManager *sm = nullptr);

// Classification used when emitting alias bindings for `from m import x as y`.
// Mirrors the variants returned by `isExportable` so the alias binding can be
// generated with the right AST shape (AssignStmt for values, TypeAliasStmt
// for types). Directives are tracked here so the alias-not-supported error
// has a dedicated branch.
enum class ExportableKind {
    Fn,
    Const,    // module-level `AssignStmt` carrying the `@const` directive
    Value,    // module-level `AssignStmt` without `@const` (treated as non-aliasable)
    Record,
    Enum,
    TypeAlias,
    Directive,
};

class ModuleLoader {
public:
    explicit ModuleLoader(const std::vector<std::string> &search_paths = {},
                          SourceManager *sm = nullptr);

    Program resolveImports(Program &prog, const std::string &referrer_dir);

    // Names from `from testing import ...` (or wildcard `from testing`)
    // observed during import resolution (#712). Populated lazily as
    // `resolveImports` walks each `ImportStmt`, filtered to the testing
    // intrinsic allow-list (`expect / mock / fail`)
    // — declarations like `each / property` that live in `testing.ry`'s
    // AST are tracked through the regular export path and excluded here.
    // Consumers (#713/#715/#716) read this set to enforce that test
    // sources explicitly import the intrinsics they reference.
    const std::unordered_set<std::string> &importedTestingIntrinsics() const {
        return imported_testing_intrinsics_;
    }

    // #1769: the canonical implicit-prelude import path. `jit_runner.cpp`
    // synthesizes `ImportStmt{kRyLangPreludePath, ...}` and the loader's
    // `canonicalStdlibName` maps the same path to the physical stdlib name
    // (`"std"`). Co-located so future renames touch exactly one constant.
    static constexpr const char *kRyLangPreludePath = "ry/lang";

    // #1769: returns true when `module_path` is in the reserved `ry/*`
    // namespace (i.e. begins with `ry/` followed by at least one segment).
    static bool isRyPath(const std::string &module_path);

    // #1769: maps a `ry/*` virtual path to the bare stdlib name used by
    // search-path resolution and the three intrinsic gating predicates
    // (`isTestingIntrinsic` / `isCppResourceKind` / `isCppBuiltinRecord`).
    //   "ry/lang"    -> "std"  (the prelude directory `share/std/`)
    //   "ry/<sub>"   -> "<sub>" (e.g. "ry/math" -> "math")
    //   anything else -> unchanged (returned as-is)
    // The remap is a pure string operation; physical layout stays at
    // `share/std/` (no file move). Error messages must continue to quote
    // the caller-supplied spelling.
    static std::string canonicalStdlibName(const std::string &module_path);

    // #1769: warnings emitted during import resolution that should surface
    // alongside codegen warnings. Currently used to flag a user-defined
    // top-level `ry/` directory that shadows the reserved stdlib namespace.
    const std::vector<std::string> &loaderWarnings() const {
        return loader_warnings_;
    }

private:
    struct ResolvedPath {
        std::string path;
        bool is_directory = false;
        // True only when the module was found in `search_paths_` (the stdlib
        // search roots), not in `referrer_dir`. Used by the testing-intrinsic
        // allow-list (#712) to ensure a project-local `testing.ry` shadow does
        // not silently bypass the `'<name>' not found` diagnostic for the three
        // intrinsic names.
        bool from_stdlib = false;
    };
    std::vector<std::string> search_paths_;
    SourceManager *sm_ = nullptr;
    std::unordered_set<std::string> loaded_;
    std::unordered_set<std::string> loading_;
    // Modules that were loaded via `import <mod>` (qualified-only) to a
    // user-defined module — their decls landed in `QualifiedImportStmt::definitions`
    // and were NOT inlined to the top-level Program. If a subsequent
    // `from <mod> import x` references the same module, the cache-hit branch
    // would normally short-circuit without pushing decls anywhere; this set
    // forces re-extract so the requested names reach the top-level Program
    // (AC3 of #1730). Stdlib modules are not tracked here because their decls
    // are still inlined to `result` on first load.
    std::unordered_set<std::string> qualified_only_user_loaded_;
    // Cache: abs_path -> map of exported name -> is_public flag (true if @public).
    // Stores every exportable kind (functions, records, lets, enums, type
    // aliases, directive defs), so the name "fn" no longer applies — kept as
    // `exports_cache_` to reflect that. @public symbols are visible across
    // packages; non-public ones are visible only within the same package
    // (the nearest ancestor directory containing `package.toml`; files
    // outside any package share an anonymous package).
    std::unordered_map<std::string, std::unordered_map<std::string, bool>> exports_cache_;
    // Parallel cache of exportable kinds keyed by abs_path then name. Populated
    // alongside `exports_cache_` so cache-hit imports can synthesize alias
    // bindings (`from m import x as y`) without re-parsing the source.
    std::unordered_map<std::string, std::unordered_map<std::string, ExportableKind>> exports_kinds_cache_;
    // Cache: directory path -> nearest ancestor `package.toml` root (or nullopt
    // if the directory is not contained in any rooted package).
    std::unordered_map<std::string, std::optional<std::string>> pkg_root_cache_;

    // Cache: raw path string -> (canonical path, error_code)
    struct CanonicalEntry {
        std::string path;
        std::error_code ec;
    };
    std::unordered_map<std::string, CanonicalEntry> canonical_cache_;
    // Cache: "module_path\0referrer_dir" -> resolved path + is_directory flag
    std::unordered_map<std::string, ResolvedPath> resolve_cache_;

    // Names imported from the testing module via `from testing import ...`
    // or `from testing` (wildcard). See `importedTestingIntrinsics()`.
    std::unordered_set<std::string> imported_testing_intrinsics_;

    // Warnings emitted during import resolution; surfaced by `loaderWarnings()`.
    std::vector<std::string> loader_warnings_;
    // #1769: referrer dirs whose `ry/` shadow probe has already been performed.
    // The probe does two filesystem stats per `ry/*` import; caching by
    // referrer collapses N stats into one for files with multiple `ry.*` imports.
    std::unordered_set<std::string> probed_ry_shadow_dirs_;
    // #2350: legacy stdlib import 形式 (`from math`, `from std.math`,
    // `from std`, `import math`) を検出した時に発行した deprecation warning の
    // dedup 用 set。key は元の resolve 文字列 (`"math"` / `"std/math"` / `"std"`)。
    // 同一スペリングに対して loader 生存期間中 1 回のみ警告する。
    std::unordered_set<std::string> warned_legacy_imports_;

    // Cached fs::canonical — populates ec with original error on failure
    std::string cachedCanonical(const std::string &raw, std::error_code &ec);
    // Cached fs::canonical — throws filesystem_error on failure (like fs::canonical)
    std::string cachedCanonical(const std::string &raw);
    // Convenience overloads accepting fs::path
    std::string cachedCanonical(const std::filesystem::path &p, std::error_code &ec);
    std::string cachedCanonical(const std::filesystem::path &p);

    // Resolve a module path to an absolute path (directory or .ry file)
    ResolvedPath resolve(const std::string &module_path, const std::string &referrer_dir);

    // Load all .ry files from a module directory and return collected statements
    Program loadModuleDir(const std::string &abs_dir_path);

    // Drop @p abs_path from `loaded_` / `qualified_only_user_loaded_` so the
    // next reference re-runs the cache-miss path. For directory modules,
    // also drops every contained `.ry` file from `loaded_` because
    // `loadModuleDir` inserts each per-file canonical path independently.
    // Without that recursive walk, re-extracting a directory module would
    // observe the per-file `loaded_` entries and silently skip every file.
    void invalidateLoaded(const std::string &abs_path, bool is_directory);

    // Cached `findProjectRoot` for a given directory.
    std::optional<std::string> packageRootOfDir(const std::string &dir);

    // Returns true if @p caller_dir and @p target_dir resolve to different packages.
    // Two directories that both lack any `package.toml` ancestor are treated as
    // sharing a single anonymous package (so existing tests / scripts that do
    // not declare a `package.toml` continue to observe same-package visibility).
    bool isCrossPackage(const std::string &caller_dir, const std::string &target_dir);
};

} // namespace ry
