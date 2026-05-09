#pragma once

#include "ry/ast.hpp"
#include "ry/source_manager.hpp"
#include <filesystem>
#include <optional>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <vector>


namespace ry {

Program loadAndParse(const std::string &abs_path, SourceManager *sm = nullptr);

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
    // Cache: abs_path -> map of exported name -> is_public flag (true if @public).
    // Stores every exportable kind (functions, records, lets, enums, type
    // aliases, directive defs), so the name "fn" no longer applies — kept as
    // `exports_cache_` to reflect that. @public symbols are visible across
    // packages; non-public ones are visible only within the same package
    // (the nearest ancestor directory containing `package.toml`; files
    // outside any package share an anonymous package).
    std::unordered_map<std::string, std::unordered_map<std::string, bool>> exports_cache_;
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

    // Cached `findProjectRoot` for a given directory.
    std::optional<std::string> packageRootOfDir(const std::string &dir);

    // Returns true if @p caller_dir and @p target_dir resolve to different packages.
    // Two directories that both lack any `package.toml` ancestor are treated as
    // sharing a single anonymous package (so existing tests / scripts that do
    // not declare a `package.toml` continue to observe same-package visibility).
    bool isCrossPackage(const std::string &caller_dir, const std::string &target_dir);
};

} // namespace ry
