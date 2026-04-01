#pragma once

#include "ry/ast.hpp"
#include "ry/source_manager.hpp"
#include <filesystem>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <vector>

Program loadAndParse(const std::string &abs_path, SourceManager *sm = nullptr);

class ModuleLoader {
public:
    explicit ModuleLoader(const std::vector<std::string> &search_paths = {},
                          SourceManager *sm = nullptr);

    Program resolveImports(Program &prog, const std::string &referrer_dir);

private:
    struct ResolvedPath {
        std::string path;
        bool is_directory = false;
    };
    std::vector<std::string> search_paths_;
    SourceManager *sm_ = nullptr;
    std::unordered_set<std::string> loaded_;
    std::unordered_set<std::string> loading_;
    // Cache: abs_path -> set of exported names defined in that package/file
    std::unordered_map<std::string, std::unordered_set<std::string>> fn_cache_;

    // Cache: raw path string -> (canonical path, error_code)
    struct CanonicalEntry {
        std::string path;
        std::error_code ec;
    };
    std::unordered_map<std::string, CanonicalEntry> canonical_cache_;
    // Cache: "package_path\0referrer_dir" -> resolved path + is_directory flag
    std::unordered_map<std::string, ResolvedPath> resolve_cache_;

    // Cached fs::canonical — populates ec with original error on failure
    std::string cachedCanonical(const std::string &raw, std::error_code &ec);
    // Cached fs::canonical — throws filesystem_error on failure (like fs::canonical)
    std::string cachedCanonical(const std::string &raw);
    // Convenience overloads accepting fs::path
    std::string cachedCanonical(const std::filesystem::path &p, std::error_code &ec);
    std::string cachedCanonical(const std::filesystem::path &p);

    // Resolve a package path to an absolute path (directory or .ry file)
    ResolvedPath resolve(const std::string &package_path, const std::string &referrer_dir);

    // Load all .ry files from a package directory and return collected statements
    Program loadPackageDir(const std::string &abs_dir_path);
};
