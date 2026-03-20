#pragma once

#include "ry/ast.hpp"
#include "ry/source_manager.hpp"
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
    std::vector<std::string> search_paths_;
    SourceManager *sm_ = nullptr;
    std::unordered_set<std::string> loaded_;
    std::unordered_set<std::string> loading_;
    // Cache: abs_path -> set of exported names defined in that package/file
    std::unordered_map<std::string, std::unordered_set<std::string>> fn_cache_;

    // Resolve a package path to an absolute path (directory or .ry file)
    std::string resolve(const std::string &package_path, const std::string &referrer_dir);

    // Load all .ry files from a package directory and return collected statements
    Program loadPackageDir(const std::string &abs_dir_path);
};
