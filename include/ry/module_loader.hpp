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
    // Cache: abs_path -> set of function names defined in that module
    std::unordered_map<std::string, std::unordered_set<std::string>> fn_cache_;

    std::string resolve(const std::string &module_path, const std::string &referrer_dir);
};
