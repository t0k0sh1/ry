#include "ry/module_loader.hpp"
#include "ry/lexer.hpp"
#include "ry/parser.hpp"
#include <cstdlib>
#include <filesystem>
#include <fstream>
#include <sstream>
#include <stdexcept>

namespace fs = std::filesystem;

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

std::string ModuleLoader::resolve(const std::string &module_path,
                                   const std::string &referrer_dir) {
    fs::path candidate = fs::path(referrer_dir) / module_path;
    if (fs::exists(candidate))
        return fs::canonical(candidate).string();

    for (const auto &dir : search_paths_) {
        candidate = fs::path(dir) / module_path;
        if (fs::exists(candidate))
            return fs::canonical(candidate).string();
    }

    throw std::runtime_error("module not found: " + module_path);
}

Program loadAndParse(const std::string &abs_path, SourceManager *sm) {
    std::ifstream file(abs_path);
    if (!file.is_open())
        throw std::runtime_error("cannot open module: " + abs_path);

    std::ostringstream ss;
    ss << file.rdbuf();
    std::string src = ss.str();

    int fileId = 0;
    if (sm) {
        fileId = sm->addSource(abs_path, src);
    }

    Lexer lex(src);
    Parser parser(lex, sm, fileId);
    return parser.parseProgram();
}

Program ModuleLoader::resolveImports(Program &prog, const std::string &referrer_dir) {
    Program result;

    for (auto &stmt : prog) {
        if (!std::holds_alternative<ImportStmt>(stmt)) {
            result.push_back(std::move(stmt));
            continue;
        }

        auto &imp = std::get<ImportStmt>(stmt);
        std::string abs_path = resolve(imp.module_path, referrer_dir);

        if (loading_.count(abs_path))
            throw std::runtime_error("circular import detected: " + abs_path);

        if (loaded_.count(abs_path)) {
            // Already loaded — validate requested names exist
            if (!imp.names.empty()) {
                auto &fns = fn_cache_[abs_path];
                for (const auto &name : imp.names) {
                    if (!fns.count(name))
                        throw std::runtime_error("line " + std::to_string(imp.loc.line) +
                                                 ": function '" + name +
                                                 "' not found in module '" +
                                                 imp.module_path + "'");
                }
            }
            continue;
        }

        loading_.insert(abs_path);

        auto sub_prog = loadAndParse(abs_path, sm_);
        std::string sub_dir = fs::path(abs_path).parent_path().string();
        sub_prog = resolveImports(sub_prog, sub_dir);

        loading_.erase(abs_path);
        loaded_.insert(abs_path);

        // Collect exported names (functions + types) for cache
        std::unordered_set<std::string> fn_names;
        for (const auto &sub_stmt : sub_prog) {
            if (std::holds_alternative<std::unique_ptr<FnStmt>>(sub_stmt)) {
                fn_names.insert(std::get<std::unique_ptr<FnStmt>>(sub_stmt)->name);
            } else if (std::holds_alternative<RecordStmt>(sub_stmt)) {
                fn_names.insert(std::get<RecordStmt>(sub_stmt).name);
            }
        }
        fn_cache_[abs_path] = fn_names;

        if (imp.names.empty()) {
            for (auto &sub_stmt : sub_prog) {
                if (std::holds_alternative<std::unique_ptr<FnStmt>>(sub_stmt) ||
                    std::holds_alternative<RecordStmt>(sub_stmt)) {
                    result.push_back(std::move(sub_stmt));
                }
            }
        } else {
            std::unordered_set<std::string> requested(imp.names.begin(), imp.names.end());
            std::unordered_set<std::string> found;
            for (auto &sub_stmt : sub_prog) {
                if (std::holds_alternative<std::unique_ptr<FnStmt>>(sub_stmt)) {
                    const auto &fn = std::get<std::unique_ptr<FnStmt>>(sub_stmt);
                    if (requested.count(fn->name)) {
                        found.insert(fn->name);
                        result.push_back(std::move(sub_stmt));
                    }
                } else if (std::holds_alternative<RecordStmt>(sub_stmt)) {
                    const auto &ts = std::get<RecordStmt>(sub_stmt);
                    if (requested.count(ts.name)) {
                        found.insert(ts.name);
                        result.push_back(std::move(sub_stmt));
                    }
                }
            }
            for (const auto &name : imp.names) {
                if (!found.count(name))
                    throw std::runtime_error("line " + std::to_string(imp.loc.line) +
                                             ": '" + name +
                                             "' not found in module '" +
                                             imp.module_path + "'");
            }
        }
    }

    return result;
}
