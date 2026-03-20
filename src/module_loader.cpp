#include "ry/module_loader.hpp"
#include "ry/lexer.hpp"
#include "ry/parser.hpp"
#include <algorithm>
#include <cstdlib>
#include <filesystem>
#include <fstream>
#include <sstream>
#include <stdexcept>

namespace fs = std::filesystem;

// Check if a statement is an exportable definition
static bool isExportable(const StmtNode &stmt) {
    return std::holds_alternative<std::unique_ptr<FnStmt>>(stmt) ||
           std::holds_alternative<RecordStmt>(stmt) ||
           std::holds_alternative<EnumStmt>(stmt) ||
           std::holds_alternative<TypeAliasStmt>(stmt);
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
    return "";
}

// Collect exported names from a program
static std::unordered_set<std::string> collectExportedNames(const Program &prog) {
    std::unordered_set<std::string> names;
    for (const auto &stmt : prog) {
        if (isExportable(stmt))
            names.insert(getExportName(stmt));
    }
    return names;
}

// Extract exportable definitions from a program, optionally filtering by requested names
static void extractDefinitions(Program &source, Program &dest,
                                const std::vector<std::string> &requested_names,
                                const std::string &import_path, int line) {
    if (requested_names.empty()) {
        for (auto &stmt : source) {
            if (isExportable(stmt))
                dest.push_back(std::move(stmt));
        }
        return;
    }

    std::unordered_set<std::string> requested(requested_names.begin(), requested_names.end());
    std::unordered_set<std::string> found;
    for (auto &stmt : source) {
        if (isExportable(stmt)) {
            std::string name = getExportName(stmt);
            if (requested.count(name)) {
                found.insert(name);
                dest.push_back(std::move(stmt));
            }
        }
    }
    for (const auto &name : requested_names) {
        if (!found.count(name))
            throw std::runtime_error("line " + std::to_string(line) +
                                     ": '" + name +
                                     "' not found in package '" +
                                     import_path + "'");
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

std::string ModuleLoader::resolve(const std::string &package_path,
                                   const std::string &referrer_dir) {
    auto try_resolve = [&](const std::string &dir) -> std::string {
        // 1. Directory (package)
        fs::path dir_candidate = fs::path(dir) / package_path;
        if (fs::is_directory(dir_candidate))
            return fs::canonical(dir_candidate).string();

        // 2. Single file (backward compatibility)
        fs::path file_candidate = fs::path(dir) / (package_path + ".ry");
        if (fs::is_regular_file(file_candidate))
            return fs::canonical(file_candidate).string();

        return "";
    };

    std::string result = try_resolve(referrer_dir);
    if (!result.empty()) return result;

    for (const auto &dir : search_paths_) {
        result = try_resolve(dir);
        if (!result.empty()) return result;
    }

    throw std::runtime_error("package not found: " + package_path);
}

Program loadAndParse(const std::string &abs_path, SourceManager *sm) {
    std::ifstream file(abs_path);
    if (!file.is_open())
        throw std::runtime_error("cannot open file: " + abs_path);

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

Program ModuleLoader::loadPackageDir(const std::string &abs_dir_path) {
    Program collected;
    std::vector<std::string> ry_files;

    for (const auto &entry : fs::directory_iterator(abs_dir_path)) {
        if (!entry.is_regular_file()) continue;
        auto filename = entry.path().filename().string();
        if (!filename.empty() && filename[0] == '_') continue;
        if (filename.size() < 3 || filename.compare(filename.size() - 3, 3, ".ry") != 0) continue;
        ry_files.push_back(fs::canonical(entry.path()).string());
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

        fn_cache_[file_path] = collectExportedNames(sub_prog);

        for (auto &stmt : sub_prog) {
            if (isExportable(stmt))
                collected.push_back(std::move(stmt));
        }
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
        std::string abs_path = resolve(imp.module_path, referrer_dir);

        if (loading_.count(abs_path))
            throw std::runtime_error("circular import detected: " + abs_path);

        if (loaded_.count(abs_path)) {
            if (!imp.names.empty()) {
                auto &fns = fn_cache_[abs_path];
                for (const auto &name : imp.names) {
                    if (!fns.count(name))
                        throw std::runtime_error("line " + std::to_string(imp.loc.line) +
                                                 ": '" + name +
                                                 "' not found in package '" +
                                                 imp.module_path + "'");
                }
            }
            continue;
        }

        if (fs::is_directory(abs_path)) {
            loading_.insert(abs_path);
            Program dir_prog = loadPackageDir(abs_path);
            loading_.erase(abs_path);
            loaded_.insert(abs_path);

            fn_cache_[abs_path] = collectExportedNames(dir_prog);
            extractDefinitions(dir_prog, result, imp.names, imp.module_path, imp.loc.line);
        } else {
            loading_.insert(abs_path);
            auto sub_prog = loadAndParse(abs_path, sm_);
            std::string sub_dir = fs::path(abs_path).parent_path().string();
            sub_prog = resolveImports(sub_prog, sub_dir);
            loading_.erase(abs_path);
            loaded_.insert(abs_path);

            fn_cache_[abs_path] = collectExportedNames(sub_prog);
            extractDefinitions(sub_prog, result, imp.names, imp.module_path, imp.loc.line);
        }
    }

    return result;
}
