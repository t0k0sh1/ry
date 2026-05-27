#include "ry/project/project_config.hpp"
#include <algorithm>
#include <cstdlib>
#include <filesystem>
#include <fstream>
#include <iostream>
#include <map>
#include <set>
#include <sstream>
#include <stdexcept>
#include <vector>
#ifndef _WIN32
#include <sys/wait.h>
#endif
#include <unordered_map>


namespace ry {

namespace fs = std::filesystem;

namespace {

void validatePathEntryValue(const std::string &val) {
    if (val.empty()) {
        throw std::runtime_error("package.toml [paths] value must not be empty");
    }
    fs::path rel(val);
    if (rel.is_absolute()) {
        throw std::runtime_error("package.toml [paths] values must be project-relative paths");
    }
    for (const auto &part : rel) {
        if (part == "..") {
            throw std::runtime_error("package.toml [paths] values must not contain '..'");
        }
    }
}

} // namespace

// --- TOML subset parser ---

ProjectConfig ProjectConfigParser::load(const std::string &toml_content) {
    std::unordered_map<std::string, std::unordered_map<std::string, std::string>> sections;
    std::string current_section;

    std::istringstream stream(toml_content);
    std::string line;
    while (std::getline(stream, line)) {
        // strip trailing \r
        if (!line.empty() && line.back() == '\r') line.pop_back();

        // trim leading/trailing whitespace
        auto start = line.find_first_not_of(" \t");
        if (start == std::string::npos) continue;
        auto end = line.find_last_not_of(" \t");
        line = line.substr(start, end - start + 1);

        // skip comments and empty lines
        if (line.empty() || line[0] == '#') continue;

        // section header
        if (line[0] == '[' && line.back() == ']') {
            current_section = line.substr(1, line.size() - 2);
            continue;
        }

        // key = "value"
        auto eq = line.find('=');
        if (eq == std::string::npos) {
            throw std::runtime_error("Invalid TOML line: " + line);
        }
        std::string key = line.substr(0, eq);
        std::string val = line.substr(eq + 1);

        // trim key
        auto ks = key.find_first_not_of(" \t");
        auto ke = key.find_last_not_of(" \t");
        if (ks == std::string::npos) throw std::runtime_error("Empty key in TOML");
        key = key.substr(ks, ke - ks + 1);

        // trim value and strip quotes
        auto vs = val.find_first_not_of(" \t");
        auto ve = val.find_last_not_of(" \t");
        if (vs == std::string::npos) throw std::runtime_error("Empty value in TOML");
        val = val.substr(vs, ve - vs + 1);
        if (val.size() >= 2 && val.front() == '"' && val.back() == '"') {
            val = val.substr(1, val.size() - 2);
        }

        sections[current_section][key] = val;
    }

    ProjectConfig config;
    config.name     = sections["project"]["name"];
    config.version  = sections["project"]["version"];
    config.entry    = sections["project"]["entry"];
    config.src_dir  = "";
    auto paths = sections.find("paths");
    if (paths != sections.end()) {
        auto src_it = paths->second.find("src");
        if (src_it != paths->second.end()) {
            config.src_dir = src_it->second;
        }
        auto it = paths->second.find("_dev_stdlib");
        if (it != paths->second.end()) {
            config.dev_stdlib_dir = it->second;
        }

        std::vector<std::pair<std::string, std::string>> path_entries;
        for (const auto &[key, val] : paths->second) {
            if (key == "_dev_stdlib") continue;
            if (!key.empty() && key[0] == '_') continue;
            validatePathEntryValue(val);
            path_entries.emplace_back(key, val);
        }
        std::sort(path_entries.begin(), path_entries.end(),
                  [](const auto &a, const auto &b) { return a.first < b.first; });
        config.paths_entries = path_entries;
        std::set<std::string> seen_roots;
        for (const auto &entry : path_entries) {
            fs::path norm = fs::path(entry.second).lexically_normal();
            std::string norm_str = norm.generic_string();
            if (seen_roots.insert(norm_str).second) {
                config.path_search_roots.push_back(norm_str);
            }
        }
    }
    auto scripts_it = sections.find("scripts");
    if (scripts_it != sections.end()) {
        config.scripts = scripts_it->second;
    }
    return config;
}

std::string ProjectConfigParser::serialize(const ProjectConfig &config) {
    std::ostringstream out;
    out << "[project]\n";
    out << "name = \"" << config.name << "\"\n";
    out << "version = \"" << config.version << "\"\n";
    out << "entry = \"" << config.entry << "\"\n";
    out << "\n";
    const bool has_paths_section = !config.paths_entries.empty() || !config.src_dir.empty()
                                   || config.dev_stdlib_dir.has_value();
    if (has_paths_section) {
        out << "[paths]\n";
        if (!config.paths_entries.empty()) {
            for (const auto &[key, val] : config.paths_entries) {
                out << key << " = \"" << val << "\"\n";
            }
        } else if (!config.src_dir.empty()) {
            out << "src = \"" << config.src_dir << "\"\n";
        }
        if (config.dev_stdlib_dir.has_value()) {
            out << "_dev_stdlib = \"" << *config.dev_stdlib_dir << "\"\n";
        }
    }
    return out.str();
}

// --- findProjectRoot ---

std::optional<std::string> findProjectRoot(const std::string &start_dir) {
    fs::path dir = start_dir.empty() ? fs::current_path() : fs::path(start_dir);
    dir = fs::absolute(dir);

    while (true) {
        if (fs::exists(dir / "package.toml")) {
            return dir.string();
        }
        auto parent = dir.parent_path();
        if (parent == dir) break; // reached filesystem root
        dir = parent;
    }
    return std::nullopt;
}

std::optional<std::string> packageRootOfFile(const std::string &file_path) {
    if (file_path.empty()) return std::nullopt;
    fs::path p(file_path);
    std::error_code ec;
    fs::path dir = p.has_parent_path() ? p.parent_path() : fs::current_path(ec);
    if (ec || dir.empty()) return std::nullopt;
    return findProjectRoot(dir.string());
}

bool inSamePackage(const std::string &file_a, const std::string &file_b) {
    auto root_a = packageRootOfFile(file_a);
    auto root_b = packageRootOfFile(file_b);
    if (!root_a.has_value() && !root_b.has_value()) return true;
    if (root_a.has_value() != root_b.has_value()) return false;
    return *root_a == *root_b;
}

// --- scaffold_project (shared helper) ---

static std::string normalizePackageName(const std::string &name) {
    std::string result = name;
    std::replace(result.begin(), result.end(), '-', '_');
    return result;
}

static int scaffold_project(const fs::path &project_dir, const std::string &project_name) {
    // 1. Create directories
    std::error_code ec;
    fs::create_directories(project_dir / "src", ec);
    if (ec) {
        std::cerr << "Error: failed to create directories: " << ec.message() << "\n";
        return 1;
    }

    // 2. Generate package.toml (normalize hyphens to underscores in project name)
    ProjectConfig config;
    config.name     = normalizePackageName(project_name);
    config.version  = "0.1.0";
    config.entry    = "src/main.ry";
    config.src_dir  = "src";
    config.paths_entries = {{"src", "src"}};

    {
        std::ofstream f(project_dir / "package.toml");
        if (!f) {
            std::cerr << "Error: failed to create package.toml\n";
            return 1;
        }
        f << ProjectConfigParser::serialize(config);
    }

    // 3. Generate src/main.ry (if not exists)
    auto main_ry = project_dir / "src" / "main.ry";
    if (!fs::exists(main_ry)) {
        std::ofstream f(main_ry);
        if (!f) {
            std::cerr << "Error: failed to create src/main.ry\n";
            return 1;
        }
        f << "print(\"Hello, World!\")\n";
    }

    return 0;
}

// --- cmd_init ---

int cmd_init() {
    fs::path cwd = fs::current_path();

    if (fs::exists(cwd / "package.toml")) {
        std::cerr << "Error: package.toml already exists\n";
        return 1;
    }

    int ret = scaffold_project(cwd, cwd.filename().string());
    if (ret != 0) return ret;

    std::cout << "Initialized ry project in " << cwd.string() << "\n";
    return 0;
}

// --- cmd_new ---

int cmd_new(int argc, char *argv[]) {
    if (argc < 1) {
        std::cerr << "Usage: ry new <project-name>\n";
        return 1;
    }

    std::string project_name = argv[0];

    // Reject absolute paths and path traversal
    fs::path name_path(project_name);
    if (name_path.is_absolute() || project_name.find("..") != std::string::npos
        || project_name.find('/') != std::string::npos
        || project_name.find('\\') != std::string::npos) {
        std::cerr << "Error: project name must be a simple directory name\n";
        return 1;
    }

    fs::path project_dir = fs::current_path() / project_name;

    if (fs::exists(project_dir)) {
        std::cerr << "Error: directory '" << project_name << "' already exists\n";
        return 1;
    }

    int ret = scaffold_project(project_dir, project_name);
    if (ret != 0) return ret;

    std::cout << "Created ry project '" << project_name << "'\n";
    return 0;
}

// --- cmd_run ---

static void printScriptList(std::ostream &out,
                            const std::unordered_map<std::string, std::string> &scripts,
                            bool show_commands) {
    std::map<std::string, std::string> sorted(scripts.begin(), scripts.end());
    for (const auto &[name, cmd] : sorted) {
        out << "  " << name;
        if (show_commands) out << " = \"" << cmd << "\"";
        out << "\n";
    }
}

int cmd_run(int argc, char *argv[]) {
    auto root = findProjectRoot();
    if (!root) {
        std::cerr << "Error: package.toml not found. Run 'ry init' first.\n";
        return 1;
    }

    std::ifstream f(fs::path(*root) / "package.toml");
    if (!f) {
        std::cerr << "Error: could not read package.toml\n";
        return 1;
    }
    std::string content((std::istreambuf_iterator<char>(f)),
                         std::istreambuf_iterator<char>());
    ProjectConfig config = ProjectConfigParser::load(content);

    if (argc == 0) {
        if (config.scripts.empty()) {
            std::cout << "No scripts defined in package.toml.\n";
            std::cout << "Add a [scripts] section to define scripts.\n";
            return 0;
        }
        std::cout << "Available scripts:\n";
        printScriptList(std::cout, config.scripts, true);
        return 0;
    }

    std::string script_name = argv[0];
    auto it = config.scripts.find(script_name);
    if (it == config.scripts.end()) {
        std::cerr << "Error: unknown script '" << script_name << "'\n";
        if (!config.scripts.empty()) {
            std::cerr << "\nAvailable scripts:\n";
            printScriptList(std::cerr, config.scripts, false);
        }
        return 1;
    }

    int status = std::system(it->second.c_str()); // NOLINT(cert-env33-c) -- intentional: ry run executes user-defined scripts
    if (status == -1) {
        std::cerr << "Error: failed to execute command\n";
        return 1;
    }
#ifdef _WIN32
    return status;
#else
    if (WIFEXITED(status)) {
        return WEXITSTATUS(status);
    }
    return 1;
#endif
}

} // namespace ry
