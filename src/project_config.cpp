#include "ry/project_config.hpp"
#include <cstdlib>
#include <filesystem>
#include <fstream>
#include <iostream>
#include <map>
#include <sstream>
#include <stdexcept>
#include <sys/wait.h>
#include <unordered_map>

namespace fs = std::filesystem;

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
    config.src_dir  = sections["paths"]["src"];
    auto paths = sections.find("paths");
    if (paths != sections.end()) {
        auto it = paths->second.find("_dev_stdlib");
        if (it != paths->second.end()) {
            config.dev_stdlib_dir = it->second;
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
    out << "[paths]\n";
    out << "src = \"" << config.src_dir << "\"\n";
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

// --- scaffold_project (shared helper) ---

static int scaffold_project(const fs::path &project_dir, const std::string &project_name) {
    // 1. Create directories
    std::error_code ec;
    fs::create_directories(project_dir / "src", ec);
    if (ec) {
        std::cerr << "Error: failed to create directories: " << ec.message() << "\n";
        return 1;
    }

    // 2. Generate package.toml
    ProjectConfig config;
    config.name     = project_name;
    config.version  = "0.1.0";
    config.entry    = "src/main.ry";
    config.src_dir  = "src";

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

    int status = std::system(it->second.c_str());
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
