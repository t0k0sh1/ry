#include "ry/project_config.hpp"
#include <filesystem>
#include <fstream>
#include <iostream>
#include <sstream>
#include <stdexcept>
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
        if (fs::exists(dir / "ry.toml")) {
            return dir.string();
        }
        auto parent = dir.parent_path();
        if (parent == dir) break; // reached filesystem root
        dir = parent;
    }
    return std::nullopt;
}

// --- cmd_init ---

int cmd_init() {
    fs::path cwd = fs::current_path();

    // 1. Check if ry.toml already exists
    if (fs::exists(cwd / "ry.toml")) {
        std::cerr << "Error: ry.toml already exists\n";
        return 1;
    }

    // 2. Create directories
    fs::create_directories(cwd / "src");

    // 3. Generate ry.toml
    ProjectConfig config;
    config.name     = cwd.filename().string();
    config.version  = "0.1.0";
    config.entry    = "src/main.ry";
    config.src_dir  = "src";

    {
        std::ofstream f(cwd / "ry.toml");
        f << ProjectConfigParser::serialize(config);
    }

    // 4. Generate src/main.ry (if not exists)
    auto main_ry = cwd / "src" / "main.ry";
    if (!fs::exists(main_ry)) {
        std::ofstream f(main_ry);
        f << "print(\"Hello, World!\")\n";
    }

    // 5. Success message
    std::cout << "Initialized ry project in " << cwd.string() << "\n";
    return 0;
}
