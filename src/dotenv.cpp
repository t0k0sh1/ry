#include "ry/dotenv.hpp"

#include <algorithm>
#include <cerrno>
#include <cstdlib>
#include <cstring>
#include <filesystem>
#include <fstream>
#include <iostream>
#include <sstream>

namespace ry {

std::string resolveRyEnv(const std::string &value) {
    if (value == "production") return "prod";
    if (value == "development") return "dev";
    return value;
}

std::vector<std::pair<std::string, std::string>>
parseDotEnv(const std::string &content) {
    std::vector<std::pair<std::string, std::string>> result;
    std::istringstream stream(content);
    std::string line;

    while (std::getline(stream, line)) {
        // Skip blank lines
        if (line.empty()) continue;

        // Trim leading whitespace
        size_t start = line.find_first_not_of(" \t\r");
        if (start == std::string::npos) continue;

        // Skip comments
        if (line[start] == '#') continue;

        // Find the first '='
        size_t eq = line.find('=', start);
        if (eq == std::string::npos) continue;

        // Extract key and trim trailing whitespace
        std::string key = line.substr(start, eq - start);
        size_t key_end = key.find_last_not_of(" \t");
        if (key_end != std::string::npos)
            key = key.substr(0, key_end + 1);
        if (key.empty()) continue;

        // Extract value
        std::string value = line.substr(eq + 1);

        // Trim trailing \r (for CRLF files)
        if (!value.empty() && value.back() == '\r')
            value.pop_back();

        // Strip surrounding quotes
        if (value.size() >= 2) {
            char front = value.front();
            char back = value.back();
            if ((front == '"' && back == '"') ||
                (front == '\'' && back == '\'')) {
                value = value.substr(1, value.size() - 2);
            }
        }

        result.emplace_back(std::move(key), std::move(value));
    }

    return result;
}

static void loadEnvFile(const std::filesystem::path &path) {
    std::ifstream file(path);
    if (!file.is_open()) return;

    std::ostringstream buf;
    buf << file.rdbuf();
    auto entries = parseDotEnv(buf.str());
    for (const auto &[key, value] : entries) {
        if (setenv(key.c_str(), value.c_str(), 0) != 0) {
            std::cerr << "warning: " << path.filename().string()
                      << ": failed to set '" << key
                      << "': " << strerror(errno) << "\n";
        }
    }
}

static bool isValidEnvName(const std::string &name) {
    return !name.empty() &&
           std::all_of(name.begin(), name.end(), [](char c) {
               return (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') ||
                      (c >= '0' && c <= '9') || c == '_' || c == '-';
           });
}

void loadDotEnv(const std::string &dir, const std::string &env_name) {
    if (env_name == "prod") return;

    std::filesystem::path base(dir);
    if (!env_name.empty() && isValidEnvName(env_name)) {
        loadEnvFile(base / (".env." + env_name));
    }
    loadEnvFile(base / ".env");
}

} // namespace ry
