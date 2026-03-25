#include "ry/dotenv.hpp"

#include <cerrno>
#include <cstdlib>
#include <filesystem>
#include <fstream>
#include <iostream>
#include <sstream>

namespace ry {

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

void loadDotEnv(const std::string &dir) {
    namespace fs = std::filesystem;
    fs::path env_path = fs::path(dir) / ".env";

    std::ifstream file(env_path);
    if (!file.is_open()) return;

    std::ostringstream buf;
    buf << file.rdbuf();
    std::string content = buf.str();

    auto entries = parseDotEnv(content);
    for (const auto &[key, value] : entries) {
        if (setenv(key.c_str(), value.c_str(), 0) != 0) {
            std::cerr << "warning: .env: failed to set '" << key
                      << "': " << strerror(errno) << "\n";
        }
    }
}

} // namespace ry
