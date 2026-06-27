#include "ry/project/paths.hpp"

#include <cstdlib>
#include <fstream>

namespace fs = std::filesystem;

namespace ry {

fs::path get_ry_home() {
    if (const char *env = std::getenv("RY_HOME")) {
        return fs::path(env);
    }
    if (const char *home = std::getenv("HOME")) {
        return fs::path(home) / ".ry";
    }
    return fs::path(".ry");
}

StdlibManifest read_manifest(const fs::path &dir) {
    StdlibManifest manifest;
    fs::path manifest_path = dir / "manifest.json";
    std::ifstream file(manifest_path);
    if (!file.is_open()) return manifest;

    std::string json((std::istreambuf_iterator<char>(file)),
                     std::istreambuf_iterator<char>());

    auto arr_start = json.find('[');
    auto arr_end = json.find(']');
    if (arr_start != std::string::npos && arr_end != std::string::npos) {
        std::string arr = json.substr(arr_start + 1, arr_end - arr_start - 1);
        size_t pos = 0;
        while (pos < arr.size()) {
            auto q1 = arr.find('"', pos);
            if (q1 == std::string::npos) break;
            auto q2 = arr.find('"', q1 + 1);
            if (q2 == std::string::npos) break;
            manifest.files.push_back(arr.substr(q1 + 1, q2 - q1 - 1));
            pos = q2 + 1;
        }
    }

    return manifest;
}

void write_manifest(const fs::path &dir,
                    const std::vector<std::string> &files) {
    fs::path manifest_path = dir / "manifest.json";
    std::ofstream out(manifest_path);
    out << "{\n  \"files\": [";
    for (size_t i = 0; i < files.size(); i++) {
        if (i > 0) out << ", ";
        out << "\"" << files[i] << "\"";
    }
    out << "]\n}\n";
}

} // namespace ry
