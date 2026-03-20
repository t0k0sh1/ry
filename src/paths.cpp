#include "ry/paths.hpp"
#include "ry/self_update.hpp"
#include <cstdlib>
#include <fstream>
#include <sstream>

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

fs::path find_lib_dir(const std::string &exe_path) {
    // 1. $RY_HOME/lib
    fs::path ry_home_lib = get_ry_home() / "lib";
    if (fs::is_directory(ry_home_lib / "std")) {
        return ry_home_lib;
    }

    // 2. exe/../lib (installed layout)
    std::error_code ec;
    fs::path exe_dir = fs::path(exe_path).parent_path();
    exe_dir = fs::canonical(exe_dir, ec);
    if (!ec) {
        fs::path parent_lib = exe_dir.parent_path() / "lib";
        if (fs::is_directory(parent_lib / "std")) {
            return parent_lib;
        }

        // 3. exe/lib (development layout)
        fs::path sibling_lib = exe_dir / "lib";
        if (fs::is_directory(sibling_lib / "std")) {
            return sibling_lib;
        }
    }

    return {};
}

StdlibManifest read_manifest(const fs::path &dir) {
    StdlibManifest manifest;
    fs::path manifest_path = dir / "manifest.json";
    std::ifstream file(manifest_path);
    if (!file.is_open()) return manifest;

    std::string json((std::istreambuf_iterator<char>(file)),
                     std::istreambuf_iterator<char>());

    manifest.version = ry::self_update::detail::extract_json_string(json, "version");

    // Extract files array
    auto arr_start = json.find("[");
    auto arr_end = json.find("]");
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
                    const std::string &version,
                    const std::vector<std::string> &files) {
    fs::path manifest_path = dir / "manifest.json";
    std::ofstream out(manifest_path);
    out << "{\n  \"version\": \"" << version << "\",\n  \"files\": [";
    for (size_t i = 0; i < files.size(); i++) {
        if (i > 0) out << ", ";
        out << "\"" << files[i] << "\"";
    }
    out << "]\n}\n";
}

} // namespace ry
