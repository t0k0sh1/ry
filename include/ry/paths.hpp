#pragma once
#include <filesystem>
#include <string>
#include <vector>

namespace ry {

// $RY_HOME (defaults to $HOME/.ry)
std::filesystem::path get_ry_home();

// Find the lib/ directory containing stdlib
// Search order: $RY_HOME/lib -> exe/../lib -> exe/lib
// When skip_global is true, $RY_HOME/lib is skipped (for RY_ENV=internal)
std::filesystem::path find_lib_dir(const std::string &exe_path,
                                   bool skip_global = false);

struct StdlibManifest {
    std::string version;
    std::vector<std::string> files;
};

StdlibManifest read_manifest(const std::filesystem::path &dir);
void write_manifest(const std::filesystem::path &dir,
                    const std::string &version,
                    const std::vector<std::string> &files);

} // namespace ry
