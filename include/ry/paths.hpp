#pragma once
#include <filesystem>
#include <string>
#include <vector>

namespace ry {

// $RY_HOME (defaults to $HOME/.ry)
std::filesystem::path get_ry_home();

// Find the lib/ directory containing stdlib
// Search order:
// 1. project-local override from package.toml (repo builds only)
// 2. $RY_HOME/lib (skipped when skip_global is true)
// 3. exe/../lib
// 4. exe/lib
std::filesystem::path find_lib_dir(const std::string &exe_path,
                                   const std::string &project_root,
                                   bool skip_global = false);
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
