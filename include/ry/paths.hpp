#pragma once
#include <filesystem>
#include <string>
#include <vector>

namespace ry {

// $RY_HOME (defaults to $HOME/.ry)
std::filesystem::path get_ry_home();

// Find the share/ directory containing stdlib
// Search order (each tier falls back to lib/ for pre-migration installs):
// 1. project-local override from package.toml (repo builds only)
// 2. $RY_HOME/share, then $RY_HOME/lib (skipped when skip_global is true)
// 3. exe/../share, then exe/../lib
// 4. exe/share, then exe/lib
std::filesystem::path find_share_dir(const std::string &exe_path,
                                     const std::string &project_root,
                                     bool skip_global = false);
std::filesystem::path find_share_dir(const std::string &exe_path,
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
