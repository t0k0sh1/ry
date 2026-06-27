#include "ry/project/paths.hpp"
#include "ry/project/project_config.hpp"
#include <fstream>
#include <stdexcept>

namespace fs = std::filesystem;

namespace ry {

namespace {

bool is_within(const fs::path &child, const fs::path &parent) {
    auto rel = child.lexically_relative(parent);
    return !rel.empty() && rel.native().find("..") != 0;
}

std::optional<fs::path> find_project_override_share_dir(const std::string &exe_path,
                                                        const std::string &project_root) {
    if (project_root.empty()) return std::nullopt;

    std::error_code ec;
    fs::path project = fs::weakly_canonical(fs::path(project_root), ec);
    if (ec || project.empty()) return std::nullopt;

    fs::path exe = fs::weakly_canonical(fs::absolute(fs::path(exe_path)), ec);
    if (ec || exe.empty() || !is_within(exe, project)) return std::nullopt;

    std::ifstream file(project / "package.toml");
    if (!file.is_open()) return std::nullopt;
    std::string toml((std::istreambuf_iterator<char>(file)),
                     std::istreambuf_iterator<char>());
    ProjectConfig config = ProjectConfigParser::load(toml);
    if (!config.dev_stdlib_dir || config.dev_stdlib_dir->empty()) return std::nullopt;

    fs::path rel(*config.dev_stdlib_dir);
    if (rel.is_absolute()) {
        throw std::runtime_error("package.toml [paths]._dev_stdlib must be a project-relative path");
    }
    for (const auto &part : rel) {
        if (part == "..") {
            throw std::runtime_error("package.toml [paths]._dev_stdlib must stay within the project root");
        }
    }

    fs::path candidate = fs::weakly_canonical(project / rel, ec);
    if (ec || candidate.empty() || !is_within(candidate, project)) {
        throw std::runtime_error("package.toml [paths]._dev_stdlib resolves outside the project root");
    }
    if (!fs::is_directory(candidate / "std")) {
        throw std::runtime_error("package.toml [paths]._dev_stdlib must point to a share directory containing std/");
    }
    return candidate;
}

} // namespace

fs::path find_share_dir(const std::string &exe_path,
                        const std::string &project_root,
                        bool skip_global) {
    // 1. project-local override from package.toml (repo builds only)
    if (auto project_share = find_project_override_share_dir(exe_path, project_root)) {
        return *project_share;
    }

    // 2. $RY_HOME/share (skipped when skip_global is true)
    //    Falls back to $RY_HOME/lib for pre-migration installs or partial upgrades
    if (!skip_global) {
        fs::path ry_home = get_ry_home();
        fs::path ry_home_share = ry_home / "share";
        if (fs::is_directory(ry_home_share / "std")) {
            return ry_home_share;
        }
        fs::path ry_home_lib = ry_home / "lib";
        if (fs::is_directory(ry_home_lib / "std")) {
            return ry_home_lib;
        }
    }

    // 3. exe/../share, then exe/../lib fallback (installed layout)
    std::error_code ec;
    fs::path exe_dir = fs::path(exe_path).parent_path();
    exe_dir = fs::canonical(exe_dir, ec);
    if (!ec) {
        fs::path parent = exe_dir.parent_path();
        fs::path parent_share = parent / "share";
        if (fs::is_directory(parent_share / "std")) {
            return parent_share;
        }
        fs::path parent_lib = parent / "lib";
        if (fs::is_directory(parent_lib / "std")) {
            return parent_lib;
        }

        // 4. exe/share, then exe/lib fallback (development layout)
        fs::path sibling_share = exe_dir / "share";
        if (fs::is_directory(sibling_share / "std")) {
            return sibling_share;
        }
        fs::path sibling_lib = exe_dir / "lib";
        if (fs::is_directory(sibling_lib / "std")) {
            return sibling_lib;
        }
    }

    return {};
}

fs::path find_share_dir(const std::string &exe_path, bool skip_global) {
    return find_share_dir(exe_path, "", skip_global);
}

fs::path find_native_library(const std::string &exe_path,
                              const std::string &lib_name) {
#ifdef __APPLE__
    std::string filename = "libry_" + lib_name + ".dylib";
#else
    std::string filename = "libry_" + lib_name + ".so";
#endif

    std::error_code ec;
    fs::path exe_dir = fs::path(exe_path).parent_path();
    exe_dir = fs::canonical(exe_dir, ec);
    if (!ec) {
        // 1. exe/../lib/ (installed layout)
        fs::path candidate = exe_dir.parent_path() / "lib" / filename;
        if (fs::is_regular_file(candidate))
            return candidate;

        // 2. exe/lib/ (development layout — e.g. build/lib/)
        candidate = exe_dir / "lib" / filename;
        if (fs::is_regular_file(candidate))
            return candidate;
    }

    // 3. $RY_HOME/lib/
    fs::path ry_home_lib = get_ry_home() / "lib" / filename;
    if (fs::is_regular_file(ry_home_lib))
        return ry_home_lib;

    return {};
}

} // namespace ry
