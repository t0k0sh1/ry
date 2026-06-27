#include "ry/cli/self_update.hpp"
#include "ry/util/executable_path.hpp"

#include <cerrno>
#include <cstdio>
#include <cstring>
#include <filesystem>
#include <iostream>
#include <vector>
#include <unistd.h>

namespace {

std::filesystem::path resolve_argv0_dir(const char *ry_argv0) {
    namespace fs = std::filesystem;
    std::error_code ec;

    if (ry_argv0 && *ry_argv0) {
        fs::path p(ry_argv0);
        if (p.has_parent_path()) {
            fs::path abs = fs::weakly_canonical(fs::absolute(p), ec);
            if (!ec && !abs.empty()) return abs.parent_path();
            ec.clear();
            return fs::absolute(p, ec).parent_path();
        }
    }

    std::string exe = ry::get_executable_path();
    if (!exe.empty()) {
        fs::path abs = fs::weakly_canonical(exe, ec);
        if (!ec && !abs.empty()) return abs.parent_path();
        ec.clear();
        return fs::path(exe).parent_path();
    }

    return {};
}

std::filesystem::path find_companion_binary(const char *ry_argv0) {
    namespace fs = std::filesystem;
    fs::path dir = resolve_argv0_dir(ry_argv0);
    if (dir.empty()) return {};
    fs::path candidate = dir / "ry-self-update";
    std::error_code ec;
    if (fs::is_regular_file(candidate, ec)) return candidate;
    return {};
}

} // namespace

int cmd_self_update(int argc, char *argv[], const char *ry_argv0) {
    namespace fs = std::filesystem;

    fs::path companion = find_companion_binary(ry_argv0);
    if (companion.empty()) {
        std::cerr << "Error: ry-self-update was not found next to ry.\n"
                  << "       Reinstall ry or run `ry-rescue` to recover a complete install.\n";
        return 1;
    }

    std::vector<std::string> owned_args;
    owned_args.reserve(static_cast<size_t>(argc) + 1);
    owned_args.push_back(companion.string());
    for (int i = 0; i < argc; ++i) {
        owned_args.emplace_back(argv[i] ? argv[i] : "");
    }

    std::vector<char *> exec_argv;
    exec_argv.reserve(owned_args.size() + 1);
    for (auto &arg : owned_args) {
        exec_argv.push_back(const_cast<char *>(arg.c_str()));
    }
    exec_argv.push_back(nullptr);

    std::fflush(stdout);
    std::fflush(stderr);
    execv(companion.c_str(), exec_argv.data());

    std::cerr << "Error: failed to execute " << companion << ": "
              << std::strerror(errno) << "\n";
    return 127;
}
