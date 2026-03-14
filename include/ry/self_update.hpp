#pragma once
#include <string>
#include <vector>

namespace ry::self_update {

struct PlatformInfo {
    std::string os;    // "darwin" or "linux"
    std::string arch;  // "arm64" or "amd64"
};

struct UpdateTarget {
    std::string tag;
    std::string download_url;
};

bool is_valid_tag(const std::string &tag);

namespace detail {

PlatformInfo detect_platform();
std::string get_executable_path();
std::string shell_exec(const std::string &cmd);
std::string extract_json_string(const std::string &json, const std::string &key);
std::vector<std::string> extract_all_json_strings(const std::string &json, const std::string &key);
bool is_prerelease(const std::string &version);
std::string build_download_url(const std::string &tag, const PlatformInfo &platform);
UpdateTarget resolve_update_target(const std::string &mode, const PlatformInfo &platform);
bool download_file(const std::string &url, const std::string &dest_path);
bool replace_binary(const std::string &download_url, const std::string &binary_path);

} // namespace detail
} // namespace ry::self_update

int cmd_self_update(int argc, char *argv[]);
