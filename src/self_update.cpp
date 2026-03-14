#include "ry/self_update.hpp"
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <iostream>
#include <array>
#include <algorithm>
#include <regex>
#include <sys/stat.h>
#include <unistd.h>

#ifdef __APPLE__
#include <mach-o/dyld.h>
#endif

namespace ry::self_update {

static const char *REPO = "t0k0sh1/ry";

bool is_valid_tag(const std::string &tag) {
    static const std::regex pattern("^v?[0-9A-Za-z._-]+$");
    return std::regex_match(tag, pattern);
}

namespace detail {

PlatformInfo detect_platform() {
    PlatformInfo info;
#ifdef __APPLE__
    info.os = "darwin";
#else
    info.os = "linux";
#endif
#if defined(__aarch64__) || defined(__arm64__)
    info.arch = "arm64";
#else
    info.arch = "amd64";
#endif
    return info;
}

std::string get_executable_path() {
#ifdef __APPLE__
    char path[4096];
    uint32_t size = sizeof(path);
    if (_NSGetExecutablePath(path, &size) != 0) {
        return "";
    }
    char *real = realpath(path, nullptr);
    if (!real) return std::string(path);
    std::string result(real);
    free(real);
    return result;
#else
    char path[4096];
    ssize_t len = readlink("/proc/self/exe", path, sizeof(path) - 1);
    if (len == -1) return "";
    path[len] = '\0';
    return std::string(path);
#endif
}

std::string shell_exec(const std::string &cmd) {
    std::array<char, 4096> buffer;
    std::string result;
    FILE *pipe = popen(cmd.c_str(), "r");
    if (!pipe) return "";
    while (fgets(buffer.data(), buffer.size(), pipe) != nullptr) {
        result += buffer.data();
    }
    int status = pclose(pipe);
    if (status != 0) return "";
    return result;
}

std::string extract_json_string(const std::string &json, const std::string &key) {
    std::string pattern = "\"" + key + "\"";
    auto pos = json.find(pattern);
    if (pos == std::string::npos) return "";

    pos += pattern.size();
    pos = json.find(':', pos);
    if (pos == std::string::npos) return "";
    pos++;

    while (pos < json.size() && (json[pos] == ' ' || json[pos] == '\t' || json[pos] == '\n' || json[pos] == '\r'))
        pos++;

    if (pos >= json.size()) return "";

    if (json[pos] == 't') return "true";
    if (json[pos] == 'f') return "false";
    if (json[pos] == 'n') return "null";

    if (json[pos] != '"') return "";
    pos++;

    std::string value;
    while (pos < json.size() && json[pos] != '"') {
        if (json[pos] == '\\' && pos + 1 < json.size()) {
            pos++;
        }
        value += json[pos];
        pos++;
    }
    return value;
}

std::vector<std::string> extract_all_json_strings(const std::string &json, const std::string &key) {
    std::vector<std::string> results;
    std::string pattern = "\"" + key + "\"";
    size_t search_pos = 0;

    while (true) {
        auto pos = json.find(pattern, search_pos);
        if (pos == std::string::npos) break;

        pos += pattern.size();
        pos = json.find(':', pos);
        if (pos == std::string::npos) break;
        pos++;

        while (pos < json.size() && (json[pos] == ' ' || json[pos] == '\t' || json[pos] == '\n' || json[pos] == '\r'))
            pos++;

        if (pos >= json.size()) break;

        if (json[pos] == 't') { results.push_back("true"); search_pos = pos + 4; continue; }
        if (json[pos] == 'f') { results.push_back("false"); search_pos = pos + 5; continue; }
        if (json[pos] == 'n') { results.push_back("null"); search_pos = pos + 4; continue; }

        if (json[pos] != '"') { search_pos = pos + 1; continue; }
        pos++;

        std::string value;
        while (pos < json.size() && json[pos] != '"') {
            if (json[pos] == '\\' && pos + 1 < json.size()) {
                pos++;
            }
            value += json[pos];
            pos++;
        }
        results.push_back(value);
        search_pos = pos + 1;
    }
    return results;
}

bool is_prerelease(const std::string &version) {
    std::string v = version;
    if (!v.empty() && v[0] == 'v') v = v.substr(1);
    return v.find('-') != std::string::npos;
}

std::string build_download_url(const std::string &tag, const PlatformInfo &platform) {
    return "https://github.com/" + std::string(REPO) +
           "/releases/download/" + tag +
           "/ry-" + platform.os + "-" + platform.arch + ".tar.gz";
}

UpdateTarget resolve_update_target(const std::string &mode, const PlatformInfo &platform) {
    UpdateTarget target;

    if (mode == "stable") {
        std::string api_url = "https://api.github.com/repos/" + std::string(REPO) + "/releases/latest";
        // Check HTTP status first to distinguish 404 from other errors
        std::string status_cmd = "curl -sfL -o /dev/null -w '%{http_code}' -H 'Accept: application/json' '" + api_url + "'";
        std::string http_status = shell_exec(status_cmd);
        http_status.erase(std::remove_if(http_status.begin(), http_status.end(), ::isspace), http_status.end());

        if (http_status == "404") {
            // No stable release exists yet — treat as already up to date
            target.tag = std::string("v") + RY_VERSION;
            return target;
        }

        std::string cmd = "curl -sfL -H 'Accept: application/json' '" + api_url + "'";
        std::string json = shell_exec(cmd);
        if (json.empty()) {
            std::cerr << "Error: Failed to fetch latest release info.\n";
            return target;
        }
        target.tag = extract_json_string(json, "tag_name");
        if (target.tag.empty()) {
            std::cerr << "Error: Could not find latest stable release.\n";
            return target;
        }
        target.download_url = build_download_url(target.tag, platform);
    } else if (mode == "nightly") {
        std::string cmd = "curl -sfL -H 'Accept: application/json' "
                          "https://api.github.com/repos/" + std::string(REPO) + "/releases?per_page=20";
        std::string json = shell_exec(cmd);
        if (json.empty()) {
            std::cerr << "Error: Failed to fetch releases.\n";
            return target;
        }

        auto tags = extract_all_json_strings(json, "tag_name");
        auto prereleases = extract_all_json_strings(json, "prerelease");

        for (size_t i = 0; i < tags.size() && i < prereleases.size(); i++) {
            if (prereleases[i] == "true") {
                target.tag = tags[i];
                target.download_url = build_download_url(target.tag, platform);
                return target;
            }
        }
        std::cerr << "Error: No nightly/prerelease version found.\n";
    } else {
        // Explicit version tag
        std::string tag = mode;
        if (!tag.empty() && tag[0] != 'v') {
            tag = "v" + tag;
        }

        if (!is_valid_tag(tag)) {
            std::cerr << "Error: Invalid version format: " << tag << "\n";
            return target;
        }

        std::string url = build_download_url(tag, platform);
        std::string cmd = "curl -sfL -o /dev/null -w '%{http_code}' --head '" + url + "'";
        std::string status = shell_exec(cmd);
        status.erase(std::remove_if(status.begin(), status.end(), ::isspace), status.end());

        if (status.empty() || (status != "200" && status != "302")) {
            std::cerr << "Error: Version " << tag << " not found.\n";
            return target;
        }
        target.tag = tag;
        target.download_url = url;
    }

    return target;
}

bool download_file(const std::string &url, const std::string &dest_path) {
    std::string cmd = "curl -sfL -o '" + dest_path + "' '" + url + "'";
    int status = system(cmd.c_str());
    return status == 0;
}

static bool file_exists(const std::string &path) {
    struct stat st;
    return stat(path.c_str(), &st) == 0 && S_ISREG(st.st_mode);
}

bool replace_binary(const std::string &download_url, const std::string &binary_path) {
    char tmp_dir[] = "/tmp/ry-update-XXXXXX";
    if (mkdtemp(tmp_dir) == nullptr) {
        std::cerr << "Error: Failed to create temporary directory.\n";
        return false;
    }

    std::string tmp_dir_str(tmp_dir);
    std::string archive_path = tmp_dir_str + "/ry.tar.gz";

    std::cerr << "Downloading..." << std::flush;
    if (!download_file(download_url, archive_path)) {
        std::cerr << " failed.\n";
        std::cerr << "Error: Download failed. Check your network connection.\n";
        system(("rm -rf '" + tmp_dir_str + "'").c_str());
        return false;
    }
    std::cerr << " done.\n";

    std::string extract_cmd = "tar xzf '" + archive_path + "' -C '" + tmp_dir_str + "'";
    if (system(extract_cmd.c_str()) != 0) {
        std::cerr << "Error: Failed to extract archive.\n";
        system(("rm -rf '" + tmp_dir_str + "'").c_str());
        return false;
    }

    std::string new_binary = tmp_dir_str + "/ry";

    if (!file_exists(new_binary)) {
        std::cerr << "Error: Expected binary not found in archive.\n";
        system(("rm -rf '" + tmp_dir_str + "'").c_str());
        return false;
    }

    if (chmod(new_binary.c_str(), 0755) != 0) {
        std::cerr << "Error: Failed to set permissions on downloaded binary.\n";
        system(("rm -rf '" + tmp_dir_str + "'").c_str());
        return false;
    }

    if (rename(new_binary.c_str(), binary_path.c_str()) != 0) {
        // rename() may fail across filesystems, try cp fallback
        std::string cp_cmd = "cp '" + new_binary + "' '" + binary_path + "'";
        if (system(cp_cmd.c_str()) != 0) {
            std::cerr << "Error: Failed to replace binary at " << binary_path << "\n";
            std::cerr << "You may need to run with sudo.\n";
            system(("rm -rf '" + tmp_dir_str + "'").c_str());
            return false;
        }
    }

    system(("rm -rf '" + tmp_dir_str + "'").c_str());
    return true;
}

} // namespace detail
} // namespace ry::self_update

int cmd_self_update(int argc, char *argv[]) {
    using namespace ry::self_update;
    using namespace ry::self_update::detail;

    std::cerr << "Current version: ry " << RY_VERSION << "\n";

    std::string mode = "stable";
    if (argc >= 1) {
        std::string arg = argv[0];
        if (arg == "--nightly") {
            mode = "nightly";
        } else if (arg == "--help" || arg == "-h") {
            std::cerr << "Usage: ry self-update [--nightly | <version>]\n";
            std::cerr << "\n";
            std::cerr << "Options:\n";
            std::cerr << "  (no args)    Update to latest stable release\n";
            std::cerr << "  --nightly    Update to latest nightly/prerelease\n";
            std::cerr << "  <version>    Update to a specific version (e.g. v0.0.1)\n";
            return 0;
        } else {
            if (!is_valid_tag(arg)) {
                std::cerr << "Error: Invalid version format: " << arg << "\n";
                return 1;
            }
            mode = arg;
        }
    }

    auto platform = detect_platform();
    auto target = resolve_update_target(mode, platform);
    if (target.tag.empty()) {
        return 1;
    }

    std::string current_tag = std::string("v") + RY_VERSION;
    if (target.tag == current_tag) {
        std::cerr << "Already up to date.\n";
        return 0;
    }

    std::cerr << "Updating to " << target.tag << "...\n";

    std::string binary_path = get_executable_path();
    if (binary_path.empty()) {
        std::cerr << "Error: Could not determine executable path.\n";
        return 1;
    }

    if (!replace_binary(target.download_url, binary_path)) {
        return 1;
    }

    std::cerr << "Successfully updated to " << target.tag << ".\n";
    return 0;
}
