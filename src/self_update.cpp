#include "ry/self_update.hpp"
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <iostream>
#include <array>
#include <algorithm>
#include <unistd.h>

#ifdef __APPLE__
#include <mach-o/dyld.h>
#endif

static const char *REPO = "pricklywiggles/ry";

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

    // Skip past key and find the colon
    pos += pattern.size();
    pos = json.find(':', pos);
    if (pos == std::string::npos) return "";
    pos++; // skip ':'

    // Skip whitespace
    while (pos < json.size() && (json[pos] == ' ' || json[pos] == '\t' || json[pos] == '\n' || json[pos] == '\r'))
        pos++;

    if (pos >= json.size()) return "";

    // Handle boolean/null values
    if (json[pos] == 't') return "true";
    if (json[pos] == 'f') return "false";
    if (json[pos] == 'n') return "null";

    // Expect a quoted string
    if (json[pos] != '"') return "";
    pos++; // skip opening quote

    std::string value;
    while (pos < json.size() && json[pos] != '"') {
        if (json[pos] == '\\' && pos + 1 < json.size()) {
            pos++; // skip escape char
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

        // Skip whitespace
        while (pos < json.size() && (json[pos] == ' ' || json[pos] == '\t' || json[pos] == '\n' || json[pos] == '\r'))
            pos++;

        if (pos >= json.size()) break;

        // Handle boolean/null
        if (json[pos] == 't') { results.push_back("true"); search_pos = pos + 4; continue; }
        if (json[pos] == 'f') { results.push_back("false"); search_pos = pos + 5; continue; }
        if (json[pos] == 'n') { results.push_back("null"); search_pos = pos + 4; continue; }

        if (json[pos] != '"') { search_pos = pos + 1; continue; }
        pos++; // skip opening quote

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
    // Strip leading 'v' if present
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
        // Use /releases/latest endpoint
        std::string cmd = "curl -sL -H 'Accept: application/json' "
                          "https://api.github.com/repos/" + std::string(REPO) + "/releases/latest";
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
        // Fetch recent releases and find first prerelease
        std::string cmd = "curl -sL -H 'Accept: application/json' "
                          "https://api.github.com/repos/" + std::string(REPO) + "/releases?per_page=20";
        std::string json = shell_exec(cmd);
        if (json.empty()) {
            std::cerr << "Error: Failed to fetch releases.\n";
            return target;
        }

        // Find prerelease entries: look for "prerelease": true paired with tag_name
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
        // Verify the release exists with HEAD request
        std::string url = build_download_url(tag, platform);
        std::string cmd = "curl -sL -o /dev/null -w '%{http_code}' --head '" + url + "'";
        std::string status = shell_exec(cmd);
        // Remove whitespace
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
    std::string cmd = "curl -sL -o '" + dest_path + "' '" + url + "'";
    std::string result = shell_exec(cmd);
    // shell_exec returns "" on non-zero exit, but for curl -o the output goes to file
    // Re-check: use system() for download since we need exit code
    int status = system(("curl -sfL -o '" + dest_path + "' '" + url + "'").c_str());
    return status == 0;
}

bool replace_binary(const std::string &download_url, const std::string &binary_path) {
    // Create temp directory
    char tmp_dir[] = "/tmp/ry-update-XXXXXX";
    if (mkdtemp(tmp_dir) == nullptr) {
        std::cerr << "Error: Failed to create temporary directory.\n";
        return false;
    }

    std::string tmp_dir_str(tmp_dir);
    std::string archive_path = tmp_dir_str + "/ry.tar.gz";

    // Download
    std::cerr << "Downloading..." << std::flush;
    if (!download_file(download_url, archive_path)) {
        std::cerr << " failed.\n";
        std::cerr << "Error: Download failed. Check your network connection.\n";
        system(("rm -rf '" + tmp_dir_str + "'").c_str());
        return false;
    }
    std::cerr << " done.\n";

    // Extract
    std::string extract_cmd = "tar xzf '" + archive_path + "' -C '" + tmp_dir_str + "'";
    if (system(extract_cmd.c_str()) != 0) {
        std::cerr << "Error: Failed to extract archive.\n";
        system(("rm -rf '" + tmp_dir_str + "'").c_str());
        return false;
    }

    // Find the extracted binary
    std::string new_binary = tmp_dir_str + "/ry";

    // Set permissions
    std::string chmod_cmd = "chmod 755 '" + new_binary + "'";
    system(chmod_cmd.c_str());

    // Atomic replace: rename over the existing binary
    if (rename(new_binary.c_str(), binary_path.c_str()) != 0) {
        // rename() may fail across filesystems, try cp + chmod
        std::string cp_cmd = "cp '" + new_binary + "' '" + binary_path + "'";
        if (system(cp_cmd.c_str()) != 0) {
            std::cerr << "Error: Failed to replace binary at " << binary_path << "\n";
            std::cerr << "You may need to run with sudo.\n";
            system(("rm -rf '" + tmp_dir_str + "'").c_str());
            return false;
        }
    }

    // Cleanup
    system(("rm -rf '" + tmp_dir_str + "'").c_str());
    return true;
}

int cmd_self_update(int argc, char *argv[]) {
    std::cerr << "Current version: ry " << RY_VERSION << "\n";

    // Parse arguments to determine mode
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
            mode = arg; // explicit version tag
        }
    }

    auto platform = detect_platform();
    auto target = resolve_update_target(mode, platform);
    if (target.tag.empty()) {
        return 1;
    }

    // Compare with current version
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
