#include "ry/self_update.hpp"
#include "ry/paths.hpp"
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <filesystem>
#include <fstream>
#include <iostream>
#include <sstream>
#include <algorithm>
#include <vector>
#include <regex>
#include <sys/stat.h>
#include <sys/wait.h>
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

int run_command(const std::vector<std::string> &args, std::string *output) {
    int pipefd[2] = {-1, -1};
    if (output) {
        if (pipe(pipefd) == -1) return -1;
    }

    pid_t pid = fork();
    if (pid == -1) {
        if (output) { close(pipefd[0]); close(pipefd[1]); }
        return -1;
    }

    if (pid == 0) {
        if (output) {
            close(pipefd[0]);
            dup2(pipefd[1], STDOUT_FILENO);
            close(pipefd[1]);
        }
        std::vector<char *> argv;
        for (auto &a : args) argv.push_back(const_cast<char *>(a.c_str()));
        argv.push_back(nullptr);
        execvp(argv[0], argv.data());
        _exit(127);
    }

    if (output) {
        close(pipefd[1]);
        char buf[4096];
        ssize_t n;
        while ((n = read(pipefd[0], buf, sizeof(buf))) > 0) {
            output->append(buf, n);
        }
        close(pipefd[0]);
    }

    int status;
    waitpid(pid, &status, 0);
    if (WIFEXITED(status)) return WEXITSTATUS(status);
    return -1;
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
        std::string http_status;
        run_command({"curl", "-sfL", "-o", "/dev/null", "-w", "%{http_code}",
                     "-H", "Accept: application/json", api_url}, &http_status);
        http_status.erase(std::remove_if(http_status.begin(), http_status.end(), ::isspace), http_status.end());

        if (http_status == "404") {
            // No stable release exists yet — treat as already up to date
            target.tag = std::string("v") + RY_VERSION;
            return target;
        }

        std::string json;
        if (run_command({"curl", "-sfL", "-H", "Accept: application/json", api_url}, &json) != 0 || json.empty()) {
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
        std::string releases_url = "https://api.github.com/repos/" + std::string(REPO) + "/releases?per_page=20";
        std::string json;
        if (run_command({"curl", "-sfL", "-H", "Accept: application/json", releases_url}, &json) != 0 || json.empty()) {
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
        std::string status;
        run_command({"curl", "-sfL", "-o", "/dev/null", "-w", "%{http_code}", "--head", url}, &status);
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
    return run_command({"curl", "-sfL", "-o", dest_path, url}) == 0;
}

std::string build_checksums_url(const std::string &tag) {
    return "https://github.com/" + std::string(REPO) +
           "/releases/download/" + tag + "/checksums.txt";
}

std::string parse_checksum_for_file(const std::string &checksums_content, const std::string &filename) {
    std::istringstream stream(checksums_content);
    std::string line;

    // Extract platform suffix for fallback matching (e.g., "darwin-arm64.tar.gz")
    std::string suffix;
    auto dash_pos = filename.rfind('-');
    if (dash_pos != std::string::npos) {
        // Look for the os-arch part: find second-to-last dash
        auto prev_dash = filename.rfind('-', dash_pos - 1);
        if (prev_dash != std::string::npos) {
            suffix = filename.substr(prev_dash + 1);
        }
    }

    std::string fallback_hash;

    while (std::getline(stream, line)) {
        if (line.empty()) continue;

        // Format: "<hash>  <filename>" (two spaces) or "<hash> <filename>" (one space)
        auto sep = line.find(' ');
        if (sep == std::string::npos) continue;

        std::string hash = line.substr(0, sep);
        // Skip spaces
        auto name_start = line.find_first_not_of(' ', sep);
        if (name_start == std::string::npos) continue;
        std::string entry_name = line.substr(name_start);

        // Exact match
        if (entry_name == filename) {
            return hash;
        }

        // Fallback: match by platform suffix
        if (!suffix.empty() && entry_name.size() >= suffix.size() &&
            entry_name.substr(entry_name.size() - suffix.size()) == suffix) {
            fallback_hash = hash;
        }
    }

    return fallback_hash;
}

std::string compute_sha256(const std::string &file_path) {
    std::string output;
    if (run_command({"shasum", "-a", "256", file_path}, &output) != 0 || output.empty()) {
        return "";
    }
    // Output format: "<hash>  <filename>"
    auto sep = output.find(' ');
    if (sep == std::string::npos) return "";
    return output.substr(0, sep);
}

bool verify_checksum(const std::string &archive_path, const std::string &expected_hash) {
    std::string actual_hash = compute_sha256(archive_path);
    if (actual_hash.empty()) {
        std::cerr << "Error: Failed to compute SHA-256 checksum.\n";
        return false;
    }
    return actual_hash == expected_hash;
}

bool validate_tar_entries(const std::string &archive_path) {
    std::string verbose_listing;
    if (run_command({"tar", "-tvzf", archive_path}, &verbose_listing) != 0) {
        std::cerr << "Error: Failed to list archive entries.\n";
        return false;
    }

    std::istringstream stream(verbose_listing);
    std::string line;
    while (std::getline(stream, line)) {
        if (line.empty()) continue;

        // Reject symlinks and hardlinks to prevent link-based file overwrites
        if (line[0] == 'l' || line[0] == 'h') {
            std::string type = (line[0] == 'l') ? "symlink" : "hardlink";
            std::cerr << "Error: Archive contains " << type << " entry: " << line << "\n";
            return false;
        }

        // Extract the entry path (last whitespace-delimited field)
        auto last_space = line.rfind(' ');
        if (last_space == std::string::npos) continue;
        std::string entry = line.substr(last_space + 1);
        if (entry.empty()) continue;

        // Reject absolute paths
        if (entry[0] == '/') {
            std::cerr << "Error: Archive contains absolute path: " << entry << "\n";
            return false;
        }

        // Reject path traversal
        if (entry == ".." || entry.find("../") != std::string::npos ||
            entry.find("/../") != std::string::npos ||
            (entry.size() >= 3 && entry.substr(entry.size() - 3) == "/..")) {
            std::cerr << "Error: Archive contains path traversal: " << entry << "\n";
            return false;
        }
    }

    return true;
}

std::string download_and_extract(const std::string &download_url, const std::string &tag) {
    char tmp_dir[] = "/tmp/ry-update-XXXXXX";
    if (mkdtemp(tmp_dir) == nullptr) {
        std::cerr << "Error: Failed to create temporary directory.\n";
        return "";
    }

    std::string tmp_dir_str(tmp_dir);
    std::string archive_path = tmp_dir_str + "/ry.tar.gz";

    std::cerr << "Downloading..." << std::flush;
    if (!download_file(download_url, archive_path)) {
        std::cerr << " failed.\n";
        std::cerr << "Error: Download failed. Check your network connection.\n";
        run_command({"rm", "-rf", tmp_dir_str});
        return "";
    }
    std::cerr << " done.\n";

    // Checksum verification
    std::string checksums_url = build_checksums_url(tag);
    std::string checksums_path = tmp_dir_str + "/checksums.txt";
    if (download_file(checksums_url, checksums_path)) {
        std::ifstream ifs(checksums_path);
        std::string checksums_content((std::istreambuf_iterator<char>(ifs)),
                                       std::istreambuf_iterator<char>());

        // Extract filename from download URL
        std::string expected_filename;
        auto slash_pos = download_url.rfind('/');
        if (slash_pos != std::string::npos) {
            expected_filename = download_url.substr(slash_pos + 1);
        }

        std::string expected_hash = parse_checksum_for_file(checksums_content, expected_filename);
        if (expected_hash.empty()) {
            std::cerr << "Error: Could not find checksum for " << expected_filename << " in checksums.txt.\n";
            run_command({"rm", "-rf", tmp_dir_str});
            return "";
        }

        std::cerr << "Verifying checksum..." << std::flush;
        if (!verify_checksum(archive_path, expected_hash)) {
            std::cerr << " failed.\n";
            std::cerr << "Error: Checksum verification failed. The downloaded file may be corrupted or tampered with.\n";
            run_command({"rm", "-rf", tmp_dir_str});
            return "";
        }
        std::cerr << " ok.\n";
    } else {
        std::cerr << "Error: checksums.txt not available. Cannot verify archive integrity.\n";
        run_command({"rm", "-rf", tmp_dir_str});
        return "";
    }

    // Validate tar entries before extraction
    if (!validate_tar_entries(archive_path)) {
        std::cerr << "Error: Archive contains unsafe entries. Aborting update.\n";
        run_command({"rm", "-rf", tmp_dir_str});
        return "";
    }

    if (run_command({"tar", "xzf", archive_path, "-C", tmp_dir_str}) != 0) {
        std::cerr << "Error: Failed to extract archive.\n";
        run_command({"rm", "-rf", tmp_dir_str});
        return "";
    }

    return tmp_dir_str;
}

bool replace_binary(const std::string &tmp_dir_str, const std::string &binary_path) {
    namespace fs = std::filesystem;
    std::string new_binary = tmp_dir_str + "/ry";

    if (!fs::is_regular_file(new_binary)) {
        std::cerr << "Error: Expected binary not found in archive.\n";
        return false;
    }

    if (chmod(new_binary.c_str(), 0755) != 0) {
        std::cerr << "Error: Failed to set permissions on downloaded binary.\n";
        return false;
    }

    if (rename(new_binary.c_str(), binary_path.c_str()) != 0) {
        // rename() may fail across filesystems, try cp fallback
        if (run_command({"cp", new_binary, binary_path}) != 0) {
            std::cerr << "Error: Failed to replace binary at " << binary_path << "\n";
            std::cerr << "You may need to run with sudo.\n";
            return false;
        }
    }

    return true;
}

bool install_stdlib(const std::string &tmp_dir_str, const std::string &new_version) {
    namespace fs = std::filesystem;

    fs::path src_std = fs::path(tmp_dir_str) / "lib" / "std";
    if (!fs::is_directory(src_std)) {
        return false;  // No stdlib in archive
    }

    fs::path dest_std = ry::get_ry_home() / "lib" / "std";

    // Read old manifest for cleanup
    auto old_manifest = ry::read_manifest(dest_std);

    // Create destination directory
    std::error_code ec;
    fs::create_directories(dest_std, ec);
    if (ec) {
        std::cerr << "Warning: Failed to create stdlib directory: " << ec.message() << "\n";
        return false;
    }

    // Copy new files (recursive)
    std::vector<std::string> new_files;
    for (const auto &entry : fs::recursive_directory_iterator(src_std)) {
        if (!entry.is_regular_file()) continue;
        auto rel_path = fs::relative(entry.path(), src_std).string();
        if (rel_path == "manifest.json") continue;

        auto dest_path = dest_std / rel_path;
        fs::create_directories(dest_path.parent_path(), ec);
        fs::copy_file(entry.path(), dest_path,
                      fs::copy_options::overwrite_existing, ec);
        if (!ec) {
            new_files.push_back(rel_path);
        }
    }

    // Delete files that were in old manifest but not in new,
    // then clean up empty parent directories
    std::sort(new_files.begin(), new_files.end());
    for (const auto &old_file : old_manifest.files) {
        if (old_file.empty() || old_file.find("..") != std::string::npos ||
            old_file[0] == '/' || old_file[0] == '\\') {
            std::cerr << "Warning: Skipping invalid stdlib manifest entry: " << old_file << "\n";
            continue;
        }
        if (!std::binary_search(new_files.begin(), new_files.end(), old_file)) {
            fs::remove(dest_std / old_file, ec);
            // Remove empty parent directories up to dest_std
            auto parent = (dest_std / old_file).parent_path();
            while (parent != dest_std && fs::is_directory(parent) && fs::is_empty(parent)) {
                fs::remove(parent, ec);
                parent = parent.parent_path();
            }
        }
    }

    // Write new manifest
    ry::write_manifest(dest_std, new_version, new_files);

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

    std::string tmp_dir = detail::download_and_extract(target.download_url, target.tag);
    if (tmp_dir.empty()) {
        return 1;
    }

    if (!detail::replace_binary(tmp_dir, binary_path)) {
        detail::run_command({"rm", "-rf", tmp_dir});
        return 1;
    }

    // Install/update stdlib
    std::string version = target.tag;
    if (!version.empty() && version[0] == 'v') version = version.substr(1);
    if (detail::install_stdlib(tmp_dir, version)) {
        std::cerr << "Standard library updated.\n";
    }

    detail::run_command({"rm", "-rf", tmp_dir});

    std::cerr << "Successfully updated to " << target.tag << ".\n";
    return 0;
}
