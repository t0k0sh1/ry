#include "ry/cli/self_update.hpp"
#include "ry/project/paths.hpp"
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <filesystem>
#include <fstream>
#include <iostream>
#include <optional>
#include <sstream>
#include <algorithm>
#include <vector>
#include <regex>
#include <sys/stat.h>
#include <sys/wait.h>
#include <unistd.h>
#include <openssl/evp.h>

#ifdef __APPLE__
#include <mach-o/dyld.h>
#endif

namespace ry::self_update {

static std::string get_repo() {
    const char *env = std::getenv("RY_UPDATE_REPO");
    if (env && *env) return env;
    return "t0k0sh1/ry";
}

// Resolve a command from a list of candidate absolute paths.
// Falls back to bare name for PATH lookup if no candidate exists.
static std::string resolve_command(std::initializer_list<const char *> candidates) {
    namespace fs = std::filesystem;
    for (const char *path : candidates) {
        if (fs::exists(path) && fs::is_regular_file(path))
            return path;
    }
    return {};
}

// Resolved paths for external commands to prevent PATH injection.
// Candidates are tried in order; if none exist, self-update will fail
// gracefully when run_command returns non-zero.
static std::string CURL_CMD  = resolve_command({"/usr/bin/curl", "/usr/local/bin/curl"});
static std::string TAR_CMD   = resolve_command({"/usr/bin/tar", "/usr/local/bin/tar"});
static std::string SHASUM_CMD = resolve_command({"/usr/bin/shasum", "/usr/local/bin/shasum"});
#ifdef __APPLE__
static std::string CP_CMD = resolve_command({"/bin/cp", "/usr/bin/cp"});
static std::string RM_CMD = resolve_command({"/bin/rm", "/usr/bin/rm"});
#else
static std::string CP_CMD = resolve_command({"/usr/bin/cp", "/bin/cp"});
static std::string RM_CMD = resolve_command({"/usr/bin/rm", "/bin/rm"});
#endif

static const char *CURL_PATH   = CURL_CMD.c_str();
static const char *TAR_PATH    = TAR_CMD.c_str();
static const char *SHASUM_PATH = SHASUM_CMD.c_str();
static const char *CP_PATH     = CP_CMD.c_str();
static const char *RM_PATH     = RM_CMD.c_str();

bool is_valid_tag(const std::string &tag) {
    static const std::regex pattern("^v?[0-9A-Za-z._-]+$");
    return std::regex_match(tag, pattern);
}

namespace detail {

std::optional<Semver> parse_semver(const std::string &tag) {
    const char *p = tag.c_str();
    const char *end = p + tag.size();

    if (p < end && (*p == 'v' || *p == 'V')) ++p;

    auto parse_uint = [&](uint64_t &out) -> bool {
        // Require a leading decimal digit. strtoull would otherwise silently
        // accept '+' / '-' / whitespace and yield garbage for tags like
        // "v-1.0.0" (which is_valid_tag's [0-9A-Za-z._-]+ permits).
        if (p >= end || *p < '0' || *p > '9') return false;
        char *eptr = nullptr;
        errno = 0;
        out = std::strtoull(p, &eptr, 10);
        if (errno == ERANGE) return false;
        if (eptr == p) return false;
        p = eptr;
        return true;
    };

    Semver v{};
    if (!parse_uint(v.major)) return std::nullopt;
    if (p >= end || *p != '.') return std::nullopt;
    ++p;
    if (!parse_uint(v.minor)) return std::nullopt;
    if (p >= end || *p != '.') return std::nullopt;
    ++p;
    if (!parse_uint(v.patch)) return std::nullopt;
    // Anything after patch (pre-release, build metadata, 4-part suffix) is
    // tolerated — only major.minor.patch matters for the legacy guard.
    return v;
}

bool is_legacy_downgrade_target(const std::string &tag) {
    auto v = parse_semver(tag);
    if (!v) return false;
    static constexpr Semver LEGACY_CUTOFF{0, 0, 29};
    return *v <= LEGACY_CUTOFF;
}

DowngradeGuardResult check_downgrade_guard(
    const std::string &tag,
    const std::string &repo,
    bool allow_legacy_env_set) {
    // Forks (custom RY_UPDATE_REPO) don't share the upstream install-filter
    // history, so skip the guard entirely for them (CI test convenience).
    if (repo != "t0k0sh1/ry") return DowngradeGuardResult::Allowed;
    if (!is_legacy_downgrade_target(tag)) return DowngradeGuardResult::Allowed;
    if (allow_legacy_env_set) return DowngradeGuardResult::AllowedWithWarning;
    return DowngradeGuardResult::Blocked;
}

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
        argv.reserve(args.size() + 1);
        for (auto &a : args) argv.push_back(const_cast<char *>(a.c_str()));
        argv.push_back(nullptr);
        // Use execv for absolute paths (secure), execvp as fallback
        if (argv[0] && argv[0][0] == '/')
            execv(argv[0], argv.data());
        else
            execvp(argv[0], argv.data());
        _exit(127);
    }

    if (output) {
        close(pipefd[1]);
        char buf[4096];
        ssize_t n;
        while ((n = read(pipefd[0], buf, sizeof(buf))) > 0) {
            output->append(buf, static_cast<size_t>(n));
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

static std::string build_release_asset_url(const std::string &tag, const std::string &filename) {
    return "https://github.com/" + get_repo() +
           "/releases/download/" + tag + "/" + filename;
}

std::string build_download_url(const std::string &tag, const PlatformInfo &platform) {
    return build_release_asset_url(tag, "ry-" + tag + "-" + platform.os + "-" + platform.arch + ".tar.gz");
}

UpdateTarget resolve_update_target(const std::optional<std::string> &tag, const PlatformInfo &platform) {
    UpdateTarget target;

    if (!tag) {
        std::string api_url = "https://api.github.com/repos/" + get_repo() + "/releases/latest";
        // Check HTTP status first to distinguish 404 from other errors
        std::string http_status;
        run_command({CURL_PATH, "-sfL", "-o", "/dev/null", "-w", "%{http_code}",
                     "-H", "Accept: application/json", api_url}, &http_status);
        http_status.erase(std::remove_if(http_status.begin(), http_status.end(), ::isspace), http_status.end());

        if (http_status == "404") {
            // No stable release exists yet — treat as already up to date
            target.tag = std::string("v") + RY_VERSION;
            return target;
        }

        std::string json;
        if (run_command({CURL_PATH, "-sfL", "-H", "Accept: application/json", api_url}, &json) != 0 || json.empty()) {
            std::cerr << "Error: Failed to fetch latest release info.\n";
            return target;
        }
        target.tag = extract_json_string(json, "tag_name");
        if (target.tag.empty()) {
            std::cerr << "Error: Could not find latest stable release.\n";
            return target;
        }
        target.download_url = build_download_url(target.tag, platform);
    } else {
        // Caller guarantees the tag is already validated (is_valid_tag) and
        // normalized with the leading "v" prefix.
        const std::string &requested = *tag;

        // Refuse downgrades to releases whose install_native_libs filter
        // predates libemit / liblower / libLLVM / libzstd (#2457). Without
        // this guard, the user's next forward `ry self-update` would skip
        // installing libs the new binary depends on, leaving it unable to
        // start — same failure mode as the original libLLVM-missing report.
        const char *allow_env = std::getenv("RY_ALLOW_LEGACY_DOWNGRADE");
        bool allow_legacy = (allow_env && strcmp(allow_env, "1") == 0);
        switch (check_downgrade_guard(requested, get_repo(), allow_legacy)) {
        case DowngradeGuardResult::Blocked:
            std::cerr << "Error: Version " << requested
                      << " is not supported as a downgrade target.\n"
                      << "  v0.0.29 and earlier are forward-incompatible with the current self-update\n"
                      << "  install logic (libemit / liblower / libLLVM / libzstd are not installed by\n"
                      << "  those versions' install_native_libs filter).\n"
                      << "  Supported downgrade range: v0.0.30 or later.\n"
                      << "  For emergency recovery, use `ry-rescue` (see #2455).\n";
            return target;
        case DowngradeGuardResult::AllowedWithWarning:
            std::cerr << "Warning: RY_ALLOW_LEGACY_DOWNGRADE=1 — proceeding with downgrade to "
                      << requested << ".\n"
                      << "  After this, re-running `ry self-update` to come forward may leave the\n"
                      << "  binary unable to start because the older install_native_libs filter\n"
                      << "  does not install libemit / liblower / libLLVM / libzstd. Use\n"
                      << "  `ry-rescue` to recover if that happens.\n";
            break;
        case DowngradeGuardResult::Allowed:
            break;
        }

        std::string url = build_download_url(requested, platform);
        std::string status;
        run_command({CURL_PATH, "-sfL", "-o", "/dev/null", "-w", "%{http_code}", "--head", url}, &status);
        status.erase(std::remove_if(status.begin(), status.end(), ::isspace), status.end());

        if (status.empty() || (status != "200" && status != "302")) {
            std::cerr << "Error: Version " << requested << " not found.\n";
            return target;
        }
        target.tag = requested;
        target.download_url = url;
    }

    return target;
}

bool download_file(const std::string &url, const std::string &dest_path) {
    return run_command({CURL_PATH, "-sfL", "-o", dest_path, url}) == 0;
}

std::string build_checksums_url(const std::string &tag) {
    return build_release_asset_url(tag, "checksums.txt");
}

std::string build_signature_url(const std::string &tag) {
    return build_release_asset_url(tag, "checksums.txt.sig");
}

// Ed25519 public key for verifying release signatures (raw 32 bytes).
static const unsigned char SIGNING_PUBLIC_KEY[32] = {
    0x66, 0x20, 0xa3, 0xe1, 0x29, 0xff, 0x97, 0x3b, 0x5a, 0xf9, 0x65, 0xfb,
    0x09, 0x7f, 0x7d, 0xe4, 0x84, 0x24, 0xe5, 0x6b, 0xe3, 0x6e, 0x4b, 0x3d,
    0x13, 0x97, 0x08, 0xf7, 0x10, 0xb9, 0x3f, 0xc4
};

std::string base64_decode(const std::string &input) {
    if (input.empty()) return "";

    // Strip whitespace (base64 output may contain line wrapping)
    std::string cleaned;
    cleaned.reserve(input.size());
    for (char c : input) {
        if (c != '\n' && c != '\r' && c != ' ' && c != '\t')
            cleaned.push_back(c);
    }
    if (cleaned.empty()) return "";

    size_t max_len = 3 * (cleaned.size() + 3) / 4;
    std::string output(max_len, '\0');

    int decoded_len = EVP_DecodeBlock(
        reinterpret_cast<unsigned char *>(&output[0]),
        reinterpret_cast<const unsigned char *>(cleaned.data()),
        static_cast<int>(cleaned.size()));

    if (decoded_len < 0) return "";

    // EVP_DecodeBlock ignores padding in its return value — adjust manually
    int padding = 0;
    if (cleaned.size() >= 1 && cleaned[cleaned.size() - 1] == '=') padding++;
    if (cleaned.size() >= 2 && cleaned[cleaned.size() - 2] == '=') padding++;

    if (decoded_len < padding) return "";
    output.resize(static_cast<size_t>(decoded_len - padding));
    return output;
}

bool verify_signature(const std::string &data, const std::string &sig_b64,
                      const unsigned char *pubkey, size_t pubkey_len) {
    if (sig_b64.empty() || pubkey_len != 32) return false;

    std::string sig_raw = base64_decode(sig_b64);
    if (sig_raw.size() != 64) return false;

    EVP_PKEY *pkey = EVP_PKEY_new_raw_public_key(
        EVP_PKEY_ED25519, nullptr, pubkey, pubkey_len);
    if (!pkey) return false;

    EVP_MD_CTX *ctx = EVP_MD_CTX_new();
    if (!ctx) {
        EVP_PKEY_free(pkey);
        return false;
    }

    bool result = false;
    if (EVP_DigestVerifyInit(ctx, nullptr, nullptr, nullptr, pkey) == 1) {
        int rc = EVP_DigestVerify(
            ctx,
            reinterpret_cast<const unsigned char *>(sig_raw.data()), sig_raw.size(),
            reinterpret_cast<const unsigned char *>(data.data()), data.size());
        result = (rc == 1);
    }

    EVP_MD_CTX_free(ctx);
    EVP_PKEY_free(pkey);
    return result;
}

SignatureAction evaluate_signature_policy(
    bool sig_downloaded, bool sig_valid, bool skip_requested) {
    if (sig_downloaded) {
        return sig_valid ? SignatureAction::Verified
                         : SignatureAction::FailInvalid;
    }
    return skip_requested ? SignatureAction::SkipAllowed
                          : SignatureAction::FailMissing;
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
    if (run_command({SHASUM_PATH, "-a", "256", file_path}, &output) != 0 || output.empty()) {
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
    if (run_command({TAR_PATH, "-tvzf", archive_path}, &verbose_listing) != 0) {
        std::cerr << "Error: Failed to list archive entries.\n";
        return false;
    }

    std::istringstream stream(verbose_listing);
    std::string line;
    while (std::getline(stream, line)) {
        if (line.empty()) continue;

        // Only allow regular files and directories — reject all other entry types
        // (symlinks, hardlinks, device nodes, FIFOs, sockets, etc.)
        if (line[0] != '-' && line[0] != 'd') {
            std::cerr << "Error: Archive contains unsupported entry type: " << line << "\n";
            return false;
        }

        // Extract the entry path (last whitespace-delimited field)
        auto last_space = line.rfind(' ');
        if (last_space == std::string::npos) {
            std::cerr << "Error: Unparseable archive entry: " << line << "\n";
            return false;
        }
        std::string entry = line.substr(last_space + 1);
        if (entry.empty()) {
            std::cerr << "Error: Unparseable archive entry: " << line << "\n";
            return false;
        }

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
        run_command({RM_PATH, "-rf", tmp_dir_str});
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

        // Signature verification (before checksum verification)
        std::string sig_url = build_signature_url(tag);
        std::string sig_path = tmp_dir_str + "/checksums.txt.sig";
        const char *skip_env = std::getenv("RY_SKIP_SIGNATURE");
        bool skip_requested = (skip_env && strcmp(skip_env, "1") == 0);

        bool sig_downloaded = download_file(sig_url, sig_path);
        bool sig_valid = false;
        if (sig_downloaded) {
            std::ifstream sig_ifs(sig_path);
            std::string sig_content((std::istreambuf_iterator<char>(sig_ifs)),
                                     std::istreambuf_iterator<char>());
            // Trim whitespace
            while (!sig_content.empty() &&
                   (sig_content.back() == '\n' || sig_content.back() == '\r' ||
                    sig_content.back() == ' '  || sig_content.back() == '\t')) {
                sig_content.pop_back();
            }

            std::cerr << "Verifying signature..." << std::flush;
            sig_valid = verify_signature(checksums_content, sig_content,
                                         SIGNING_PUBLIC_KEY, sizeof(SIGNING_PUBLIC_KEY));
        }

        auto action = evaluate_signature_policy(sig_downloaded, sig_valid, skip_requested);
        switch (action) {
        case SignatureAction::Verified:
            std::cerr << " ok.\n";
            break;
        case SignatureAction::SkipAllowed:
            std::cerr << "Warning: Signature file not available and "
                      << "RY_SKIP_SIGNATURE is set. "
                      << "Proceeding WITHOUT signature verification. "
                      << "This is UNSAFE and disables supply-chain protection.\n";
            break;
        case SignatureAction::FailMissing:
            std::cerr << "Error: Signature file not available. "
                      << "Cannot verify release authenticity.\n"
                      << "If you must proceed without signature verification, "
                      << "set RY_SKIP_SIGNATURE=1 (not recommended).\n";
            run_command({RM_PATH, "-rf", tmp_dir_str});
            return "";
        case SignatureAction::FailInvalid:
            std::cerr << " failed.\n";
            std::cerr << "Error: Signature verification failed. "
                      << "The checksums file may have been tampered with.\n";
            run_command({RM_PATH, "-rf", tmp_dir_str});
            return "";
        }

        // Extract filename from download URL
        std::string expected_filename;
        auto slash_pos = download_url.rfind('/');
        if (slash_pos != std::string::npos) {
            expected_filename = download_url.substr(slash_pos + 1);
        }

        std::string expected_hash = parse_checksum_for_file(checksums_content, expected_filename);
        if (expected_hash.empty()) {
            std::cerr << "Error: Could not find checksum for " << expected_filename << " in checksums.txt.\n";
            run_command({RM_PATH, "-rf", tmp_dir_str});
            return "";
        }

        std::cerr << "Verifying checksum..." << std::flush;
        if (!verify_checksum(archive_path, expected_hash)) {
            std::cerr << " failed.\n";
            std::cerr << "Error: Checksum verification failed. The downloaded file may be corrupted or tampered with.\n";
            run_command({RM_PATH, "-rf", tmp_dir_str});
            return "";
        }
        std::cerr << " ok.\n";
    } else {
        std::cerr << "Error: checksums.txt not available. Cannot verify archive integrity.\n";
        run_command({RM_PATH, "-rf", tmp_dir_str});
        return "";
    }

    // Validate tar entries before extraction
    if (!validate_tar_entries(archive_path)) {
        std::cerr << "Error: Archive contains unsafe entries. Aborting update.\n";
        run_command({RM_PATH, "-rf", tmp_dir_str});
        return "";
    }

    if (run_command({TAR_PATH, "xzf", archive_path, "-C", tmp_dir_str}) != 0) {
        std::cerr << "Error: Failed to extract archive.\n";
        run_command({RM_PATH, "-rf", tmp_dir_str});
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
        if (run_command({CP_PATH, new_binary, binary_path}) != 0) {
            std::cerr << "Error: Failed to replace binary at " << binary_path << "\n";
            std::cerr << "You may need to run with sudo.\n";
            return false;
        }
    }

    return true;
}

bool install_stdlib(const std::string &tmp_dir_str) {
    namespace fs = std::filesystem;

    // Detect archive layout: new archives use share/std, old ones use lib/std.
    // Install destination matches the archive layout so the corresponding binary
    // can find stdlib at the path it expects.
    bool new_layout = true;
    fs::path src_std = fs::path(tmp_dir_str) / "share" / "std";
    if (!fs::is_directory(src_std)) {
        src_std = fs::path(tmp_dir_str) / "lib" / "std";
        new_layout = false;
        if (!fs::is_directory(src_std)) {
            return false;  // No stdlib in archive
        }
    }

    const fs::path ry_home = ry::get_ry_home();
    fs::path dest_std = new_layout
        ? ry_home / "share" / "std"
        : ry_home / "lib" / "std";

    // Read old manifest for cleanup
    auto old_manifest = ry::read_manifest(dest_std);

    // Create destination directory
    std::error_code ec;
    fs::create_directories(dest_std, ec);
    if (ec) {
        std::cerr << "Warning: Failed to create stdlib directory: " << ec.message() << "\n";
        return false;
    }

    // Copy new files (recursive), tracking source count for integrity check
    std::vector<std::string> new_files;
    size_t src_file_count = 0;
    for (const auto &entry : fs::recursive_directory_iterator(src_std)) {
        if (!entry.is_regular_file()) continue;
        auto rel_path = fs::relative(entry.path(), src_std).string();
        if (rel_path == "manifest.json") continue;
        ++src_file_count;

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
    ry::write_manifest(dest_std, new_files);

    // Clean up old lib/std only when ALL files were copied successfully.
    // On partial failure, keep old lib/std so the user retains a working stdlib.
    if (new_layout && new_files.size() == src_file_count && src_file_count > 0) {
        fs::path old_std = ry_home / "lib" / "std";
        if (fs::is_directory(old_std)) {
            fs::remove_all(old_std, ec);
            // Remove lib/ if now empty; fs::remove is a no-op on non-empty dirs
            fs::remove(ry_home / "lib", ec);
        }
    }

    return true;
}

bool install_rescue_script(const std::string &tmp_dir, const std::string &binary_path) {
    namespace fs = std::filesystem;
    fs::path src = fs::path(tmp_dir) / "ry-rescue";
    // Pre-#2455 tarballs do not ship ry-rescue. Reject silently so a stable
    // self-update against an older release can still succeed. Use symlink_status
    // so we never follow a malicious symlink in the extracted tree.
    std::error_code ec;
    auto status = fs::symlink_status(src, ec);
    if (ec || status.type() != fs::file_type::regular)
        return false;

    fs::path dest = fs::path(binary_path).parent_path() / "ry-rescue";
    fs::copy_file(src, dest, fs::copy_options::overwrite_existing, ec);
    if (ec) {
        std::cerr << "Warning: Failed to install ry-rescue: " << ec.message() << "\n";
        return false;
    }
    // Force +x for all roles. copy_file copies the source mode, but the
    // recovery story collapses if execute bits get stripped on a tarball
    // unpacked through a tool that masks them (e.g. via a noexec mount in
    // the middle of the path; or a CI runner with a tight umask).
    if (chmod(dest.c_str(), 0755) != 0) {
        std::cerr << "Warning: Failed to mark ry-rescue executable.\n";
        return false;
    }
    return true;
}

bool install_native_libs(const std::string &tmp_dir) {
    namespace fs = std::filesystem;
    fs::path src_lib = fs::path(tmp_dir) / "lib";
    if (!fs::is_directory(src_lib))
        return false;

    const fs::path dest_lib = ry::get_ry_home() / "lib";
    std::error_code ec;
    fs::create_directories(dest_lib, ec);
    if (ec) {
        std::cerr << "Warning: Failed to create lib directory: " << ec.message() << "\n";
        return false;
    }

    bool any_installed = false;
    for (const auto &entry : fs::directory_iterator(src_lib)) {
        if (!entry.is_regular_file()) continue;
        auto filename = entry.path().filename().string();
        // Install the Rust cdylibs (libemit, liblower) + stdlib native libs
        // (libry_*), and — post-#1999 cutover — the bundled shared libLLVM
        // (plus its macOS chain dep libzstd), so the relocated runtime stays
        // self-contained (#2005). libemit was renamed from libry_codegen
        // (#2040) and liblower is the upper-codegen Rust kickoff (#2397), so
        // neither matches the libry_ prefix and each needs its own check.
        // Ignore anything else.
        bool is_bundled_lib = filename.find("libemit") == 0 ||
                              filename.find("liblower") == 0 ||
                              filename.find("libry_") == 0 ||
                              filename.find("libLLVM") == 0 ||
                              filename.find("libzstd") == 0;
        if (!is_bundled_lib) continue;
        auto dest_path = dest_lib / filename;
        fs::copy_file(entry.path(), dest_path,
                      fs::copy_options::overwrite_existing, ec);
        if (ec) {
            std::cerr << "Warning: Failed to copy " << filename << ": " << ec.message() << "\n";
        } else {
            any_installed = true;
        }
    }
    return any_installed;
}

} // namespace detail
} // namespace ry::self_update

int cmd_self_update(int argc, char *argv[]) {
    using namespace ry::self_update;
    using namespace ry::self_update::detail;

    std::cerr << "Current version: ry " << RY_VERSION << "\n";

    std::optional<std::string> requested_tag;
    if (argc >= 1) {
        std::string arg = argv[0];
        if (arg == "--help" || arg == "-h") {
            std::cerr << "Usage: ry self-update [<version>]\n";
            std::cerr << "\n";
            std::cerr << "Options:\n";
            std::cerr << "  (no args)    Update to latest stable release\n";
            std::cerr << "  <version>    Update to a specific version (e.g. v0.0.1)\n";
            std::cerr << "\nEnvironment:\n";
            std::cerr << "  RY_SKIP_SIGNATURE=1         Skip signature verification (unsafe)\n";
            std::cerr << "  RY_ALLOW_LEGACY_DOWNGRADE=1 Allow downgrade to v0.0.29 or earlier (unsafe)\n";
            std::cerr << "\nEmergency recovery:\n";
            std::cerr << "  If ry fails to start (e.g. missing ~/.ry/lib/libLLVM),\n";
            std::cerr << "  run 'ry-rescue' instead — it is a standalone POSIX shell\n";
            std::cerr << "  script that does not link libLLVM and can reinstall a\n";
            std::cerr << "  working release.\n";
            return 0;
        } else if (arg == "--nightly") {
            std::cerr << "Error: --nightly is no longer supported. self-update always targets the latest stable release.\n";
            std::cerr << "       (removed in v0.0.14 — see #1372)\n";
            return 1;
        } else {
            if (!is_valid_tag(arg)) {
                std::cerr << "Error: Invalid version format: " << arg << "\n";
                return 1;
            }
            requested_tag = (arg.empty() || arg[0] == 'v') ? arg : "v" + arg;
        }
    }

    auto platform = detect_platform();
    auto target = resolve_update_target(requested_tag, platform);
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
        detail::run_command({RM_PATH, "-rf", tmp_dir});
        return 1;
    }

    // Install/update stdlib
    if (detail::install_stdlib(tmp_dir)) {
        std::cerr << "Standard library updated.\n";
    }

    // Install/update native shared libraries
    if (detail::install_native_libs(tmp_dir)) {
        std::cerr << "Native libraries updated.\n";
    }

    // Refresh the bundled ry-rescue emergency recovery script (#2455). Older
    // releases (pre-#2455) do not ship it — install_rescue_script returns
    // false in that case and we stay silent.
    if (detail::install_rescue_script(tmp_dir, binary_path)) {
        std::cerr << "ry-rescue recovery script updated.\n";
    }

    detail::run_command({RM_PATH, "-rf", tmp_dir});

    std::cerr << "Successfully updated to " << target.tag << ".\n";
    return 0;
}
