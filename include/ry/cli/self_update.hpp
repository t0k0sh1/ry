#pragma once
#include <cstdint>
#include <filesystem>
#include <optional>
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

struct Semver {
    uint64_t major;
    uint64_t minor;
    uint64_t patch;

    bool operator<=(const Semver &other) const {
        if (major != other.major) return major < other.major;
        if (minor != other.minor) return minor < other.minor;
        return patch <= other.patch;
    }
};

// Parse major.minor.patch from a tag string. Tolerates an optional leading
// 'v', leading zeros, and any trailing pre-release / build / extra-segment
// suffix (e.g. "v0.0.29-rc.1", "v0.0.29.1"); only the first three numeric
// components are inspected. Returns nullopt for malformed input.
std::optional<Semver> parse_semver(const std::string &tag);

// True iff `tag` parses to a version <= v0.0.29. Those releases predate
// the libemit / liblower / libLLVM / libzstd entries in install_native_libs,
// so a later forward jump after downgrading to them leaves the binary
// unable to resolve its bundled libs.
bool is_legacy_downgrade_target(const std::string &tag);

enum class DowngradeGuardResult {
    Allowed,             // Tag is not legacy, or repo is a fork
    AllowedWithWarning,  // Legacy tag, but RY_ALLOW_LEGACY_DOWNGRADE=1 opted in
    Blocked,             // Legacy tag, no escape hatch — refuse update
};

// Pure policy used by resolve_update_target. Kept side-effect-free so the
// full branch matrix can be tested without env mutation or network.
DowngradeGuardResult check_downgrade_guard(
    const std::string &tag,
    const std::string &repo,
    bool allow_legacy_env_set);

PlatformInfo detect_platform();
std::string get_executable_path();
int run_command(const std::vector<std::string> &args, std::string *output = nullptr);
std::string extract_json_string(const std::string &json, const std::string &key);
std::string build_download_url(const std::string &tag, const PlatformInfo &platform);
UpdateTarget resolve_update_target(const std::optional<std::string> &tag, const PlatformInfo &platform);
bool download_file(const std::string &url, const std::string &dest_path);
std::string build_checksums_url(const std::string &tag);
std::string build_signature_url(const std::string &tag);
std::string parse_checksum_for_file(const std::string &checksums_content, const std::string &filename);
std::string compute_sha256(const std::string &file_path);
bool verify_checksum(const std::string &archive_path, const std::string &expected_hash);
bool validate_tar_entries(const std::string &archive_path);
std::string base64_decode(const std::string &input);
bool verify_signature(const std::string &data, const std::string &sig_b64,
                      const unsigned char *pubkey, size_t pubkey_len);

enum class SignatureAction {
    Verified,       // Signature present and valid
    SkipAllowed,    // Signature missing, user opted out via RY_SKIP_SIGNATURE=1
    FailMissing,    // Signature missing, no opt-out — abort update
    FailInvalid,    // Signature present but verification failed — abort update
};

SignatureAction evaluate_signature_policy(
    bool sig_downloaded, bool sig_valid, bool skip_requested);

std::string download_and_extract(const std::string &download_url, const std::string &tag);
bool replace_binary(const std::string &tmp_dir, const std::string &binary_path);
bool install_stdlib(const std::string &tmp_dir);
bool install_native_libs(const std::string &tmp_dir);
bool install_rescue_script(const std::string &tmp_dir, const std::string &binary_path);
bool install_self_update_binary(const std::string &tmp_dir, const std::string &binary_path);

// --- Backup / smoke-test / rollback (#2456) ---

// Snapshot the pre-update state (binary, stdlib tree, native libs, rescue
// script) into `backup_root`. Returns true iff every existing component
// was copied successfully; on failure, the partial backup is removed so
// the caller can abort cleanly. `backup_root` is wiped before snapshot.
bool create_backup(const std::filesystem::path &backup_root,
                   const std::string &binary_path,
                   const std::filesystem::path &ry_home);

// Restore every component captured by create_backup to its original
// location. The current stdlib tree is removed before the backed-up
// tree is copied back so files added by the failed update are purged.
// Returns true iff every component restores successfully.
bool restore_backup(const std::filesystem::path &backup_root,
                    const std::string &binary_path,
                    const std::filesystem::path &ry_home);

// Best-effort removal of `backup_root`. Failures are logged but ignored —
// a stale backup directory is harmless and will be wiped on the next run.
void cleanup_backup(const std::filesystem::path &backup_root);

enum class SmokeTestResult {
    Pass,           // exit 0 + stdout contains the version banner ("ry ")
    FailExit,       // process exited with a non-zero status
    FailSignal,     // process was killed by a signal (e.g. dyld load failure)
    FailTimeout,    // process did not exit within the allotted seconds
    FailNoVersion,  // exit 0 but stdout did not look like a version banner
    FailLaunch,     // fork/exec/pipe machinery itself failed
};

// Run `<binary_path> --version` in a child process, kill it after
// `timeout_secs` if it has not exited. Pure with respect to the
// filesystem (no temp files written).
SmokeTestResult smoke_test_binary(const std::string &binary_path,
                                  int timeout_secs);

enum class UpdateLockResult {
    Acquired,           // Lock obtained (callers own the fd)
    BusyOtherProcess,   // Another live ry process holds the lock
    Error,              // I/O failure unrelated to contention
};

struct UpdateLockHandle {
    int fd;
    UpdateLockResult result;
};

// Atomically create the PID lock at `lock_path` via O_CREAT|O_EXCL. If a
// stale file is found (PID is no longer alive), it is reclaimed. Callers
// release with release_update_lock(handle.fd, lock_path) on every exit
// path — RAII guard recommended.
UpdateLockHandle acquire_update_lock(const std::filesystem::path &lock_path);
void release_update_lock(int fd, const std::filesystem::path &lock_path);

} // namespace detail
} // namespace ry::self_update

int cmd_ry_self_update(int argc, char *argv[]);
int cmd_self_update(int argc, char *argv[], const char *ry_argv0);
