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
int run_command(const std::vector<std::string> &args, std::string *output = nullptr);
std::string extract_json_string(const std::string &json, const std::string &key);
std::vector<std::string> extract_all_json_strings(const std::string &json, const std::string &key);
bool is_prerelease(const std::string &version);
std::string build_download_url(const std::string &tag, const PlatformInfo &platform);
UpdateTarget resolve_update_target(const std::string &mode, const PlatformInfo &platform);
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
    VERIFIED,       // Signature present and valid
    SKIP_ALLOWED,   // Signature missing, user opted out via RY_SKIP_SIGNATURE=1
    FAIL_MISSING,   // Signature missing, no opt-out — abort update
    FAIL_INVALID,   // Signature present but verification failed — abort update
};

SignatureAction evaluate_signature_policy(
    bool sig_downloaded, bool sig_valid, bool skip_requested);

std::string download_and_extract(const std::string &download_url, const std::string &tag);
bool replace_binary(const std::string &tmp_dir, const std::string &binary_path);
bool install_stdlib(const std::string &tmp_dir, const std::string &new_version);
bool install_native_libs(const std::string &tmp_dir_str);

} // namespace detail
} // namespace ry::self_update

int cmd_self_update(int argc, char *argv[]);
