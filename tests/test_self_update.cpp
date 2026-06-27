#include <gtest/gtest.h>
#include "ry/cli/self_update.hpp"
#include "ry/project/paths.hpp"
#include <filesystem>
#include <fstream>
#include <iterator>
#include <cstdlib>
#include <cstdio>
#include <string>

using namespace ry::self_update;
using namespace ry::self_update::detail;
namespace fs = std::filesystem;

TEST(SelfUpdate, BuildDownloadUrl) {
    PlatformInfo darwin_arm64{"darwin", "arm64"};
    PlatformInfo linux_amd64{"linux", "amd64"};

    auto url1 = build_download_url("v0.0.1", darwin_arm64);
    EXPECT_EQ(url1, "https://github.com/t0k0sh1/ry/releases/download/v0.0.1/ry-v0.0.1-darwin-arm64.tar.gz");

    auto url2 = build_download_url("v0.0.2-dev.20250101", linux_amd64);
    EXPECT_EQ(url2, "https://github.com/t0k0sh1/ry/releases/download/v0.0.2-dev.20250101/ry-v0.0.2-dev.20250101-linux-amd64.tar.gz");
}

TEST(SelfUpdate, ExtractJsonString) {
    std::string json = R"({"tag_name": "v0.0.1", "name": "Release v0.0.1", "prerelease": false})";

    EXPECT_EQ(extract_json_string(json, "tag_name"), "v0.0.1");
    EXPECT_EQ(extract_json_string(json, "name"), "Release v0.0.1");
    // Non-quoted JSON values (bool / null) are intentionally not extracted —
    // the only production callers (`tag_name`, `version`) read string values.
    EXPECT_EQ(extract_json_string(json, "prerelease"), "");
    EXPECT_EQ(extract_json_string(json, "nonexistent"), "");
}

TEST(SelfUpdate, ExtractJsonStringWithEscapes) {
    std::string json = R"({"url": "https://example.com/path"})";
    EXPECT_EQ(extract_json_string(json, "url"), "https://example.com/path");
}

TEST(SelfUpdate, DetectPlatformNonEmpty) {
    auto platform = detect_platform();
    EXPECT_FALSE(platform.os.empty());
    EXPECT_FALSE(platform.arch.empty());
    EXPECT_TRUE(platform.os == "darwin" || platform.os == "linux");
    EXPECT_TRUE(platform.arch == "arm64" || platform.arch == "amd64");
}

TEST(SelfUpdate, IsValidTag) {
    EXPECT_TRUE(is_valid_tag("v0.0.1"));
    EXPECT_TRUE(is_valid_tag("0.1.0"));
    EXPECT_TRUE(is_valid_tag("v1.0.0-rc.1"));
    EXPECT_TRUE(is_valid_tag("v0.0.2-dev.20250101"));

    EXPECT_FALSE(is_valid_tag("v0.0.1; rm -rf /"));
    EXPECT_FALSE(is_valid_tag("v0.0.1' && echo pwned"));
    EXPECT_FALSE(is_valid_tag("$(whoami)"));
    EXPECT_FALSE(is_valid_tag(""));
}

// Shared base for install_stdlib / install_native_libs tests.
// Provides tmp dir lifecycle, RY_HOME save/restore, and write_file helper.
class RyInstallTestBase : public ::testing::Test {
protected:
    fs::path tmp_root;
    fs::path src_dir;
    fs::path dest_home;
    std::string old_ry_home;

    void SetUpDirs(const char *tmp_name) {
        tmp_root = fs::temp_directory_path() / tmp_name;
        fs::remove_all(tmp_root);
        src_dir = tmp_root / "archive";
        dest_home = tmp_root / "ry_home";
        fs::create_directories(src_dir);
        fs::create_directories(dest_home);
        if (const char *env = std::getenv("RY_HOME")) {
            old_ry_home = env;
        }
        setenv("RY_HOME", dest_home.c_str(), 1);
    }

    void TearDown() override {
        if (old_ry_home.empty()) {
            unsetenv("RY_HOME");
        } else {
            setenv("RY_HOME", old_ry_home.c_str(), 1);
        }
        fs::remove_all(tmp_root);
    }

    void write_file(const fs::path &path, const std::string &content) {
        fs::create_directories(path.parent_path());
        std::ofstream(path) << content;
    }
};

class InstallStdlibTest : public RyInstallTestBase {
protected:
    void SetUp() override {
        SetUpDirs("ry_test_install_stdlib");
        fs::create_directories(src_dir / "share" / "std");
        fs::create_directories(dest_home / "share" / "std");
    }
};

TEST_F(InstallStdlibTest, CopiesNestedModules) {
    auto src_std = src_dir / "share" / "std";
    write_file(src_std / "builtins.ry", "# builtins");
    write_file(src_std / "http" / "http.ry", "# http module");
    write_file(src_std / "net" / "net.ry", "# net module");

    bool ok = install_stdlib(src_dir.string());
    ASSERT_TRUE(ok);

    auto dest_std = dest_home / "share" / "std";
    EXPECT_TRUE(fs::exists(dest_std / "builtins.ry"));
    EXPECT_TRUE(fs::exists(dest_std / "http" / "http.ry"));
    EXPECT_TRUE(fs::exists(dest_std / "net" / "net.ry"));
}

TEST_F(InstallStdlibTest, ManifestContainsRelativePaths) {
    auto src_std = src_dir / "share" / "std";
    write_file(src_std / "builtins.ry", "# builtins");
    write_file(src_std / "http" / "http.ry", "# http");

    install_stdlib(src_dir.string());

    auto manifest = ry::read_manifest(dest_home / "share" / "std");

    // Check that both top-level and nested files are in the manifest
    auto &files = manifest.files;
    EXPECT_NE(std::find(files.begin(), files.end(), "builtins.ry"), files.end());
    EXPECT_NE(std::find(files.begin(), files.end(), "http/http.ry"), files.end());
}

TEST_F(InstallStdlibTest, CleansUpOldNestedFiles) {
    auto dest_std = dest_home / "share" / "std";

    // Pre-populate dest with an old nested file and manifest
    write_file(dest_std / "old_mod" / "old.ry", "# old");
    ry::write_manifest(dest_std, {"old_mod/old.ry"});

    // New archive has only builtins.ry
    auto src_std = src_dir / "share" / "std";
    write_file(src_std / "builtins.ry", "# builtins");

    install_stdlib(src_dir.string());

    EXPECT_TRUE(fs::exists(dest_std / "builtins.ry"));
    EXPECT_FALSE(fs::exists(dest_std / "old_mod" / "old.ry"));
}

TEST_F(InstallStdlibTest, CleansUpEmptyDirectories) {
    auto dest_std = dest_home / "share" / "std";

    // Pre-populate with a nested file
    write_file(dest_std / "removed_mod" / "removed.ry", "# removed");
    ry::write_manifest(dest_std, {"removed_mod/removed.ry"});

    // New archive has no removed_mod
    auto src_std = src_dir / "share" / "std";
    write_file(src_std / "builtins.ry", "# builtins");

    install_stdlib(src_dir.string());

    EXPECT_FALSE(fs::exists(dest_std / "removed_mod"));
}

TEST_F(InstallStdlibTest, CleansUpDeeplyNestedEmptyDirectories) {
    auto dest_std = dest_home / "share" / "std";

    // Pre-populate with a deeply nested file (a/b/c.ry)
    write_file(dest_std / "a" / "b" / "c.ry", "# deep");
    ry::write_manifest(dest_std, {"a/b/c.ry"});

    // New archive has no a/b/c.ry
    auto src_std = src_dir / "share" / "std";
    write_file(src_std / "builtins.ry", "# builtins");

    install_stdlib(src_dir.string());

    // Both a/b/ and a/ should be removed
    EXPECT_FALSE(fs::exists(dest_std / "a" / "b"));
    EXPECT_FALSE(fs::exists(dest_std / "a"));
}

TEST_F(InstallStdlibTest, OldLayoutArchiveInstallsToLibStd) {
    // Simulate an old release archive with lib/std instead of share/std.
    // The old binary expects lib/std, so stdlib must be installed there.
    fs::remove_all(src_dir / "share");
    fs::create_directories(src_dir / "lib" / "std");
    write_file(src_dir / "lib" / "std" / "builtins.ry", "# builtins from old layout");

    bool ok = install_stdlib(src_dir.string());
    ASSERT_TRUE(ok);

    // Old layout archive → install to lib/std (matching old binary expectation)
    auto dest_std = dest_home / "lib" / "std";
    EXPECT_TRUE(fs::exists(dest_std / "builtins.ry"));
}

TEST_F(InstallStdlibTest, ReturnsFalseWhenNoStdlibInArchive) {
    auto dest_std = dest_home / "share" / "std";
    EXPECT_TRUE(fs::is_empty(dest_std));
    fs::remove_all(src_dir / "share" / "std");

    bool ok = install_stdlib(src_dir.string());
    EXPECT_FALSE(ok);
    EXPECT_TRUE(fs::is_empty(dest_std));
}

TEST_F(InstallStdlibTest, HandlesEmptyStdlibDirectory) {
    bool ok = install_stdlib(src_dir.string());
    EXPECT_TRUE(ok);
}

class InstallNativeLibsTest : public RyInstallTestBase {
protected:
    void SetUp() override {
        SetUpDirs("ry_test_install_native_libs");
    }
};

TEST_F(InstallNativeLibsTest, ReturnsFalseWhenNoLibDirectory) {
    bool ok = install_native_libs(src_dir.string());
    EXPECT_FALSE(ok);
    EXPECT_FALSE(fs::exists(dest_home / "lib"));
}

TEST_F(InstallNativeLibsTest, CopiesLibryPrefixedFiles) {
    write_file(src_dir / "lib" / "libry_parser.dylib", "parser lib");
    write_file(src_dir / "lib" / "libry_runtime.so", "runtime lib");

    bool ok = install_native_libs(src_dir.string());
    ASSERT_TRUE(ok);

    EXPECT_TRUE(fs::exists(dest_home / "lib" / "libry_parser.dylib"));
    EXPECT_TRUE(fs::exists(dest_home / "lib" / "libry_runtime.so"));
}

TEST_F(InstallNativeLibsTest, SkipsNonLibryFiles) {
    write_file(src_dir / "lib" / "libry_parser.dylib", "parser lib");
    write_file(src_dir / "lib" / "libother.so", "other lib");
    write_file(src_dir / "lib" / "readme.txt", "readme");

    bool ok = install_native_libs(src_dir.string());
    ASSERT_TRUE(ok);

    EXPECT_TRUE(fs::exists(dest_home / "lib" / "libry_parser.dylib"));
    EXPECT_FALSE(fs::exists(dest_home / "lib" / "libother.so"));
    EXPECT_FALSE(fs::exists(dest_home / "lib" / "readme.txt"));
}

TEST_F(InstallNativeLibsTest, CopiesBundledLLVMAndZstd) {
    // After the #1999 cutover `ry` needs a shared libLLVM at runtime; the
    // release tarball bundles it (plus its macOS chain dep libzstd) in lib/,
    // so self-update must install them next to libry_* (#2005).
    write_file(src_dir / "lib" / "libry_parser.dylib", "parser lib");
    write_file(src_dir / "lib" / "libLLVM.dylib", "llvm macos");
    write_file(src_dir / "lib" / "libLLVM.so.21", "llvm linux soname");
    write_file(src_dir / "lib" / "libzstd.1.dylib", "zstd macos");

    bool ok = install_native_libs(src_dir.string());
    ASSERT_TRUE(ok);

    EXPECT_TRUE(fs::exists(dest_home / "lib" / "libry_parser.dylib"));
    EXPECT_TRUE(fs::exists(dest_home / "lib" / "libLLVM.dylib"));
    EXPECT_TRUE(fs::exists(dest_home / "lib" / "libLLVM.so.21"));
    EXPECT_TRUE(fs::exists(dest_home / "lib" / "libzstd.1.dylib"));
}

TEST_F(InstallNativeLibsTest, CopiesRustCdylibLibemit) {
    // The Rust LLVM-IR-emission cdylib `libemit.*` (renamed from `libry_codegen`
    // in #2040) no longer starts with `libry_`, so it is NOT caught by the
    // `libry_*` selector and must be matched explicitly — otherwise the installed
    // runtime cannot load the cdylib `ry` links at startup (#2040). The libry_
    // sibling is present so this isolates "libemit is also installed".
    write_file(src_dir / "lib" / "libry_parser.dylib", "parser lib");
    write_file(src_dir / "lib" / "libemit.dylib", "emit cdylib macos");
    write_file(src_dir / "lib" / "libemit.so", "emit cdylib linux");

    bool ok = install_native_libs(src_dir.string());
    ASSERT_TRUE(ok);

    EXPECT_TRUE(fs::exists(dest_home / "lib" / "libemit.dylib"));
    EXPECT_TRUE(fs::exists(dest_home / "lib" / "libemit.so"));
}

TEST_F(InstallNativeLibsTest, InstallsLLVMWithoutLibryFiles) {
    // A bundle could in principle carry only libLLVM (no libry_*); it must
    // still install and report success.
    write_file(src_dir / "lib" / "libLLVM.so.21", "llvm");

    bool ok = install_native_libs(src_dir.string());
    EXPECT_TRUE(ok);
    EXPECT_TRUE(fs::exists(dest_home / "lib" / "libLLVM.so.21"));
}

TEST_F(InstallNativeLibsTest, OverwritesExistingFiles) {
    write_file(dest_home / "lib" / "libry_parser.dylib", "old version");
    write_file(src_dir / "lib" / "libry_parser.dylib", "new version");

    bool ok = install_native_libs(src_dir.string());
    ASSERT_TRUE(ok);

    std::ifstream ifs(dest_home / "lib" / "libry_parser.dylib");
    std::string content((std::istreambuf_iterator<char>(ifs)),
                         std::istreambuf_iterator<char>());
    EXPECT_EQ(content, "new version");
}

TEST_F(InstallNativeLibsTest, ReturnsFalseWhenNoLibryFiles) {
    write_file(src_dir / "lib" / "libother.so", "other lib");

    bool ok = install_native_libs(src_dir.string());
    EXPECT_FALSE(ok);
}

TEST_F(InstallNativeLibsTest, SkipsSubdirectories) {
    write_file(src_dir / "lib" / "libry_parser.dylib", "parser lib");
    write_file(src_dir / "lib" / "subdir" / "libry_nested.so", "nested lib");

    bool ok = install_native_libs(src_dir.string());
    ASSERT_TRUE(ok);

    EXPECT_TRUE(fs::exists(dest_home / "lib" / "libry_parser.dylib"));
    EXPECT_FALSE(fs::exists(dest_home / "lib" / "subdir"));
}

TEST_F(InstallNativeLibsTest, ReturnsFalseWhenDestLibPathIsAFile) {
    write_file(dest_home / "lib", "not a directory");
    write_file(src_dir / "lib" / "libry_parser.dylib", "parser lib");

    testing::internal::CaptureStderr();
    bool ok = install_native_libs(src_dir.string());
    std::string err = testing::internal::GetCapturedStderr();
    EXPECT_FALSE(ok);
    EXPECT_NE(err.find("Warning: Failed to create lib directory"), std::string::npos);
}

TEST_F(InstallNativeLibsTest, ReturnsFalseWhenCopyFails) {
    fs::create_directories(dest_home / "lib" / "libry_parser.dylib");
    write_file(src_dir / "lib" / "libry_parser.dylib", "parser lib");

    testing::internal::CaptureStderr();
    bool ok = install_native_libs(src_dir.string());
    std::string err = testing::internal::GetCapturedStderr();
    EXPECT_FALSE(ok);
    EXPECT_NE(err.find("Warning: Failed to copy"), std::string::npos);
}

// --- ry-rescue install tests (#2455) ---
//
// install_rescue_script copies a bundled `ry-rescue` shell script from the
// extracted tarball next to the running ry binary (same directory as
// `binary_path`). This keeps the recovery tool reachable via the same PATH
// the user already has for `ry`. Older tarballs (pre-#2455) do not carry
// ry-rescue — the function must report false in that case so the self-update
// success message can stay accurate.

class InstallRescueScriptTest : public RyInstallTestBase {
protected:
    fs::path binary_path;

    void SetUp() override {
        SetUpDirs("ry_test_install_rescue_script");
        binary_path = tmp_root / "bin" / "ry";
        fs::create_directories(binary_path.parent_path());
        write_file(binary_path, "fake ry");
    }
};

TEST_F(InstallRescueScriptTest, CopiesRescueScriptNextToBinary) {
    write_file(src_dir / "ry-rescue", "#!/bin/sh\necho rescue\n");

    bool ok = install_rescue_script(src_dir.string(), binary_path.string());
    ASSERT_TRUE(ok);

    auto dest = binary_path.parent_path() / "ry-rescue";
    EXPECT_TRUE(fs::exists(dest));

    std::ifstream ifs(dest);
    std::string content((std::istreambuf_iterator<char>(ifs)),
                         std::istreambuf_iterator<char>());
    EXPECT_EQ(content, "#!/bin/sh\necho rescue\n");
}

TEST_F(InstallRescueScriptTest, MarksRescueScriptExecutable) {
    // The shipped tarball already sets mode 0755, but copy_file on some
    // filesystems may strip execute bits — assert the install path forces
    // them back on so users can run ry-rescue directly.
    write_file(src_dir / "ry-rescue", "#!/bin/sh\n");

    bool ok = install_rescue_script(src_dir.string(), binary_path.string());
    ASSERT_TRUE(ok);

    auto dest = binary_path.parent_path() / "ry-rescue";
    auto perms = fs::status(dest).permissions();
    EXPECT_NE(perms & fs::perms::owner_exec, fs::perms::none);
    EXPECT_NE(perms & fs::perms::group_exec, fs::perms::none);
    EXPECT_NE(perms & fs::perms::others_exec, fs::perms::none);
}

TEST_F(InstallRescueScriptTest, ReturnsFalseWhenNoRescueInArchive) {
    // Pre-#2455 tarballs do not ship ry-rescue. The function must report
    // false instead of warning/failing — self-update should succeed
    // against older releases that legitimately have no rescue script.
    bool ok = install_rescue_script(src_dir.string(), binary_path.string());
    EXPECT_FALSE(ok);
    EXPECT_FALSE(fs::exists(binary_path.parent_path() / "ry-rescue"));
}

TEST_F(InstallRescueScriptTest, OverwritesExistingRescue) {
    write_file(binary_path.parent_path() / "ry-rescue", "#!/bin/sh\necho OLD\n");
    write_file(src_dir / "ry-rescue", "#!/bin/sh\necho NEW\n");

    bool ok = install_rescue_script(src_dir.string(), binary_path.string());
    ASSERT_TRUE(ok);

    std::ifstream ifs(binary_path.parent_path() / "ry-rescue");
    std::string content((std::istreambuf_iterator<char>(ifs)),
                         std::istreambuf_iterator<char>());
    EXPECT_EQ(content, "#!/bin/sh\necho NEW\n");
}

TEST_F(InstallRescueScriptTest, IgnoresNonRegularFileEntry) {
    // A symlink at tmp_dir/ry-rescue would otherwise resolve through the
    // copy. install_rescue_script must reject anything that isn't a
    // regular file (matches the validate-before-install posture used
    // elsewhere in self_update).
    fs::create_symlink("/etc/passwd", src_dir / "ry-rescue");

    bool ok = install_rescue_script(src_dir.string(), binary_path.string());
    EXPECT_FALSE(ok);
    EXPECT_FALSE(fs::exists(binary_path.parent_path() / "ry-rescue"));
}

class InstallSelfUpdateBinaryTest : public RyInstallTestBase {
protected:
    fs::path binary_path;

    void SetUp() override {
        SetUpDirs("ry_test_install_self_update_binary");
        binary_path = tmp_root / "bin" / "ry";
        fs::create_directories(binary_path.parent_path());
        write_file(binary_path, "fake ry");
    }
};

TEST_F(InstallSelfUpdateBinaryTest, CopiesUpdaterNextToBinary) {
    write_file(src_dir / "ry-self-update", "#!/bin/sh\necho updater\n");

    bool ok = install_self_update_binary(src_dir.string(), binary_path.string());
    ASSERT_TRUE(ok);

    auto dest = binary_path.parent_path() / "ry-self-update";
    EXPECT_TRUE(fs::exists(dest));

    std::ifstream ifs(dest);
    std::string content((std::istreambuf_iterator<char>(ifs)),
                         std::istreambuf_iterator<char>());
    EXPECT_EQ(content, "#!/bin/sh\necho updater\n");
}

TEST_F(InstallSelfUpdateBinaryTest, MarksUpdaterExecutable) {
    write_file(src_dir / "ry-self-update", "#!/bin/sh\n");

    bool ok = install_self_update_binary(src_dir.string(), binary_path.string());
    ASSERT_TRUE(ok);

    auto dest = binary_path.parent_path() / "ry-self-update";
    auto perms = fs::status(dest).permissions();
    EXPECT_NE(perms & fs::perms::owner_exec, fs::perms::none);
    EXPECT_NE(perms & fs::perms::group_exec, fs::perms::none);
    EXPECT_NE(perms & fs::perms::others_exec, fs::perms::none);
}

TEST_F(InstallSelfUpdateBinaryTest, ReturnsFalseWhenNoUpdaterInArchive) {
    bool ok = install_self_update_binary(src_dir.string(), binary_path.string());
    EXPECT_FALSE(ok);
    EXPECT_FALSE(fs::exists(binary_path.parent_path() / "ry-self-update"));
}

TEST_F(InstallSelfUpdateBinaryTest, IgnoresNonRegularFileEntry) {
    fs::create_symlink("/etc/passwd", src_dir / "ry-self-update");

    bool ok = install_self_update_binary(src_dir.string(), binary_path.string());
    EXPECT_FALSE(ok);
    EXPECT_FALSE(fs::exists(binary_path.parent_path() / "ry-self-update"));
}

// --- Checksum verification tests ---

TEST(SelfUpdate, BuildChecksumsUrl) {
    auto url = build_checksums_url("v0.0.5");
    EXPECT_EQ(url, "https://github.com/t0k0sh1/ry/releases/download/v0.0.5/checksums.txt");
}

TEST(SelfUpdate, ParseChecksumForFileExactMatch) {
    std::string content =
        "abc123def456  ry-darwin-arm64.tar.gz\n"
        "789012fed345  ry-linux-amd64.tar.gz\n";

    EXPECT_EQ(parse_checksum_for_file(content, "ry-darwin-arm64.tar.gz"), "abc123def456");
    EXPECT_EQ(parse_checksum_for_file(content, "ry-linux-amd64.tar.gz"), "789012fed345");
}

TEST(SelfUpdate, ParseChecksumForFileFallback) {
    // CI produces "ry-v0.0.5-darwin-arm64.tar.gz" but download URL uses "ry-darwin-arm64.tar.gz"
    std::string content =
        "abc123def456  ry-v0.0.5-darwin-arm64.tar.gz\n"
        "789012fed345  ry-v0.0.5-linux-amd64.tar.gz\n";

    // Exact match fails, but fallback by platform suffix should work
    EXPECT_EQ(parse_checksum_for_file(content, "ry-darwin-arm64.tar.gz"), "abc123def456");
    EXPECT_EQ(parse_checksum_for_file(content, "ry-linux-amd64.tar.gz"), "789012fed345");
}

TEST(SelfUpdate, ParseChecksumForFileNotFound) {
    std::string content = "abc123  ry-darwin-arm64.tar.gz\n";
    EXPECT_EQ(parse_checksum_for_file(content, "ry-windows-amd64.tar.gz"), "");
}

TEST(SelfUpdate, ParseChecksumForFileEmpty) {
    EXPECT_EQ(parse_checksum_for_file("", "ry-darwin-arm64.tar.gz"), "");
}

TEST(SelfUpdate, ParseChecksumForFileMalformed) {
    // Lines without proper separator
    std::string content = "nospacehere\n";
    EXPECT_EQ(parse_checksum_for_file(content, "nospacehere"), "");
}

TEST(SelfUpdate, ComputeSha256KnownContent) {
    auto tmp = fs::temp_directory_path() / "ry_test_sha256";
    {
        std::ofstream ofs(tmp);
        ofs << "hello world\n";
    }
    std::string hash = compute_sha256(tmp.string());
    EXPECT_FALSE(hash.empty());
    // sha256 of "hello world\n" is well-known
    EXPECT_EQ(hash, "a948904f2f0f479b8f8197694b30184b0d2ed1c1cd2a1ec0fb85d299a192a447");
    fs::remove(tmp);
}

TEST(SelfUpdate, VerifyChecksumCorrect) {
    auto tmp = fs::temp_directory_path() / "ry_test_verify";
    {
        std::ofstream ofs(tmp);
        ofs << "hello world\n";
    }
    EXPECT_TRUE(verify_checksum(tmp.string(), "a948904f2f0f479b8f8197694b30184b0d2ed1c1cd2a1ec0fb85d299a192a447"));
    fs::remove(tmp);
}

TEST(SelfUpdate, VerifyChecksumWrong) {
    auto tmp = fs::temp_directory_path() / "ry_test_verify_bad";
    {
        std::ofstream ofs(tmp);
        ofs << "hello world\n";
    }
    EXPECT_FALSE(verify_checksum(tmp.string(), "0000000000000000000000000000000000000000000000000000000000000000"));
    fs::remove(tmp);
}

// --- Tar validation tests ---

class TarValidationTest : public ::testing::Test {
protected:
    fs::path tmp_root;

    void SetUp() override {
        tmp_root = fs::temp_directory_path() / "ry_test_tar_validation";
        fs::remove_all(tmp_root);
        fs::create_directories(tmp_root);
    }

    void TearDown() override {
        fs::remove_all(tmp_root);
    }

    fs::path create_tar_with_python(const std::string &filename,
                                     const std::string &entry_setup) {
        auto archive = tmp_root / filename;
        std::string script =
            "import tarfile, io\n"
            "with tarfile.open('" + archive.string() + "', 'w:gz') as t:\n"
            + entry_setup;
        int status = run_command({"python3", "-c", script});
        EXPECT_EQ(status, 0) << "python3 failed to create tar archive";
        EXPECT_TRUE(fs::exists(archive)) << "Expected archive to be created at " << archive;
        return archive;
    }
};

TEST_F(TarValidationTest, SafeArchive) {
    // Create a safe tar archive
    auto content_dir = tmp_root / "content";
    fs::create_directories(content_dir / "share" / "std");
    { std::ofstream(content_dir / "ry") << "binary"; }
    { std::ofstream(content_dir / "share" / "std" / "builtins.ry") << "# builtins"; }

    auto archive = tmp_root / "safe.tar.gz";
    run_command({"tar", "-czf", archive.string(), "-C", content_dir.string(), "ry", "share"});

    EXPECT_TRUE(validate_tar_entries(archive.string()));
}

TEST_F(TarValidationTest, PathTraversal) {
    auto archive = tmp_root / "traversal.tar.gz";
    // Use python to create a tar with path traversal entry (portable across macOS/Linux)
    std::string script =
        "import tarfile, io\n"
        "with tarfile.open('" + archive.string() + "', 'w:gz') as t:\n"
        "    info = tarfile.TarInfo(name='../etc/passwd')\n"
        "    info.size = 4\n"
        "    t.addfile(info, io.BytesIO(b'evil'))\n";
    run_command({"python3", "-c", script});

    EXPECT_FALSE(validate_tar_entries(archive.string()));
}

TEST_F(TarValidationTest, AbsolutePath) {
    auto archive = tmp_root / "absolute.tar.gz";
    std::string script =
        "import tarfile, io\n"
        "with tarfile.open('" + archive.string() + "', 'w:gz') as t:\n"
        "    info = tarfile.TarInfo(name='/etc/passwd')\n"
        "    info.size = 4\n"
        "    t.addfile(info, io.BytesIO(b'evil'))\n";
    run_command({"python3", "-c", script});

    EXPECT_FALSE(validate_tar_entries(archive.string()));
}

TEST_F(TarValidationTest, Symlink) {
    // Create a tar archive containing a symlink
    auto content_dir = tmp_root / "content";
    fs::create_directories(content_dir);
    fs::create_symlink("/etc/passwd", content_dir / "link");

    auto archive = tmp_root / "symlink.tar.gz";
    run_command({"tar", "-czf", archive.string(), "-C", content_dir.string(), "link"});

    EXPECT_FALSE(validate_tar_entries(archive.string()));
}

TEST_F(TarValidationTest, Hardlink) {
    auto archive = tmp_root / "hardlink.tar.gz";
    std::string script =
        "import tarfile\n"
        "with tarfile.open('" + archive.string() + "', 'w:gz') as t:\n"
        "    info = tarfile.TarInfo(name='evil')\n"
        "    info.type = tarfile.LNKTYPE\n"
        "    info.linkname = 'some_other_file'\n"
        "    info.size = 0\n"
        "    t.addfile(info)\n";
    run_command({"python3", "-c", script});

    EXPECT_FALSE(validate_tar_entries(archive.string()));
}

TEST_F(TarValidationTest, BlockDevice) {
    auto archive = create_tar_with_python("blockdev.tar.gz",
        "    info = tarfile.TarInfo(name='devzero')\n"
        "    info.type = tarfile.BLKTYPE\n"
        "    info.devmajor = 1\n"
        "    info.devminor = 5\n"
        "    info.size = 0\n"
        "    t.addfile(info)\n");
    EXPECT_FALSE(validate_tar_entries(archive.string()));
}

TEST_F(TarValidationTest, CharDevice) {
    auto archive = create_tar_with_python("chardev.tar.gz",
        "    info = tarfile.TarInfo(name='devnull')\n"
        "    info.type = tarfile.CHRTYPE\n"
        "    info.devmajor = 1\n"
        "    info.devminor = 3\n"
        "    info.size = 0\n"
        "    t.addfile(info)\n");
    EXPECT_FALSE(validate_tar_entries(archive.string()));
}

TEST_F(TarValidationTest, Fifo) {
    auto archive = create_tar_with_python("fifo.tar.gz",
        "    info = tarfile.TarInfo(name='mypipe')\n"
        "    info.type = tarfile.FIFOTYPE\n"
        "    info.size = 0\n"
        "    t.addfile(info)\n");
    EXPECT_FALSE(validate_tar_entries(archive.string()));
}

// --- Signature verification tests ---

// Dedicated test Ed25519 key pair — distinct from the production SIGNING_PUBLIC_KEY.
static const unsigned char TEST_PUBKEY[32] = {
    0xcb, 0xe4, 0xb6, 0x66, 0x4e, 0x4b, 0x97, 0x5c, 0x79, 0x50, 0x4d, 0x1a,
    0x0c, 0x5a, 0x28, 0x46, 0x8e, 0x1e, 0x01, 0xdc, 0x36, 0x08, 0xc9, 0xc1,
    0x83, 0x0c, 0x9a, 0x76, 0xeb, 0x64, 0x74, 0x00
};
static const char *TEST_MESSAGE = "test message for signature verification";
static const char *TEST_SIGNATURE_B64 =
    "LWqzIGYP9gIf2aFVk7qaTIkD2g36unUqDtw4jG8+EhiGA1bTbHJ0+YkpWKJTwNfKIymzI9HQ8XHbo1B8G20gAA==";

TEST(SelfUpdate, BuildSignatureUrl) {
    auto url = build_signature_url("v0.0.5");
    EXPECT_EQ(url, "https://github.com/t0k0sh1/ry/releases/download/v0.0.5/checksums.txt.sig");
}

TEST(SelfUpdate, Base64DecodeKnown) {
    auto decoded = base64_decode("aGVsbG8=");
    EXPECT_EQ(decoded, "hello");
}

TEST(SelfUpdate, Base64DecodePaddedShort) {
    auto decoded = base64_decode("aGk=");
    EXPECT_EQ(decoded, "hi");
}

TEST(SelfUpdate, Base64DecodeEmpty) {
    auto decoded = base64_decode("");
    EXPECT_TRUE(decoded.empty());
}

TEST(SelfUpdate, VerifySignatureValid) {
    EXPECT_TRUE(verify_signature(TEST_MESSAGE, TEST_SIGNATURE_B64,
                                 TEST_PUBKEY, sizeof(TEST_PUBKEY)));
}

TEST(SelfUpdate, VerifySignatureInvalid) {
    std::string bad_sig = std::string(TEST_SIGNATURE_B64);
    bad_sig[0] = (bad_sig[0] == 'A') ? 'B' : 'A';
    EXPECT_FALSE(verify_signature(TEST_MESSAGE, bad_sig.c_str(),
                                  TEST_PUBKEY, sizeof(TEST_PUBKEY)));
}

TEST(SelfUpdate, VerifySignatureWrongMessage) {
    EXPECT_FALSE(verify_signature("wrong message", TEST_SIGNATURE_B64,
                                  TEST_PUBKEY, sizeof(TEST_PUBKEY)));
}

TEST(SelfUpdate, VerifySignatureMalformedBase64) {
    EXPECT_FALSE(verify_signature(TEST_MESSAGE, "!!!not-base64!!!",
                                  TEST_PUBKEY, sizeof(TEST_PUBKEY)));
}

TEST(SelfUpdate, VerifySignatureEmptySignature) {
    EXPECT_FALSE(verify_signature(TEST_MESSAGE, "",
                                  TEST_PUBKEY, sizeof(TEST_PUBKEY)));
}

TEST(SelfUpdate, VerifySignatureShortSignature) {
    EXPECT_FALSE(verify_signature(TEST_MESSAGE, "AAAA",
                                  TEST_PUBKEY, sizeof(TEST_PUBKEY)));
}

// --- Signature policy tests ---

TEST(SelfUpdate, SignaturePolicyValidSignature) {
    auto action = evaluate_signature_policy(true, true, false);
    EXPECT_EQ(action, SignatureAction::Verified);
}

TEST(SelfUpdate, SignaturePolicyValidSignatureWithSkipSet) {
    auto action = evaluate_signature_policy(true, true, true);
    EXPECT_EQ(action, SignatureAction::Verified);
}

TEST(SelfUpdate, SignaturePolicyInvalidSignature) {
    auto action = evaluate_signature_policy(true, false, false);
    EXPECT_EQ(action, SignatureAction::FailInvalid);
}

TEST(SelfUpdate, SignaturePolicyInvalidSignatureWithSkipSet) {
    auto action = evaluate_signature_policy(true, false, true);
    EXPECT_EQ(action, SignatureAction::FailInvalid);
}

TEST(SelfUpdate, SignaturePolicyMissingSignatureDefault) {
    auto action = evaluate_signature_policy(false, false, false);
    EXPECT_EQ(action, SignatureAction::FailMissing);
}

TEST(SelfUpdate, SignaturePolicyMissingSignatureSkipAllowed) {
    auto action = evaluate_signature_policy(false, false, true);
    EXPECT_EQ(action, SignatureAction::SkipAllowed);
}

// --- Semver parsing tests (#2457) ---

TEST(SelfUpdate, ParseSemverBasic) {
    auto v = parse_semver("v0.0.29");
    ASSERT_TRUE(v.has_value());
    EXPECT_EQ(v->major, 0u);
    EXPECT_EQ(v->minor, 0u);
    EXPECT_EQ(v->patch, 29u);
}

TEST(SelfUpdate, ParseSemverWithoutVPrefix) {
    auto v = parse_semver("0.0.30");
    ASSERT_TRUE(v.has_value());
    EXPECT_EQ(v->major, 0u);
    EXPECT_EQ(v->minor, 0u);
    EXPECT_EQ(v->patch, 30u);
}

TEST(SelfUpdate, ParseSemverWithPreRelease) {
    auto v = parse_semver("v0.0.30-rc.1");
    ASSERT_TRUE(v.has_value());
    EXPECT_EQ(v->patch, 30u);
}

TEST(SelfUpdate, ParseSemverWithBuildMetadata) {
    auto v = parse_semver("v1.2.3+build.42");
    ASSERT_TRUE(v.has_value());
    EXPECT_EQ(v->major, 1u);
    EXPECT_EQ(v->minor, 2u);
    EXPECT_EQ(v->patch, 3u);
}

TEST(SelfUpdate, ParseSemverFourPartTreatedAsThreePart) {
    // Hypothetical 4-segment hotfix tag: the legacy guard inspects only the
    // first three numeric components, so an extra ".1" must not be confused
    // for the patch.
    auto v = parse_semver("v0.0.29.1");
    ASSERT_TRUE(v.has_value());
    EXPECT_EQ(v->patch, 29u);
}

TEST(SelfUpdate, ParseSemverLeadingZeros) {
    auto v = parse_semver("v00.00.29");
    ASSERT_TRUE(v.has_value());
    EXPECT_EQ(v->major, 0u);
    EXPECT_EQ(v->minor, 0u);
    EXPECT_EQ(v->patch, 29u);
}

TEST(SelfUpdate, ParseSemverRejectsEmpty) {
    EXPECT_FALSE(parse_semver("").has_value());
    EXPECT_FALSE(parse_semver("v").has_value());
}

TEST(SelfUpdate, ParseSemverRejectsMissingSegments) {
    EXPECT_FALSE(parse_semver("v0").has_value());
    EXPECT_FALSE(parse_semver("v0.1").has_value());
}

TEST(SelfUpdate, ParseSemverRejectsNonDigit) {
    // is_valid_tag's regex permits "-" inside the tag, so verify the
    // semver parser refuses to swallow a leading sign that strtoull
    // would otherwise silently accept.
    EXPECT_FALSE(parse_semver("v-1.0.0").has_value());
    EXPECT_FALSE(parse_semver("v0.-1.0").has_value());
    EXPECT_FALSE(parse_semver("not-a-version").has_value());
}

// --- Legacy downgrade guard policy tests (#2457) ---

TEST(SelfUpdate, IsLegacyDowngradeTargetBlocksThroughV0029) {
    EXPECT_TRUE(is_legacy_downgrade_target("v0.0.0"));
    EXPECT_TRUE(is_legacy_downgrade_target("v0.0.1"));
    EXPECT_TRUE(is_legacy_downgrade_target("v0.0.29"));
    EXPECT_TRUE(is_legacy_downgrade_target("0.0.29"));
}

TEST(SelfUpdate, IsLegacyDowngradeTargetAllowsV0030AndLater) {
    EXPECT_FALSE(is_legacy_downgrade_target("v0.0.30"));
    EXPECT_FALSE(is_legacy_downgrade_target("v0.0.31"));
    EXPECT_FALSE(is_legacy_downgrade_target("v0.1.0"));
    EXPECT_FALSE(is_legacy_downgrade_target("v1.0.0"));
}

TEST(SelfUpdate, IsLegacyDowngradeTargetTreatsFourPartByFirstThree) {
    // v0.0.29.1 → major.minor.patch = 0.0.29 → blocked.
    EXPECT_TRUE(is_legacy_downgrade_target("v0.0.29.1"));
    // v0.0.0.99 → major.minor.patch = 0.0.0 → blocked.
    EXPECT_TRUE(is_legacy_downgrade_target("v0.0.0.99"));
}

TEST(SelfUpdate, IsLegacyDowngradeTargetAcceptsPreReleaseOnAllowedPatch) {
    EXPECT_FALSE(is_legacy_downgrade_target("v0.0.30-rc.1"));
}

TEST(SelfUpdate, IsLegacyDowngradeTargetIgnoresUnparseable) {
    // Unparseable tags fall through — let the URL existence check report
    // them as "not found" rather than mislabel them as a downgrade attempt.
    EXPECT_FALSE(is_legacy_downgrade_target("not-a-version"));
    EXPECT_FALSE(is_legacy_downgrade_target("v"));
}

TEST(SelfUpdate, CheckDowngradeGuardBlocksLegacyOnCanonicalRepo) {
    EXPECT_EQ(check_downgrade_guard("v0.0.29", "t0k0sh1/ry", false),
              DowngradeGuardResult::Blocked);
    EXPECT_EQ(check_downgrade_guard("v0.0.1", "t0k0sh1/ry", false),
              DowngradeGuardResult::Blocked);
}

TEST(SelfUpdate, CheckDowngradeGuardAllowsLegacyWithOptIn) {
    EXPECT_EQ(check_downgrade_guard("v0.0.29", "t0k0sh1/ry", true),
              DowngradeGuardResult::AllowedWithWarning);
}

TEST(SelfUpdate, CheckDowngradeGuardSkipsForForkRepo) {
    // A fork's tags don't share the upstream install-filter history, so the
    // guard must not interfere with self-update against a forked release.
    EXPECT_EQ(check_downgrade_guard("v0.0.29", "fork/ry", false),
              DowngradeGuardResult::Allowed);
    EXPECT_EQ(check_downgrade_guard("v0.0.1", "other/fork", false),
              DowngradeGuardResult::Allowed);
}

TEST(SelfUpdate, CheckDowngradeGuardAllowsCurrentAndForward) {
    EXPECT_EQ(check_downgrade_guard("v0.0.30", "t0k0sh1/ry", false),
              DowngradeGuardResult::Allowed);
    EXPECT_EQ(check_downgrade_guard("v0.0.31", "t0k0sh1/ry", false),
              DowngradeGuardResult::Allowed);
    EXPECT_EQ(check_downgrade_guard("v0.1.0", "t0k0sh1/ry", false),
              DowngradeGuardResult::Allowed);
}

// --- resolve_update_target legacy-downgrade integration (#2457) ---
//
// The guard must short-circuit before any network call. We isolate RY_UPDATE_REPO
// and RY_ALLOW_LEGACY_DOWNGRADE so the test result is independent of the
// developer's shell environment.

class ResolveUpdateTargetLegacyGuardTest : public ::testing::Test {
protected:
    std::optional<std::string> saved_repo;
    std::optional<std::string> saved_allow;

    void SetUp() override {
        if (const char *env = std::getenv("RY_UPDATE_REPO")) saved_repo = env;
        if (const char *env = std::getenv("RY_ALLOW_LEGACY_DOWNGRADE")) saved_allow = env;
        unsetenv("RY_UPDATE_REPO");
        unsetenv("RY_ALLOW_LEGACY_DOWNGRADE");
    }

    void TearDown() override {
        if (saved_repo) setenv("RY_UPDATE_REPO", saved_repo->c_str(), 1);
        else unsetenv("RY_UPDATE_REPO");
        if (saved_allow) setenv("RY_ALLOW_LEGACY_DOWNGRADE", saved_allow->c_str(), 1);
        else unsetenv("RY_ALLOW_LEGACY_DOWNGRADE");
    }
};

TEST_F(ResolveUpdateTargetLegacyGuardTest, ReturnsEmptyTargetForV0029) {
    PlatformInfo p{"linux", "amd64"};
    testing::internal::CaptureStderr();
    auto target = resolve_update_target(std::optional<std::string>{"v0.0.29"}, p);
    std::string err = testing::internal::GetCapturedStderr();

    EXPECT_TRUE(target.tag.empty());
    EXPECT_TRUE(target.download_url.empty());
    EXPECT_NE(err.find("not supported as a downgrade target"), std::string::npos);
    EXPECT_NE(err.find("v0.0.30 or later"), std::string::npos);
    EXPECT_NE(err.find("ry-rescue"), std::string::npos);
}

TEST_F(ResolveUpdateTargetLegacyGuardTest, ReturnsEmptyTargetForV001) {
    PlatformInfo p{"linux", "amd64"};
    testing::internal::CaptureStderr();
    auto target = resolve_update_target(std::optional<std::string>{"v0.0.1"}, p);
    std::string err = testing::internal::GetCapturedStderr();

    EXPECT_TRUE(target.tag.empty());
    EXPECT_NE(err.find("not supported as a downgrade target"), std::string::npos);
}

TEST_F(ResolveUpdateTargetLegacyGuardTest, ReturnsEmptyTargetForFourPartLegacyTag) {
    PlatformInfo p{"linux", "amd64"};
    testing::internal::CaptureStderr();
    auto target = resolve_update_target(std::optional<std::string>{"v0.0.29.1"}, p);
    std::string err = testing::internal::GetCapturedStderr();

    EXPECT_TRUE(target.tag.empty());
    EXPECT_NE(err.find("not supported as a downgrade target"), std::string::npos);
}

// --- Atomic update: backup / smoke-test / rollback (#2456) ---
//
// The shared fixture below populates a fake "installed" tree under RY_HOME and
// `bin/` (binary location). It plays the role both of the pre-update state
// captured by create_backup and the destination that restore_backup writes
// back to. RyInstallTestBase already handles RY_HOME save/restore.

class AtomicUpdateTest : public RyInstallTestBase {
protected:
    fs::path binary_path;
    fs::path backup_root;

    void SetUp() override {
        SetUpDirs("ry_test_atomic_update");
        binary_path = tmp_root / "bin" / "ry";
        backup_root = dest_home / ".backup";
        fs::create_directories(binary_path.parent_path());
    }

    void install_old_state() {
        write_file(binary_path, "#!/bin/sh\necho ry 0.0.old\n");
        chmod(binary_path.c_str(), 0755);
        write_file(dest_home / "share" / "std" / "builtins.ry", "# old builtins");
        write_file(dest_home / "share" / "std" / "http" / "http.ry", "# old http");
        write_file(dest_home / "lib" / "libemit.dylib", "old emit");
        write_file(dest_home / "lib" / "libry_parser.dylib", "old parser");
        write_file(dest_home / "lib" / "libLLVM.dylib", "old llvm");
        write_file(binary_path.parent_path() / "ry-rescue", "#!/bin/sh\necho old rescue\n");
        ry::write_manifest(dest_home / "share" / "std",
                           {"builtins.ry", "http/http.ry"});
    }

    std::string read_file(const fs::path &p) {
        std::ifstream ifs(p);
        return std::string((std::istreambuf_iterator<char>(ifs)),
                           std::istreambuf_iterator<char>());
    }
};

TEST_F(AtomicUpdateTest, CreateBackupCapturesBinaryStdlibLibsAndRescue) {
    install_old_state();

    bool ok = create_backup(backup_root, binary_path.string(), dest_home);
    ASSERT_TRUE(ok);

    // Binary backed up verbatim
    EXPECT_EQ(read_file(backup_root / "binary"), "#!/bin/sh\necho ry 0.0.old\n");
    // Stdlib tree backed up
    EXPECT_EQ(read_file(backup_root / "share" / "std" / "builtins.ry"), "# old builtins");
    EXPECT_EQ(read_file(backup_root / "share" / "std" / "http" / "http.ry"), "# old http");
    // Selected native libs backed up
    EXPECT_EQ(read_file(backup_root / "lib" / "libemit.dylib"), "old emit");
    EXPECT_EQ(read_file(backup_root / "lib" / "libry_parser.dylib"), "old parser");
    EXPECT_EQ(read_file(backup_root / "lib" / "libLLVM.dylib"), "old llvm");
    // Rescue script next to binary backed up
    EXPECT_EQ(read_file(backup_root / "ry-rescue"), "#!/bin/sh\necho old rescue\n");
}

TEST_F(AtomicUpdateTest, CreateBackupWipesStaleDirectory) {
    install_old_state();
    // Pre-populate a stale backup that must NOT survive the next snapshot.
    write_file(backup_root / "share" / "std" / "stale.ry", "stale");
    write_file(backup_root / "lib" / "libstale.dylib", "stale lib");

    bool ok = create_backup(backup_root, binary_path.string(), dest_home);
    ASSERT_TRUE(ok);

    EXPECT_FALSE(fs::exists(backup_root / "share" / "std" / "stale.ry"));
    EXPECT_FALSE(fs::exists(backup_root / "lib" / "libstale.dylib"));
}

TEST_F(AtomicUpdateTest, CreateBackupHandlesOldLayoutStdlib) {
    write_file(binary_path, "#!/bin/sh\n");
    chmod(binary_path.c_str(), 0755);
    write_file(dest_home / "lib" / "std" / "builtins.ry", "# old layout builtins");
    write_file(dest_home / "lib" / "libry_parser.dylib", "parser");

    bool ok = create_backup(backup_root, binary_path.string(), dest_home);
    ASSERT_TRUE(ok);

    EXPECT_TRUE(fs::exists(backup_root / "lib" / "std" / "builtins.ry"));
    EXPECT_FALSE(fs::is_directory(backup_root / "share" / "std"));
}

TEST_F(AtomicUpdateTest, CreateBackupSucceedsWhenNoRescueScript) {
    install_old_state();
    fs::remove(binary_path.parent_path() / "ry-rescue");

    bool ok = create_backup(backup_root, binary_path.string(), dest_home);
    EXPECT_TRUE(ok);
    EXPECT_FALSE(fs::exists(backup_root / "ry-rescue"));
}

TEST_F(AtomicUpdateTest, RestoreBackupRestoresBinaryStdlibLibsAndRescue) {
    install_old_state();
    ASSERT_TRUE(create_backup(backup_root, binary_path.string(), dest_home));

    // Simulate a failed update having corrupted everything.
    write_file(binary_path, "broken new binary");
    fs::remove_all(dest_home / "share" / "std");
    fs::remove(dest_home / "lib" / "libemit.dylib");
    write_file(dest_home / "lib" / "libLLVM.dylib", "new llvm (missing chain dep)");
    write_file(binary_path.parent_path() / "ry-rescue", "#!/bin/sh\necho NEW\n");

    bool ok = restore_backup(backup_root, binary_path.string(), dest_home);
    ASSERT_TRUE(ok);

    EXPECT_EQ(read_file(binary_path), "#!/bin/sh\necho ry 0.0.old\n");
    EXPECT_EQ(read_file(dest_home / "share" / "std" / "builtins.ry"), "# old builtins");
    EXPECT_EQ(read_file(dest_home / "share" / "std" / "http" / "http.ry"), "# old http");
    EXPECT_EQ(read_file(dest_home / "lib" / "libemit.dylib"), "old emit");
    EXPECT_EQ(read_file(dest_home / "lib" / "libLLVM.dylib"), "old llvm");
    EXPECT_EQ(read_file(binary_path.parent_path() / "ry-rescue"),
              "#!/bin/sh\necho old rescue\n");
}

TEST_F(AtomicUpdateTest, RestoreBackupPurgesFilesAddedByFailedUpdate) {
    install_old_state();
    ASSERT_TRUE(create_backup(backup_root, binary_path.string(), dest_home));

    // The failed update added a new stdlib file that wasn't in the backup.
    // After restore the stdlib tree must match the backup exactly.
    write_file(dest_home / "share" / "std" / "experimental.ry", "# new file");

    ASSERT_TRUE(restore_backup(backup_root, binary_path.string(), dest_home));

    EXPECT_FALSE(fs::exists(dest_home / "share" / "std" / "experimental.ry"));
    EXPECT_TRUE(fs::exists(dest_home / "share" / "std" / "builtins.ry"));
}

TEST_F(AtomicUpdateTest, RestoreBackupPreservesBinaryExecutableBit) {
    install_old_state();
    ASSERT_TRUE(create_backup(backup_root, binary_path.string(), dest_home));

    write_file(binary_path, "broken");
    chmod(binary_path.c_str(), 0644);  // strip +x to simulate damage

    ASSERT_TRUE(restore_backup(backup_root, binary_path.string(), dest_home));

    auto perms = fs::status(binary_path).permissions();
    EXPECT_NE(perms & fs::perms::owner_exec, fs::perms::none);
}

TEST_F(AtomicUpdateTest, CleanupBackupRemovesBackupRoot) {
    install_old_state();
    ASSERT_TRUE(create_backup(backup_root, binary_path.string(), dest_home));
    ASSERT_TRUE(fs::exists(backup_root));

    cleanup_backup(backup_root);
    EXPECT_FALSE(fs::exists(backup_root));
}

TEST_F(AtomicUpdateTest, CleanupBackupTolerantWhenAbsent) {
    EXPECT_FALSE(fs::exists(backup_root));
    cleanup_backup(backup_root);  // must not throw
    EXPECT_FALSE(fs::exists(backup_root));
}

// --- Smoke test (#2456) ---

class SmokeTestBinaryTest : public ::testing::Test {
protected:
    fs::path tmp_root;
    fs::path binary;

    void SetUp() override {
        tmp_root = fs::temp_directory_path() / "ry_test_smoke";
        fs::remove_all(tmp_root);
        fs::create_directories(tmp_root);
        binary = tmp_root / "fake_ry";
    }
    void TearDown() override { fs::remove_all(tmp_root); }

    void write_script(const std::string &body) {
        std::ofstream(binary) << body;
        chmod(binary.c_str(), 0755);
    }
};

TEST_F(SmokeTestBinaryTest, PassWhenExitZeroAndVersionInStdout) {
    write_script("#!/bin/sh\necho 'ry 0.0.42'\nexit 0\n");
    EXPECT_EQ(smoke_test_binary(binary.string(), 5), SmokeTestResult::Pass);
}

TEST_F(SmokeTestBinaryTest, FailExitOnNonZeroStatus) {
    write_script("#!/bin/sh\nexit 1\n");
    EXPECT_EQ(smoke_test_binary(binary.string(), 5), SmokeTestResult::FailExit);
}

TEST_F(SmokeTestBinaryTest, FailSignalWhenKilled) {
    // SIGSEGV mimics dyld load failure killing the process before main().
    write_script("#!/bin/sh\nkill -SEGV $$\n");
    EXPECT_EQ(smoke_test_binary(binary.string(), 5), SmokeTestResult::FailSignal);
}

TEST_F(SmokeTestBinaryTest, FailTimeoutWhenChildHangs) {
    write_script("#!/bin/sh\nsleep 30\n");
    auto r = smoke_test_binary(binary.string(), 1);
    EXPECT_EQ(r, SmokeTestResult::FailTimeout);
}

TEST_F(SmokeTestBinaryTest, FailNoVersionWhenStdoutLacksBanner) {
    write_script("#!/bin/sh\necho 'hello world'\nexit 0\n");
    EXPECT_EQ(smoke_test_binary(binary.string(), 5), SmokeTestResult::FailNoVersion);
}

TEST_F(SmokeTestBinaryTest, FailLaunchWhenBinaryDoesNotExist) {
    auto missing = tmp_root / "does_not_exist";
    auto r = smoke_test_binary(missing.string(), 5);
    EXPECT_TRUE(r == SmokeTestResult::FailLaunch || r == SmokeTestResult::FailExit);
}

// --- Update lock (#2456) ---

class UpdateLockTest : public ::testing::Test {
protected:
    fs::path tmp_root;
    fs::path lock_path;

    void SetUp() override {
        tmp_root = fs::temp_directory_path() / "ry_test_update_lock";
        fs::remove_all(tmp_root);
        fs::create_directories(tmp_root);
        lock_path = tmp_root / ".update.lock";
    }
    void TearDown() override { fs::remove_all(tmp_root); }
};

TEST_F(UpdateLockTest, AcquireOnFreshPath) {
    auto h = acquire_update_lock(lock_path);
    ASSERT_EQ(h.result, UpdateLockResult::Acquired);
    EXPECT_GE(h.fd, 0);
    EXPECT_TRUE(fs::exists(lock_path));
    release_update_lock(h.fd, lock_path);
    EXPECT_FALSE(fs::exists(lock_path));
}

TEST_F(UpdateLockTest, RejectsWhenLiveHolderPresent) {
    auto h1 = acquire_update_lock(lock_path);
    ASSERT_EQ(h1.result, UpdateLockResult::Acquired);

    auto h2 = acquire_update_lock(lock_path);
    EXPECT_EQ(h2.result, UpdateLockResult::BusyOtherProcess);

    release_update_lock(h1.fd, lock_path);
}

TEST_F(UpdateLockTest, TakesOverStaleLock) {
    // Write a lockfile whose PID will never be alive. PID 1 is init and
    // is always alive, so pick something high enough to be safely dead
    // in any sane test environment. Use 0x7fffffff (INT32_MAX) — kill(2)
    // returns ESRCH for it on every POSIX OS we ship on.
    {
        std::ofstream(lock_path) << "2147483645\n";
    }

    auto h = acquire_update_lock(lock_path);
    EXPECT_EQ(h.result, UpdateLockResult::Acquired);
    release_update_lock(h.fd, lock_path);
}

TEST_F(UpdateLockTest, ReleaseIsIdempotentOnMissingFile) {
    auto h = acquire_update_lock(lock_path);
    ASSERT_EQ(h.result, UpdateLockResult::Acquired);
    fs::remove(lock_path);  // simulate a tidy admin
    release_update_lock(h.fd, lock_path);  // must not throw / abort
    SUCCEED();
}
