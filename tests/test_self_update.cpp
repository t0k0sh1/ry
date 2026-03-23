#include <gtest/gtest.h>
#include "ry/self_update.hpp"
#include "ry/paths.hpp"
#include <filesystem>
#include <fstream>
#include <cstdlib>
#include <cstdio>

using namespace ry::self_update;
using namespace ry::self_update::detail;
namespace fs = std::filesystem;

TEST(SelfUpdate, IsPrereleaseStable) {
    EXPECT_FALSE(is_prerelease("v0.0.1"));
    EXPECT_FALSE(is_prerelease("0.1.0"));
    EXPECT_FALSE(is_prerelease("v1.2.3"));
}

TEST(SelfUpdate, IsPrereleaseNightly) {
    EXPECT_TRUE(is_prerelease("v0.0.2-dev.20250101"));
    EXPECT_TRUE(is_prerelease("0.0.2-nightly.1"));
    EXPECT_TRUE(is_prerelease("v1.0.0-alpha"));
    EXPECT_TRUE(is_prerelease("v1.0.0-rc.1"));
}

TEST(SelfUpdate, BuildDownloadUrl) {
    PlatformInfo darwin_arm64{"darwin", "arm64"};
    PlatformInfo linux_amd64{"linux", "amd64"};

    auto url1 = build_download_url("v0.0.1", darwin_arm64);
    EXPECT_EQ(url1, "https://github.com/t0k0sh1/ry/releases/download/v0.0.1/ry-darwin-arm64.tar.gz");

    auto url2 = build_download_url("v0.0.2-dev.20250101", linux_amd64);
    EXPECT_EQ(url2, "https://github.com/t0k0sh1/ry/releases/download/v0.0.2-dev.20250101/ry-linux-amd64.tar.gz");
}

TEST(SelfUpdate, ExtractJsonString) {
    std::string json = R"({"tag_name": "v0.0.1", "name": "Release v0.0.1", "prerelease": false})";

    EXPECT_EQ(extract_json_string(json, "tag_name"), "v0.0.1");
    EXPECT_EQ(extract_json_string(json, "name"), "Release v0.0.1");
    EXPECT_EQ(extract_json_string(json, "prerelease"), "false");
    EXPECT_EQ(extract_json_string(json, "nonexistent"), "");
}

TEST(SelfUpdate, ExtractJsonStringWithEscapes) {
    std::string json = R"({"url": "https://example.com/path"})";
    EXPECT_EQ(extract_json_string(json, "url"), "https://example.com/path");
}

TEST(SelfUpdate, ExtractAllJsonStrings) {
    std::string json = R"([{"tag_name": "v0.0.2", "prerelease": true}, {"tag_name": "v0.0.1", "prerelease": false}])";

    auto tags = extract_all_json_strings(json, "tag_name");
    ASSERT_EQ(tags.size(), 2u);
    EXPECT_EQ(tags[0], "v0.0.2");
    EXPECT_EQ(tags[1], "v0.0.1");

    auto prereleases = extract_all_json_strings(json, "prerelease");
    ASSERT_EQ(prereleases.size(), 2u);
    EXPECT_EQ(prereleases[0], "true");
    EXPECT_EQ(prereleases[1], "false");
}

TEST(SelfUpdate, ExtractAllJsonStringsEmpty) {
    std::string json = R"({"other": "value"})";
    auto results = extract_all_json_strings(json, "tag_name");
    EXPECT_TRUE(results.empty());
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

// --- install_stdlib tests ---

class InstallStdlibTest : public ::testing::Test {
protected:
    fs::path tmp_root;
    fs::path src_dir;   // simulated archive extraction dir
    fs::path dest_home; // simulated RY_HOME
    std::string old_ry_home;

    void SetUp() override {
        tmp_root = fs::temp_directory_path() / "ry_test_install_stdlib";
        fs::remove_all(tmp_root);

        src_dir = tmp_root / "archive";
        dest_home = tmp_root / "ry_home";
        fs::create_directories(src_dir / "lib" / "std");
        fs::create_directories(dest_home / "lib" / "std");

        // Save and override RY_HOME
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

TEST_F(InstallStdlibTest, CopiesNestedModules) {
    auto src_std = src_dir / "lib" / "std";
    write_file(src_std / "builtins.ry", "# builtins");
    write_file(src_std / "http" / "http.ry", "# http module");
    write_file(src_std / "net" / "net.ry", "# net module");

    bool ok = install_stdlib(src_dir.string(), "0.1.0");
    ASSERT_TRUE(ok);

    auto dest_std = dest_home / "lib" / "std";
    EXPECT_TRUE(fs::exists(dest_std / "builtins.ry"));
    EXPECT_TRUE(fs::exists(dest_std / "http" / "http.ry"));
    EXPECT_TRUE(fs::exists(dest_std / "net" / "net.ry"));
}

TEST_F(InstallStdlibTest, ManifestContainsRelativePaths) {
    auto src_std = src_dir / "lib" / "std";
    write_file(src_std / "builtins.ry", "# builtins");
    write_file(src_std / "http" / "http.ry", "# http");

    install_stdlib(src_dir.string(), "0.1.0");

    auto manifest = ry::read_manifest(dest_home / "lib" / "std");
    EXPECT_EQ(manifest.version, "0.1.0");

    // Check that both top-level and nested files are in the manifest
    auto &files = manifest.files;
    EXPECT_NE(std::find(files.begin(), files.end(), "builtins.ry"), files.end());
    EXPECT_NE(std::find(files.begin(), files.end(), "http/http.ry"), files.end());
}

TEST_F(InstallStdlibTest, CleansUpOldNestedFiles) {
    auto dest_std = dest_home / "lib" / "std";

    // Pre-populate dest with an old nested file and manifest
    write_file(dest_std / "old_mod" / "old.ry", "# old");
    ry::write_manifest(dest_std, "0.0.1", {"old_mod/old.ry"});

    // New archive has only builtins.ry
    auto src_std = src_dir / "lib" / "std";
    write_file(src_std / "builtins.ry", "# builtins");

    install_stdlib(src_dir.string(), "0.1.0");

    EXPECT_TRUE(fs::exists(dest_std / "builtins.ry"));
    EXPECT_FALSE(fs::exists(dest_std / "old_mod" / "old.ry"));
}

TEST_F(InstallStdlibTest, CleansUpEmptyDirectories) {
    auto dest_std = dest_home / "lib" / "std";

    // Pre-populate with a nested file
    write_file(dest_std / "removed_mod" / "removed.ry", "# removed");
    ry::write_manifest(dest_std, "0.0.1", {"removed_mod/removed.ry"});

    // New archive has no removed_mod
    auto src_std = src_dir / "lib" / "std";
    write_file(src_std / "builtins.ry", "# builtins");

    install_stdlib(src_dir.string(), "0.1.0");

    EXPECT_FALSE(fs::exists(dest_std / "removed_mod"));
}

TEST_F(InstallStdlibTest, CleansUpDeeplyNestedEmptyDirectories) {
    auto dest_std = dest_home / "lib" / "std";

    // Pre-populate with a deeply nested file (a/b/c.ry)
    write_file(dest_std / "a" / "b" / "c.ry", "# deep");
    ry::write_manifest(dest_std, "0.0.1", {"a/b/c.ry"});

    // New archive has no a/b/c.ry
    auto src_std = src_dir / "lib" / "std";
    write_file(src_std / "builtins.ry", "# builtins");

    install_stdlib(src_dir.string(), "0.1.0");

    // Both a/b/ and a/ should be removed
    EXPECT_FALSE(fs::exists(dest_std / "a" / "b"));
    EXPECT_FALSE(fs::exists(dest_std / "a"));
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
};

TEST_F(TarValidationTest, SafeArchive) {
    // Create a safe tar archive
    auto content_dir = tmp_root / "content";
    fs::create_directories(content_dir / "lib" / "std");
    { std::ofstream(content_dir / "ry") << "binary"; }
    { std::ofstream(content_dir / "lib" / "std" / "builtins.ry") << "# builtins"; }

    auto archive = tmp_root / "safe.tar.gz";
    run_command({"tar", "-czf", archive.string(), "-C", content_dir.string(), "ry", "lib"});

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
        "import tarfile, io\n"
        "with tarfile.open('" + archive.string() + "', 'w:gz') as t:\n"
        "    info = tarfile.TarInfo(name='evil')\n"
        "    info.type = tarfile.LNKTYPE\n"
        "    info.linkname = 'some_other_file'\n"
        "    info.size = 0\n"
        "    t.addfile(info)\n";
    run_command({"python3", "-c", script});

    EXPECT_FALSE(validate_tar_entries(archive.string()));
}
