#include <gtest/gtest.h>
#include "ry/self_update.hpp"
#include "ry/paths.hpp"
#include <filesystem>
#include <fstream>
#include <cstdlib>

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
