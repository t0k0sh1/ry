#include <gtest/gtest.h>
#include "ry/self_update.hpp"

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
    EXPECT_EQ(url1, "https://github.com/pricklywiggles/ry/releases/download/v0.0.1/ry-darwin-arm64.tar.gz");

    auto url2 = build_download_url("v0.0.2-dev.20250101", linux_amd64);
    EXPECT_EQ(url2, "https://github.com/pricklywiggles/ry/releases/download/v0.0.2-dev.20250101/ry-linux-amd64.tar.gz");
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
    // On macOS CI/dev
    EXPECT_TRUE(platform.os == "darwin" || platform.os == "linux");
    EXPECT_TRUE(platform.arch == "arm64" || platform.arch == "amd64");
}
