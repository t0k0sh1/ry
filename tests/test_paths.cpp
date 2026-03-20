#include <gtest/gtest.h>
#include "ry/paths.hpp"
#include <cstdlib>
#include <filesystem>
#include <fstream>

namespace fs = std::filesystem;

TEST(Paths, GetRyHomeDefault) {
    // Unset RY_HOME to test default behavior
    const char *orig = std::getenv("RY_HOME");
    unsetenv("RY_HOME");

    auto home = ry::get_ry_home();
    const char *user_home = std::getenv("HOME");
    if (user_home) {
        EXPECT_EQ(home, fs::path(user_home) / ".ry");
    }

    // Restore
    if (orig) setenv("RY_HOME", orig, 1);
}

TEST(Paths, GetRyHomeFromEnv) {
    const char *orig = std::getenv("RY_HOME");

    char tmp_dir[] = "/tmp/test-ry-home-XXXXXX";
    ASSERT_NE(mkdtemp(tmp_dir), nullptr);

    setenv("RY_HOME", tmp_dir, 1);

    auto home = ry::get_ry_home();
    EXPECT_EQ(home, fs::path(tmp_dir));

    fs::remove_all(tmp_dir);

    // Restore
    if (orig) setenv("RY_HOME", orig, 1);
    else unsetenv("RY_HOME");
}

TEST(Paths, FindLibDirWithStd) {
    // Create a temp directory structure: tmp/lib/std/
    char tmp_dir[] = "/tmp/ry-test-XXXXXX";
    ASSERT_NE(mkdtemp(tmp_dir), nullptr);
    std::string tmp(tmp_dir);

    fs::create_directories(tmp + "/bin");
    fs::create_directories(tmp + "/lib/std");

    // Write a dummy file so the directory is recognized
    std::ofstream(tmp + "/lib/std/builtins.ry") << "fn noop():\n    return\n";

    // exe/../lib should work (exe is in tmp/bin/)
    auto result = ry::find_lib_dir(tmp + "/bin/ry");
    EXPECT_EQ(result, fs::canonical(tmp + "/lib"));

    fs::remove_all(tmp);
}

TEST(Paths, FindLibDirNotFound) {
    auto result = ry::find_lib_dir("/nonexistent/path/ry");
    EXPECT_TRUE(result.empty());
}

TEST(Paths, ManifestReadWrite) {
    char tmp_dir[] = "/tmp/ry-manifest-XXXXXX";
    ASSERT_NE(mkdtemp(tmp_dir), nullptr);
    std::string tmp(tmp_dir);

    std::vector<std::string> files = {"builtins.ry", "str.ry"};
    ry::write_manifest(tmp, "0.0.3", files);

    auto manifest = ry::read_manifest(tmp);
    EXPECT_EQ(manifest.version, "0.0.3");
    ASSERT_EQ(manifest.files.size(), 2u);
    EXPECT_EQ(manifest.files[0], "builtins.ry");
    EXPECT_EQ(manifest.files[1], "str.ry");

    fs::remove_all(tmp);
}

TEST(Paths, ManifestReadMissing) {
    auto manifest = ry::read_manifest("/nonexistent/path");
    EXPECT_TRUE(manifest.version.empty());
    EXPECT_TRUE(manifest.files.empty());
}
