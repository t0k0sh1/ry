#include <gtest/gtest.h>
#include "ry/paths.hpp"
#include <cstdlib>
#include <filesystem>
#include <fstream>
#include <string>
#include <optional>

namespace fs = std::filesystem;

// RAII guard that saves and restores an environment variable
class ScopedEnv {
public:
    explicit ScopedEnv(const char *name, const char *value = nullptr)
        : name_(name) {
        const char *prev = std::getenv(name);
        if (prev) orig_ = std::string(prev);
        if (value) setenv(name, value, 1);
        else unsetenv(name);
    }
    ~ScopedEnv() {
        if (orig_) setenv(name_.c_str(), orig_->c_str(), 1);
        else unsetenv(name_.c_str());
    }
    ScopedEnv(const ScopedEnv&) = delete;
    ScopedEnv& operator=(const ScopedEnv&) = delete;
private:
    std::string name_;
    std::optional<std::string> orig_;
};

TEST(Paths, GetRyHomeDefault) {
    ScopedEnv guard("RY_HOME"); // unset RY_HOME

    auto home = ry::get_ry_home();
    const char *user_home = std::getenv("HOME");
    if (user_home) {
        EXPECT_EQ(home, fs::path(user_home) / ".ry");
    }
}

TEST(Paths, GetRyHomeFromEnv) {
    char tmp_dir[] = "/tmp/test-ry-home-XXXXXX";
    ASSERT_NE(mkdtemp(tmp_dir), nullptr);

    ScopedEnv guard("RY_HOME", tmp_dir);

    auto home = ry::get_ry_home();
    EXPECT_EQ(home, fs::path(tmp_dir));

    fs::remove_all(tmp_dir);
}

TEST(Paths, FindLibDirWithStd) {
    ScopedEnv guard("RY_HOME", "/nonexistent-ry-home");

    char tmp_dir[] = "/tmp/ry-test-XXXXXX";
    ASSERT_NE(mkdtemp(tmp_dir), nullptr);
    std::string tmp(tmp_dir);

    fs::create_directories(tmp + "/bin");
    fs::create_directories(tmp + "/lib/std");

    std::ofstream(tmp + "/lib/std/builtins.ry") << "fn noop():\n    return\n";

    auto result = ry::find_lib_dir(tmp + "/bin/ry");
    EXPECT_EQ(result, fs::canonical(tmp + "/lib"));

    fs::remove_all(tmp);
}

TEST(Paths, FindLibDirNotFound) {
    ScopedEnv guard("RY_HOME", "/nonexistent-ry-home");

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
