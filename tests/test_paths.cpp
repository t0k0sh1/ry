#include <gtest/gtest.h>
#include "ry/paths.hpp"
#include <cstdlib>
#include <filesystem>
#include <fstream>
#include <string>
#include <optional>


using namespace ry;
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

TEST(Paths, FindShareDirWithStd) {
    ScopedEnv guard("RY_HOME", "/nonexistent-ry-home");

    char tmp_dir[] = "/tmp/ry-test-XXXXXX";
    ASSERT_NE(mkdtemp(tmp_dir), nullptr);
    std::string tmp(tmp_dir);

    fs::create_directories(tmp + "/bin");
    fs::create_directories(tmp + "/share/std");

    std::ofstream(tmp + "/share/std/builtins.ry") << "function noop():\n    return\n";

    auto result = ry::find_share_dir(tmp + "/bin/ry");
    EXPECT_EQ(result, fs::canonical(tmp + "/share"));

    fs::remove_all(tmp);
}

TEST(Paths, FindShareDirExeRelativeFallsBackToLib) {
    ScopedEnv guard("RY_HOME", "/nonexistent-ry-home");

    char tmp_dir[] = "/tmp/ry-test-exelib-XXXXXX";
    ASSERT_NE(mkdtemp(tmp_dir), nullptr);
    std::string tmp(tmp_dir);

    // Only lib/std exists (old layout), no share/std
    fs::create_directories(tmp + "/bin");
    fs::create_directories(tmp + "/lib/std");
    std::ofstream(tmp + "/lib/std/builtins.ry") << "function noop():\n    return\n";

    auto result = ry::find_share_dir(tmp + "/bin/ry");
    EXPECT_EQ(result, fs::canonical(tmp + "/lib"));

    fs::remove_all(tmp);
}

TEST(Paths, FindShareDirNotFound) {
    ScopedEnv guard("RY_HOME", "/nonexistent-ry-home");

    auto result = ry::find_share_dir("/nonexistent/path/ry");
    EXPECT_TRUE(result.empty());
}

TEST(Paths, FindShareDirFallsBackToLibStd) {
    // When RY_HOME has only lib/std (pre-migration), find_share_dir should
    // fall back to it so the new binary can still find stdlib.
    char home_dir[] = "/tmp/ry-home-fallback-XXXXXX";
    ASSERT_NE(mkdtemp(home_dir), nullptr);
    std::string home(home_dir);
    fs::create_directories(home + "/lib/std");
    std::ofstream(home + "/lib/std/builtins.ry") << "function noop():\n    return\n";

    ScopedEnv guard("RY_HOME", home_dir);

    auto result = ry::find_share_dir("/nonexistent/path/ry", false);
    EXPECT_EQ(result, fs::path(home) / "lib");

    fs::remove_all(home);
}

TEST(Paths, FindShareDirPrefersShareOverLib) {
    // When both share/std and lib/std exist, share/std takes priority.
    char home_dir[] = "/tmp/ry-home-both-XXXXXX";
    ASSERT_NE(mkdtemp(home_dir), nullptr);
    std::string home(home_dir);
    fs::create_directories(home + "/share/std");
    std::ofstream(home + "/share/std/builtins.ry") << "function noop():\n    return\n";
    fs::create_directories(home + "/lib/std");
    std::ofstream(home + "/lib/std/builtins.ry") << "function noop():\n    return\n";

    ScopedEnv guard("RY_HOME", home_dir);

    auto result = ry::find_share_dir("/nonexistent/path/ry", false);
    EXPECT_EQ(result, fs::path(home) / "share");

    fs::remove_all(home);
}

TEST(Paths, FindShareDirSkipGlobal) {
    // Set up RY_HOME with a valid std/ directory
    char home_dir[] = "/tmp/ry-home-XXXXXX";
    ASSERT_NE(mkdtemp(home_dir), nullptr);
    std::string home(home_dir);
    fs::create_directories(home + "/share/std");
    std::ofstream(home + "/share/std/builtins.ry") << "function noop():\n    return\n";

    // Set up exe-relative share/ directory
    char exe_dir[] = "/tmp/ry-exe-XXXXXX";
    ASSERT_NE(mkdtemp(exe_dir), nullptr);
    std::string exe(exe_dir);
    fs::create_directories(exe + "/bin");
    fs::create_directories(exe + "/share/std");
    std::ofstream(exe + "/share/std/builtins.ry") << "function noop():\n    return\n";

    ScopedEnv guard("RY_HOME", home_dir);

    // Without skip_global: should find RY_HOME/share
    auto result_normal = ry::find_share_dir(exe + "/bin/ry", false);
    EXPECT_EQ(result_normal, fs::path(home) / "share");

    // With skip_global: should skip RY_HOME/share and find exe/../share
    auto result_skip = ry::find_share_dir(exe + "/bin/ry", true);
    EXPECT_EQ(result_skip, fs::canonical(exe + "/share"));

    fs::remove_all(home);
    fs::remove_all(exe);
}

TEST(Paths, FindShareDirPrefersProjectOverrideForRepoBuild) {
    char home_dir[] = "/tmp/ry-home-override-XXXXXX";
    ASSERT_NE(mkdtemp(home_dir), nullptr);
    std::string home(home_dir);
    fs::create_directories(home + "/share/std");
    std::ofstream(home + "/share/std/builtins.ry") << "function noop():\n    return\n";

    char project_dir[] = "/tmp/ry-project-XXXXXX";
    ASSERT_NE(mkdtemp(project_dir), nullptr);
    std::string project(project_dir);
    fs::create_directories(project + "/build");
    fs::create_directories(project + "/share/std");
    std::ofstream(project + "/share/std/builtins.ry") << "function noop():\n    return\n";
    std::ofstream(project + "/package.toml") << R"(
[project]
name = "ry"
version = "0.1.0"
entry = "src/main.ry"

[paths]
src = "src"
_dev_stdlib = "share"
)";

    ScopedEnv guard("RY_HOME", home_dir);

    auto result = ry::find_share_dir(project + "/build/ry", project, false);
    EXPECT_EQ(result, fs::canonical(project + "/share"));

    fs::remove_all(home);
    fs::remove_all(project);
}

TEST(Paths, FindShareDirIgnoresProjectOverrideForInstalledBinary) {
    char home_dir[] = "/tmp/ry-home-installed-XXXXXX";
    ASSERT_NE(mkdtemp(home_dir), nullptr);
    std::string home(home_dir);
    fs::create_directories(home + "/share/std");
    std::ofstream(home + "/share/std/builtins.ry") << "function noop():\n    return\n";

    char project_dir[] = "/tmp/ry-project-installed-XXXXXX";
    ASSERT_NE(mkdtemp(project_dir), nullptr);
    std::string project(project_dir);
    fs::create_directories(project + "/share/std");
    std::ofstream(project + "/share/std/builtins.ry") << "function noop():\n    return\n";
    std::ofstream(project + "/package.toml") << R"(
[project]
name = "ry"
version = "0.1.0"
entry = "src/main.ry"

[paths]
src = "src"
_dev_stdlib = "share"
)";

    char install_dir[] = "/tmp/ry-install-XXXXXX";
    ASSERT_NE(mkdtemp(install_dir), nullptr);
    std::string install(install_dir);
    fs::create_directories(install + "/bin");

    ScopedEnv guard("RY_HOME", home_dir);

    auto result = ry::find_share_dir(install + "/bin/ry", project, false);
    EXPECT_EQ(result, fs::path(home) / "share");

    fs::remove_all(home);
    fs::remove_all(project);
    fs::remove_all(install);
}

TEST(Paths, FindShareDirRejectsInvalidProjectOverride) {
    char project_dir[] = "/tmp/ry-project-invalid-XXXXXX";
    ASSERT_NE(mkdtemp(project_dir), nullptr);
    std::string project(project_dir);
    fs::create_directories(project + "/build");
    std::ofstream(project + "/package.toml") << R"(
[project]
name = "ry"
version = "0.1.0"
entry = "src/main.ry"

[paths]
src = "src"
_dev_stdlib = "../lib"
)";

    EXPECT_THROW(ry::find_share_dir(project + "/build/ry", project, false), std::runtime_error);

    fs::remove_all(project);
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

// ===== find_native_library tests =====

TEST(Paths, FindNativeLibraryExeParentLib) {
    ScopedEnv guard("RY_HOME", "/nonexistent-ry-home");

    char tmp_dir[] = "/tmp/ry-nlib-XXXXXX";
    ASSERT_NE(mkdtemp(tmp_dir), nullptr);
    std::string tmp(tmp_dir);

    // Set up exe/../lib/ layout (installed layout)
    fs::create_directories(tmp + "/bin");
    fs::create_directories(tmp + "/lib");
#ifdef __APPLE__
    std::string filename = "libry_base64.dylib";
#else
    std::string filename = "libry_base64.so";
#endif
    std::ofstream(tmp + "/lib/" + filename) << "fake";

    auto result = ry::find_native_library(tmp + "/bin/ry", "base64");
    EXPECT_EQ(result, fs::canonical(tmp + "/lib/" + filename));

    fs::remove_all(tmp);
}

TEST(Paths, FindNativeLibraryExeSiblingLib) {
    ScopedEnv guard("RY_HOME", "/nonexistent-ry-home");

    char tmp_dir[] = "/tmp/ry-nlib-sibling-XXXXXX";
    ASSERT_NE(mkdtemp(tmp_dir), nullptr);
    std::string tmp(tmp_dir);

    // Set up exe/lib/ layout (development build layout)
    fs::create_directories(tmp + "/build/lib");
#ifdef __APPLE__
    std::string filename = "libry_path.dylib";
#else
    std::string filename = "libry_path.so";
#endif
    std::ofstream(tmp + "/build/lib/" + filename) << "fake";

    auto result = ry::find_native_library(tmp + "/build/ry", "path");
    EXPECT_EQ(result, fs::canonical(tmp + "/build/lib/" + filename));

    fs::remove_all(tmp);
}

TEST(Paths, FindNativeLibraryRyHome) {
    char home_dir[] = "/tmp/ry-nlib-home-XXXXXX";
    ASSERT_NE(mkdtemp(home_dir), nullptr);
    std::string home(home_dir);

    ScopedEnv guard("RY_HOME", home_dir);

    fs::create_directories(home + "/lib");
#ifdef __APPLE__
    std::string filename = "libry_json.dylib";
#else
    std::string filename = "libry_json.so";
#endif
    std::ofstream(home + "/lib/" + filename) << "fake";

    auto result = ry::find_native_library("/nonexistent/bin/ry", "json");
    EXPECT_EQ(result, fs::path(home) / "lib" / filename);

    fs::remove_all(home);
}

TEST(Paths, FindNativeLibraryNotFound) {
    ScopedEnv guard("RY_HOME", "/nonexistent-ry-home");

    auto result = ry::find_native_library("/nonexistent/bin/ry", "nonexistent_pkg");
    EXPECT_TRUE(result.empty());
}

TEST(Paths, FindNativeLibraryPrefersExeParentOverRyHome) {
    char tmp_dir[] = "/tmp/ry-nlib-pref-XXXXXX";
    ASSERT_NE(mkdtemp(tmp_dir), nullptr);
    std::string tmp(tmp_dir);

    char home_dir[] = "/tmp/ry-nlib-home2-XXXXXX";
    ASSERT_NE(mkdtemp(home_dir), nullptr);
    std::string home(home_dir);

    ScopedEnv guard("RY_HOME", home_dir);

#ifdef __APPLE__
    std::string filename = "libry_io.dylib";
#else
    std::string filename = "libry_io.so";
#endif
    // Both locations have the library
    fs::create_directories(tmp + "/bin");
    fs::create_directories(tmp + "/lib");
    std::ofstream(tmp + "/lib/" + filename) << "exe-relative";
    fs::create_directories(home + "/lib");
    std::ofstream(home + "/lib/" + filename) << "ry-home";

    auto result = ry::find_native_library(tmp + "/bin/ry", "io");
    // Should prefer exe/../lib/ over $RY_HOME/lib/
    EXPECT_EQ(result, fs::canonical(tmp + "/lib/" + filename));

    fs::remove_all(tmp);
    fs::remove_all(home);
}

TEST(Paths, ManifestReadMissing) {
    auto manifest = ry::read_manifest("/nonexistent/path");
    EXPECT_TRUE(manifest.version.empty());
    EXPECT_TRUE(manifest.files.empty());
}
