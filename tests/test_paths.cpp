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

TEST(Paths, FindLibDirSkipGlobal) {
    // Set up RY_HOME with a valid std/ directory
    char home_dir[] = "/tmp/ry-home-XXXXXX";
    ASSERT_NE(mkdtemp(home_dir), nullptr);
    std::string home(home_dir);
    fs::create_directories(home + "/lib/std");
    std::ofstream(home + "/lib/std/builtins.ry") << "fn noop():\n    return\n";

    // Set up exe-relative lib/ directory
    char exe_dir[] = "/tmp/ry-exe-XXXXXX";
    ASSERT_NE(mkdtemp(exe_dir), nullptr);
    std::string exe(exe_dir);
    fs::create_directories(exe + "/bin");
    fs::create_directories(exe + "/lib/std");
    std::ofstream(exe + "/lib/std/builtins.ry") << "fn noop():\n    return\n";

    ScopedEnv guard("RY_HOME", home_dir);

    // Without skip_global: should find RY_HOME/lib
    auto result_normal = ry::find_lib_dir(exe + "/bin/ry", false);
    EXPECT_EQ(result_normal, fs::path(home) / "lib");

    // With skip_global: should skip RY_HOME/lib and find exe/../lib
    auto result_skip = ry::find_lib_dir(exe + "/bin/ry", true);
    EXPECT_EQ(result_skip, fs::canonical(exe + "/lib"));

    fs::remove_all(home);
    fs::remove_all(exe);
}

TEST(Paths, FindLibDirPrefersProjectOverrideForRepoBuild) {
    char home_dir[] = "/tmp/ry-home-override-XXXXXX";
    ASSERT_NE(mkdtemp(home_dir), nullptr);
    std::string home(home_dir);
    fs::create_directories(home + "/lib/std");
    std::ofstream(home + "/lib/std/builtins.ry") << "fn noop():\n    return\n";

    char project_dir[] = "/tmp/ry-project-XXXXXX";
    ASSERT_NE(mkdtemp(project_dir), nullptr);
    std::string project(project_dir);
    fs::create_directories(project + "/build");
    fs::create_directories(project + "/lib/std");
    std::ofstream(project + "/lib/std/builtins.ry") << "fn noop():\n    return\n";
    std::ofstream(project + "/package.toml") << R"(
[project]
name = "ry"
version = "0.1.0"
entry = "src/main.ry"

[paths]
src = "src"
_dev_stdlib = "lib"
)";

    ScopedEnv guard("RY_HOME", home_dir);

    auto result = ry::find_lib_dir(project + "/build/ry", project, false);
    EXPECT_EQ(result, fs::canonical(project + "/lib"));

    fs::remove_all(home);
    fs::remove_all(project);
}

TEST(Paths, FindLibDirIgnoresProjectOverrideForInstalledBinary) {
    char home_dir[] = "/tmp/ry-home-installed-XXXXXX";
    ASSERT_NE(mkdtemp(home_dir), nullptr);
    std::string home(home_dir);
    fs::create_directories(home + "/lib/std");
    std::ofstream(home + "/lib/std/builtins.ry") << "fn noop():\n    return\n";

    char project_dir[] = "/tmp/ry-project-installed-XXXXXX";
    ASSERT_NE(mkdtemp(project_dir), nullptr);
    std::string project(project_dir);
    fs::create_directories(project + "/lib/std");
    std::ofstream(project + "/lib/std/builtins.ry") << "fn noop():\n    return\n";
    std::ofstream(project + "/package.toml") << R"(
[project]
name = "ry"
version = "0.1.0"
entry = "src/main.ry"

[paths]
src = "src"
_dev_stdlib = "lib"
)";

    char install_dir[] = "/tmp/ry-install-XXXXXX";
    ASSERT_NE(mkdtemp(install_dir), nullptr);
    std::string install(install_dir);
    fs::create_directories(install + "/bin");

    ScopedEnv guard("RY_HOME", home_dir);

    auto result = ry::find_lib_dir(install + "/bin/ry", project, false);
    EXPECT_EQ(result, fs::path(home) / "lib");

    fs::remove_all(home);
    fs::remove_all(project);
    fs::remove_all(install);
}

TEST(Paths, FindLibDirRejectsInvalidProjectOverride) {
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

    EXPECT_THROW(ry::find_lib_dir(project + "/build/ry", project, false), std::runtime_error);

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

TEST(Paths, ManifestReadMissing) {
    auto manifest = ry::read_manifest("/nonexistent/path");
    EXPECT_TRUE(manifest.version.empty());
    EXPECT_TRUE(manifest.files.empty());
}
