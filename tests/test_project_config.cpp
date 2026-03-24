#include <gtest/gtest.h>
#include "ry/project_config.hpp"
#include <filesystem>
#include <fstream>
#include <string>

namespace fs = std::filesystem;

// --- TOML Parser Tests ---

TEST(ProjectConfigParser, LoadBasic) {
    std::string toml = R"(
[project]
name = "my-app"
version = "1.0.0"
entry = "src/main.ry"

[paths]
src = "src"
test = "test"
)";
    auto config = ProjectConfigParser::load(toml);
    EXPECT_EQ(config.name, "my-app");
    EXPECT_EQ(config.version, "1.0.0");
    EXPECT_EQ(config.entry, "src/main.ry");
    EXPECT_EQ(config.src_dir, "src");
}

TEST(ProjectConfigParser, LoadWithCommentsAndBlankLines) {
    std::string toml = R"(
# Project configuration
[project]
name = "test-project"
version = "0.1.0"

# Entry point
entry = "src/main.ry"

[paths]
# Source directory
src = "src"
)";
    auto config = ProjectConfigParser::load(toml);
    EXPECT_EQ(config.name, "test-project");
    EXPECT_EQ(config.version, "0.1.0");
    EXPECT_EQ(config.entry, "src/main.ry");
    EXPECT_EQ(config.src_dir, "src");
}

TEST(ProjectConfigParser, SerializeRoundTrip) {
    ProjectConfig original;
    original.name = "round-trip";
    original.version = "2.0.0";
    original.entry = "src/main.ry";
    original.src_dir = "src";

    std::string toml = ProjectConfigParser::serialize(original);
    auto loaded = ProjectConfigParser::load(toml);

    EXPECT_EQ(loaded.name, original.name);
    EXPECT_EQ(loaded.version, original.version);
    EXPECT_EQ(loaded.entry, original.entry);
    EXPECT_EQ(loaded.src_dir, original.src_dir);
}

TEST(ProjectConfigParser, LoadInvalidLine) {
    std::string toml = "not a valid line";
    EXPECT_THROW(ProjectConfigParser::load(toml), std::runtime_error);
}

// --- findProjectRoot Tests ---

TEST(FindProjectRoot, FindsInCurrentDir) {
    auto tmpdir = fs::temp_directory_path() / "ry_test_find_root";
    fs::create_directories(tmpdir);
    { std::ofstream f(tmpdir / "ry.toml"); f << "[project]\n"; }

    auto result = findProjectRoot(tmpdir.string());
    ASSERT_TRUE(result.has_value());
    EXPECT_EQ(fs::path(*result), fs::absolute(tmpdir));

    fs::remove_all(tmpdir);
}

TEST(FindProjectRoot, FindsInParentDir) {
    auto tmpdir = fs::temp_directory_path() / "ry_test_find_parent";
    auto subdir = tmpdir / "sub" / "deep";
    fs::create_directories(subdir);
    { std::ofstream f(tmpdir / "ry.toml"); f << "[project]\n"; }

    auto result = findProjectRoot(subdir.string());
    ASSERT_TRUE(result.has_value());
    EXPECT_EQ(fs::path(*result), fs::absolute(tmpdir));

    fs::remove_all(tmpdir);
}

TEST(FindProjectRoot, ReturnsNulloptWhenNotFound) {
    auto tmpdir = fs::temp_directory_path() / "ry_test_no_root";
    fs::create_directories(tmpdir);

    auto result = findProjectRoot(tmpdir.string());
    EXPECT_FALSE(result.has_value());

    fs::remove_all(tmpdir);
}

// --- cmd_init Tests ---

class CmdInitTest : public ::testing::Test {
protected:
    fs::path tmpdir;
    fs::path original_cwd;

    void SetUp() override {
        original_cwd = fs::current_path();
        tmpdir = fs::temp_directory_path() / "ry_test_init";
        fs::create_directories(tmpdir);
        fs::current_path(tmpdir);
    }

    void TearDown() override {
        fs::current_path(original_cwd);
        fs::remove_all(tmpdir);
    }
};

TEST_F(CmdInitTest, CreatesProjectStructure) {
    ASSERT_EQ(cmd_init(), 0);

    EXPECT_TRUE(fs::exists(tmpdir / "ry.toml"));
    EXPECT_TRUE(fs::is_directory(tmpdir / "src"));
    EXPECT_FALSE(fs::exists(tmpdir / "test"));
    EXPECT_TRUE(fs::exists(tmpdir / "src" / "main.ry"));

    // Verify ry.toml content
    std::ifstream f(tmpdir / "ry.toml");
    std::string content((std::istreambuf_iterator<char>(f)),
                         std::istreambuf_iterator<char>());
    auto config = ProjectConfigParser::load(content);
    EXPECT_EQ(config.name, tmpdir.filename().string());
    EXPECT_EQ(config.version, "0.1.0");
    EXPECT_EQ(config.entry, "src/main.ry");

    // Verify src/main.ry content
    std::ifstream mf(tmpdir / "src" / "main.ry");
    std::string main_content((std::istreambuf_iterator<char>(mf)),
                              std::istreambuf_iterator<char>());
    EXPECT_EQ(main_content, "print(\"Hello, World!\")\n");
}

TEST_F(CmdInitTest, FailsIfRyTomlExists) {
    { std::ofstream f(tmpdir / "ry.toml"); f << "existing"; }
    EXPECT_EQ(cmd_init(), 1);
}

TEST_F(CmdInitTest, DoesNotOverwriteExistingMainRy) {
    fs::create_directories(tmpdir / "src");
    { std::ofstream f(tmpdir / "src" / "main.ry"); f << "# my code\n"; }

    ASSERT_EQ(cmd_init(), 0);

    std::ifstream mf(tmpdir / "src" / "main.ry");
    std::string content((std::istreambuf_iterator<char>(mf)),
                         std::istreambuf_iterator<char>());
    EXPECT_EQ(content, "# my code\n");
}

// --- cmd_new Tests ---

class CmdNewTest : public ::testing::Test {
protected:
    fs::path tmpdir;
    fs::path original_cwd;

    void SetUp() override {
        original_cwd = fs::current_path();
        tmpdir = fs::temp_directory_path() / "ry_test_new";
        fs::remove_all(tmpdir);
        fs::create_directories(tmpdir);
        fs::current_path(tmpdir);
    }

    void TearDown() override {
        fs::current_path(original_cwd);
        fs::remove_all(tmpdir);
    }
};

TEST_F(CmdNewTest, CreatesProjectStructure) {
    char arg[] = "my-app";
    char *argv[] = {arg};
    ASSERT_EQ(cmd_new(1, argv), 0);

    auto project_dir = tmpdir / "my-app";
    EXPECT_TRUE(fs::exists(project_dir / "ry.toml"));
    EXPECT_TRUE(fs::is_directory(project_dir / "src"));
    EXPECT_TRUE(fs::exists(project_dir / "src" / "main.ry"));

    // Verify ry.toml content
    std::ifstream f(project_dir / "ry.toml");
    std::string content((std::istreambuf_iterator<char>(f)),
                         std::istreambuf_iterator<char>());
    auto config = ProjectConfigParser::load(content);
    EXPECT_EQ(config.name, "my-app");
    EXPECT_EQ(config.version, "0.1.0");
    EXPECT_EQ(config.entry, "src/main.ry");
}

TEST_F(CmdNewTest, FailsWithNoArgs) {
    EXPECT_EQ(cmd_new(0, nullptr), 1);
}

TEST_F(CmdNewTest, FailsIfDirectoryExists) {
    fs::create_directories(tmpdir / "existing");
    char arg[] = "existing";
    char *argv[] = {arg};
    EXPECT_EQ(cmd_new(1, argv), 1);
}

TEST_F(CmdNewTest, RejectsAbsolutePath) {
    char arg[] = "/tmp/bad-project";
    char *argv[] = {arg};
    EXPECT_EQ(cmd_new(1, argv), 1);
}

TEST_F(CmdNewTest, RejectsPathTraversal) {
    char arg[] = "../bad-project";
    char *argv[] = {arg};
    EXPECT_EQ(cmd_new(1, argv), 1);
}

TEST_F(CmdNewTest, RejectsNestedPath) {
    char arg[] = "foo/bar";
    char *argv[] = {arg};
    EXPECT_EQ(cmd_new(1, argv), 1);
}
