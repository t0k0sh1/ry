#include "ry/file_watcher.hpp"
#include <gtest/gtest.h>
#include <filesystem>
#include <fstream>

namespace fs = std::filesystem;

class FileWatcherTest : public ::testing::Test {
protected:
    fs::path tmp_dir;

    void SetUp() override {
        tmp_dir = fs::temp_directory_path() / "ry_watcher_test";
        fs::remove_all(tmp_dir);
        fs::create_directories(tmp_dir);
    }

    void TearDown() override {
        fs::remove_all(tmp_dir);
    }

    void createFile(const fs::path &path, const std::string &content = "") {
        fs::create_directories(path.parent_path());
        std::ofstream ofs(path);
        ofs << content;
    }
};

TEST_F(FileWatcherTest, CollectsRyFiles) {
    createFile(tmp_dir / "main.ry", "x = 1");
    createFile(tmp_dir / "sub" / "lib.ry", "y = 2");
    createFile(tmp_dir / "readme.md", "# Readme");

    auto mtimes = ry::collectRyFileMtimes(tmp_dir.string());

    EXPECT_EQ(mtimes.size(), 2u);
    EXPECT_TRUE(mtimes.count((tmp_dir / "main.ry").string()));
    EXPECT_TRUE(mtimes.count((tmp_dir / "sub" / "lib.ry").string()));
    EXPECT_FALSE(mtimes.count((tmp_dir / "readme.md").string()));
}

TEST_F(FileWatcherTest, SkipsBuildDirectory) {
    createFile(tmp_dir / "src" / "app.ry", "a = 1");
    createFile(tmp_dir / "build" / "out.ry", "b = 2");

    auto mtimes = ry::collectRyFileMtimes(tmp_dir.string());

    EXPECT_EQ(mtimes.size(), 1u);
    EXPECT_TRUE(mtimes.count((tmp_dir / "src" / "app.ry").string()));
    EXPECT_FALSE(mtimes.count((tmp_dir / "build" / "out.ry").string()));
}

TEST_F(FileWatcherTest, SkipsGitDirectory) {
    createFile(tmp_dir / "app.ry", "a = 1");
    createFile(tmp_dir / ".git" / "hooks" / "pre-commit.ry", "hook");

    auto mtimes = ry::collectRyFileMtimes(tmp_dir.string());

    EXPECT_EQ(mtimes.size(), 1u);
    EXPECT_TRUE(mtimes.count((tmp_dir / "app.ry").string()));
}

TEST_F(FileWatcherTest, SkipsNodeModulesDirectory) {
    createFile(tmp_dir / "app.ry", "a = 1");
    createFile(tmp_dir / "node_modules" / "pkg" / "index.ry", "pkg");

    auto mtimes = ry::collectRyFileMtimes(tmp_dir.string());

    EXPECT_EQ(mtimes.size(), 1u);
    EXPECT_TRUE(mtimes.count((tmp_dir / "app.ry").string()));
}

TEST_F(FileWatcherTest, EmptyDirectoryReturnsEmpty) {
    auto mtimes = ry::collectRyFileMtimes(tmp_dir.string());
    EXPECT_TRUE(mtimes.empty());
}

TEST_F(FileWatcherTest, CollectsTestRyFiles) {
    createFile(tmp_dir / "foo.test.ry", "test");
    createFile(tmp_dir / "bar.ry", "src");

    auto mtimes = ry::collectRyFileMtimes(tmp_dir.string());

    EXPECT_EQ(mtimes.size(), 2u);
    EXPECT_TRUE(mtimes.count((tmp_dir / "foo.test.ry").string()));
    EXPECT_TRUE(mtimes.count((tmp_dir / "bar.ry").string()));
}
