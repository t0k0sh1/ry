#include <gtest/gtest.h>
#include <cstdio>
#include <filesystem>
#include <fstream>
#include <string>
#include <array>
#include <utility>
#include <vector>
#include <unistd.h>
#include <sys/wait.h>


namespace fs = std::filesystem;

// Run `ry run <extra_args...>` in the given working directory.
// Returns {stdout+stderr combined, exit_code}.
static std::pair<std::string, int> runRyRunInDir(
    const std::string &cwd,
    const std::vector<const char *> &extra_args = {}) {

    int pipeOut[2];
    if (pipe(pipeOut) != 0) return {"", -1};

    pid_t pid = fork();
    if (pid < 0) {
        close(pipeOut[0]);
        close(pipeOut[1]);
        return {"", -1};
    }

    if (pid == 0) {
        close(pipeOut[0]);
        dup2(pipeOut[1], STDOUT_FILENO);
        dup2(pipeOut[1], STDERR_FILENO);
        close(pipeOut[1]);

        if (chdir(cwd.c_str()) != 0) _exit(126);
        setenv("RY_ENV", "internal", 1);

        // Build argv: RY_BINARY_PATH (path AND argv[0]) + "run" + extra_args + nullptr.
        // argv[0] uses the full RY_BINARY_PATH (not bare "ry") so that Linux glibc's
        // `fs::canonical(parent_path(argv[0]))` resolves the exe-adjacent stdlib correctly
        // (.claude/rules/tests-cpp-conventions.md).
        std::vector<const char *> argv_vec;
        argv_vec.push_back(RY_BINARY_PATH);
        argv_vec.push_back("run");
        for (auto a : extra_args) argv_vec.push_back(a);
        argv_vec.push_back(nullptr);

        execv(RY_BINARY_PATH, const_cast<char *const *>(argv_vec.data()));
        _exit(127);
    }

    close(pipeOut[1]);

    std::string output;
    std::array<char, 4096> buf;
    ssize_t n;
    while ((n = read(pipeOut[0], buf.data(), buf.size())) > 0) {
        output.append(buf.data(), static_cast<size_t>(n));
    }
    close(pipeOut[0]);

    int status;
    waitpid(pid, &status, 0);
    int exit_code = WIFEXITED(status) ? WEXITSTATUS(status) : -1;
    return {output, exit_code};
}

class CmdRunFileExecTest : public ::testing::Test {
protected:
    fs::path tmp_dir_;

    void SetUp() override {
        tmp_dir_ = fs::temp_directory_path() /
                   ("ry_cmd_run_test_" + std::to_string(getpid()));
        fs::create_directories(tmp_dir_ / "src");
    }

    void TearDown() override {
        std::error_code ec;
        fs::remove_all(tmp_dir_, ec);
    }

    void writeFile(const std::string &rel_path, const std::string &content) {
        const fs::path abs = tmp_dir_ / rel_path;
        fs::create_directories(abs.parent_path());
        std::ofstream ofs(abs);
        ofs << content;
    }

    void writePackageToml(const std::string &entry = "src/main.ry",
                          const std::string &extra = "") {
        writeFile("package.toml",
                  "[project]\nname = \"test\"\nversion = \"0.1.0\"\n"
                  "entry = \"" + entry + "\"\n\n"
                  "[paths]\nsrc = \"src\"\n" + extra);
    }
};

// `ry run main.ry` — bare .ry 名を [paths] 経由で解決して実行
TEST_F(CmdRunFileExecTest, RunBareRyFileViaPaths) {
    writePackageToml();
    writeFile("src/main.ry", "print(\"file-ok\")");
    auto [out, rc] = runRyRunInDir(tmp_dir_.string(), {"main.ry"});
    EXPECT_EQ(rc, 0);
    EXPECT_EQ(out, "file-ok\n");
}

// `ry run main` — scripts に無い → bare-name fallback で main.ry を解決
TEST_F(CmdRunFileExecTest, RunBareNameFallbackToRyFile) {
    writePackageToml();
    writeFile("src/main.ry", "print(\"bare-fallback-ok\")");
    auto [out, rc] = runRyRunInDir(tmp_dir_.string(), {"main"});
    EXPECT_EQ(rc, 0);
    EXPECT_EQ(out, "bare-fallback-ok\n");
}

// `ry run src/main.ry` — relative path で実行
TEST_F(CmdRunFileExecTest, RunRelativePath) {
    writePackageToml();
    writeFile("src/main.ry", "print(\"rel-ok\")");
    auto [out, rc] = runRyRunInDir(tmp_dir_.string(), {"src/main.ry"});
    EXPECT_EQ(rc, 0);
    EXPECT_EQ(out, "rel-ok\n");
}

// `ry run main.ry arg1 arg2` — args 付きファイル実行
TEST_F(CmdRunFileExecTest, RunFileWithArgs) {
    writePackageToml();
    writeFile("src/main.ry",
              "a = args()\nprint(a[0])\nprint(a[1])");
    auto [out, rc] = runRyRunInDir(tmp_dir_.string(),
                                   {"main.ry", "hello", "world"});
    EXPECT_EQ(rc, 0);
    EXPECT_EQ(out, "hello\nworld\n");
}

// `ry run -- arg1 arg2` — entry を args 付きで実行
TEST_F(CmdRunFileExecTest, RunEntryWithDoubleDashArgs) {
    writePackageToml();
    writeFile("src/main.ry",
              "a = args()\nprint(a[0])\nprint(a[1])");
    auto [out, rc] = runRyRunInDir(tmp_dir_.string(), {"--", "foo", "bar"});
    EXPECT_EQ(rc, 0);
    EXPECT_EQ(out, "foo\nbar\n");
}

// `ry run show` — 同名 .ry ファイルがあっても [scripts] が優先される
TEST_F(CmdRunFileExecTest, ScriptWinsOverBareNameWithSameName) {
    writePackageToml("src/main.ry",
                     "\n[scripts]\nshow = \"echo from-script\"\n");
    writeFile("src/main.ry", "print(\"would-not-run\")");
    writeFile("src/show.ry", "print(\"from-file\")");
    auto [out, rc] = runRyRunInDir(tmp_dir_.string(), {"show"});
    EXPECT_EQ(rc, 0);
    EXPECT_NE(out.find("from-script"), std::string::npos);
    EXPECT_EQ(out.find("from-file"), std::string::npos);
}

// `ry run show.ry` — 拡張子付きは scripts 解決をスキップしてファイルを実行
TEST_F(CmdRunFileExecTest, ExplicitRyExtensionBypassesScript) {
    writePackageToml("src/main.ry",
                     "\n[scripts]\nshow = \"echo from-script\"\n");
    writeFile("src/show.ry", "print(\"from-file\")");
    auto [out, rc] = runRyRunInDir(tmp_dir_.string(), {"show.ry"});
    EXPECT_EQ(rc, 0);
    EXPECT_NE(out.find("from-file"), std::string::npos);
    EXPECT_EQ(out.find("from-script"), std::string::npos);
}

// `ry run nonexistent` — script にも .ry にも無い → エラー + scripts 一覧
TEST_F(CmdRunFileExecTest, UnknownNameReportsErrorAndListsScripts) {
    writePackageToml("src/main.ry",
                     "\n[scripts]\nbuild = \"echo building\"\n");
    writeFile("src/main.ry", "print(1)");
    auto [out, rc] = runRyRunInDir(tmp_dir_.string(), {"nonexistent"});
    EXPECT_NE(rc, 0);
    EXPECT_NE(out.find("no such file: nonexistent.ry"), std::string::npos);
    EXPECT_NE(out.find("Available scripts:"), std::string::npos);
    EXPECT_NE(out.find("build"), std::string::npos);
}
