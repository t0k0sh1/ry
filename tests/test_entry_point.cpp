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

// Run ry binary in the given working directory with optional extra arguments.
// Returns {stdout+stderr, exit_code}.
static std::pair<std::string, int> runRyInDir(
    const std::string &cwd,
    const std::vector<const char *> &extra_args = {},
    const std::string &stdin_data = "") {

    int pipeIn[2];
    int pipeOut[2];
    if (pipe(pipeIn) != 0 || pipe(pipeOut) != 0) return {"", -1};

    pid_t pid = fork();
    if (pid < 0) return {"", -1};

    if (pid == 0) {
        // Child
        close(pipeIn[1]);
        close(pipeOut[0]);
        dup2(pipeIn[0], STDIN_FILENO);
        dup2(pipeOut[1], STDOUT_FILENO);
        dup2(pipeOut[1], STDERR_FILENO);
        close(pipeIn[0]);
        close(pipeOut[1]);

        if (chdir(cwd.c_str()) != 0) _exit(126);
        setenv("RY_ENV", "internal", 1);

        // Build argv: "ry" + extra_args + nullptr
        std::vector<const char *> argv_vec;
        argv_vec.push_back("ry");
        for (auto a : extra_args) argv_vec.push_back(a);
        argv_vec.push_back(nullptr);

        execv(RY_BINARY_PATH, const_cast<char *const *>(argv_vec.data()));
        _exit(127);
    }

    // Parent
    close(pipeIn[0]);
    close(pipeOut[1]);

    if (!stdin_data.empty()) {
        write(pipeIn[1], stdin_data.data(), stdin_data.size());
    }
    close(pipeIn[1]);

    std::string output;
    std::array<char, 4096> buf;
    ssize_t n;
    while ((n = read(pipeOut[0], buf.data(), buf.size())) > 0) {
        output.append(buf.data(), n);
    }
    close(pipeOut[0]);

    int status;
    waitpid(pid, &status, 0);
    int exit_code = WIFEXITED(status) ? WEXITSTATUS(status) : -1;
    return {output, exit_code};
}

class EntryPointTest : public ::testing::Test {
protected:
    fs::path tmp_dir_;

    void SetUp() override {
        tmp_dir_ = fs::temp_directory_path() /
                   ("ry_entry_test_" + std::to_string(getpid()));
        fs::create_directories(tmp_dir_ / "src");
    }

    void TearDown() override {
        std::error_code ec;
        fs::remove_all(tmp_dir_, ec);
    }

    void writeFile(const std::string &rel_path, const std::string &content) {
        std::ofstream ofs(tmp_dir_ / rel_path);
        ofs << content;
    }

    void writePackageToml(const std::string &entry = "src/main.ry") {
        writeFile("package.toml",
                  "[project]\nname = \"test\"\nversion = \"0.1.0\"\n"
                  "entry = \"" + entry + "\"\n\n"
                  "[paths]\nsrc = \"src\"\n");
    }
};

TEST_F(EntryPointTest, RunsEntryFromPackageToml) {
    writePackageToml();
    writeFile("src/main.ry", "print(\"entry-ok\")");
    auto [out, rc] = runRyInDir(tmp_dir_.string());
    EXPECT_EQ(rc, 0);
    EXPECT_EQ(out, "entry-ok\n");
}

TEST_F(EntryPointTest, RunsEntryWithArgs) {
    writePackageToml();
    writeFile("src/main.ry",
              "a = arguments()\n"
              "print(a[0])\n"
              "print(a[1])");
    auto [out, rc] = runRyInDir(tmp_dir_.string(), {"--", "hello", "world"});
    EXPECT_EQ(rc, 0);
    EXPECT_EQ(out, "hello\nworld\n");
}

TEST_F(EntryPointTest, RunsBareFilenameViaPaths) {
    writePackageToml();
    writeFile("src/main.ry", "print(\"bare-ok\")");
    auto [out, rc] = runRyInDir(tmp_dir_.string(), {"main.ry"});
    EXPECT_EQ(rc, 0);
    EXPECT_EQ(out, "bare-ok\n");
}

TEST_F(EntryPointTest, BareFilenameNotFoundListsSearchRoots) {
    writePackageToml();
    writeFile("src/main.ry", "print(1)");
    auto [out, rc] = runRyInDir(tmp_dir_.string(), {"missing.ry"});
    EXPECT_NE(rc, 0);
    EXPECT_NE(out.find("file not found: missing.ry"), std::string::npos);
    EXPECT_NE(out.find("Searched under [paths]"), std::string::npos);
}

TEST_F(EntryPointTest, BareFilenameAmbiguous) {
    writeFile("package.toml",
              "[project]\nname = \"test\"\nversion = \"0.1.0\"\nentry = \"src/main.ry\"\n\n"
              "[paths]\na = \"src\"\nb = \"lib\"\n");
    fs::create_directories(tmp_dir_ / "lib");
    writeFile("src/foo.ry", "print(\"src\")");
    writeFile("lib/foo.ry", "print(\"lib\")");
    auto [out, rc] = runRyInDir(tmp_dir_.string(), {"foo.ry"});
    EXPECT_NE(rc, 0);
    EXPECT_NE(out.find("ambiguous"), std::string::npos);
}

TEST_F(EntryPointTest, PrintsHelpWithoutPackageToml) {
    // No package.toml in tmp_dir
    auto [out, rc] = runRyInDir(tmp_dir_.string());
    EXPECT_EQ(rc, 1);
    EXPECT_NE(out.find("Usage:"), std::string::npos);
}

TEST_F(EntryPointTest, PipedStdinWithoutCFlagDoesNotExecute) {
    // Even with piped stdin, without -c flag, bare ry should NOT execute stdin
    // (it should run entry or show help instead)
    auto [out, rc] = runRyInDir(tmp_dir_.string(), {}, "print(999)");
    // No package.toml → help, not stdin execution
    EXPECT_EQ(rc, 1);
    EXPECT_EQ(out.find("999"), std::string::npos);
}
