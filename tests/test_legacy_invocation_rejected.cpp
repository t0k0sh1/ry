#include <gtest/gtest.h>
#include <array>
#include <cstdlib>
#include <cstring>
#include <filesystem>
#include <string>
#include <utility>
#include <vector>
#include <unistd.h>
#include <sys/wait.h>


namespace fs = std::filesystem;

struct LegacyRunResult {
    std::string out;
    std::string err;
    int exit_code;
};

// Run the ry binary in `cwd` with the given args (no `run` subcommand insertion).
// Used to exercise the deprecation rejection paths in src/app/main.cpp.
// argv[0] is set to RY_BINARY_PATH (full path) so Linux glibc's
// `fs::canonical(parent_path(argv[0]))` resolves the exe-adjacent stdlib correctly
// (.claude/rules/tests-cpp-conventions.md).
static LegacyRunResult runRyInDir(
    const std::string &cwd,
    std::initializer_list<const char *> args) {

    int pipeOut[2];
    int pipeErr[2];
    if (pipe(pipeOut) != 0) return {"", "", -1};
    if (pipe(pipeErr) != 0) {
        close(pipeOut[0]);
        close(pipeOut[1]);
        return {"", "", -1};
    }

    pid_t pid = fork();
    if (pid < 0) {
        close(pipeOut[0]); close(pipeOut[1]);
        close(pipeErr[0]); close(pipeErr[1]);
        return {"", "", -1};
    }

    if (pid == 0) {
        close(pipeOut[0]);
        close(pipeErr[0]);
        dup2(pipeOut[1], STDOUT_FILENO);
        dup2(pipeErr[1], STDERR_FILENO);
        close(pipeOut[1]);
        close(pipeErr[1]);
        close(STDIN_FILENO);

        if (chdir(cwd.c_str()) != 0) _exit(126);
        setenv("RY_ENV", "internal", 1);

        std::vector<const char *> argv_vec;
        argv_vec.push_back(RY_BINARY_PATH);
        for (auto a : args) argv_vec.push_back(a);
        argv_vec.push_back(nullptr);

        execv(RY_BINARY_PATH, const_cast<char *const *>(argv_vec.data()));
        _exit(127);
    }

    close(pipeOut[1]);
    close(pipeErr[1]);

    auto readAll = [](int fd) -> std::string {
        std::string result;
        std::array<char, 4096> buf;
        ssize_t n;
        while ((n = read(fd, buf.data(), buf.size())) > 0)
            result.append(buf.data(), static_cast<size_t>(n));
        close(fd);
        return result;
    };

    std::string out = readAll(pipeOut[0]);
    std::string err = readAll(pipeErr[0]);

    int status = 0;
    if (waitpid(pid, &status, 0) < 0) {
        return {out, err, -1};
    }
    int exit_code = WIFEXITED(status) ? WEXITSTATUS(status) : -1;
    return {out, err, exit_code};
}

class LegacyInvocationRejectedTest : public ::testing::Test {
protected:
    fs::path tmp_dir_;

    void SetUp() override {
        tmp_dir_ = fs::temp_directory_path() /
                   ("ry_legacy_test_" + std::to_string(getpid()));
        fs::create_directories(tmp_dir_);
    }

    void TearDown() override {
        std::error_code ec;
        fs::remove_all(tmp_dir_, ec);
    }
};

// `ry main.ry` — emits a deprecation error pointing at `ry run main.ry`.
// Detection is by argv shape (`.ry` suffix), not by file existence.
TEST_F(LegacyInvocationRejectedTest, BareRyFileEmitsDeprecationError) {
    auto r = runRyInDir(tmp_dir_.string(), {"main.ry"});
    EXPECT_NE(r.exit_code, 0);
    EXPECT_NE(r.err.find("'ry <file>' is no longer supported"), std::string::npos);
    EXPECT_NE(r.err.find("Use 'ry run <file>' instead"), std::string::npos);
}

// `ry ./src/foo.ry` — same deprecation error for relative paths.
TEST_F(LegacyInvocationRejectedTest, RelativeRyPathEmitsDeprecationError) {
    auto r = runRyInDir(tmp_dir_.string(), {"./src/foo.ry"});
    EXPECT_NE(r.exit_code, 0);
    EXPECT_NE(r.err.find("'ry <file>' is no longer supported"), std::string::npos);
}

// `ry --` — emits a deprecation error pointing at `ry run --`.
TEST_F(LegacyInvocationRejectedTest, DoubleDashEmitsDeprecationError) {
    auto r = runRyInDir(tmp_dir_.string(), {"--"});
    EXPECT_NE(r.exit_code, 0);
    EXPECT_NE(r.err.find("'ry --' is no longer supported"), std::string::npos);
    EXPECT_NE(r.err.find("Use 'ry run --' instead"), std::string::npos);
}

// `ry` (no args) — prints main help on stdout and exits non-zero.
TEST_F(LegacyInvocationRejectedTest, NoArgsPrintsHelpAndExitsNonZero) {
    auto r = runRyInDir(tmp_dir_.string(), {});
    EXPECT_NE(r.exit_code, 0);
    EXPECT_NE(r.out.find("Usage:"), std::string::npos);
}

// `ry foo` (no `.ry` extension) — still hits the existing unknown-command path,
// not the deprecation rejection. Confirms `looksLikeRyFile` is suffix-gated.
TEST_F(LegacyInvocationRejectedTest, BareNameWithoutExtensionFallsThroughToUnknownError) {
    auto r = runRyInDir(tmp_dir_.string(), {"foo"});
    EXPECT_NE(r.exit_code, 0);
    EXPECT_NE(r.err.find("unknown command 'foo'"), std::string::npos);
    EXPECT_NE(r.out.find("Usage:"), std::string::npos);
}
