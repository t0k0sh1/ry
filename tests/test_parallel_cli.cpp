#include <gtest/gtest.h>

#include <array>
#include <string>
#include <vector>

#include <sys/wait.h>
#include <unistd.h>


// Local copy of the runRy helper used in tests/test_help.cpp.
// Each subprocess test file keeps its own copy per the existing pattern.
// RY_BINARY_PATH is passed as BOTH execv's path and argv[0] per
// .claude/rules/tests-cpp-conventions.md (Linux glibc's fs::canonical("")
// fails when argv[0] is a bare "ry", breaking stdlib discovery).
struct RunResult {
    std::string out;
    std::string err;
    int exit_code;
};

static RunResult runRy(std::initializer_list<const char *> args) {
    int pipeOut[2];
    int pipeErr[2];
    if (pipe(pipeOut) != 0 || pipe(pipeErr) != 0) return {"", "", -1};

    pid_t pid = fork();
    if (pid < 0) return {"", "", -1};

    if (pid == 0) {
        close(pipeOut[0]);
        close(pipeErr[0]);
        dup2(pipeOut[1], STDOUT_FILENO);
        dup2(pipeErr[1], STDERR_FILENO);
        close(pipeOut[1]);
        close(pipeErr[1]);
        close(STDIN_FILENO);
        setenv("RY_ENV", "internal", 1);

        std::vector<const char *> argv;
        argv.push_back(RY_BINARY_PATH);
        for (auto a : args) argv.push_back(a);
        argv.push_back(nullptr);

        execv(RY_BINARY_PATH, const_cast<char *const *>(argv.data()));
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

    int status;
    waitpid(pid, &status, 0);
    int exit_code = WIFEXITED(status) ? WEXITSTATUS(status) : -1;

    return {out, err, exit_code};
}

// --- Help text mentions the optional N ---

TEST(ParallelCLI, HelpMentionsOptionalN) {
    auto r = runRy({"test", "--help"});
    EXPECT_EQ(r.exit_code, 0);
    EXPECT_NE(r.out.find("--parallel"), std::string::npos);
    EXPECT_NE(r.out.find("[N]"), std::string::npos);
}

// --- Rejection: zero / empty / non-digit / negative / malformed ---

TEST(ParallelCLI, RejectsZeroSpaced) {
    auto r = runRy({"test", "-p", "0"});
    EXPECT_NE(r.exit_code, 0);
    EXPECT_NE(r.err.find("must be a positive integer"), std::string::npos);
}

TEST(ParallelCLI, RejectsZeroEqualsForm) {
    auto r = runRy({"test", "--parallel=0"});
    EXPECT_NE(r.exit_code, 0);
    EXPECT_NE(r.err.find("must be a positive integer"), std::string::npos);
}

TEST(ParallelCLI, RejectsEmptyEqualsForm) {
    auto r = runRy({"test", "--parallel="});
    EXPECT_NE(r.exit_code, 0);
    EXPECT_NE(r.err.find("requires a positive integer"), std::string::npos);
}

TEST(ParallelCLI, RejectsNonDigitEqualsForm) {
    auto r = runRy({"test", "--parallel=abc"});
    EXPECT_NE(r.exit_code, 0);
    EXPECT_NE(r.err.find("must be a positive integer"), std::string::npos);
}

TEST(ParallelCLI, RejectsNegativeSpaced) {
    auto r = runRy({"test", "-p", "-5"});
    EXPECT_NE(r.exit_code, 0);
    EXPECT_NE(r.err.find("must be a positive integer"), std::string::npos);
}

TEST(ParallelCLI, RejectsMalformedSpaced) {
    auto r = runRy({"test", "-p", "1.5"});
    EXPECT_NE(r.exit_code, 0);
    EXPECT_NE(r.err.find("must be a positive integer"), std::string::npos);
}

TEST(ParallelCLI, RejectsTrailingJunkSpaced) {
    auto r = runRy({"test", "-p", "1abc"});
    EXPECT_NE(r.exit_code, 0);
    EXPECT_NE(r.err.find("must be a positive integer"), std::string::npos);
}

// --- Disambiguation: non-digit / non-count next arg is treated as target,
// NOT as an invalid count. The parser must NOT report a parallel-count
// error for these. The "no such file" or "package.toml not found"
// diagnostic from the path-handling code is acceptable; the parallel-
// count error is not.

TEST(ParallelCLI, NonDigitNonCountArgTreatedAsTarget) {
    auto r = runRy({"test", "-p", "no_such_path_2177_xyz.ry"});
    EXPECT_NE(r.exit_code, 0);
    EXPECT_EQ(r.err.find("worker count must be a positive integer"),
              std::string::npos);
}
