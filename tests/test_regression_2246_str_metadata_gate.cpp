#include <gtest/gtest.h>

#include <array>
#include <filesystem>
#include <fstream>
#include <string>
#include <vector>

#include <sys/wait.h>
#include <unistd.h>

namespace fs = std::filesystem;

// #2246 regression guard. The bug is heap-layout-sensitive: the testing
// framework's per-iteration scaffolding inside an `@it` block stabilises
// the heap and masks the failure entirely (verified — 1000-iter `@it`
// loops pass), so a subprocess loop over the minimum 3-line script is
// the only reliable detector short of sanitizer instrumentation.
// Pre-fix repro on macOS ≈ 15/100; 0/100 with the fix is strong
// evidence (`P(0/100 | p=0.15) = 0.85^100 ≈ 8.7e-8`).
// See `.claude/rules/codegen-arc-cow.md` and KNOWLEDGE.md L49 for the
// root-cause history (`#1266` → `#1799` → `#2246`).

namespace {

struct RunResult {
    int exit_code;
    std::string out;
};

RunResult runScript(const std::string &scriptPath) {
    int pipefd[2];
    if (pipe(pipefd) != 0) return {-1, ""};

    pid_t pid = fork();
    if (pid < 0) {
        close(pipefd[0]);
        close(pipefd[1]);
        return {-1, ""};
    }

    if (pid == 0) {
        close(pipefd[0]);
        dup2(pipefd[1], STDOUT_FILENO);
        dup2(pipefd[1], STDERR_FILENO);
        close(pipefd[1]);
        close(STDIN_FILENO);

        // `RY_BINARY_PATH` is the absolute path to the built `ry` binary
        // (CMake-injected). Pass it as both argv[0] and execv's path per
        // `.claude/rules/tests-cpp-conventions.md` — bare "ry" breaks
        // Linux glibc's `fs::canonical("")` and stdlib discovery.
        std::vector<const char *> argv;
        argv.push_back(RY_BINARY_PATH);
        argv.push_back("run");
        argv.push_back(scriptPath.c_str());
        argv.push_back(nullptr);
        execv(RY_BINARY_PATH, const_cast<char *const *>(argv.data()));
        _exit(127);
    }

    close(pipefd[1]);
    std::string out;
    std::array<char, 4096> buf;
    ssize_t n;
    while ((n = read(pipefd[0], buf.data(), buf.size())) > 0) {
        out.append(buf.data(), static_cast<size_t>(n));
    }
    close(pipefd[0]);

    int status = 0;
    pid_t wp;
    while ((wp = waitpid(pid, &status, 0)) == -1) {
        if (errno != EINTR) break;
    }
    int code = -1;
    if (wp != -1 && WIFEXITED(status))
        code = WEXITSTATUS(status);
    return {code, out};
}

} // namespace

class Regression2246StrMetadataGate : public ::testing::Test {
protected:
    fs::path scriptPath_;

    void SetUp() override {
        // Co-locate next to the ry binary the same way TestRunnerFanOut
        // does (`tests/test_parallel_cli.cpp`). Keeps the script out of
        // the repo worktree per AGENTS.md "Do not create temporary
        // files in the worktree."
        scriptPath_ = fs::path(RY_BINARY_PATH).parent_path() /
                      "regression_2246_filter_map_str_int.ry";
        std::ofstream(scriptPath_)
            << "xs: List<Map<str, int>> = [{\"a\": 1}, {\"b\": 2}, "
               "{\"a\": 3}]\n"
            << "ys = filter(xs, m => true)\n"
            << "print(ys[2][\"a\"])\n";
    }

    void TearDown() override {
        std::error_code ec;
        fs::remove(scriptPath_, ec);
    }
};

TEST_F(Regression2246StrMetadataGate,
       FilterListMapStrIntKeyAccessNoHeapCorruption) {
    constexpr int kIterations = 100;
    int fails = 0;
    std::string sample_fail_output;
    for (int i = 0; i < kIterations; ++i) {
        auto r = runScript(scriptPath_.string());
        if (r.exit_code != 0) {
            ++fails;
            if (sample_fail_output.empty()) sample_fail_output = r.out;
        }
    }
    EXPECT_EQ(fails, 0)
        << "Regression #2246: " << fails << "/" << kIterations
        << " process runs aborted on the minimum repro. The bug was "
           "heap-layout-sensitive ~15% repro pre-fix; a single failure "
           "here is overwhelming evidence the wrapInAny str retain "
           "metadata-gate has regressed.\nSample failing output:\n"
        << sample_fail_output;
}
