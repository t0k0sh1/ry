#include <gtest/gtest.h>

#include <array>
#include <filesystem>
#include <fstream>
#include <string>
#include <vector>

#include <sys/wait.h>
#include <unistd.h>

namespace fs = std::filesystem;

// #2248 regression guard. After rewriting `isStringValue` to a
// positive-evidence predicate, exercise a combinator + map-index +
// `str(int)` + str concat chain that funnels values through every
// evidence channel (`arc_str_owned_values_` via concat / `str()` result,
// `arc_str_managed_vars_` load via `k = keys[i]`, `GlobalVariable` GEP
// via literal `"="`, `str_elem` via `Map<str, _>` value extraction).
// A regression that flips any channel to false-negative would either
// fail the `+` dispatch (compile error) or leak / UAF the wrapped value
// at scope exit; a false-positive would route a non-str ptr through the
// `-24` retain and corrupt the adjacent heap block — symptomatic on the
// 100-iter subprocess loop the same way #2246 was. See also
// `tests/test_regression_2246_str_metadata_gate.cpp` for the original
// shape that motivated `wrapInAny`'s evidence gate.

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

class Regression2248PositiveStrPredicate : public ::testing::Test {
protected:
    fs::path scriptPath_;

    void SetUp() override {
        scriptPath_ = fs::path(RY_BINARY_PATH).parent_path() /
                      "regression_2248_positive_str_predicate.ry";
        std::ofstream(scriptPath_)
            << "xs: List<Map<str, int>> = [{\"a\": 1}, {\"b\": 2}, "
               "{\"a\": 3}]\n"
            << "keys: List<str> = [\"a\", \"b\", \"a\"]\n"
            << "ys = filter(xs, m => true)\n"
            << "i = 2\n"
            << "k = keys[i]\n"
            << "v = ys[i][k]\n"
            << "out = k + \"=\" + str(v)\n"
            << "print(out)\n";
    }

    void TearDown() override {
        std::error_code ec;
        fs::remove(scriptPath_, ec);
    }
};

TEST_F(Regression2248PositiveStrPredicate,
       CombinatorIndexConcatNoHeapCorruption) {
    constexpr int kIterations = 100;
    int fails = 0;
    int mismatches = 0;
    std::string sample_fail_output;
    std::string sample_mismatch_output;
    for (int i = 0; i < kIterations; ++i) {
        auto r = runScript(scriptPath_.string());
        if (r.exit_code != 0) {
            ++fails;
            if (sample_fail_output.empty()) sample_fail_output = r.out;
        } else if (r.out != "a=3\n") {
            ++mismatches;
            if (sample_mismatch_output.empty())
                sample_mismatch_output = r.out;
        }
    }
    EXPECT_EQ(fails, 0)
        << "Regression #2248: " << fails << "/" << kIterations
        << " process runs aborted. A false-positive in the new "
           "isStringValue (positive-evidence) gate would route a "
           "non-str ptr through emitStrGetHeaderFromData (-24) and "
           "corrupt the adjacent heap block — the same failure shape "
           "as #1266 / #1799 / #2246.\nSample failing output:\n"
        << sample_fail_output;
    EXPECT_EQ(mismatches, 0)
        << "Regression #2248: " << mismatches << "/" << kIterations
        << " process runs produced unexpected output. A "
           "false-negative in any of the four evidence channels "
           "(arc_str_owned_values_ / arc_str_managed_vars_ load / "
           "GlobalVariable / str_elem) would either fail the `+` "
           "dispatch or leak / UAF the wrapped str at scope "
           "exit.\nSample mismatched output:\n"
        << sample_mismatch_output;
}
