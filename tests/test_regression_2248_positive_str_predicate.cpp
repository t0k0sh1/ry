#include <gtest/gtest.h>

#include <array>
#include <filesystem>
#include <fstream>
#include <string>
#include <vector>

#include <sys/wait.h>
#include <unistd.h>

namespace fs = std::filesystem;

// #2248 smoke + heap-corruption guard.
//
// **What this test verifies in the default build**: that the wrapInAny
// str-retain path stays compositionally correct for the four scenarios
// the #2248 rewrite affects — concat-then-wrap, str-var-then-wrap,
// Map<str,str>-then-wrap, and the #2246 filter / Map<str,int> shape.
// Failures here mean either (a) heap corruption from a wrongly-fired
// `-24` retain (subprocess exit / mismatched output) or (b) a
// regression in one of the upstream paths (concat result handling, str
// var decl, Map indexer metadata) that re-introduces the source-side
// trap shape #2247 fixed.
//
// **What this test does NOT verify**: under-retain UAF specifically
// caused by `isStringValue` returning false at `wrapInAny:doStrRetain`.
// The behavioural delta of the rewrite is a strict superset of the
// shipped #2246 inline gate (same four channels, just hoisted), so an
// under-retain cannot regress relative to #2246 — and empirically the
// payload retain that `arc_any_managed_vars_` registration emits at
// decl time (`aLoad.any.retain.*` in the IR) catches every channel the
// wrapInAny retain would have. The real residual risk of the rewrite
// is **over-retain → leak** on a non-str ptr that happens to match one
// of the positive channels (e.g. the GEP-to-Global walk hitting a
// non-str global). That is sanitizer-only territory: run under
// `./docker/run.sh asan ry_tests` per `KNOWLEDGE.md ### ASan` /
// `docker/README.md` for leak detection. The default macOS build
// cannot observe it.
//
// The 100-iter subprocess loop catches the same heap-layout-sensitive
// failure shape as #2246's guard — combinator / index / format chains
// + concat-produced heap strs — across both #2247's metadata
// propagation and the new positive predicate. See
// `tests/test_regression_2246_str_metadata_gate.cpp` for the
// dedicated #2246 guard and `tests/spec/higher_order_elem_metadata.test.ry`
// for the #2247 positive-correctness pin.

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
            // Heap-str alias + drop via str var reassignment.
            << "s: str = \"a\" + \"b\"\n"
            << "aLoad: any = s\n"
            << "s = \"c\" + \"d\"\n"
            // Heap-str alias + drop via Map<str, str> value
            // reassignment.
            << "m: Map<str, str> = {}\n"
            << "m[\"k\"] = \"e\" + \"f\"\n"
            << "aMap: any = m[\"k\"]\n"
            << "m[\"k\"] = \"g\" + \"h\"\n"
            // #2246 / #2247 combinator + Map<str,int> indexing chain.
            << "xs: List<Map<str, int>> = [{\"a\": 1}, {\"b\": 2}, "
               "{\"a\": 3}]\n"
            << "keys: List<str> = [\"a\", \"b\", \"a\"]\n"
            << "ys = filter(xs, mm => true)\n"
            << "out = keys[2] + \"=\" + str(ys[2][keys[2]])\n"
            << "print(aLoad)\n"
            << "print(aMap)\n"
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
        } else if (r.out != "ab\nef\na=3\n") {
            ++mismatches;
            if (sample_mismatch_output.empty())
                sample_mismatch_output = r.out;
        }
    }
    EXPECT_EQ(fails, 0)
        << "Regression #2248: " << fails << "/" << kIterations
        << " process runs aborted on the heap-str alias + drop and "
           "combinator chains. A false-positive in the new "
           "isStringValue would route a non-str ptr through "
           "emitStrGetHeaderFromData (-24) and corrupt the adjacent "
           "heap block — the same failure shape as #1266 / #1799 / "
           "#2246. Leak-side over-retain regressions are sanitizer-only "
           "(run under docker/run.sh asan ry_tests).\nSample failing "
           "output:\n"
        << sample_fail_output;
    EXPECT_EQ(mismatches, 0)
        << "Regression #2248: " << mismatches << "/" << kIterations
        << " process runs produced unexpected output. Either an upstream "
           "regression in #2247's metadata propagation or a "
           "metadata-stamping path the rewrite indirectly broke is the "
           "likely cause.\nSample mismatched output:\n"
        << sample_mismatch_output;
}
