#include <gtest/gtest.h>
#include <array>
#include <cstdlib>
#include <filesystem>
#include <fstream>
#include <string>
#include <thread>
#include <vector>
#include <unistd.h>
#include <sys/wait.h>

namespace fs = std::filesystem;

namespace {

struct RunResult {
    std::string out;
    std::string err;
    int exit_code = -1;
};

// Mirrors tests/test_deprecated_warnings.cpp::runRy. Strict-any semantics
// have been the compiler default since #2322, so this harness no longer
// needs env-var plumbing; the fork target inherits the parent's RY_ENV
// only.
RunResult runRy(std::vector<const char *> args) {
    int pipeOut[2];
    int pipeErr[2];
    if (pipe(pipeOut) != 0)
        return {};
    if (pipe(pipeErr) != 0) {
        close(pipeOut[0]);
        close(pipeOut[1]);
        return {};
    }

    pid_t pid = fork();
    if (pid < 0) {
        close(pipeOut[0]);
        close(pipeOut[1]);
        close(pipeErr[0]);
        close(pipeErr[1]);
        return {};
    }

    if (pid == 0) {
        close(pipeOut[0]);
        close(pipeErr[0]);
        dup2(pipeOut[1], STDOUT_FILENO);
        dup2(pipeErr[1], STDERR_FILENO);
        close(pipeOut[1]);
        close(pipeErr[1]);
        close(STDIN_FILENO);
        setenv("RY_ENV", "internal", 1);

        // argv[0] must be the full RY_BINARY_PATH (not bare "ry") for
        // Linux glibc exe-adjacent stdlib resolution; see
        // .claude/rules/tests-cpp-conventions.md.
        std::vector<const char *> argv{RY_BINARY_PATH};
        argv.insert(argv.end(), args.begin(), args.end());
        argv.push_back(nullptr);

        execv(RY_BINARY_PATH, const_cast<char *const *>(argv.data()));
        _exit(127);
    }

    close(pipeOut[1]);
    close(pipeErr[1]);

    auto readAll = [](int fd) -> std::string {
        std::string result;
        std::array<char, 4096> buf{};
        ssize_t n;
        while ((n = read(fd, buf.data(), buf.size())) > 0)
            result.append(buf.data(), static_cast<size_t>(n));
        close(fd);
        return result;
    };

    RunResult r;
    std::thread outReader([&]() { r.out = readAll(pipeOut[0]); });
    std::thread errReader([&]() { r.err = readAll(pipeErr[0]); });
    outReader.join();
    errReader.join();
    int status = 0;
    waitpid(pid, &status, 0);
    r.exit_code = WIFEXITED(status) ? WEXITSTATUS(status) : -1;
    return r;
}

class StrictAnyTest : public ::testing::Test {
protected:
    fs::path tmpDir_;

    void SetUp() override {
        // Place fixtures under the build tree so the source file's
        // ancestor chain reaches the repo's package.toml.
        tmpDir_ = fs::path(RY_BINARY_PATH).parent_path() / "ry_strict_any_test";
        fs::create_directories(tmpDir_);
    }

    void TearDown() override { fs::remove_all(tmpDir_); }

    fs::path writeTmp(const std::string &name, const std::string &content) {
        auto p = tmpDir_ / name;
        std::ofstream(p) << content;
        return p;
    }

    // Shared rejection assertion: write the source to a tmp file, run it,
    // expect non-zero exit and the given rule tag in stderr.
    void expectRejectedWithTag(const std::string &fileName,
                               const std::string &src,
                               const char *ruleTag) {
        auto p = writeTmp(fileName, src);
        auto r = runRy({"run", p.string().c_str()});
        EXPECT_NE(r.exit_code, 0)
            << "stdout: " << r.out << "\nstderr: " << r.err;
        EXPECT_NE(r.err.find(ruleTag), std::string::npos)
            << "stderr was: " << r.err;
    }
};

constexpr const char *kArithRuleTag = "[strict-any/any-arithmetic]";
constexpr const char *kImplicitUnwrapTag = "[strict-any/any-implicit-unwrap]";

} // namespace

// ---------------------------------------------------------------------------
// any-arithmetic rule (#2319, ordering ops added in #2322). Covers binary
// arithmetic (`+`/`-`/`*`/`/`/`%`/`//`/`**`), ordering comparisons (`<`/`<=`
// /`>`/`>=`), unary `-`, and compound assignment. Equality (`==`, `!=`)
// stays permitted because `__ry_any_eq` is type-safe (returns 0 on
// mismatch), whereas ordering would trap on heterogeneous operands at
// runtime.
// ---------------------------------------------------------------------------

TEST_F(StrictAnyTest, AnyArithRejectedByDefault) {
    expectRejectedWithTag("any_arith.ry",
        "a: any = 1\n"
        "b: any = 2\n"
        "print(a + b)\n",
        kArithRuleTag);
}

TEST_F(StrictAnyTest, AnyEqualityStillWorks) {
    auto p = writeTmp("any_cmp.ry",
        "a: any = 1\n"
        "print(a == 1)\n");
    auto r = runRy({"run", p.string().c_str()});
    EXPECT_EQ(r.exit_code, 0)
        << "stdout: " << r.out << "\nstderr: " << r.err;
    EXPECT_EQ(r.out, "true\n");
}

TEST_F(StrictAnyTest, AnyOrderingLtRejectedByDefault) {
    expectRejectedWithTag("any_lt.ry",
        "a: any = 1\n"
        "b: any = 2\n"
        "print(a < b)\n",
        kArithRuleTag);
}

TEST_F(StrictAnyTest, AnyOrderingLeRejectedByDefault) {
    expectRejectedWithTag("any_le.ry",
        "a: any = 1\n"
        "b: any = 2\n"
        "print(a <= b)\n",
        kArithRuleTag);
}

TEST_F(StrictAnyTest, AnyOrderingGtRejectedByDefault) {
    expectRejectedWithTag("any_gt.ry",
        "a: any = 1\n"
        "b: any = 2\n"
        "print(a > b)\n",
        kArithRuleTag);
}

TEST_F(StrictAnyTest, AnyOrderingGeRejectedByDefault) {
    expectRejectedWithTag("any_ge.ry",
        "a: any = 1\n"
        "b: any = 2\n"
        "print(a >= b)\n",
        kArithRuleTag);
}

TEST_F(StrictAnyTest, UnaryAnyArithRejectedByDefault) {
    expectRejectedWithTag("any_unary.ry",
        "a: any = 5\n"
        "print(-a)\n",
        kArithRuleTag);
}

// Compound assignment routes through applyCompoundOp → emitBinaryOp
// (codegen_stmt_misc.cpp), so the guard fires for `a += 1` the same way
// it does for `a = a + 1`. Locks in that the rule covers the compound
// path — a regression here would silently break the headline contract.
TEST_F(StrictAnyTest, CompoundAnyArithRejectedByDefault) {
    expectRejectedWithTag("any_compound.ry",
        "a: any = 1\n"
        "a += 2\n"
        "print(a)\n",
        kArithRuleTag);
}

// ---------------------------------------------------------------------------
// any-implicit-unwrap rule (#2321). Covers the four Path 9 sub-cases from
// docs/architecture/implicit-any-paths.md:
//   9a — variable declaration `n: int = v` where `v: any`
//   9b — named function-call argument `f(v)` (and the default-value path)
//   9c — lambda-call argument
//   9d — `Ok(v)` / `Err(v)` / `Some(v)` into a typed Result/Option slot
// ---------------------------------------------------------------------------

TEST_F(StrictAnyTest, AnyImplicitUnwrapVarDeclRejectedByDefault) {
    expectRejectedWithTag("any_unwrap_var.ry",
        "v: any = 1\n"
        "n: int = v\n"
        "print(n)\n",
        kImplicitUnwrapTag);
}

TEST_F(StrictAnyTest, AnyImplicitUnwrapNamedCallArgRejectedByDefault) {
    expectRejectedWithTag("any_unwrap_call.ry",
        "fn f(n: int) -> int:\n"
        "    return n + 1\n"
        "v: any = 1\n"
        "print(f(v))\n",
        kImplicitUnwrapTag);
}

// Default-value branch (codegen_call_user.cpp:220-222) emits a distinct
// "passing 'any' default value" message. Pins that the branch is wired —
// the explicit-arg test above hits the :192-194 branch and would not
// surface a regression in the default-value codegen.
TEST_F(StrictAnyTest, AnyImplicitUnwrapDefaultValueRejectedByDefault) {
    auto p = writeTmp("any_unwrap_default.ry",
        "fn getAny() -> any:\n"
        "    return 42\n"
        "fn f(x: int = getAny()) -> int:\n"
        "    return x + 1\n"
        "print(f())\n");
    auto r = runRy({"run", p.string().c_str()});
    EXPECT_NE(r.exit_code, 0) << "stdout: " << r.out << "\nstderr: " << r.err;
    EXPECT_NE(r.err.find(kImplicitUnwrapTag), std::string::npos)
        << "stderr was: " << r.err;
    EXPECT_NE(r.err.find("default value"), std::string::npos)
        << "stderr was: " << r.err;
}

TEST_F(StrictAnyTest, AnyImplicitUnwrapLambdaCallArgRejectedByDefault) {
    expectRejectedWithTag("any_unwrap_lambda.ry",
        "g = (n: int) -> int => n + 1\n"
        "v: any = 1\n"
        "print(g(v))\n",
        kImplicitUnwrapTag);
}

TEST_F(StrictAnyTest, AnyImplicitUnwrapOkSlotRejectedByDefault) {
    expectRejectedWithTag("any_unwrap_ok.ry",
        "fn f(v: any) -> Result<int, str>:\n"
        "    return Ok(v)\n"
        "case f(1):\n"
        "    Ok(n): print(n)\n"
        "    Err(e): print(e)\n",
        kImplicitUnwrapTag);
}

TEST_F(StrictAnyTest, AnyImplicitUnwrapErrSlotRejectedByDefault) {
    expectRejectedWithTag("any_unwrap_err.ry",
        "fn f(v: any) -> Result<int, str>:\n"
        "    return Err(v)\n"
        "case f(\"oops\"):\n"
        "    Ok(n): print(n)\n"
        "    Err(e): print(e)\n",
        kImplicitUnwrapTag);
}

TEST_F(StrictAnyTest, AnyImplicitUnwrapSomeSlotRejectedByDefault) {
    expectRejectedWithTag("any_unwrap_some.ry",
        "fn f(v: any) -> Option<int>:\n"
        "    return Some(v)\n"
        "case f(1):\n"
        "    Some(n): print(n)\n"
        "    None: print(\"none\")\n",
        kImplicitUnwrapTag);
}

// ---------------------------------------------------------------------------
// any-implicit-unwrap rule extension (#2379). Extends the rule to three
// structurally similar hazards previously carved out by strict-any.md:73:
//   - reassignment: `x = anyVal` where x is a previously-declared typed var
//   - return:       `return anyVal` from a typed function (incl. lambda)
//   - collection mutation: append! / appended / insert / set add / set
//                          remove / map[k]= / list[i]= of an `any` value
//                          into a typed collection slot
// Each test mirrors the Path 9 pattern: minimal source, hard-rejected with
// the `[strict-any/any-implicit-unwrap]` tag.
// ---------------------------------------------------------------------------

TEST_F(StrictAnyTest, AnyImplicitUnwrapReassignLocalRejectedByDefault) {
    expectRejectedWithTag("any_unwrap_reassign_local.ry",
        "x: int = 1\n"
        "v: any = 2\n"
        "x = v\n"
        "print(x)\n",
        kImplicitUnwrapTag);
}

// Module-global reassignment routes through emitModuleGlobalWriteThrough
// rather than the function-local AssignStmt path. Pin the mirror branches.
TEST_F(StrictAnyTest, AnyImplicitUnwrapReassignModuleGlobalRejectedByDefault) {
    expectRejectedWithTag("any_unwrap_reassign_global.ry",
        "x: int = 1\n"
        "fn touch():\n"
        "    v: any = 2\n"
        "    x = v\n"
        "touch()\n"
        "print(x)\n",
        kImplicitUnwrapTag);
}

TEST_F(StrictAnyTest, AnyImplicitUnwrapFnReturnRejectedByDefault) {
    expectRejectedWithTag("any_unwrap_return.ry",
        "fn f() -> int:\n"
        "    v: any = 1\n"
        "    return v\n"
        "print(f())\n",
        kImplicitUnwrapTag);
}

TEST_F(StrictAnyTest, AnyImplicitUnwrapLambdaReturnRejectedByDefault) {
    expectRejectedWithTag("any_unwrap_lambda_return.ry",
        "v: any = 1\n"
        "g = () -> int => v\n"
        "print(g())\n",
        kImplicitUnwrapTag);
}

TEST_F(StrictAnyTest, AnyImplicitUnwrapMapIndexAssignRejectedByDefault) {
    expectRejectedWithTag("any_unwrap_map_idx.ry",
        "m: Map<str, int> = {\"a\": 1}\n"
        "v: any = 2\n"
        "m[\"a\"] = v\n"
        "print(m[\"a\"])\n",
        kImplicitUnwrapTag);
}

TEST_F(StrictAnyTest, AnyImplicitUnwrapListIndexAssignRejectedByDefault) {
    expectRejectedWithTag("any_unwrap_list_idx.ry",
        "xs: List<int> = [1, 2, 3]\n"
        "v: any = 9\n"
        "xs[0] = v\n"
        "print(xs[0])\n",
        kImplicitUnwrapTag);
}

TEST_F(StrictAnyTest, AnyImplicitUnwrapAppendRejectedByDefault) {
    expectRejectedWithTag("any_unwrap_append.ry",
        "xs: List<int> = []\n"
        "v: any = 1\n"
        "append!(xs, v)\n"
        "print(xs[0])\n",
        kImplicitUnwrapTag);
}

TEST_F(StrictAnyTest, AnyImplicitUnwrapAppendedRejectedByDefault) {
    expectRejectedWithTag("any_unwrap_appended.ry",
        "xs: List<int> = []\n"
        "v: any = 1\n"
        "ys = appended(xs, v)\n"
        "print(ys[0])\n",
        kImplicitUnwrapTag);
}

TEST_F(StrictAnyTest, AnyImplicitUnwrapInsertRejectedByDefault) {
    expectRejectedWithTag("any_unwrap_insert.ry",
        "xs: List<int> = [1]\n"
        "v: any = 9\n"
        "insert(xs, 0, v)\n"
        "print(xs[0])\n",
        kImplicitUnwrapTag);
}

TEST_F(StrictAnyTest, AnyImplicitUnwrapSetAddRejectedByDefault) {
    expectRejectedWithTag("any_unwrap_set_add.ry",
        "s: Set<int> = {1, 2}\n"
        "v: any = 3\n"
        "add(s, v)\n"
        "print(3 in s)\n",
        kImplicitUnwrapTag);
}

TEST_F(StrictAnyTest, AnyImplicitUnwrapSetRemoveRejectedByDefault) {
    expectRejectedWithTag("any_unwrap_set_remove.ry",
        "s: Set<int> = {1, 2}\n"
        "v: any = 1\n"
        "remove(s, v)\n"
        "print(1 in s)\n",
        kImplicitUnwrapTag);
}

// Reassignment with a Result<any, X> source widening to Result<int, X>
// (or symmetrically the Err slot): the per-slot any → concrete unwrap
// inside coerceResultType is the same hazard class as the raw
// `x = anyVal` case, just at the Result Ok/Err slot granularity.
TEST_F(StrictAnyTest, AnyImplicitUnwrapResultOkSlotReassignRejectedByDefault) {
    expectRejectedWithTag("any_unwrap_result_ok_reassign.ry",
        "fn produce() -> Result<any, str>:\n"
        "    v: any = 42\n"
        "    return Ok(v)\n"
        "r: Result<int, str> = Ok(0)\n"
        "r = produce()\n"
        "case r:\n"
        "    Ok(n): print(n)\n"
        "    Err(e): print(e)\n",
        kImplicitUnwrapTag);
}

TEST_F(StrictAnyTest, AnyImplicitUnwrapResultErrSlotReassignRejectedByDefault) {
    expectRejectedWithTag("any_unwrap_result_err_reassign.ry",
        "fn produce() -> Result<int, any>:\n"
        "    v: any = \"oops\"\n"
        "    return Err(v)\n"
        "r: Result<int, str> = Ok(0)\n"
        "r = produce()\n"
        "case r:\n"
        "    Ok(n): print(n)\n"
        "    Err(e): print(e)\n",
        kImplicitUnwrapTag);
}
