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

RunResult runRy(std::vector<const char *> args) {
    int pipeOut[2];
    int pipeErr[2];
    if (pipe(pipeOut) != 0) return {};
    if (pipe(pipeErr) != 0) {
        close(pipeOut[0]);
        close(pipeOut[1]);
        return {};
    }

    pid_t pid = fork();
    if (pid < 0) {
        close(pipeOut[0]); close(pipeOut[1]);
        close(pipeErr[0]); close(pipeErr[1]);
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

        // argv[0] must be the full RY_BINARY_PATH so Linux glibc's
        // fs::canonical(parent_path(argv[0])) resolves the exe-adjacent
        // stdlib search path (.claude/rules/tests-cpp-conventions.md).
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

class LegacyImportErrorsTest : public ::testing::Test {
protected:
    fs::path tmpDir_;

    void SetUp() override {
        tmpDir_ = fs::path(RY_BINARY_PATH).parent_path() / "legacy_import_errors_test";
        fs::remove_all(tmpDir_);
        fs::create_directories(tmpDir_);
    }

    void TearDown() override { fs::remove_all(tmpDir_); }

    fs::path writeScript(const std::string &name, const std::string &content) {
        auto p = tmpDir_ / name;
        std::ofstream(p) << content;
        return p;
    }
};

} // namespace

TEST_F(LegacyImportErrorsTest, FlatFormIsRejected) {
    auto p = writeScript("flat_math.ry",
        "from math import sqrt\n"
        "print(sqrt(4.0))\n");

    auto r = runRy({"run", p.string().c_str()});
    EXPECT_NE(r.exit_code, 0) << "stderr: " << r.err;
    EXPECT_NE(r.err.find("legacy stdlib import form"), std::string::npos)
        << "stderr should mention 'legacy stdlib import form': " << r.err;
    EXPECT_NE(r.err.find("ry.math"), std::string::npos)
        << "stderr should suggest 'ry.math': " << r.err;
    EXPECT_NE(r.err.find("'from math import"), std::string::npos)
        << "stderr should quote the legacy form: " << r.err;
}

TEST_F(LegacyImportErrorsTest, StdDottedFormIsRejected) {
    auto p = writeScript("std_dotted_math.ry",
        "from std.math import NAN\n"
        "print(NAN)\n");

    auto r = runRy({"run", p.string().c_str()});
    EXPECT_NE(r.exit_code, 0) << "stderr: " << r.err;
    EXPECT_NE(r.err.find("legacy stdlib import form"), std::string::npos)
        << "stderr should mention 'legacy stdlib import form': " << r.err;
    EXPECT_NE(r.err.find("ry.math"), std::string::npos)
        << "stderr should suggest 'ry.math': " << r.err;
    EXPECT_NE(r.err.find("'from std.math import"), std::string::npos)
        << "stderr should quote the legacy form with dot spelling: " << r.err;
}

TEST_F(LegacyImportErrorsTest, FlatStdFormIsRejected) {
    // `print` is exported from share/std/builtins.ry; confirm `from std import print`
    // (the flat-std legacy form) is rejected and suggests `ry.lang`.
    auto p = writeScript("flat_std.ry",
        "from std import print\n"
        "print(\"hi\")\n");

    auto r = runRy({"run", p.string().c_str()});
    EXPECT_NE(r.exit_code, 0) << "stderr: " << r.err;
    EXPECT_NE(r.err.find("legacy stdlib import form"), std::string::npos)
        << "stderr should mention 'legacy stdlib import form': " << r.err;
    EXPECT_NE(r.err.find("ry.lang"), std::string::npos)
        << "stderr should suggest 'ry.lang': " << r.err;
}

TEST_F(LegacyImportErrorsTest, QualifiedFlatFormIsRejected) {
    auto p = writeScript("qualified_flat_math.ry",
        "import math\n"
        "print(math.sqrt(9.0))\n");

    auto r = runRy({"run", p.string().c_str()});
    EXPECT_NE(r.exit_code, 0) << "stderr: " << r.err;
    EXPECT_NE(r.err.find("legacy stdlib import form"), std::string::npos)
        << "stderr should mention 'legacy stdlib import form': " << r.err;
    EXPECT_NE(r.err.find("ry.math"), std::string::npos)
        << "stderr should suggest 'ry.math': " << r.err;
    EXPECT_NE(r.err.find("'import math'"), std::string::npos)
        << "stderr should quote the legacy form: " << r.err;
}

TEST_F(LegacyImportErrorsTest, CanonicalFormSucceeds) {
    auto p = writeScript("canonical.ry",
        "from ry.math import sqrt\n"
        "print(sqrt(4.0))\n");

    auto r = runRy({"run", p.string().c_str()});
    EXPECT_EQ(r.exit_code, 0) << "stderr: " << r.err;
}

TEST_F(LegacyImportErrorsTest, RyLangExplicitFormSucceeds) {
    // Explicit `from ry.lang import …` is canonical, not legacy.
    auto p = writeScript("ry_lang_explicit.ry",
        "from ry.lang import print\n"
        "print(\"hi\")\n");

    auto r = runRy({"run", p.string().c_str()});
    EXPECT_EQ(r.exit_code, 0) << "stderr: " << r.err;
}

TEST_F(LegacyImportErrorsTest, UserDefinedModuleNoFalsePositive) {
    // A user-defined `math.ry` shadows the stdlib name. Loader resolves to the
    // local file (rp.from_stdlib == false) — detector must NOT fire. This is the
    // single most important regression guard for Phase 2: a false positive would
    // break every existing project that happens to have a local module sharing
    // a stdlib name.
    std::ofstream(tmpDir_ / "math.ry")
        << "@public\nfn add(a: int, b: int) -> int:\n    return a + b\n";

    auto p = writeScript("uses_local_math.ry",
        "from math import add\n"
        "print(add(2, 3))\n");

    auto r = runRy({"run", p.string().c_str()});
    EXPECT_EQ(r.exit_code, 0) << "stderr: " << r.err;
    EXPECT_EQ(r.out, "5\n");
    EXPECT_EQ(r.err.find("legacy stdlib import form"), std::string::npos)
        << "user-defined module must not trigger legacy rejection: " << r.err;
}

TEST_F(LegacyImportErrorsTest, AliasedImportIsRejected) {
    // Alias (`as flatPI`) does not affect detection — module_path is still "math".
    // Use a constant (`PI`) because stdlib `@native` fn alias is not yet
    // supported (see tests/spec/ry_namespace/ry_module_import.test.ry:18).
    auto p = writeScript("aliased_math.ry",
        "from math import PI as flatPI\n"
        "print(flatPI)\n");

    auto r = runRy({"run", p.string().c_str()});
    EXPECT_NE(r.exit_code, 0) << "stderr: " << r.err;
    EXPECT_NE(r.err.find("legacy stdlib import form"), std::string::npos)
        << "aliased legacy import must still be rejected: " << r.err;
    EXPECT_NE(r.err.find("ry.math"), std::string::npos)
        << "stderr should suggest 'ry.math': " << r.err;
}
