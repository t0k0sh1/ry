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

size_t countOccurrences(const std::string &haystack, const std::string &needle) {
    if (needle.empty()) return 0;
    size_t count = 0;
    size_t pos = 0;
    while ((pos = haystack.find(needle, pos)) != std::string::npos) {
        ++count;
        pos += needle.size();
    }
    return count;
}

class LegacyImportWarningsTest : public ::testing::Test {
protected:
    fs::path tmpDir_;

    void SetUp() override {
        tmpDir_ = fs::path(RY_BINARY_PATH).parent_path() / "legacy_import_warnings_test";
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

TEST_F(LegacyImportWarningsTest, FlatFormEmitsWarning) {
    auto p = writeScript("flat_math.ry",
        "from math import sqrt\n"
        "print(sqrt(4.0))\n");

    auto r = runRy({"run", p.string().c_str()});
    EXPECT_EQ(r.exit_code, 0) << "stderr: " << r.err;
    EXPECT_NE(r.err.find("deprecated"), std::string::npos)
        << "stderr should contain 'deprecated': " << r.err;
    EXPECT_NE(r.err.find("ry.math"), std::string::npos)
        << "stderr should suggest 'ry.math': " << r.err;
    EXPECT_NE(r.err.find("'from math import"), std::string::npos)
        << "stderr should quote the legacy form: " << r.err;
}

TEST_F(LegacyImportWarningsTest, StdDottedFormEmitsWarning) {
    auto p = writeScript("std_dotted_math.ry",
        "from std.math import NAN\n"
        "print(NAN)\n");

    auto r = runRy({"run", p.string().c_str()});
    EXPECT_EQ(r.exit_code, 0) << "stderr: " << r.err;
    EXPECT_NE(r.err.find("deprecated"), std::string::npos)
        << "stderr should contain 'deprecated': " << r.err;
    EXPECT_NE(r.err.find("ry.math"), std::string::npos)
        << "stderr should suggest 'ry.math': " << r.err;
    EXPECT_NE(r.err.find("'from std.math import"), std::string::npos)
        << "stderr should quote the legacy form with dot spelling: " << r.err;
}

TEST_F(LegacyImportWarningsTest, FlatStdFormEmitsWarning) {
    // `print` is exported from share/std/builtins.ry; confirm `from std import print`
    // (the flat-std legacy form) warns and suggests `ry.lang`.
    auto p = writeScript("flat_std.ry",
        "from std import print\n"
        "print(\"hi\")\n");

    auto r = runRy({"run", p.string().c_str()});
    EXPECT_EQ(r.exit_code, 0) << "stderr: " << r.err;
    EXPECT_NE(r.err.find("deprecated"), std::string::npos)
        << "stderr should contain 'deprecated': " << r.err;
    EXPECT_NE(r.err.find("ry.lang"), std::string::npos)
        << "stderr should suggest 'ry.lang': " << r.err;
}

TEST_F(LegacyImportWarningsTest, QualifiedFlatFormEmitsWarning) {
    auto p = writeScript("qualified_flat_math.ry",
        "import math\n"
        "print(math.sqrt(9.0))\n");

    auto r = runRy({"run", p.string().c_str()});
    EXPECT_EQ(r.exit_code, 0) << "stderr: " << r.err;
    EXPECT_NE(r.err.find("deprecated"), std::string::npos)
        << "stderr should contain 'deprecated': " << r.err;
    EXPECT_NE(r.err.find("ry.math"), std::string::npos)
        << "stderr should suggest 'ry.math': " << r.err;
    EXPECT_NE(r.err.find("'import math'"), std::string::npos)
        << "stderr should quote the legacy form: " << r.err;
}

TEST_F(LegacyImportWarningsTest, CanonicalFormNoWarning) {
    auto p = writeScript("canonical.ry",
        "from ry.math import sqrt\n"
        "print(sqrt(4.0))\n");

    auto r = runRy({"run", p.string().c_str()});
    EXPECT_EQ(r.exit_code, 0) << "stderr: " << r.err;
    EXPECT_EQ(r.err.find("deprecated"), std::string::npos)
        << "canonical form must not emit deprecation warning: " << r.err;
}

TEST_F(LegacyImportWarningsTest, RyLangExplicitFormNoWarning) {
    // Explicit `from ry.lang import …` is canonical, not legacy.
    auto p = writeScript("ry_lang_explicit.ry",
        "from ry.lang import print\n"
        "print(\"hi\")\n");

    auto r = runRy({"run", p.string().c_str()});
    EXPECT_EQ(r.exit_code, 0) << "stderr: " << r.err;
    EXPECT_EQ(r.err.find("deprecated"), std::string::npos)
        << "ry.lang explicit form must not emit deprecation warning: " << r.err;
}

TEST_F(LegacyImportWarningsTest, FlatFormDeduplicatedAcrossMultipleImports) {
    auto p = writeScript("dedup_math.ry",
        "from math import sqrt\n"
        "from math import PI\n"
        "print(sqrt(PI))\n");

    auto r = runRy({"run", p.string().c_str()});
    EXPECT_EQ(r.exit_code, 0) << "stderr: " << r.err;
    EXPECT_EQ(countOccurrences(r.err, "deprecated"), 1u)
        << "same legacy spelling must warn at most once, stderr was: " << r.err;
}

TEST_F(LegacyImportWarningsTest, UserDefinedModuleNoFalsePositive) {
    // A user-defined `math.ry` shadows the stdlib name. Loader resolves to the
    // local file (rp.from_stdlib == false) — detector must NOT fire.
    std::ofstream(tmpDir_ / "math.ry")
        << "@public\nfn add(a: int, b: int) -> int:\n    return a + b\n";

    auto p = writeScript("uses_local_math.ry",
        "from math import add\n"
        "print(add(2, 3))\n");

    auto r = runRy({"run", p.string().c_str()});
    EXPECT_EQ(r.exit_code, 0) << "stderr: " << r.err;
    EXPECT_EQ(r.out, "5\n");
    EXPECT_EQ(r.err.find("deprecated"), std::string::npos)
        << "user-defined module must not trigger deprecation warning: " << r.err;
}

TEST_F(LegacyImportWarningsTest, AliasedImportEmitsWarning) {
    // Alias (`as flatPI`) does not affect detection — module_path is still "math".
    // Use a constant (`PI`) because stdlib `@native` fn alias is not yet
    // supported (see tests/spec/ry_namespace/ry_module_import.test.ry:18).
    auto p = writeScript("aliased_math.ry",
        "from math import PI as flatPI\n"
        "print(flatPI)\n");

    auto r = runRy({"run", p.string().c_str()});
    EXPECT_EQ(r.exit_code, 0) << "stderr: " << r.err;
    EXPECT_NE(r.err.find("deprecated"), std::string::npos)
        << "aliased legacy import must still warn: " << r.err;
    EXPECT_NE(r.err.find("ry.math"), std::string::npos)
        << "stderr should suggest 'ry.math': " << r.err;
}

TEST_F(LegacyImportWarningsTest, MultipleDistinctLegacyModulesEachWarnOnce) {
    auto p = writeScript("multi_distinct.ry",
        "from math import sqrt\n"
        "from testing import it\n"
        "print(sqrt(4.0))\n");

    auto r = runRy({"run", p.string().c_str()});
    EXPECT_EQ(r.exit_code, 0) << "stderr: " << r.err;
    EXPECT_EQ(countOccurrences(r.err, "deprecated"), 2u)
        << "two distinct legacy modules should each warn once, stderr was: " << r.err;
    EXPECT_NE(r.err.find("ry.math"), std::string::npos)
        << "stderr should mention ry.math: " << r.err;
    EXPECT_NE(r.err.find("ry.testing"), std::string::npos)
        << "stderr should mention ry.testing: " << r.err;
}
