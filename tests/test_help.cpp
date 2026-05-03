#include <gtest/gtest.h>
#include <array>
#include <string>
#include <utility>
#include <unistd.h>
#include <sys/wait.h>


// Run ry binary with given arguments, capturing stdout and stderr separately.
// Returns {stdout, stderr, exit_code}.
struct RunResult {
    std::string out;
    std::string err;
    int exit_code;
};

static RunResult runRy(std::initializer_list<const char *> args) {
    int pipeOut[2];  // child stdout → parent
    int pipeErr[2];  // child stderr → parent
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
        // Close stdin so ry doesn't wait for pipe input
        close(STDIN_FILENO);
        setenv("RY_ENV", "internal", 1);

        // Build argv: "ry" + args + nullptr
        std::vector<const char *> argv;
        argv.push_back("ry");
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

// --- Main help ---

TEST(HelpOption, MainHelpWithDashH) {
    auto r = runRy({"-h"});
    EXPECT_EQ(r.exit_code, 0);
    EXPECT_NE(r.out.find("Usage:"), std::string::npos);
    EXPECT_NE(r.out.find("ry <file.ry>"), std::string::npos);
    EXPECT_NE(r.out.find("ry test"), std::string::npos);
    EXPECT_NE(r.out.find("ry init"), std::string::npos);
    EXPECT_NE(r.out.find("ry new"), std::string::npos);
    EXPECT_NE(r.out.find("ry fmt"), std::string::npos);
    EXPECT_NE(r.out.find("ry self-update"), std::string::npos);
}

TEST(HelpOption, MainHelpWithDoubleDashHelp) {
    auto r = runRy({"--help"});
    EXPECT_EQ(r.exit_code, 0);
    EXPECT_NE(r.out.find("Usage:"), std::string::npos);
}

TEST(HelpOption, MainHelpWithEnvFlag) {
    auto r = runRy({"--env=internal", "-h"});
    EXPECT_EQ(r.exit_code, 0);
    EXPECT_NE(r.out.find("Usage:"), std::string::npos);
}

// --- Subcommand help ---

TEST(HelpOption, TestSubcommandHelp) {
    auto r = runRy({"test", "--help"});
    EXPECT_EQ(r.exit_code, 0);
    EXPECT_NE(r.out.find("ry test"), std::string::npos);
    EXPECT_NE(r.out.find("--parallel"), std::string::npos);
    EXPECT_NE(r.out.find("--watch"), std::string::npos);
    EXPECT_NE(r.out.find("--coverage"), std::string::npos);
}

TEST(HelpOption, TestSubcommandHelpPriority) {
    // -h/--help should take priority over other options
    auto r = runRy({"test", "-p", "--help"});
    EXPECT_EQ(r.exit_code, 0);
    EXPECT_NE(r.out.find("ry test"), std::string::npos);
}

TEST(HelpOption, FmtSubcommandHelp) {
    auto r = runRy({"fmt", "--help"});
    EXPECT_EQ(r.exit_code, 0);
    EXPECT_NE(r.out.find("ry fmt"), std::string::npos);
    EXPECT_NE(r.out.find("--check"), std::string::npos);
}

TEST(HelpOption, InitSubcommandHelp) {
    auto r = runRy({"init", "--help"});
    EXPECT_EQ(r.exit_code, 0);
    EXPECT_NE(r.out.find("ry init"), std::string::npos);
}

TEST(HelpOption, NewSubcommandHelp) {
    auto r = runRy({"new", "--help"});
    EXPECT_EQ(r.exit_code, 0);
    EXPECT_NE(r.out.find("ry new"), std::string::npos);
}

TEST(HelpOption, SelfUpdateSubcommandHelp) {
    auto r = runRy({"self-update", "--help"});
    EXPECT_EQ(r.exit_code, 0);
    EXPECT_NE(r.out.find("ry self-update"), std::string::npos);
}

TEST(HelpOption, SelfUpdateNightlyRejected) {
    auto r = runRy({"self-update", "--nightly"});
    EXPECT_NE(r.exit_code, 0);
    EXPECT_NE(r.err.find("--nightly is no longer supported"), std::string::npos);
    EXPECT_NE(r.err.find("v0.0.14"), std::string::npos);
}

// --- Short -h flag for subcommands ---

TEST(HelpOption, TestSubcommandShortHelp) {
    auto r = runRy({"test", "-h"});
    EXPECT_EQ(r.exit_code, 0);
    EXPECT_NE(r.out.find("ry test"), std::string::npos);
}

TEST(HelpOption, FmtSubcommandShortHelp) {
    auto r = runRy({"fmt", "-h"});
    EXPECT_EQ(r.exit_code, 0);
    EXPECT_NE(r.out.find("ry fmt"), std::string::npos);
}

// --- Unknown subcommand ---

TEST(HelpOption, UnknownSubcommandShowsError) {
    auto r = runRy({"nonexistent"});
    EXPECT_NE(r.exit_code, 0);
    EXPECT_NE(r.err.find("unknown command"), std::string::npos);
    // Main help should also be displayed (on stdout)
    EXPECT_NE(r.out.find("Usage:"), std::string::npos);
}

// --- Stdin pipe with -c flag still works (regression) ---

TEST(HelpOption, StdinPipeStillWorks) {
    int pipeIn[2], pipeOut[2];
    if (pipe(pipeIn) != 0 || pipe(pipeOut) != 0) FAIL();

    pid_t pid = fork();
    if (pid < 0) FAIL();

    if (pid == 0) {
        close(pipeIn[1]);
        close(pipeOut[0]);
        dup2(pipeIn[0], STDIN_FILENO);
        dup2(pipeOut[1], STDOUT_FILENO);
        dup2(pipeOut[1], STDERR_FILENO);
        close(pipeIn[0]);
        close(pipeOut[1]);
        setenv("RY_ENV", "internal", 1);
        execl(RY_BINARY_PATH, "ry", "-c", nullptr);
        _exit(127);
    }

    close(pipeIn[0]);
    close(pipeOut[1]);

    const char *code = "print(\"hello\")";
    write(pipeIn[1], code, strlen(code));
    close(pipeIn[1]);

    std::string output;
    std::array<char, 4096> buf;
    ssize_t n;
    while ((n = read(pipeOut[0], buf.data(), buf.size())) > 0)
        output.append(buf.data(), static_cast<size_t>(n));
    close(pipeOut[0]);

    int status;
    waitpid(pid, &status, 0);
    int exit_code = WIFEXITED(status) ? WEXITSTATUS(status) : -1;

    EXPECT_EQ(output, "hello\n");
    EXPECT_EQ(exit_code, 0);
}
