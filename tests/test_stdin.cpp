#include <gtest/gtest.h>
#include <cstdio>
#include <filesystem>
#include <fstream>
#include <string>
#include <array>
#include <utility>
#include <unistd.h>
#include <sys/wait.h>

// Run ry binary with code written to its stdin via pipe (no shell involved).
// Returns {stdout+stderr, exit_code}.
static std::pair<std::string, int> runRyStdin(const std::string &code) {
    int pipeIn[2];   // parent writes code → child stdin
    int pipeOut[2];  // child stdout/stderr → parent reads
    if (pipe(pipeIn) != 0 || pipe(pipeOut) != 0) return {"", -1};

    pid_t pid = fork();
    if (pid < 0) return {"", -1};

    if (pid == 0) {
        // Child: wire pipes and exec ry
        close(pipeIn[1]);
        close(pipeOut[0]);
        dup2(pipeIn[0], STDIN_FILENO);
        dup2(pipeOut[1], STDOUT_FILENO);
        dup2(pipeOut[1], STDERR_FILENO);
        close(pipeIn[0]);
        close(pipeOut[1]);
        setenv("RY_ENV", "internal", 1);
        execl(RY_BINARY_PATH, "ry", nullptr);
        _exit(127);
    }

    // Parent: write code to child stdin, then read output
    close(pipeIn[0]);
    close(pipeOut[1]);

    write(pipeIn[1], code.data(), code.size());
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
    int exit_code;
    if (status == -1) {
        exit_code = -1;
    } else {
        exit_code = WIFEXITED(status) ? WEXITSTATUS(status) : -1;
    }
    return {output, exit_code};
}

TEST(StdinExecution, SingleLineExpression) {
    auto [out, rc] = runRyStdin("print(42)");
    EXPECT_EQ(out, "42\n");
    EXPECT_EQ(rc, 0);
}

TEST(StdinExecution, StringOutput) {
    auto [out, rc] = runRyStdin("print(\"hello\")");
    EXPECT_EQ(out, "hello\n");
    EXPECT_EQ(rc, 0);
}

TEST(StdinExecution, MultilineCode) {
    auto [out, rc] = runRyStdin("a = 1\nb = 2\nprint(a + b)");
    EXPECT_EQ(out, "3\n");
    EXPECT_EQ(rc, 0);
}

TEST(StdinExecution, IndentedCode) {
    auto [out, rc] = runRyStdin(
        "fn add(x: int, y: int) -> int:\n"
        "    return x + y\n"
        "print(add(3, 4))");
    EXPECT_EQ(out, "7\n");
    EXPECT_EQ(rc, 0);
}

TEST(StdinExecution, SyntaxErrorReturnsNonZero) {
    auto [out, rc] = runRyStdin("if if if");
    EXPECT_NE(rc, 0);
}

TEST(StdinExecution, FileExecutionStillWorks) {
    namespace fs = std::filesystem;
    auto tmp = fs::temp_directory_path() / ("ry_test_" + std::to_string(getpid()) + ".ry");
    {
        std::ofstream ofs(tmp);
        ofs << "print(99)";
    }

    // Run via file argument (not stdin)
    int pipeOut[2];
    if (pipe(pipeOut) != 0) { fs::remove(tmp); FAIL(); }
    pid_t pid = fork();
    if (pid < 0) { fs::remove(tmp); FAIL(); }

    if (pid == 0) {
        close(pipeOut[0]);
        dup2(pipeOut[1], STDOUT_FILENO);
        dup2(pipeOut[1], STDERR_FILENO);
        close(pipeOut[1]);
        setenv("RY_ENV", "internal", 1);
        execl(RY_BINARY_PATH, "ry", tmp.c_str(), nullptr);
        _exit(127);
    }

    close(pipeOut[1]);
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

    fs::remove(tmp);

    EXPECT_EQ(output, "99\n");
    EXPECT_EQ(exit_code, 0);
}
