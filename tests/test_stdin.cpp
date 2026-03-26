#include <gtest/gtest.h>
#include <cstdio>
#include <string>
#include <array>
#include <sys/wait.h>

static std::pair<std::string, int> runCmd(const std::string &cmd) {
    std::array<char, 4096> buf;
    std::string output;
    FILE *fp = popen(cmd.c_str(), "r");
    if (!fp) return {"", -1};
    while (fgets(buf.data(), buf.size(), fp)) {
        output += buf.data();
    }
    int status = pclose(fp);
    int exit_code = WIFEXITED(status) ? WEXITSTATUS(status) : -1;
    return {output, exit_code};
}

static std::pair<std::string, int> runStdin(const std::string &input) {
    return runCmd("echo " + input + " | RY_ENV=internal ./build/ry 2>&1");
}

static std::pair<std::string, int> runStdinMultiline(const std::string &code) {
    return runCmd("printf '%s' '" + code + "' | RY_ENV=internal ./build/ry 2>&1");
}

TEST(StdinExecution, SingleLineExpression) {
    auto [out, rc] = runStdin("'print(42)'");
    EXPECT_EQ(out, "42\n");
    EXPECT_EQ(rc, 0);
}

TEST(StdinExecution, StringOutput) {
    auto [out, rc] = runStdin("'print(\"hello\")'");
    EXPECT_EQ(out, "hello\n");
    EXPECT_EQ(rc, 0);
}

TEST(StdinExecution, MultilineCode) {
    auto [out, rc] = runStdinMultiline("a = 1\nb = 2\nprint(a + b)");
    EXPECT_EQ(out, "3\n");
    EXPECT_EQ(rc, 0);
}

TEST(StdinExecution, IndentedCode) {
    auto [out, rc] = runStdinMultiline(
        "fn add(x: int, y: int) -> int:\n"
        "    return x + y\n"
        "print(add(3, 4))");
    EXPECT_EQ(out, "7\n");
    EXPECT_EQ(rc, 0);
}

TEST(StdinExecution, SyntaxErrorReturnsNonZero) {
    auto [out, rc] = runStdin("'if if if'");
    EXPECT_NE(rc, 0);
}

TEST(StdinExecution, FileExecutionStillWorks) {
    auto [out, rc] = runCmd(
        "echo 'print(99)' > /tmp/ry_stdin_test.ry && "
        "RY_ENV=internal ./build/ry /tmp/ry_stdin_test.ry 2>&1");
    EXPECT_EQ(out, "99\n");
    EXPECT_EQ(rc, 0);
}
