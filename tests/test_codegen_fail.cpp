#include "test_codegen_common.hpp"

// ============================================================
// fail(msg) marks current test as failed with message
// ============================================================

TEST_F(CodeGenTest, FailWithMessage) {
    EXPECT_EQ(runTestSource(
        "describe(\"fail helper\", function():\n"
        "    it(\"marks test as failed\", function():\n"
        "        fail(\"intentional failure\")\n"
        "    )\n"
        ")\n"
    ), "fail helper\n    \033[31mline 3: intentional failure\033[0m\n  \033[31m- marks test as failed\033[0m\n\n0 passed, 1 failed\n");
}

// ============================================================
// fail() with no argument uses generic message
// ============================================================

TEST_F(CodeGenTest, FailWithoutMessage) {
    EXPECT_EQ(runTestSource(
        "describe(\"fail bare\", function():\n"
        "    it(\"fails generically\", function():\n"
        "        fail()\n"
        "    )\n"
        ")\n"
    ), "fail bare\n    \033[31mline 3: test failed\033[0m\n  \033[31m- fails generically\033[0m\n\n0 passed, 1 failed\n");
}

// ============================================================
// fail() outside test mode is rejected at compile time
// ============================================================

TEST_F(CodeGenTest, FailOutsideTestModeIsRejected) {
    EXPECT_THROW(runSource(
        "fail(\"should not compile\")\n"
    ), std::runtime_error);
}
