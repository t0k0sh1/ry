#include "ry/runtime_print.hpp"
#include "ry/runtime_string.hpp"
#include <gtest/gtest.h>
#include <cstdlib>

using namespace ry;

// Death tests use "threadsafe" style to avoid fork issues with LLVM state
class RuntimePrintDeathTest : public ::testing::Test {
protected:
    static void SetUpTestSuite() {
        GTEST_FLAG_SET(death_test_style, "threadsafe");
    }
};

TEST_F(RuntimePrintDeathTest, SprintDepthOverflowAborts) {
    EXPECT_DEATH(
        {
            for (int i = 0; i <= 64; ++i)
                __ry_sprint_begin();
        },
        "sprint nesting depth exceeded");
}

TEST(RuntimePrintTest, SprintNestingWorks) {
    __ry_sprint_begin();
    __ry_sprint_printf("outer");
    __ry_sprint_begin();
    __ry_sprint_printf("inner");
    char *inner = __ry_sprint_end();
    __ry_sprint_printf(" + done");
    char *outer = __ry_sprint_end();
    EXPECT_STREQ(inner, "inner");
    EXPECT_STREQ(outer, "outer + done");
    freeStringSlot(inner);
    freeStringSlot(outer);
}
