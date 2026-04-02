#include <gtest/gtest.h>
#include "ry/runtime_print.hpp"
#include <cstdlib>
#include <cstring>
#include <string>
#include <thread>
#include <vector>
#include <mutex>

// ============================================================
//  runtime_print unit tests
//
//  Tests the __ry_print_begin / __ry_print_printf / __ry_print_end
//  buffered-print API and the __ry_sprint_begin / __ry_sprint_printf /
//  __ry_sprint_end string-capture API introduced in this PR.
// ============================================================

// ===== Sprint (string capture) tests =====
// These functions are pure buffer operations with no I/O side effects,
// making them suitable for direct unit testing.

TEST(RuntimePrintTest, SprintBasic) {
    __ry_sprint_begin();
    __ry_sprint_printf("%s", "hello");
    char *result = __ry_sprint_end();
    ASSERT_NE(result, nullptr);
    EXPECT_STREQ(result, "hello");
    std::free(result);
}

TEST(RuntimePrintTest, SprintEmpty) {
    __ry_sprint_begin();
    char *result = __ry_sprint_end();
    ASSERT_NE(result, nullptr);
    EXPECT_STREQ(result, "");
    std::free(result);
}

TEST(RuntimePrintTest, SprintMultipleWrites) {
    __ry_sprint_begin();
    __ry_sprint_printf("%s", "foo");
    __ry_sprint_printf("%s", "bar");
    __ry_sprint_printf("%d", 42);
    char *result = __ry_sprint_end();
    ASSERT_NE(result, nullptr);
    EXPECT_STREQ(result, "foobar42");
    std::free(result);
}

TEST(RuntimePrintTest, SprintInteger) {
    __ry_sprint_begin();
    __ry_sprint_printf("%d", 12345);
    char *result = __ry_sprint_end();
    ASSERT_NE(result, nullptr);
    EXPECT_STREQ(result, "12345");
    std::free(result);
}

TEST(RuntimePrintTest, SprintFloat) {
    __ry_sprint_begin();
    __ry_sprint_printf("%g", 3.14);
    char *result = __ry_sprint_end();
    ASSERT_NE(result, nullptr);
    EXPECT_STREQ(result, "3.14");
    std::free(result);
}

TEST(RuntimePrintTest, SprintNested) {
    // Outer sprint captures the full string; inner sprint captures only the inner part.
    __ry_sprint_begin();
    __ry_sprint_printf("%s", "outer-");

    // Inner sprint
    __ry_sprint_begin();
    __ry_sprint_printf("%s", "inner");
    char *inner = __ry_sprint_end();
    ASSERT_NE(inner, nullptr);
    EXPECT_STREQ(inner, "inner");

    // Outer continues after inner
    __ry_sprint_printf("%s", inner);
    __ry_sprint_printf("%s", "-end");
    std::free(inner);

    char *outer = __ry_sprint_end();
    ASSERT_NE(outer, nullptr);
    EXPECT_STREQ(outer, "outer-inner-end");
    std::free(outer);
}

TEST(RuntimePrintTest, SprintNestedTwoLevels) {
    // Three levels of nesting: verify offset bookkeeping is correct.
    __ry_sprint_begin();
    __ry_sprint_printf("%s", "L1");

    __ry_sprint_begin();
    __ry_sprint_printf("%s", "L2");

    __ry_sprint_begin();
    __ry_sprint_printf("%s", "L3");
    char *r3 = __ry_sprint_end();
    ASSERT_NE(r3, nullptr);
    EXPECT_STREQ(r3, "L3");
    std::free(r3);

    char *r2 = __ry_sprint_end();
    ASSERT_NE(r2, nullptr);
    EXPECT_STREQ(r2, "L2");
    std::free(r2);

    char *r1 = __ry_sprint_end();
    ASSERT_NE(r1, nullptr);
    EXPECT_STREQ(r1, "L1");
    std::free(r1);
}

TEST(RuntimePrintTest, SprintLargeString) {
    // Verify the buffer grows correctly when output exceeds the initial capacity.
    __ry_sprint_begin();
    std::string expected;
    for (int i = 0; i < 2000; ++i) {
        __ry_sprint_printf("%c", 'a' + (i % 26));
        expected += static_cast<char>('a' + (i % 26));
    }
    char *result = __ry_sprint_end();
    ASSERT_NE(result, nullptr);
    EXPECT_EQ(std::string(result), expected);
    std::free(result);
}

TEST(RuntimePrintTest, SprintNullTerminated) {
    // The returned string must be null-terminated.
    __ry_sprint_begin();
    __ry_sprint_printf("%s", "test");
    char *result = __ry_sprint_end();
    ASSERT_NE(result, nullptr);
    EXPECT_EQ(result[4], '\0');
    std::free(result);
}

TEST(RuntimePrintTest, SprintSpecialCharacters) {
    __ry_sprint_begin();
    __ry_sprint_printf("%s", "line1\nline2\ttab");
    char *result = __ry_sprint_end();
    ASSERT_NE(result, nullptr);
    EXPECT_STREQ(result, "line1\nline2\ttab");
    std::free(result);
}

TEST(RuntimePrintTest, SprintConsecutiveCalls) {
    // Two consecutive independent sprint calls should each return isolated results.
    __ry_sprint_begin();
    __ry_sprint_printf("%s", "first");
    char *r1 = __ry_sprint_end();
    ASSERT_NE(r1, nullptr);
    EXPECT_STREQ(r1, "first");

    __ry_sprint_begin();
    __ry_sprint_printf("%s", "second");
    char *r2 = __ry_sprint_end();
    ASSERT_NE(r2, nullptr);
    EXPECT_STREQ(r2, "second");

    std::free(r1);
    std::free(r2);
}

// ===== Print buffer tests =====
// These test the stdout-buffering mechanism.  We capture stdout via
// testing::internal::CaptureStdout / GetCapturedStdout.

TEST(RuntimePrintTest, PrintBeginEndFlushesAtomically) {
    __ry_print_begin();
    __ry_print_printf("%s", "hello from buffer");
    testing::internal::CaptureStdout();
    __ry_print_end();
    std::string out = testing::internal::GetCapturedStdout();
    EXPECT_EQ(out, "hello from buffer");
}

TEST(RuntimePrintTest, PrintBeginEndMultipleWrites) {
    __ry_print_begin();
    __ry_print_printf("%s", "part1 ");
    __ry_print_printf("%s", "part2 ");
    __ry_print_printf("%d", 99);
    testing::internal::CaptureStdout();
    __ry_print_end();
    std::string out = testing::internal::GetCapturedStdout();
    EXPECT_EQ(out, "part1 part2 99");
}

TEST(RuntimePrintTest, PrintEndWithNothingBufferedNoOutput) {
    __ry_print_begin();
    // No printf calls — end should flush nothing.
    testing::internal::CaptureStdout();
    __ry_print_end();
    std::string out = testing::internal::GetCapturedStdout();
    EXPECT_EQ(out, "");
}

TEST(RuntimePrintTest, PrintBeginEndNewline) {
    __ry_print_begin();
    __ry_print_printf("%s", "line\n");
    testing::internal::CaptureStdout();
    __ry_print_end();
    std::string out = testing::internal::GetCapturedStdout();
    EXPECT_EQ(out, "line\n");
}

// ===== Thread-safety regression test =====
// Verify that concurrent print calls do not interleave within a single
// logical print.  Each thread writes a complete line via begin/printf/end;
// each output line should be complete and not interleaved.

TEST(RuntimePrintTest, PrintThreadSafetyNoInterleaving) {
    const int kThreads = 4;
    const int kIter = 10;

    testing::internal::CaptureStdout();

    std::vector<std::thread> threads;
    threads.reserve(kThreads);
    for (int t = 0; t < kThreads; ++t) {
        threads.emplace_back([t]() {
            for (int i = 0; i < kIter; ++i) {
                __ry_print_begin();
                __ry_print_printf("thread-%d line-%d\n", t, i);
                __ry_print_end();
            }
        });
    }
    for (auto &th : threads) th.join();

    std::string out = testing::internal::GetCapturedStdout();

    // Count lines: each "thread-X line-Y\n" should appear exactly once.
    int found = 0;
    for (int t = 0; t < kThreads; ++t) {
        for (int i = 0; i < kIter; ++i) {
            std::string expected = "thread-" + std::to_string(t) +
                                   " line-" + std::to_string(i) + "\n";
            auto pos = out.find(expected);
            if (pos != std::string::npos) ++found;
        }
    }
    EXPECT_EQ(found, kThreads * kIter);

    // Each complete line should appear as an unbroken substring.
    // Verify total line count equals expected output.
    int newlines = 0;
    for (char c : out) if (c == '\n') ++newlines;
    EXPECT_EQ(newlines, kThreads * kIter);
}