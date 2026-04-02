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
//  Tests __ry_print_begin/printf/end (atomic stdout buffering)
//  and __ry_sprint_begin/printf/end (string capture).
// ============================================================

// ===== __ry_sprint_* tests =====
// These don't touch stdout so they don't need stdout capture.

TEST(SprintTest, EmptyCapture) {
    __ry_sprint_begin();
    char *s = __ry_sprint_end();
    ASSERT_NE(s, nullptr);
    EXPECT_STREQ(s, "");
    std::free(s);
}

TEST(SprintTest, SimpleString) {
    __ry_sprint_begin();
    __ry_sprint_printf("hello");
    char *s = __ry_sprint_end();
    ASSERT_NE(s, nullptr);
    EXPECT_STREQ(s, "hello");
    std::free(s);
}

TEST(SprintTest, FormattedOutput) {
    __ry_sprint_begin();
    __ry_sprint_printf("%d + %d = %d", 1, 2, 3);
    char *s = __ry_sprint_end();
    ASSERT_NE(s, nullptr);
    EXPECT_STREQ(s, "1 + 2 = 3");
    std::free(s);
}

TEST(SprintTest, MultipleAppends) {
    __ry_sprint_begin();
    __ry_sprint_printf("foo");
    __ry_sprint_printf("bar");
    __ry_sprint_printf("baz");
    char *s = __ry_sprint_end();
    ASSERT_NE(s, nullptr);
    EXPECT_STREQ(s, "foobarbaz");
    std::free(s);
}

TEST(SprintTest, NestedCapture) {
    // Outer capture starts
    __ry_sprint_begin();
    __ry_sprint_printf("outer_before_");

    // Inner (nested) capture
    __ry_sprint_begin();
    __ry_sprint_printf("inner");
    char *inner = __ry_sprint_end();
    ASSERT_NE(inner, nullptr);
    EXPECT_STREQ(inner, "inner");
    std::free(inner);

    // Outer capture should contain only what was written before and after inner,
    // but NOT the inner content (it was popped off).
    __ry_sprint_printf("_outer_after");
    char *outer = __ry_sprint_end();
    ASSERT_NE(outer, nullptr);
    EXPECT_STREQ(outer, "outer_before__outer_after");
    std::free(outer);
}

TEST(SprintTest, LargeOutput) {
    // Force buffer reallocation by writing more than the initial capacity (256).
    __ry_sprint_begin();
    std::string big(300, 'x');
    __ry_sprint_printf("%s", big.c_str());
    char *s = __ry_sprint_end();
    ASSERT_NE(s, nullptr);
    EXPECT_EQ(std::string(s), big);
    std::free(s);
}

TEST(SprintTest, TwoIndependentCaptures) {
    // First capture
    __ry_sprint_begin();
    __ry_sprint_printf("first");
    char *s1 = __ry_sprint_end();
    ASSERT_NE(s1, nullptr);
    EXPECT_STREQ(s1, "first");
    std::free(s1);

    // Second capture — must be independent of the first.
    __ry_sprint_begin();
    __ry_sprint_printf("second");
    char *s2 = __ry_sprint_end();
    ASSERT_NE(s2, nullptr);
    EXPECT_STREQ(s2, "second");
    std::free(s2);
}

TEST(SprintTest, PrintfReturnValue) {
    __ry_sprint_begin();
    int n = __ry_sprint_printf("abc");
    EXPECT_EQ(n, 3);
    char *s = __ry_sprint_end();
    std::free(s);
}

TEST(SprintTest, NullTermination) {
    __ry_sprint_begin();
    __ry_sprint_printf("hi");
    char *s = __ry_sprint_end();
    ASSERT_NE(s, nullptr);
    // Must be null-terminated.
    EXPECT_EQ(s[2], '\0');
    std::free(s);
}

// ===== __ry_print_* tests =====
// These emit to stdout; use GTest's stdout capture.

TEST(PrintBufferTest, BeginEndFlushesToStdout) {
    testing::internal::CaptureStdout();
    __ry_print_begin();
    __ry_print_printf("hello world");
    __ry_print_end();
    std::string out = testing::internal::GetCapturedStdout();
    EXPECT_EQ(out, "hello world");
}

TEST(PrintBufferTest, MultipleWritesFlushedAtomically) {
    testing::internal::CaptureStdout();
    __ry_print_begin();
    __ry_print_printf("line ");
    __ry_print_printf("%d", 42);
    __ry_print_printf("\n");
    __ry_print_end();
    std::string out = testing::internal::GetCapturedStdout();
    EXPECT_EQ(out, "line 42\n");
}

TEST(PrintBufferTest, EmptyBufferNotWritten) {
    // Begin then end immediately — should not write anything.
    testing::internal::CaptureStdout();
    __ry_print_begin();
    __ry_print_end();
    std::string out = testing::internal::GetCapturedStdout();
    EXPECT_EQ(out, "");
}

TEST(PrintBufferTest, LargeOutput) {
    // Force buffer reallocation inside the print buffer.
    std::string big(500, 'A');
    testing::internal::CaptureStdout();
    __ry_print_begin();
    __ry_print_printf("%s", big.c_str());
    __ry_print_end();
    std::string out = testing::internal::GetCapturedStdout();
    EXPECT_EQ(out, big);
}

TEST(PrintBufferTest, PrintfReturnValueInsideBuffer) {
    __ry_print_begin();
    int n = __ry_print_printf("xyz");
    EXPECT_EQ(n, 3);
    // Discard captured output.
    testing::internal::CaptureStdout();
    __ry_print_end();
    testing::internal::GetCapturedStdout();
}

TEST(PrintBufferTest, ConsecutiveBeginEndCycles) {
    testing::internal::CaptureStdout();
    __ry_print_begin();
    __ry_print_printf("first\n");
    __ry_print_end();

    __ry_print_begin();
    __ry_print_printf("second\n");
    __ry_print_end();
    std::string out = testing::internal::GetCapturedStdout();
    EXPECT_EQ(out, "first\nsecond\n");
}

// Regression: multiple threads should each have independent print buffers.
// (All writes are serialised via the stdout mutex, but buffering is per-thread.)
TEST(PrintBufferTest, MultithreadedCapture) {
    // Each thread uses sprint to produce a string (no stdout involved here,
    // so this is a simple thread-safety check for the sprint buffer).
    constexpr int kThreads = 8;
    std::vector<std::string> results(kThreads);
    std::vector<std::thread> threads;

    for (int i = 0; i < kThreads; ++i) {
        threads.emplace_back([i, &results]() {
            __ry_sprint_begin();
            __ry_sprint_printf("thread_%d", i);
            char *s = __ry_sprint_end();
            results[i] = s;
            std::free(s);
        });
    }
    for (auto &t : threads)
        t.join();

    for (int i = 0; i < kThreads; ++i) {
        EXPECT_EQ(results[i], "thread_" + std::to_string(i));
    }
}

// Regression: sprint nesting up to a reasonable depth should not corrupt output.
TEST(SprintTest, DeepNesting) {
    constexpr int kDepth = 5;
    for (int i = 0; i < kDepth; ++i)
        __ry_sprint_begin();

    // Innermost content
    __ry_sprint_printf("deep");

    // Unwind the stack
    for (int i = kDepth - 1; i >= 0; --i) {
        char *s = __ry_sprint_end();
        if (i == kDepth - 1) {
            ASSERT_NE(s, nullptr);
            EXPECT_STREQ(s, "deep");
        }
        std::free(s);
    }
}