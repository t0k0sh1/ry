#include "ry/test_runtime.hpp"
#include <cstdio>
#include <string>

static int g_passed = 0;
static int g_failed = 0;
static std::string g_current_describe;
static std::string g_current_it;
static bool g_current_it_failed = false;

extern "C" {

void __ry_test_describe_begin(const char *name) {
    g_current_describe = name;
    std::printf("%s\n", name);
}

void __ry_test_describe_end() {
    g_current_describe.clear();
}

void __ry_test_it_begin(const char *name) {
    g_current_it = name;
    g_current_it_failed = false;
}

void __ry_test_it_end() {
    if (g_current_it_failed) {
        std::printf("  \033[31m- %s\033[0m\n", g_current_it.c_str());
        ++g_failed;
    } else {
        std::printf("  \033[32m+ %s\033[0m\n", g_current_it.c_str());
        ++g_passed;
    }
    g_current_it.clear();
}

void __ry_test_expect_fail(int line, const char *actual, const char *expected) {
    g_current_it_failed = true;
    std::printf("    \033[31mline %d: expected %s, got %s\033[0m\n", line, expected, actual);
}

int __ry_test_summary() {
    std::printf("\n%d passed, %d failed\n", g_passed, g_failed);
    int result = g_failed;
    // Reset for potential multiple invocations
    g_passed = 0;
    g_failed = 0;
    return result;
}

} // extern "C"
