#pragma once

extern "C" {
    void __ry_test_describe_begin(const char *name);
    void __ry_test_describe_end();
    void __ry_test_it_begin(const char *name);
    void __ry_test_it_end();
    void __ry_test_expect_fail(int line, const char *actual, const char *expected);
    int  __ry_test_summary();
}
