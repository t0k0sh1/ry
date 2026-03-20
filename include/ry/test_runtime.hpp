#pragma once

#include <cstdint>

extern "C" {
    void __ry_test_describe_begin(const char *name);
    void __ry_test_describe_end();
    void __ry_test_it_begin(const char *name);
    void __ry_test_it_end();
    void __ry_test_expect_fail(int line, const char *actual, const char *expected);
    int  __ry_test_summary();

    // Mock support
    void    __ry_mock_set(const char *name, void *fn_ptr);
    void   *__ry_mock_get(const char *name);
    int64_t __ry_mock_get_call_count(const char *name);
    void    __ry_mock_increment_call(const char *name);
    void    __ry_mock_clear_all();
}
