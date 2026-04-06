#pragma once

#include <cstdint>


namespace ry {

extern "C" {
    void __ry_test_describe_begin(const char *name);
    void __ry_test_describe_end();
    void __ry_test_it_begin(const char *name);
    void __ry_test_it_end();
    void __ry_test_expect_fail(int line, const char *actual, const char *expected);
    void __ry_test_fail(int line, const char *msg);
    int  __ry_test_summary();

    // Mock support
    void    __ry_mock_set(const char *name, void *fn_ptr);
    void   *__ry_mock_get(const char *name);
    int64_t __ry_mock_get_call_count(const char *name);
    void    __ry_mock_increment_call(const char *name);
    void    __ry_mock_clear_all();

    // Property-based test support
    void        __ry_test_prop_init_rng();
    int64_t     __ry_test_rand_int();
    double      __ry_test_rand_float();
    int64_t     __ry_test_rand_bool();
    const char *__ry_test_rand_str();
    int64_t     __ry_test_it_is_failed();
}

} // namespace ry
