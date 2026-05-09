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
    void    __ry_mock_set_closure(const char *name, void *thunk_ptr,
                                   void *env_ptr, void (*env_dtor)(void *));
    void   *__ry_mock_get(const char *name);
    void   *__ry_mock_get_env(const char *name);
    int64_t __ry_mock_get_call_count(const char *name);
    void    __ry_mock_increment_call(const char *name);
    void   *__ry_mock_begin_call_record(const char *name);
    void    __ry_mock_store_arg(void *record, int64_t kind, int64_t value,
                                 const char *mockName);
    int64_t __ry_mock_count_matching_calls(const char *name, int64_t numArgs,
                                            const int64_t *kinds,
                                            const int64_t *values);
    void    __ry_mock_clear_all();

    // Indent helpers
    const char *__ry_test_indent(int extra);

    // Property-based test support
    void        __ry_test_prop_init_rng();
    int64_t     __ry_test_rand_int();
    double      __ry_test_rand_float();
    int64_t     __ry_test_rand_bool();
    const char *__ry_test_rand_str();
    int64_t     __ry_test_it_is_failed();
}

} // namespace ry
