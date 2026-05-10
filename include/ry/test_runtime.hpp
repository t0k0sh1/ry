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
    // Record a List<T> argument by deep-copying the element buffer into a
    // MockListSnapshot. element_kind ∈ {1=int, 2=float, 3=bool, 4=str}.
    // element_size is the per-element stride supplied by codegen via DataLayout.
    void    __ry_mock_store_arg_list(void *record, void *listHeaderPtr,
                                      int64_t element_kind, int64_t element_size,
                                      const char *mockName);
    // Record a Set<T> argument by deep-copying SetHeader.elems[0..len-1] into
    // a MockListSnapshot (kind tag 7). The snapshot shape is shared with List;
    // only the comparison semantics differ (unordered membership in mockArgEqual).
    void    __ry_mock_store_arg_set(void *record, void *setHeaderPtr,
                                     int64_t element_kind, int64_t element_size,
                                     const char *mockName);
    // Record a Map<K, V> argument by deep-copying MapHeader.{keys,vals}[0..len-1]
    // into a MockMapSnapshot (kind tag 8). key_kind / val_kind ∈ {1=int, 2=float,
    // 3=bool, 4=str}. Comparison is unordered key→value (mockArgEqual kind 8).
    void    __ry_mock_store_arg_map(void *record, void *mapHeaderPtr,
                                     int64_t key_kind, int64_t key_size,
                                     int64_t val_kind, int64_t val_size,
                                     const char *mockName);
    // Build an expected-value snapshot for verifyCalledWith. The returned
    // pointer's ownership is transferred to __ry_mock_count_matching_calls,
    // which frees it after comparison.
    void   *__ry_mock_make_list_snapshot(void *listHeaderPtr, int64_t element_kind,
                                          int64_t element_size);
    void   *__ry_mock_make_set_snapshot(void *setHeaderPtr, int64_t element_kind,
                                         int64_t element_size);
    void   *__ry_mock_make_map_snapshot(void *mapHeaderPtr,
                                         int64_t key_kind, int64_t key_size,
                                         int64_t val_kind, int64_t val_size);
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
