#pragma once

#include "ry/ry_layout.hpp"


namespace ry {

struct RyAny {
    int64_t tag;
    alignas(8) char data[8];
};

using RyRecordBoxDtor = void (*)(void *dataPtr);
using RyRecordBoxEq = int64_t (*)(const void *dataA, const void *dataB);

struct RyRecordDescriptor {
    RyRecordBoxDtor dtor;
    RyRecordBoxEq eq;
    const char *type_name;
};

static_assert(sizeof(RyRecordDescriptor) == 24,
              "RyRecordDescriptor must be 3 pointers (24 bytes) — codegen emits "
              "a global with this layout per record type");

#ifdef __cplusplus
extern "C" {
#endif

void __ry_arc_dtor_record_dispatch(void *dataPtr);

void __ry_any_type_error(const char *op, int64_t tag_a, int64_t tag_b);

void __ry_any_add(RyAny *result, const RyAny *a, const RyAny *b);
void __ry_any_sub(RyAny *result, const RyAny *a, const RyAny *b);
void __ry_any_mul(RyAny *result, const RyAny *a, const RyAny *b);
void __ry_any_div(RyAny *result, const RyAny *a, const RyAny *b);
void __ry_any_mod(RyAny *result, const RyAny *a, const RyAny *b);
void __ry_any_floordiv(RyAny *result, const RyAny *a, const RyAny *b);
void __ry_any_pow(RyAny *result, const RyAny *a, const RyAny *b);
void __ry_any_neg(RyAny *result, const RyAny *a);

const char *__ry_any_to_string(const RyAny *a);
const char *__ry_any_to_string_in_collection(const RyAny *a);

int64_t __ry_any_eq(const RyAny *a, const RyAny *b);
int64_t __ry_any_ne(const RyAny *a, const RyAny *b);
int64_t __ry_any_lt(const RyAny *a, const RyAny *b);
int64_t __ry_any_le(const RyAny *a, const RyAny *b);
int64_t __ry_any_gt(const RyAny *a, const RyAny *b);
int64_t __ry_any_ge(const RyAny *a, const RyAny *b);

#ifdef __cplusplus
}
#endif

} // namespace ry
