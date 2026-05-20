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
    // Parent record's descriptor, or nullptr if this record has no parent.
    // Walked at unwrap time to support `let a: Parent = anyHoldingChild`
    // (#1802 cross-type subtype unwrap from `any`).
    const RyRecordDescriptor *parent_desc;
};

static_assert(sizeof(RyRecordDescriptor) == 32,
              "RyRecordDescriptor must be 4 pointers (32 bytes) — codegen emits "
              "a global with this layout per record type");

using RyEnumBoxDtor = void (*)(void *dataPtr);
using RyEnumBoxEq = int64_t (*)(const void *dataA, const void *dataB);

struct RyEnumDescriptor {
    RyEnumBoxDtor dtor;
    RyEnumBoxEq eq;
    const char *type_name;
};

static_assert(sizeof(RyEnumDescriptor) == 24,
              "RyEnumDescriptor must be 3 pointers (24 bytes) — codegen emits "
              "a global with this layout per enum type. Enums have no subtype "
              "relation so no parent_desc field is needed (cf. RyRecordDescriptor).");

#ifdef __cplusplus
extern "C" {
#endif

void __ry_arc_dtor_record_dispatch(void *dataPtr);
void __ry_arc_dtor_enum_dispatch(void *dataPtr);

// Walk the `parent_desc` chain of `actual` looking for `expected`. Returns 1
// when `expected` is `actual` itself or any ancestor in the chain, 0 otherwise.
// Used by `unwrapFromAny` to admit `let a: Parent = anyHoldingChild` while still
// trapping on unrelated record types (#1802).
int64_t __ry_record_is_subtype_desc(const RyRecordDescriptor *actual,
                                    const RyRecordDescriptor *expected);

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

// C++-only constructors used by runtime modules that synthesize `any` values
// from C (e.g. JSON parser building Map<str, any> / List<any> from text).
// Callers are responsible for the ARC initial refcount: the pre-existing
// refcount of the passed handle (1 for fresh `makeString` / `arc_alloc`
// results) flows into the resulting RyAny without an extra retain. Pair
// with `emitAnyReleaseVar` on the codegen side which decrements that
// refcount when the `any` slot leaves scope.
RyAny anyFromUnit();
RyAny anyFromInt(int64_t v);
RyAny anyFromFloat(double v);
RyAny anyFromBool(int64_t v);
RyAny anyFromStr(const char *handle);
RyAny anyFromListOfAny(void *list_header);
RyAny anyFromMapStrAny(void *map_header);

} // namespace ry
