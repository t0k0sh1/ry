#pragma once

#include <cstdint>

#include "ry/runtime/core/any.hpp"


namespace ry {

extern "C" {

// Parses JSON5 text into a RyAny value. Caller owns the resulting payloads
// (str / List<any> / Map<str, any>) — codegen's `emitAnyReleaseVar`
// drives the lifetime via the standard `arc_any_managed_vars_` path.
//
//   text: Ry str handle (StringHeader-prefixed). NUL bytes inside the
//         payload are accepted; the parser uses `stringByteLen`.
//   out:  Stack-allocated RyAny slot to receive the parsed value on
//         success. Contents are undefined on error.
//   Returns 0 on success; non-zero on error (caller reads message via
//   `__ry_get_last_error`).
int64_t __ry_json5_parse_to_any(const char *text, RyAny *out);

// Unified stringify with runtime sort_keys flag. Codegen routes
// `json5.stringify(v, [indent], sortKeys=bool)` through this symbol.
// sort_keys: 0 = insertion order, non-zero = byte-lex order.
//
// Output is strict-JSON compatible (double-quoted keys / strings, no
// trailing commas) except for non-finite floats, which emit JSON5
// bare tokens: NaN → "NaN", ±Inf → "Infinity" / "-Infinity".
const char *__ry_json5_stringify_any_ex(const RyAny *value,
                                         int64_t indent_or_neg1,
                                         uint8_t sort_keys);

// Result-mode counterpart. NULL on failure with __ry_set_last_error
// populated — same nullable-ptr contract so codegen can wrap via
// wrapPtrAsResult. Non-finite floats are NOT failures (json5 permits
// NaN/Inf); only typed-collection / Set / Record / Enum / non-str
// Map keys fail.
const char *__ry_json5_stringify_any_safe_ex(const RyAny *value,
                                              int64_t indent_or_neg1,
                                              uint8_t sort_keys);

// File handle overloads (#1854 mirror). Status-return contract:
// 0 ok / non-zero err, so codegen-side reuses emitResultBranch /
// buildErrorFromRuntime / wrapStatusAsResult.
int64_t __ry_json5_load_file(void *handle, RyAny *out);
int64_t __ry_json5_dump_file(void *handle, const RyAny *value,
                              int64_t indent_or_neg1);

// Recursively releases a RyAny value produced by `__ry_json5_parse_to_any`.
// Codegen-emitted Ry code uses `emitAnyReleaseVar` instead and never calls
// this directly. C++ test / fuzz consumers that build a RyAny from C call
// this to walk the tree and free str / List<any> / Map<str, any> payloads.
void __ry_json5_release_any(RyAny *value);

}

} // namespace ry
