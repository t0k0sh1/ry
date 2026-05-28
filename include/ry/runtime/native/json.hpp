#pragma once

#include <cstdint>

#include "ry/runtime/core/any.hpp"


namespace ry {

extern "C" {

// Parses JSON text into a RyAny value. Caller owns the resulting payloads
// (str / List<any> / Map<str, any>) — codegen's `emitAnyReleaseVar`
// drives the lifetime via the standard `arc_any_managed_vars_` path.
//
//   text: Ry str handle (StringHeader-prefixed). NUL bytes inside the
//         payload are accepted; the parser uses `stringByteLen`.
//   out:  Stack-allocated RyAny slot to receive the parsed value on
//         success. Contents are undefined on error.
//   Returns 0 on success; non-zero on error (caller reads message via
//   `__ry_get_last_error`).
int64_t __ry_json_parse_to_any(const char *text, RyAny *out);

// Stringifies a RyAny value to JSON.
//   value:           pointer to the RyAny to encode (read-only).
//   indent_or_neg1:  < 0 → compact form (no whitespace);
//                    ≥ 0 → pretty-print with that indent width.
// Panics (fprintf + exit(1)) on tags that JSON cannot represent
// (Set, Record, Enum, non-`str` Map keys). The return type is `-> str`,
// so there is no Result channel to surface the error through — pin the
// behaviour via spec tests if you depend on it.
const char *__ry_json_stringify_any(const RyAny *value, int64_t indent_or_neg1);

// Recursively releases a RyAny value produced by `__ry_json_parse_to_any`.
// Codegen-emitted Ry code uses `emitAnyReleaseVar` instead and never calls
// this directly. C++ test / fuzz consumers that build a RyAny from C call
// this to walk the tree and free str / List<any> / Map<str, any> payloads.
void __ry_json_release_any(RyAny *value);

}

} // namespace ry
