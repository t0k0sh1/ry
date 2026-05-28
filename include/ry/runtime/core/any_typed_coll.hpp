#pragma once

// Side-table that records the source-level type name of a typed (non-`any`)
// collection at the moment it is wrapped in `any`. Looked up at JSON
// stringify time to convert what would otherwise be an OOB read on an
// 8-byte-stride buffer (e.g. `List<int>` walked as `RyAny[]`) into a
// deterministic panic with an actionable message.
//
// **Register-only design (#1811)**: the table is keyed by the **collection
// data pointer** — i.e. the pointer value stored in `any.data[8]`
// (== `arc_header_ptr + ARC_HEADER_SIZE`). Entries are never erased;
// re-registration at the same address simply overwrites the previous entry
// via `insert_or_assign`. An earlier draft erased entries from
// `__ry_arc_free_counted` to keep the table in lockstep with header
// lifetime, but that hook deterministically triggered a Linux glibc heap
// check crash that local ASan/UBSan/TSan could not reproduce. See
// `src/runtime/core/any_typed_coll.cpp` for the false-positive caveat the
// register-only design accepts in exchange.
//
// Pure C++ runtime side data — no ARC, no GC. Returns nullptr on miss.

#ifdef __cplusplus
extern "C" {
#endif

void __ry_any_register_typed_coll(void *data_ptr, const char *type_name);
const char *__ry_any_lookup_typed_coll(void *data_ptr);

#ifdef __cplusplus
}
#endif
