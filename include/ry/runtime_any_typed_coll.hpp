#pragma once

// Side-table that records the source-level type name of a typed (non-`any`)
// collection at the moment it is wrapped in `any`. Looked up at JSON
// stringify time to convert what would otherwise be an OOB read on an
// 8-byte-stride buffer (e.g. `List<int>` walked as `RyAny[]`) into a
// deterministic panic with an actionable message. Entries are erased from
// `__ry_arc_free_counted` so the table never outlives the collection
// header it keys on.
//
// Keying: the table is keyed by the **collection data pointer** — i.e. the
// pointer value stored in `any.data[8]` (== `arc_header_ptr + ARC_HEADER_SIZE`).
// This is the same pointer the JSON stringify path holds at lookup time, so
// the key never crosses an offset boundary. Callers that have an ARC
// header pointer instead must shift it forward by `ry::ARC_HEADER_SIZE`
// before calling any of the three helpers (see `__ry_arc_free_counted`
// in `src/runtime_arc_counter.cpp` for the canonical example).
//
// Pure C++ runtime side data — no ARC, no GC. Returns nullptr on miss.

#ifdef __cplusplus
extern "C" {
#endif

void __ry_any_register_typed_coll(void *data_ptr, const char *type_name);
const char *__ry_any_lookup_typed_coll(void *data_ptr);
void __ry_any_unregister_typed_coll(void *data_ptr);

#ifdef __cplusplus
}
#endif
