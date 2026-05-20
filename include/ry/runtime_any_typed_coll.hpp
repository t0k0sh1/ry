#pragma once

// Side-table that records the source-level type name of a typed (non-`any`)
// collection at the moment it is wrapped in `any`. Looked up at JSON
// stringify time to convert what would otherwise be an OOB read on an
// 8-byte-stride buffer (e.g. `List<int>` walked as `RyAny[]`) into a
// deterministic panic with an actionable message. Entries are erased from
// `__ry_arc_free_counted` so the table never outlives the collection
// header it keys on.
//
// Pure C++ runtime side data — no ARC, no GC. Returns nullptr on miss.

#ifdef __cplusplus
extern "C" {
#endif

void __ry_any_register_typed_coll(void *header_ptr, const char *type_name);
const char *__ry_any_lookup_typed_coll(void *header_ptr);
void __ry_any_unregister_typed_coll(void *header_ptr);

#ifdef __cplusplus
}
#endif
