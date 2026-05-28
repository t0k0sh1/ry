#include "ry/runtime/core/any_typed_coll.hpp"

#include <atomic>
#include <mutex>
#include <string>
#include <unordered_map>


namespace {

// Maps collection data pointer (the value stored in `any.data[8]`, which is
// `arc_header_ptr + ARC_HEADER_SIZE`) to the source-level type name recorded
// at wrap time.
//
// **Design (register-only, #1811)**: this table is keyed by collection data
// pointer and is never erased. An earlier draft erased entries from
// `__ry_arc_free_counted` (the ARC header free path) to keep the table in
// lockstep with header lifetime, but that hook deterministically triggered a
// Linux glibc heap-check crash (`corrupted size vs. prev_size while
// consolidating`) that ASan/UBSan/TSan could not reproduce. The trigger was
// the hook call itself, not any specific code path inside it. The retreat:
//
//   - register uses `insert_or_assign`, so re-allocations at the same data
//     pointer overwrite the previous entry. Typed-collection wraps record
//     their source-level name; the next typed wrap at the same address
//     overwrites with the new name.
//   - no `__ry_any_unregister_typed_coll` symbol — the function was removed
//     entirely to avoid any path that calls it from `__ry_arc_free_counted`.
//
// **False-positive caveat**: when a typed collection is wrapped in `any`,
// the wrapping site freed, and the same data address later reused by a
// `List<any>` / `Map<str, any>` / `Set<any>` (which does NOT call register),
// a JSON stringify of the new `any` will lookup the stale entry and panic
// with the prior typed name. The user-visible effect is a misleading but
// deterministic panic message — strictly better than the OOB UB the
// side-table was introduced to replace. A follow-up issue may revisit this
// trade-off (e.g. "register on every wrap with a safe sentinel for `any`
// element types") once the root-cause of the original glibc crash is
// understood.
std::unordered_map<void *, std::string> g_typed_coll_names;
std::mutex g_typed_coll_mu;

// Hot-path gate for lookup: most programs never wrap a typed collection in
// `any`, so the table stays empty and lookup can skip the mutex. With the
// register-only design this counter only ever increments (no decrement),
// so it serves as a one-shot "has anything ever been registered" gate
// rather than a live-entry count.
std::atomic<std::size_t> g_typed_coll_count{0};

} // anonymous namespace

extern "C" {

void __ry_any_register_typed_coll(void *data_ptr, const char *type_name) {
    if (!data_ptr || !type_name) return;
    std::lock_guard<std::mutex> lock(g_typed_coll_mu);
    auto sz_before = g_typed_coll_names.size();
    g_typed_coll_names.insert_or_assign(data_ptr, type_name);
    if (g_typed_coll_names.size() > sz_before) {
        g_typed_coll_count.fetch_add(1, std::memory_order_relaxed);
    }
}

const char *__ry_any_lookup_typed_coll(void *data_ptr) {
    if (!data_ptr) return nullptr;
    if (g_typed_coll_count.load(std::memory_order_relaxed) == 0) return nullptr;
    std::lock_guard<std::mutex> lock(g_typed_coll_mu);
    auto it = g_typed_coll_names.find(data_ptr);
    return it == g_typed_coll_names.end() ? nullptr : it->second.c_str();
}

} // extern "C"
