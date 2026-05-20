#include "ry/runtime_any_typed_coll.hpp"

#include <atomic>
#include <mutex>
#include <string>
#include <unordered_map>


namespace {

// Maps collection data pointer (the value stored in `any.data[8]`, which is
// `arc_header_ptr + ARC_HEADER_SIZE`) to the source-level type name recorded
// at wrap time. Entry lifetime is bounded by the collection header itself —
// `__ry_arc_free_counted` shifts the header pointer to the data pointer and
// calls `__ry_any_unregister_typed_coll` before `std::free`, so a fresh
// allocation at the same address cannot pick up a stale entry.
std::unordered_map<void *, std::string> g_typed_coll_names;
std::mutex g_typed_coll_mu;

// Hot-path gate for the free path: `__ry_arc_free_counted` runs on every
// ARC header free across the whole runtime, but only a small subset are
// typed-non-any collections wrapped in any. Reading this relaxed-atomic
// counter avoids acquiring the mutex for the vast majority of frees.
std::atomic<std::size_t> g_typed_coll_count{0};

} // anonymous namespace

extern "C" {

void __ry_any_register_typed_coll(void *data_ptr, const char *type_name) {
    if (!data_ptr || !type_name) return;
    std::lock_guard<std::mutex> lock(g_typed_coll_mu);
    auto [it, inserted] = g_typed_coll_names.try_emplace(data_ptr, type_name);
    if (inserted) {
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

void __ry_any_unregister_typed_coll(void *data_ptr) {
    if (!data_ptr) return;
    if (g_typed_coll_count.load(std::memory_order_relaxed) == 0) return;
    std::lock_guard<std::mutex> lock(g_typed_coll_mu);
    if (g_typed_coll_names.erase(data_ptr) > 0) {
        g_typed_coll_count.fetch_sub(1, std::memory_order_relaxed);
    }
}

} // extern "C"
