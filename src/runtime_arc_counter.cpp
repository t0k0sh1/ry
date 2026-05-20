#include "ry/runtime_alloc.hpp"
#include "ry/runtime_any_typed_coll.hpp"
#include "ry/ry_layout.hpp"
#include <cstdint>
#include <cstdlib>


// =========================================================================
// ARC header allocation/free counter
//
// Tracks every ARC *header* allocation (+1) and free (-1).  The running
// total can be queried from Ry test code via the `runtime_internal` module
// to write delta-based leak assertions without relying on LSan.
//
// Counter is relaxed-atomic: ordering guarantees are not needed — only the
// numeric balance matters.  This is safe under @parallel for because all
// ARC ops there are already sequentially-consistent; the counter's own
// atomicity is sufficient to avoid torn reads.
// =========================================================================

namespace {
    alignas(int64_t) int64_t g_arc_live_count = 0;
} // anonymous namespace

extern "C" {

// C++ path (resource handles: TcpListenerHandle, ThreadHandle, etc.)
// called from include/ry/runtime_arc.hpp arc_alloc / arc_free.
void *__ry_arc_alloc_counted(int64_t total_size) {
    void *p = ry::checked_malloc(static_cast<size_t>(total_size));
    __atomic_fetch_add(&g_arc_live_count, 1, __ATOMIC_RELAXED);
    return p;
}

void __ry_arc_free_counted(void *header_ptr) {
    if (!header_ptr) return;
    // Erase the typed-collection side-table entry (if any) BEFORE freeing
    // the header — once `std::free` returns, the address could be reused
    // by a different allocation and a stale entry would mislead
    // `__ry_json_stringify_any`. The side-table is keyed by the inner
    // *data* pointer (the value stored in `any.data[8]`), which is
    // `header_ptr + ARC_HEADER_SIZE`, not the header pointer itself.
    // Erase-if-present, no-op for the common case where the header was
    // never registered.
    void *data_ptr = static_cast<char *>(header_ptr) + ry::ARC_HEADER_SIZE;
    __ry_any_unregister_typed_coll(data_ptr);
    __atomic_fetch_sub(&g_arc_live_count, 1, __ATOMIC_RELAXED);
    std::free(header_ptr);
}

// String-allocation symmetry helpers (#1576): makeString/makeStringUninit
// allocate a StringHeader-prefixed block via checked_malloc directly (not
// via __ry_arc_alloc_counted), so they must hand-stamp the live-count to
// stay symmetric with codegen-emitted retain/release on str container
// elements.  freeStringSlot mirrors with a decrement.
void __ry_arc_counter_increment() {
    __atomic_fetch_add(&g_arc_live_count, 1, __ATOMIC_RELAXED);
}

void __ry_arc_counter_decrement() {
    __atomic_fetch_sub(&g_arc_live_count, 1, __ATOMIC_RELAXED);
}

// Returns the address of the counter so that codegen_arc.cpp can embed it
// as an inttoptr constant and emit inline atomicrmw without creating a new
// function-call symbol in the JIT module (avoids JITLink stub creation on
// Linux that triggers a teardown crash during RT->remove()).
int64_t *__ry_arc_counter_address() {
    return &g_arc_live_count;
}

int64_t __ry_runtime_internal_arc_live_count() {
    return __atomic_load_n(&g_arc_live_count, __ATOMIC_RELAXED);
}

} // extern "C"
