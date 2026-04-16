#include "ry/runtime_alloc.hpp"
#include <atomic>
#include <cstdint>
#include <cstdlib>


// =========================================================================
// ARC header allocation/free counter
//
// Tracks every ARC *header* allocation (+1) and free (-1).  The running
// total can be queried from Ry test code via the `runtime_internal` package
// to write delta-based leak assertions without relying on LSan.
//
// Counter is relaxed-atomic: ordering guarantees are not needed — only the
// numeric balance matters.  This is safe under @parallel for because all
// ARC ops there are already sequentially-consistent; the counter's own
// atomicity is sufficient to avoid torn reads.
// =========================================================================

namespace {
    std::atomic<int64_t> g_arc_live_count{0};
} // anonymous namespace

extern "C" {

// C++ path (resource handles: TcpListenerHandle, ThreadHandle, etc.)
// called from include/ry/runtime_arc.hpp arc_alloc / arc_free.
void *__ry_arc_alloc_counted(int64_t total_size) {
    void *p = ry::checked_malloc(static_cast<size_t>(total_size));
    g_arc_live_count.fetch_add(1, std::memory_order_relaxed);
    return p;
}

void __ry_arc_free_counted(void *header_ptr) {
    if (!header_ptr) return;
    g_arc_live_count.fetch_sub(1, std::memory_order_relaxed);
    std::free(header_ptr);
}

// Returns the address of the counter so that codegen_arc.cpp can embed it
// as an inttoptr constant and emit inline atomicrmw without creating a new
// function-call symbol in the JIT module (avoids JITLink stub creation on
// Linux that triggers a teardown crash during RT->remove()).
int64_t *__ry_arc_counter_address() {
    return reinterpret_cast<int64_t *>(&g_arc_live_count);
}

int64_t __ry_runtime_internal_arc_live_count() {
    return g_arc_live_count.load(std::memory_order_relaxed);
}

} // extern "C"
