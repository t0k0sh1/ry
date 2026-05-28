#pragma once
#include "ry/ry_layout.hpp"
#include "ry/runtime/core/alloc.hpp"
#include <cstdint>
#include <cstdlib>

// Forward declarations for the counted alloc/free wrappers defined in
// src/runtime/core/arc_counter.cpp.  All ARC header allocations and frees are
// routed through these so that the live-count counter stays accurate for both
// the codegen-emitted path (collections) and the C++ helper path (resource
// handles such as TcpListenerHandle, ThreadHandle, etc.).
extern "C" void *__ry_arc_alloc_counted(int64_t total_size);
extern "C" void  __ry_arc_free_counted(void *header_ptr);


namespace ry {

// Allocate a block with an ARC header prepended.
// Returns a pointer to the data area (past ARC_HEADER_SIZE bytes).
// Caller must placement-new or initialize the data area.
inline void *arc_alloc(size_t data_size) {
    size_t total;
    if (__builtin_add_overflow(ARC_HEADER_SIZE, data_size, &total)) {
        add_overflow_abort(ARC_HEADER_SIZE, data_size); // NOLINT(bugprone-narrowing-conversions)
    }
    void *block = __ry_arc_alloc_counted(static_cast<int64_t>(total));
    auto *header = static_cast<int64_t *>(block);
    header[0] = 1; // strong_count
    header[1] = 0; // weak_count
    return static_cast<char *>(block) + ARC_HEADER_SIZE;
}

// Free an ARC-managed block given a data pointer.
inline void arc_free(void *data_ptr) {
    if (!data_ptr) return;
    __ry_arc_free_counted(static_cast<char *>(data_ptr) - ARC_HEADER_SIZE);
}

} // namespace ry
