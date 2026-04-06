#pragma once
#include "ry/ry_layout.hpp"
#include "ry/runtime_alloc.hpp"
#include <cstdlib>


namespace ry {

// Allocate a block with an ARC header prepended.
// Returns a pointer to the data area (past ARC_HEADER_SIZE bytes).
// Caller must placement-new or initialize the data area.
inline void *arc_alloc(size_t data_size) {
    void *block = checked_malloc(ARC_HEADER_SIZE + data_size);
    auto *header = static_cast<int64_t *>(block);
    header[0] = 1; // strong_count
    header[1] = 0; // weak_count
    return static_cast<char *>(block) + ARC_HEADER_SIZE;
}

// Free an ARC-managed block given a data pointer.
inline void arc_free(void *data_ptr) {
    if (!data_ptr) return;
    std::free(static_cast<char *>(data_ptr) - ARC_HEADER_SIZE);
}

} // namespace ry
