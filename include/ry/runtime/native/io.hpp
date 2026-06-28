#pragma once

#include <cstdint>
#include <cstdlib>
#include <cstring>

#include "ry/runtime/core/alloc.hpp"
#include "ry/runtime/core/arc.hpp"


namespace ry {

// ListHeader layout: {i64 len, i64 cap, ptr data}
// Matches the generic list layout used by codegen.
// Shared between net.cpp, json.cpp, json5.cpp, http/*.cpp and the
// `crates/native_io` Rust port (mirrored as `#[repr(C)] IOListHeader`
// in `crates/native_io/src/ffi.rs`). The static_assert that pins the
// layout lives in `tests/test_abi_layout.cpp`.
struct IOListHeader {
    int64_t len;
    int64_t cap;
    int8_t *data;
};

// Build an ARC-managed empty IOListHeader (len=0, data=nullptr).
inline IOListHeader *makeEmptyIOList() {
    auto *header = (IOListHeader *)arc_alloc(sizeof(IOListHeader));
    header->len = 0;
    header->cap = 0;
    header->data = nullptr;
    return header;
}

// Build an ARC-managed IOListHeader from raw bytes.
// Each byte is stored as an int8_t element.
inline IOListHeader *makeByteList(const uint8_t *bytes, int64_t len) {
    auto *header = (IOListHeader *)arc_alloc(sizeof(IOListHeader));
    header->len = len;
    header->cap = len;
    if (len > 0) {
        header->data = (int8_t *)checked_malloc(static_cast<size_t>(len));
        memcpy(header->data, bytes, static_cast<size_t>(len));
    } else {
        header->data = nullptr;
    }
    return header;
}

extern "C" {

// File handle I/O (handle is an opaque pointer to a TU-local IoFileHandle
// defined in crates/native_io/src/ffi.rs as `#[repr(C)] IoFileHandle`;
// declared here as void * to keep the type internal — exposing the struct
// in a header would risk ODR collisions with unrelated `IoFileHandle`
// symbols, per runtime-memory-safety.md). Used by json.cpp / json5.cpp
// (load_file / dump_file) which delegate to the Rust-provided
// libry_io.{dylib,so} via the host-process symbol resolution path.
const char *__ry_io_file_read_all(void *handle);
int64_t __ry_io_file_write_text(void *handle, const char *s);

// Shared error channel. `__ry_set_last_error` writes the host-side
// `last_error_buf` thread_local in `src/runtime/core/error.cpp`;
// `__ry_get_last_error` reads it. Used by thread.cpp, json.cpp,
// json5.cpp, http/*.cpp, and the dual-write helper in
// `crates/native_io/src/lib.rs::set_last_error`.
const char *__ry_get_last_error();
void __ry_set_last_error(const char *msg);

}

} // namespace ry
