#pragma once

#include <cstdint>
#include <cstdlib>
#include <cstring>

#include "ry/runtime/core/alloc.hpp"
#include "ry/runtime/core/arc.hpp"


namespace ry {

// ListHeader layout: {i64 len, i64 cap, ptr data}
// Matches the generic list layout used by codegen.
// Shared between runtime_io.cpp and runtime_net.cpp.
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

// Standard input
// Tri-state result: 0 = line read into *out_line, 1 = EOF (no data),
// -1 = I/O error (call __ry_io_get_last_error() for message).
// On a 0-return *out_line is set to a Ry string handle (trailing '\n' stripped).
int64_t __ry_io_input_prompt(const char *prompt, const char **out_line);
const char *__ry_io_read_all();

// File I/O (return NULL / non-zero on error; use __ry_io_get_last_error() for message)
const char *__ry_io_read_text(const char *path);
int64_t __ry_io_write_text(const char *path, const char *content);
int64_t __ry_io_append_text(const char *path, const char *content);
int64_t __ry_io_exists(const char *path);
int64_t __ry_io_delete_file(const char *path);
void *__ry_io_read_bytes(const char *path);
int64_t __ry_io_write_bytes(const char *path, void *list);

// Byte conversions
void *__ry_io_to_bytes(const char *s);
const char *__ry_io_bytes_to_str(void *list);

// File handle I/O (handle is an opaque pointer to a TU-local IoFileHandle
// defined in runtime_io.cpp; declared here as void * to keep the type
// internal — exposing the struct in a header would risk ODR collisions
// with unrelated `IoFileHandle` symbols, per runtime-memory-safety.md).
const char *__ry_io_file_read_all(void *handle);
int64_t __ry_io_file_write_text(void *handle, const char *s);

// Module-local error channel (defined via DEFINE_LAST_ERROR(io) in io.cpp).
// __ry_get_last_error / __ry_set_last_error remain in ry_lib as the
// shared error API for io-adjacent callers that haven't migrated to the
// module-local channel (e.g. emitBuiltinCore's input/close pair in
// src/codegen_call.cpp).
const char *__ry_io_get_last_error();
const char *__ry_get_last_error();
void __ry_set_last_error(const char *msg);

}

} // namespace ry
