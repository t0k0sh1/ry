#pragma once

#include <cstdint>
#include <cstdlib>
#include <cstring>

#include "ry/runtime_alloc.hpp"
#include "ry/runtime_arc.hpp"


namespace ry {

// ListHeader layout: {i64 len, i64 cap, ptr data}
// Matches the generic list ABI used by codegen.
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
const char *__ry_read_line();
const char *__ry_read_all();
const char *__ry_input_prompt(const char *prompt);

// File I/O (return NULL / non-zero on error; use __ry_get_last_error() for message)
const char *__ry_read_text(const char *path);
int64_t __ry_write_text(const char *path, const char *content);
int64_t __ry_append_text(const char *path, const char *content);
int64_t __ry_file_exists(const char *path);
int64_t __ry_delete_file(const char *path);
void *__ry_read_bytes(const char *path);
int64_t __ry_write_bytes(const char *path, void *list);

// Byte conversions
void *__ry_str_to_bytes(const char *s);
const char *__ry_bytes_to_str(void *list);

// Error message retrieval/setting (thread-local)
const char *__ry_get_last_error();
void __ry_set_last_error(const char *msg);

}

} // namespace ry
