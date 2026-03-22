#pragma once

#include <cstdint>

// ListHeader layout: {i64 len, i64 cap, ptr data}
// Matches the generic list ABI used by codegen.
// Shared between runtime_io.cpp and runtime_net.cpp.
struct IOListHeader {
    int64_t len;
    int64_t cap;
    int8_t *data;
};

extern "C" {

// Standard input
const char *__ry_read_line();
const char *__ry_read_all();

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

// Error message retrieval (thread-local)
const char *__ry_get_last_error();

}
