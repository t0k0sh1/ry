#pragma once

#include <cstdint>

extern "C" {

// Standard input
const char *__ry_read_line();
const char *__ry_read_all();

// File I/O
const char *__ry_read_text(const char *path);
void __ry_write_text(const char *path, const char *content);
void __ry_append_text(const char *path, const char *content);
int64_t __ry_file_exists(const char *path);
void __ry_delete_file(const char *path);
void *__ry_read_bytes(const char *path);
void __ry_write_bytes(const char *path, void *list);

// Byte conversions
void *__ry_str_to_bytes(const char *s);
const char *__ry_bytes_to_str(void *list);

}
