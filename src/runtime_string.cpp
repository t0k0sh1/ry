#include "ry/runtime_string.hpp"
#include <cstdint>
#include <cstring>

// ── StringHeader C-level exports called from JIT-compiled IR ─────────────
//
// Codegen emits CreateCall to these symbols when it needs to allocate Ry str
// values at runtime (string concat, repeat, f-string construction, etc.).
// C++ runtime code that returns str (runtime_io, runtime_path, ...) uses the
// inline ry::makeString / ry::makeStringUninit helpers from runtime_string.hpp
// directly instead of going through these symbols.

extern "C" {

// Allocate a StringHeader-managed string of `byte_len` bytes, uninitialized
// data (except for the null terminator at data[byte_len]).  Returns the handle
// (data pointer).  Called by codegen for string concat / repeat.
char *__ry_string_make_uninit(int64_t byte_len) {
    if (byte_len < 0) byte_len = 0;
    return ry::makeStringUninit(static_cast<size_t>(byte_len));
}

// Allocate a StringHeader-managed string by copying `byte_len` bytes from
// `src` (NUL bytes included).  Returns the handle.
char *__ry_string_make(const char *src, int64_t byte_len) {
    if (byte_len < 0) byte_len = 0;
    return ry::makeString(src, static_cast<size_t>(byte_len));
}

// Return the byte_len stored in the StringHeader for `handle`.
int64_t __ry_string_len(const char *handle) {
    return ry::stringByteLen(handle);
}

// NUL-safe string comparison: compares first min(la,lb) bytes with memcmp,
// then breaks ties by byte length.  Semantics match strcmp for NUL-free strings.
// Called by codegen for str == / != / < / <= / > / >= operators.
int32_t __ry_str_cmp(const char *a, int64_t la, const char *b, int64_t lb) {
    int64_t min_len = la < lb ? la : lb;
    int r = memcmp(a, b, static_cast<size_t>(min_len));
    if (r != 0) return r;
    if (la > lb) return 1;
    if (la < lb) return -1;
    return 0;
}

} // extern "C"
