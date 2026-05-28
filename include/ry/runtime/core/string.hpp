#pragma once

#include "ry/ry_layout.hpp"
#include "ry/runtime/core/alloc.hpp"
#include <cstdint>
#include <cstring>

// Forward declarations for the live-count symmetry helpers defined in
// src/runtime/core/arc_counter.cpp.  makeString/makeStringUninit increment and
// freeStringSlot decrements so that dynamic str allocations contribute to
// `arcLiveCount()` symmetrically with codegen-emitted retain/release on
// str container elements (#1576).
extern "C" void __ry_arc_counter_increment();
extern "C" void __ry_arc_counter_decrement();

// ── StringHeader helpers ────────────────────────────────────────────────────
//
// Every `str` value in Ry points to the *data* portion of a StringHeader.
// The full allocation layout is (see ry_layout.hpp for constants):
//
//   [ strong_count : i64 ]   ← STRING_HEADER_SIZE (24) bytes before handle
//   [ weak_count   : i64 ]
//   [ byte_len     : i64 ]   ← 8 bytes before handle (STRING_BYTELEN_OFFSET)
//   [ data[0..N-1] : i8  ]   ← handle (the char* exposed to Ry / codegen)
//   [ '\0'         : i8  ]   ← handle[byte_len] — kept for @native / libc compat
//
// Invariants
// ----------
// 1. All `str` values are StringHeader handles (either immortal literal globals
//    or dynamically allocated with makeString/makeStringUninit).
// 2. handle[byte_len] == '\0' always (null terminator for C library compat).
// 3. Since PR #1046, str IS ARC-managed by codegen: fieldTypeIsArcManaged("str")
//    returns true, retain/release are emitted, and collection destructors release
//    str elements.  Use emitStrGetHeaderFromData (offset −24) not
//    emitArcGetHeaderFromData (offset −16) when computing the ARC header.
// 4. Literal globals use ARC_IMMORTAL for strong_count (no-op on any retain).

namespace ry {

// Return the byte_len stored in the StringHeader for `handle`.
// handle must be a valid StringHeader data pointer (not a raw C string).
inline int64_t stringByteLen(const char *handle) {
    // NOLINTNEXTLINE(cppcoreguidelines-pro-bounds-pointer-arithmetic)
    return *reinterpret_cast<const int64_t *>(handle - static_cast<ptrdiff_t>(STRING_BYTELEN_OFFSET));
}

// Return a pointer to the beginning of the StringHeader for `handle`
// (i.e. handle - STRING_HEADER_SIZE).  This points to strong_count.
// Equivalent to emitStrGetHeaderFromData in codegen.
inline char *stringHeaderPtr(const char *handle) {
    // NOLINTNEXTLINE(cppcoreguidelines-pro-bounds-pointer-arithmetic)
    return const_cast<char *>(handle) - static_cast<ptrdiff_t>(STRING_HEADER_SIZE);
}

// Allocate a StringHeader-managed string of `byte_len` bytes, copy `byte_len`
// bytes from `src` (may contain embedded NUL bytes), append a null terminator,
// and return a handle (pointer to data).
//
// If `src` is nullptr an empty string is returned (byte_len is forced to 0).
// Aborts on OOM or integer overflow.
inline char *makeString(const char *src, size_t byte_len) {
    if (!src) byte_len = 0;

    // Overflow-safe total: STRING_HEADER_SIZE + byte_len + 1 (null terminator)
    size_t total;
    if (__builtin_add_overflow(STRING_HEADER_SIZE, byte_len, &total) ||
        __builtin_add_overflow(total, size_t{1}, &total)) {
        oom_abort(byte_len);
    }

    auto *block = static_cast<int64_t *>(checked_malloc(total));
    block[0] = 1;  // strong_count
    block[1] = 0;  // weak_count
    block[2] = static_cast<int64_t>(byte_len);  // byte_len

    auto *data = reinterpret_cast<char *>(block + 3);  // past the 3×i64 header
    if (byte_len > 0) memcpy(data, src, byte_len);
    data[byte_len] = '\0';

    __ry_arc_counter_increment();
    return data;
}

// Allocate a StringHeader-managed string of `byte_len` bytes without copying
// any data (the data region is uninitialized).  The null terminator at
// data[byte_len] IS written.  Used by concat/repeat where the caller fills the
// buffer with memcpy before returning the handle.
//
// Aborts on OOM or integer overflow.
inline char *makeStringUninit(size_t byte_len) {
    size_t total;
    if (__builtin_add_overflow(STRING_HEADER_SIZE, byte_len, &total) ||
        __builtin_add_overflow(total, size_t{1}, &total)) {
        oom_abort(byte_len);
    }

    auto *block = static_cast<int64_t *>(checked_malloc(total));
    block[0] = 1;
    block[1] = 0;
    block[2] = static_cast<int64_t>(byte_len);

    auto *data = reinterpret_cast<char *>(block + 3);
    data[byte_len] = '\0';

    __ry_arc_counter_increment();
    return data;
}

// Return true if `handle` contains an embedded NUL byte (i.e. a NUL before the
// terminal NUL written by makeString).  Use this before passing a Ry str to any
// C API that treats the first NUL as the end of the string.
inline bool hasEmbeddedNul(const char *handle) {
    if (!handle) return false;
    size_t n = static_cast<size_t>(stringByteLen(handle));
    return n > 0 && memchr(handle, '\0', n) != nullptr;
}

// Free a StringHeader-managed string given its handle.
// Use this instead of plain free() whenever the pointer came from
// makeString/makeStringUninit (not from a literal global).
// Calling this on a literal global (ARC_IMMORTAL) is NOT safe; only call it
// on dynamically-allocated strings.
inline void freeStringSlot(char *handle) {
    if (!handle) return;
    __ry_arc_counter_decrement();
    // NOLINTNEXTLINE(cppcoreguidelines-pro-bounds-pointer-arithmetic)
    free(handle - static_cast<ptrdiff_t>(STRING_HEADER_SIZE));
}

} // namespace ry
