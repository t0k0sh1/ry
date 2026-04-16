#pragma once
#include <cstddef>
#include <cstdint>


namespace ry {

// ── ARC header layout ──────────────────────────────────
// { int64_t strong_count, int64_t weak_count }
// Computed from the field types to avoid ABI drift if layout changes.
inline constexpr size_t  ARC_HEADER_SIZE = sizeof(int64_t) * 2;
inline constexpr int64_t ARC_IMMORTAL    = INT64_MAX;

// ── StringHeader layout ────────────────────────────────
// Strings use an extended header that appends a byte_len field after the
// standard ARC header, followed by the string bytes and a null terminator.
//
//   [ strong_count : i64 ]  ← STRING_HEADER_SIZE bytes before handle
//   [ weak_count   : i64 ]
//   [ byte_len     : i64 ]  ← handle - STRING_BYTELEN_OFFSET (== 8)
//   [ data[0..N-1] : i8  ]  ← handle (the char* exposed to Ry code)
//   [ '\0'         : i8  ]  ← handle[byte_len] (null-terminator for @native compat)
//
// ARC_HEADER_SIZE (16) is intentionally NOT changed — collection headers
// (ListHeader, MapHeader, etc.) still reside exactly 16 bytes past ARC alloc.
// String-specific helpers use STRING_HEADER_SIZE (24) for their own offsets.
//
// In PR #1022, strings carry this header but are NOT yet ARC-managed by codegen
// (fieldTypeIsArcManaged("str") remains false). The strong_count/weak_count
// fields are present for forward compatibility: literals get ARC_IMMORTAL, and
// dynamic strings get strong_count=1 (written by makeString but never decremented
// until a follow-up issue enables full ARC management for str).
inline constexpr size_t  STRING_HEADER_EXTRA  = sizeof(int64_t);    // byte_len field
inline constexpr size_t  STRING_HEADER_SIZE   = ARC_HEADER_SIZE + STRING_HEADER_EXTRA; // 24
inline constexpr int64_t STRING_BYTELEN_OFFSET = static_cast<int64_t>(STRING_HEADER_EXTRA); // 8

// ── RyAny type tag ─────────────────────────────────────
enum class RyAnyTag : int64_t {
    Int   = 0,
    Float = 1,
    Bool  = 2,
    Str   = 3,
    Unit  = 4,
};

} // namespace ry
