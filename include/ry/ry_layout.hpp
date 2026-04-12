#pragma once
#include <cstddef>
#include <cstdint>


namespace ry {

// ── ARC header layout ──────────────────────────────────
// { int64_t strong_count, int64_t weak_count }
inline constexpr size_t  ARC_HEADER_SIZE = 16;
inline constexpr int64_t ARC_IMMORTAL    = INT64_MAX;

// ── RyAny type tag ─────────────────────────────────────
enum class RyAnyTag : int64_t {
    Int   = 0,
    Float = 1,
    Bool  = 2,
    Str   = 3,
    Unit  = 4,
};

} // namespace ry
