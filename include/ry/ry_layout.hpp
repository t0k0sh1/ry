#pragma once
#include <cstddef>
#include <cstdint>

// ── ARC header layout ──────────────────────────────────
// { int64_t strong_count, int64_t weak_count }
inline constexpr size_t  ARC_HEADER_SIZE = 16;
inline constexpr int64_t ARC_IMMORTAL    = INT64_MAX;

// ── RyAny type tag ─────────────────────────────────────
enum RyAnyTag : int64_t {
    TAG_INT   = 0,
    TAG_FLOAT = 1,
    TAG_BOOL  = 2,
    TAG_STR   = 3,
    TAG_UNIT  = 4,
};
