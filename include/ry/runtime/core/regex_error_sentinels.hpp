#pragma once

#include <cstdint>
#include <limits>

namespace ry {

inline constexpr int64_t kRegexMatchError = -1;
inline constexpr int64_t kRegexSearchError = std::numeric_limits<int64_t>::min();

} // namespace ry
