#include "ry/test_runtime.hpp"

#include <cstdint>

namespace ry {

extern "C" int64_t __ry_testing__mockGetCallCount(const char *name) {
    return __ry_mock_get_call_count(name);
}

extern "C" void __ry_testing__mockClear(const char *name) {
    __ry_mock_clear(name);
}

extern "C" void __ry_testing__mockReset(const char *name) {
    __ry_mock_reset(name);
}

extern "C" void __ry_testing__mockResetAll() {
    __ry_mock_clear_all();
}

} // namespace ry
