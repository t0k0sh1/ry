#include "ry/test_runtime.hpp"

#include <cstdint>

namespace ry {

// Ry's `int` lowers to `i64` in IR, so the @native("testing") dispatch
// emits `call void @__ry_testing__reportFail(i64 %line, ptr %msg)`.  The
// C signature must therefore receive `int64_t` to match the ABI.  We
// narrow to `int` for the existing `__ry_test_fail` helper, which keeps
// its legacy 32-bit line parameter (line numbers fit comfortably).
extern "C" void __ry_testing__reportFail(int64_t line, const char *msg) {
    __ry_test_fail(static_cast<int>(line), msg);
}

} // namespace ry
