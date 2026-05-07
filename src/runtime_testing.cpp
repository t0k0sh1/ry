#include "ry/test_runtime.hpp"

namespace ry {

extern "C" void __ry_testing__reportFail(int line, const char *msg) {
    __ry_test_fail(line, msg);
}

} // namespace ry
