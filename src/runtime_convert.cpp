#include "ry/runtime_error.hpp"

#include <cerrno>
#include <cstdint>
#include <cstdlib>


namespace ry {

DEFINE_LAST_ERROR(convert)

extern "C" int64_t __ry_str_to_int(const char *str, int64_t *out) {
    if (!str || *str == '\0') {
        setLastError("toInt: empty string");
        return 1;
    }

    errno = 0;
    char *end = nullptr;
    long long val = strtoll(str, &end, 10);

    if (errno == ERANGE) {
        setLastError("toInt: overflow in '%s'", str);
        return 1;
    }
    if (end == str || *end != '\0') {
        setLastError("toInt: invalid character in '%s'", str);
        return 1;
    }

    *out = static_cast<int64_t>(val);
    return 0;
}

extern "C" int64_t __ry_str_to_float(const char *str, double *out) {
    if (!str || *str == '\0') {
        setLastError("toFloat: empty string");
        return 1;
    }

    errno = 0;
    char *end = nullptr;
    double val = strtod(str, &end);

    if (errno == ERANGE) {
        setLastError("toFloat: out of range in '%s'", str);
        return 1;
    }
    if (end == str || *end != '\0') {
        setLastError("toFloat: invalid character in '%s'", str);
        return 1;
    }

    *out = val;
    return 0;
}

} // namespace ry
