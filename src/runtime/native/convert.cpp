#include "ry/runtime/core/error.hpp"

#include <cerrno>
#include <cstdint>
#include <cstdlib>


namespace ry {

DEFINE_LAST_ERROR(convert)

// Shared parsing core for the integer converters. `label` selects the
// diagnostic prefix so each public entry point (toInt / parseInt) owns its
// own error messages while sharing one parsing implementation.
static int64_t parse_int_impl(const char *str, int64_t *out, const char *label) {
    if (!str || *str == '\0') {
        setLastError("%s: empty string", label);
        return 1;
    }

    errno = 0;
    char *end = nullptr;
    long long val = strtoll(str, &end, 10);

    if (errno == ERANGE) {
        setLastError("%s: overflow in '%s'", label, str);
        return 1;
    }
    if (end == str || *end != '\0') {
        setLastError("%s: invalid character in '%s'", label, str);
        return 1;
    }

    *out = static_cast<int64_t>(val);
    return 0;
}

// Shared parsing core for the float converters; see parse_int_impl.
static int64_t parse_float_impl(const char *str, double *out, const char *label) {
    if (!str || *str == '\0') {
        setLastError("%s: empty string", label);
        return 1;
    }

    errno = 0;
    char *end = nullptr;
    double val = strtod(str, &end);

    if (errno == ERANGE) {
        setLastError("%s: out of range in '%s'", label, str);
        return 1;
    }
    if (end == str || *end != '\0') {
        setLastError("%s: invalid character in '%s'", label, str);
        return 1;
    }

    *out = val;
    return 0;
}

extern "C" int64_t __ry_str_to_int(const char *str, int64_t *out) {
    return parse_int_impl(str, out, "toInt");
}

extern "C" int64_t __ry_str_to_float(const char *str, double *out) {
    return parse_float_impl(str, out, "toFloat");
}

extern "C" int64_t __ry_str_parse_int(const char *str, int64_t *out) {
    return parse_int_impl(str, out, "parseInt");
}

extern "C" int64_t __ry_str_parse_float(const char *str, double *out) {
    return parse_float_impl(str, out, "parseFloat");
}

} // namespace ry
