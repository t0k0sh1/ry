#pragma once

#include "ry/runtime_alloc.hpp"
#include "ry/runtime_string.hpp"

#include <cstdarg>
#include <cstdio>
#include <cstdlib>
#include <cstring>


namespace ry {

// Define a module-private thread-local error buffer with:
//   - static void setLastError(const char *fmt, ...)
//   - extern "C" const char *__ry_<prefix>_get_last_error()
//
// Each expansion creates an independent thread-local buffer per module.
// The getter returns a makeString-allocated ARC-managed str handle (strong_count=1).
//
// Place at file scope (outside anonymous namespaces) because the getter
// needs external C linkage.
//
// Expand at most once per translation unit (the buffer name is not prefixed).
// Not used by runtime_io.cpp which has its own shared error API.
#define DEFINE_LAST_ERROR(prefix)                                              \
    static thread_local char last_error_buf[512] = {0};                        \
                                                                               \
    static void setLastError(const char *fmt, ...) {                           \
        va_list args;                                                          \
        va_start(args, fmt);                                                   \
        vsnprintf(last_error_buf, sizeof(last_error_buf), fmt, args);          \
        va_end(args);                                                          \
    }                                                                          \
                                                                               \
    extern "C" const char *__ry_##prefix##_get_last_error() {                  \
        return makeString(last_error_buf, strlen(last_error_buf));              \
    }

} // namespace ry
