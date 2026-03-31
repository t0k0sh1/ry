#include "ry/runtime_print.hpp"

#include <cstdarg>
#include <cstdio>
#include <cstdlib>
#include <mutex>

// Thread-local growable buffer for atomic print output.
static thread_local char *tl_buf = nullptr;
static thread_local size_t tl_len = 0;
static thread_local size_t tl_cap = 0;
// Nesting depth — supports recursive print (e.g. print arg evaluates another print).
static thread_local int tl_depth = 0;

static std::mutex stdout_mu;

static constexpr size_t kInitialCap = 256;

extern "C" void __ry_print_begin() {
    ++tl_depth;
    if (tl_depth == 1) {
        tl_len = 0;
        if (!tl_buf) {
            tl_cap = kInitialCap;
            tl_buf = static_cast<char *>(std::malloc(tl_cap));
            if (!tl_buf) {
                tl_cap = 0;
                --tl_depth;
            }
        }
    }
}

extern "C" int __ry_print_printf(const char *fmt, ...) {
    std::va_list args;
    va_start(args, fmt);

    if (tl_depth <= 0) {
        int n = std::vprintf(fmt, args);
        va_end(args);
        return n;
    }

    std::va_list args2;
    va_copy(args2, args);
    size_t remaining = tl_cap - tl_len;
    int n = std::vsnprintf(tl_buf + tl_len, remaining, fmt, args);
    va_end(args);

    if (n < 0) {
        va_end(args2);
        return n;
    }

    if (static_cast<size_t>(n) >= remaining) {
        size_t newCap = tl_len + static_cast<size_t>(n) + 1;
        char *newBuf = static_cast<char *>(std::realloc(tl_buf, newCap));
        if (!newBuf) {
            va_end(args2);
            return -1;
        }
        tl_buf = newBuf;
        tl_cap = newCap;
        std::vsnprintf(tl_buf + tl_len, tl_cap - tl_len, fmt, args2);
    }
    va_end(args2);

    tl_len += static_cast<size_t>(n);
    return n;
}

extern "C" void __ry_print_end() {
    if (tl_depth > 0)
        --tl_depth;
    if (tl_depth == 0 && tl_len > 0) {
        std::lock_guard<std::mutex> lock(stdout_mu);
        std::fwrite(tl_buf, 1, tl_len, stdout);
        std::fflush(stdout);
        tl_len = 0;
    }
}
