#include "ry/runtime_print.hpp"

#include <cstdarg>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <mutex>


namespace ry {

// Thread-local growable buffer for atomic print output.
static thread_local char *tl_buf = nullptr;
static thread_local size_t tl_len = 0;
static thread_local size_t tl_cap = 0;
// Nesting depth — supports recursive print (e.g. print arg evaluates another print).
static thread_local int tl_depth = 0;

static std::mutex stdout_mu;

static constexpr size_t kInitialCap = 256;

// Shared helper: append vprintf-formatted text to a growable buffer.
// Returns the number of characters written, or -1 on error.
static int append_vprintf(char *&buf, size_t &len, size_t &cap,
                          const char *fmt, std::va_list args) {
    if (!buf) return -1;

    std::va_list args2;
    va_copy(args2, args);

    size_t remaining = cap > len ? cap - len : 0;
    int n = std::vsnprintf(buf + len, remaining, fmt, args);

    if (n < 0) {
        va_end(args2);
        return n;
    }

    if (static_cast<size_t>(n) >= remaining) {
        size_t newCap = len + static_cast<size_t>(n) + 1;
        if (newCap < cap * 2) newCap = cap * 2;
        char *newBuf = static_cast<char *>(std::realloc(buf, newCap));
        if (!newBuf) {
            va_end(args2);
            return -1;
        }
        buf = newBuf;
        cap = newCap;
        std::vsnprintf(buf + len, cap - len, fmt, args2);
    }
    va_end(args2);

    len += static_cast<size_t>(n);
    return n;
}

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

    int n = append_vprintf(tl_buf, tl_len, tl_cap, fmt, args);
    va_end(args);
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

// ===== Sprint (string-print) buffer — independent of the print buffer =====
// Supports nesting via a fixed-depth offset stack.

static thread_local char *tl_sprint_buf = nullptr;
static thread_local size_t tl_sprint_len = 0;
static thread_local size_t tl_sprint_cap = 0;
static constexpr int kMaxSprintDepth = 64;
static thread_local size_t tl_sprint_offsets[kMaxSprintDepth];
static thread_local int tl_sprint_depth = 0;

extern "C" void __ry_sprint_begin() {
    if (tl_sprint_depth < kMaxSprintDepth)
        tl_sprint_offsets[tl_sprint_depth] = tl_sprint_len;
    ++tl_sprint_depth;
    if (!tl_sprint_buf) {
        tl_sprint_cap = kInitialCap;
        tl_sprint_buf = static_cast<char *>(std::malloc(tl_sprint_cap));
        if (!tl_sprint_buf)
            tl_sprint_cap = 0;
    }
}

extern "C" int __ry_sprint_printf(const char *fmt, ...) {
    std::va_list args;
    va_start(args, fmt);
    int n = append_vprintf(tl_sprint_buf, tl_sprint_len, tl_sprint_cap, fmt, args);
    va_end(args);
    return n;
}

extern "C" char *__ry_sprint_end() {
    if (tl_sprint_depth > 0)
        --tl_sprint_depth;
    size_t start = 0;
    if (tl_sprint_depth < kMaxSprintDepth)
        start = tl_sprint_offsets[tl_sprint_depth];
    size_t len = tl_sprint_len - start;
    char *result = static_cast<char *>(std::malloc(len + 1));
    if (result) {
        if (len > 0)
            std::memcpy(result, tl_sprint_buf + start, len);
        result[len] = '\0';
    }
    tl_sprint_len = start;
    return result;
}

} // namespace ry
