#ifndef RY_RUNTIME_PRINT_HPP
#define RY_RUNTIME_PRINT_HPP

extern "C" {
/// Begin buffered print — subsequent __ry_print_printf calls accumulate
/// into a thread-local buffer instead of writing to stdout directly.
void __ry_print_begin();

/// Drop-in replacement for printf that respects the buffered-print state.
/// When buffering is active, output is appended to the thread-local buffer.
/// When buffering is not active, output is written to stdout immediately.
int __ry_print_printf(const char *fmt, ...);

/// End buffered print — flushes the accumulated buffer to stdout atomically
/// under a mutex, then resets the buffer length.
void __ry_print_end();
}

#endif // RY_RUNTIME_PRINT_HPP
