#pragma once

namespace ry {

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

/// Begin sprint (string-print) — pushes a new capture context onto the sprint
/// stack.  Subsequent __ry_sprint_printf calls accumulate into a thread-local
/// buffer.  Supports nesting via a saved-offset stack.
void __ry_sprint_begin();

/// Drop-in replacement for printf that appends to the sprint buffer.
int __ry_sprint_printf(const char *fmt, ...);

/// End sprint — pops the capture context, returns a malloc'd, null-terminated
/// copy of the content written since the matching __ry_sprint_begin.
char *__ry_sprint_end();
}

} // namespace ry
