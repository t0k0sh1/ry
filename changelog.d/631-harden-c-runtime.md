### Changed

- All C runtime memory allocations now use OOM-safe wrappers (`checked_malloc`, `checked_strdup`, etc.) that abort with a clear message instead of silently returning NULL (#631)
- Integer overflow checks added to array-size calculations in hash table rehash, UTF-8 reverse, and JSON parser (#631)
- CI now enforces a lint check that blocks raw `malloc`/`realloc`/`strdup` in new code (#631)
