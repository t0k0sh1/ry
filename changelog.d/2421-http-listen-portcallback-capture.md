### Fixed

- The 5-argument form of `http.listen` (with a `portCallback: fn(int) -> Unit`) now works when `portCallback` captures variables. Previously the callback was invoked through a raw `void(i64)` call that treated the closure value as a bare function pointer, crashing at runtime for any capturing callback — including the documented pattern of binding to port `0` and storing the OS-assigned port through a captured handle. The callback is now dispatched through the standard closure-call path, so every closure form runs correctly. (#2421)
