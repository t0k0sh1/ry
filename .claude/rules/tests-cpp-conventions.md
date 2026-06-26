---
paths:
  - "tests/test_*.cpp"
  - "tests/test_*.hpp"
---

# Tests - C++ Conventions

- When renaming stdlib `@native` functions, sweep embedded Ry source in C++ tests, not only `.ry` files.
- C++ harnesses that skip `ModuleLoader` need helpers such as `withStdlibDirectiveDecls()` for stdlib-declared directives.
- Append inline declarations for `runTestSource` helpers; do not prepend when tests assert line numbers.
- For loader-pipeline changes with codegen no-ops, test `resolveImportsOnly()` and AST variants directly.
- Use `-> Unit` in `@it` / `@describe` rejection tests when isolating directive checks.
- Do not write C++ string literals like `"\x00a"`; `\x` consumes following hex digits. Use char arrays or adjacent literals.
- Clear or overwrite thread-local HTTP error state before asserting message content.
- In subprocess tests, pass the full `RY_BINARY_PATH` as `argv[0]` to preserve Linux stdlib resolution.
- HTTP server tests using `http.listen` bind with OS-assigned port `0` and pass a capturing `portCallback: fn(int) -> Unit` that records the bound port for a readiness poll (e.g. a captured `AtomicInt`); never hard-code a port + `sleep()` (flaky under the full suite: #2421). The `portCallback` may be a block-body lambda or a single-expression `-> Unit` lambda whose body is a Unit-returning call (e.g. `(p: int) -> Unit => store(p)`); both are supported since #2421. A non-Unit single-expression body (e.g. `=> x + 1`) is a type error.
