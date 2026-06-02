### Changed

- codegen: the LLVM IR emission shared library (`ry_llvm_emit`) is now built unconditionally from the Rust crate (`crates/ry_llvm_emit/`). Because `ry` now always links the shared `libLLVM`, building from source outside the Docker CI image requires a shared `libLLVM` in the LLVM prefix and a Rust 1.83+ toolchain on `PATH`. On macOS use `cmake --preset rust-emit` (Homebrew `llvm@21` ships `libLLVM.dylib`); the static-only `/usr/local/llvm` no longer satisfies `--preset default`. The `ry-ci` Docker image bakes in both the shared libLLVM and Rust, so container builds need no extra setup. (#1993)

### Removed

- codegen: removed the C++ LLVM IR emission implementation (`src/llvm_emit/impl.cpp`) and the `RY_LLVM_EMIT_IMPL_RUST` CMake option — the Rust cdylib is now the only implementation. The locked `extern "C"` ABI surface (`include/ry/llvm_emit/api.h`, `cast_helpers.hpp`) is unchanged. (#1993)
