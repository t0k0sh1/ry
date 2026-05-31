// On macOS, the cdylib resolves both LLVM (`LLVMBuild*`, `LLVMConst*`,
// etc.) and `__ry_*` runtime symbols from the host process at runtime,
// mirroring the legacy `add_library(ry_llvm_emit SHARED ...)` target's
// `-undefined dynamic_lookup` linker flag (see CMakeLists.txt under
// option(RY_LLVM_EMIT_IMPL_RUST OFF)).
//
// Linux is satisfied by the host binary's `-rdynamic` link option (set
// on the `ry` and `ry_tests` executables); nothing extra is needed on
// the cdylib side.
fn main() {
    let target_os = std::env::var("CARGO_CFG_TARGET_OS").unwrap_or_default();
    if target_os == "macos" {
        println!("cargo:rustc-cdylib-link-arg=-Wl,-undefined,dynamic_lookup");
    }
}
