// On macOS, the cdylib resolves `ry_emit_*` symbols from the host process
// at runtime (the host loads the `emit` cdylib at startup; its symbols are
// then visible to this cdylib via the global dyld scope).
// `-undefined dynamic_lookup` defers those unresolved symbols to runtime
// lookup. Linux is satisfied by the host binary's `-rdynamic` link option
// (set on `ry` and `ry_tests`).
//
// Same shape as crates/emit/build.rs and crates/native_base64/build.rs.
fn main() {
    let target_os = std::env::var("CARGO_CFG_TARGET_OS").unwrap_or_default();
    if target_os == "macos" {
        println!("cargo:rustc-cdylib-link-arg=-Wl,-undefined,dynamic_lookup");
    }
}
