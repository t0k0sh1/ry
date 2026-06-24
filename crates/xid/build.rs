// Mirrors `crates/native_base64/build.rs`. The cdylib has no undefined
// host symbols today (unicode-ident is self-contained), but keeping the
// link arg matches the runtime ABI posture and leaves room for future
// host-shim use without a second build.rs edit.
fn main() {
    let target_os = std::env::var("CARGO_CFG_TARGET_OS").unwrap_or_default();
    if target_os == "macos" {
        println!("cargo:rustc-cdylib-link-arg=-Wl,-undefined,dynamic_lookup");
    }
}
