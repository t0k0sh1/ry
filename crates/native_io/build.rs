// On macOS, the cdylib resolves host runtime symbols (`__ry_host_make_string`,
// `__ry_host_make_byte_list`, `__ry_host_make_empty_io_list`,
// `__ry_arc_alloc_counted`, `__ry_set_last_error`) from the host `ry`
// process at runtime. `-undefined dynamic_lookup` defers those unresolved
// symbols to runtime lookup from the host. Linux is satisfied by the host
// binary's `-rdynamic` link option (set on `ry` and `ry_tests`).
fn main() {
    let target_os = std::env::var("CARGO_CFG_TARGET_OS").unwrap_or_default();
    if target_os == "macos" {
        println!("cargo:rustc-cdylib-link-arg=-Wl,-undefined,dynamic_lookup");
    }
}
