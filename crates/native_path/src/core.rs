//! Pure-Rust POSIX path operations. No `unsafe`, no FFI, no allocation
//! shims. Byte-for-byte equivalent to `src/runtime/native/path.cpp`'s
//! `strip_trailing` / `find_base` / `join2_impl` / `basename` /
//! `dirname` / `ext` bodies.
//!
//! NUL-safety check (`hasEmbeddedNul` in C++) lives in `abi.rs` —
//! inputs reaching `core` are guaranteed NUL-free byte slices.

/// Strip trailing `/` bytes; root `"/"` always retains its single byte
/// (`while len > 1 && p[len-1] == '/'` — same guard as C++
/// `strip_trailing` in path.cpp:18-22).
fn strip_trailing(bytes: &[u8]) -> &[u8] {
    let mut len = bytes.len();
    while len > 1 && bytes[len - 1] == b'/' {
        len -= 1;
    }
    &bytes[..len]
}

/// Return the byte offset of the start of the final path component
/// within `bytes`. `bytes` should already have trailing slashes stripped
/// by the caller. Mirrors C++ `find_base` in path.cpp:26-32: walks
/// backward from the last byte until the previous byte is `/`, returning
/// the index of the first basename byte.
///
/// Returns `0` when `bytes` is empty or contains no `/`. If the basename
/// is preceded by a `/`, the returned offset points at the byte AFTER
/// that slash (i.e. the slash itself is excluded from the basename).
fn find_base_offset(bytes: &[u8]) -> usize {
    if bytes.is_empty() {
        return 0;
    }
    let mut s = bytes.len() - 1;
    while s > 0 && bytes[s - 1] != b'/' {
        s -= 1;
    }
    s
}

/// Join two path segments following POSIX semantics — byte-identical to
/// `join2_impl` in `src/runtime/native/path.cpp:39-65`:
///
/// - `a` empty → return `b.to_vec()`
/// - `b` empty → return `a.to_vec()`
/// - `b` starts with `/` (absolute second arg) → return `b.to_vec()`
/// - general → `strip_trailing(a)` + optional `/` separator + `b`
///   (separator inserted iff the stripped `a` does not already end in `/`)
///
/// Used directly by `__ry_path_join2` and chained for `join3` / `join4`
/// in `abi.rs`. The C++ uses host `makeString` / `makeStringUninit` for
/// intermediates and `freeStringSlot` to free them; the Rust port keeps
/// intermediates as `Vec<u8>` (Rust-drop) and only crosses the FFI
/// boundary once for the final result — no `freeStringSlot` host shim
/// is required.
pub fn join2(a: &[u8], b: &[u8]) -> Vec<u8> {
    if a.is_empty() {
        return b.to_vec();
    }
    if b.is_empty() {
        return a.to_vec();
    }
    if b[0] == b'/' {
        return b.to_vec();
    }

    let a_stripped = strip_trailing(a);
    // If a (after strip) ends with '/' (root path), don't insert an
    // extra separator: "/" + "tmp" → "/tmp", not "//tmp".
    let need_sep = a_stripped.last() != Some(&b'/');

    let mut out = Vec::with_capacity(a_stripped.len() + (need_sep as usize) + b.len());
    out.extend_from_slice(a_stripped);
    if need_sep {
        out.push(b'/');
    }
    out.extend_from_slice(b);
    out
}

/// Return the final path component as a borrowed slice. Byte-identical
/// to `__ry_path_basename` in `path.cpp:92-109`:
///
/// - `""` → `b""`
/// - `"/"` → `b""` (after `strip_trailing` reduces to `"/"`,
///   `len == 1 && first byte == '/'` → empty)
/// - general → `find_base_offset(stripped)`, skipping a leading `/`
pub fn basename(bytes: &[u8]) -> &[u8] {
    if bytes.is_empty() {
        return b"";
    }
    let stripped = strip_trailing(bytes);
    if stripped.len() == 1 && stripped[0] == b'/' {
        return b"";
    }
    let base_off = find_base_offset(stripped);
    let base = &stripped[base_off..];
    // If `base` starts with '/', skip it (C++ `if (*base == '/') base++`).
    if !base.is_empty() && base[0] == b'/' {
        &base[1..]
    } else {
        base
    }
}

/// Return the parent directory as a borrowed slice. Byte-identical to
/// `__ry_path_dirname` in `path.cpp:111-138`:
///
/// - `""` → `b"."`
/// - `"/"` → `b"/"` (slash reached `p`, `*slash == '/'`)
/// - `"file"` → `b"."` (slash reached `p`, `*slash != '/'`)
/// - `"/file"` → `b"/"` (dirname portion empty → root)
/// - general → trailing-stripped prefix up to (and not including) the
///   last `/`, with additional trailing slashes stripped from the
///   dirname portion
pub fn dirname(bytes: &[u8]) -> &[u8] {
    if bytes.is_empty() {
        return b".";
    }
    let stripped = strip_trailing(bytes);
    // Scan backward from the last byte for the last '/'.
    let mut slash = stripped.len() - 1;
    while slash > 0 && stripped[slash] != b'/' {
        slash -= 1;
    }

    if slash == 0 {
        return if stripped[0] == b'/' { b"/" } else { b"." };
    }

    // Strip trailing slashes from the dirname portion (handles
    // "/a/b///c" → dirname "/a/b").
    let mut dir_end = slash;
    while dir_end > 0 && stripped[dir_end - 1] == b'/' {
        dir_end -= 1;
    }

    if dir_end == 0 {
        return b"/";
    }

    &stripped[..dir_end]
}

/// Return the extension (including the leading `.`) as a borrowed
/// slice. Byte-identical to `__ry_path_ext` in `path.cpp:140-163`:
///
/// - `""` → `b""`
/// - `".gitignore"` → `b""` (dot at offset 0 of basename → hidden file,
///   no extension)
/// - `".config.json"` → `b".json"` (last dot wins, but offset-0 dot is
///   excluded)
/// - `"archive.tar.gz"` → `b".gz"`
/// - `"/usr/local.d/file"` → `b""` (dot in directory component, not
///   basename — basename is `"file"`)
pub fn ext(bytes: &[u8]) -> &[u8] {
    if bytes.is_empty() {
        return b"";
    }
    let stripped = strip_trailing(bytes);
    let base_off = find_base_offset(stripped);
    let base = &stripped[base_off..];

    // Scan forward through basename for the LAST dot (the C++ loops
    // forward and keeps overwriting `dot` to find the last one).
    let mut dot_off: Option<usize> = None;
    for (i, b) in base.iter().enumerate() {
        if *b == b'.' {
            dot_off = Some(i);
        }
    }

    match dot_off {
        // No dot, or dot at offset 0 (hidden file like ".gitignore").
        None | Some(0) => b"",
        Some(i) => &base[i..],
    }
}
