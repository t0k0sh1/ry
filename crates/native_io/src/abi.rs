//! C boundary for the @native("io") stdlib module — 19 `extern "C"`
//! symbols consumed by `emitGenericNativeCall` driven by
//! `share/std/io/io.ry` descriptors. The deleted `src/runtime/native/
//! io.cpp` body for each symbol is reproduced here line-for-line in
//! behaviour (return sentinels, error wording, MAX_READ_SIZE
//! enforcement, `O_NOFOLLOW` posture, tri-state return contract,
//! IoFileHandle close idempotency); see the per-function comments for
//! the matching `io.cpp` line ranges.
//!
//! Return sentinel quick-reference (the C++ side is inconsistent — be
//! careful when copy-pasting symbol bodies):
//!
//! * tri-state read_line family (`__ry_io_read_line`,
//!   `__ry_io_input_prompt`, `__ry_io_file_read_line`):
//!     `0` = line read (writes `*out_line`), `1` = EOF, `-1` = error.
//! * `*const c_char` returns (`read_all`, `read_text`, `bytes_to_str`,
//!   `file_read_all`, `get_last_error`):
//!     non-null Ry str handle on success, `nullptr` + `set_last_error`
//!     on failure.
//! * `*mut c_void` returns (`read_bytes`, `to_bytes`, `file_open`):
//!     non-null IOListHeader* / IoFileHandle* on success, `nullptr` +
//!     `set_last_error` on failure.
//! * **`1`-on-error** family (path-shaped): `write_text`,
//!   `append_text`, `write_bytes`, `delete_file`.
//! * **`-1`-on-error** family (handle-shaped): `file_write_text`
//!   (mirrors `io.cpp:402` / `io.cpp:408`). Do NOT normalise to `1`.
//! * `exists`: `1` true / `0` false. No `set_last_error` call (matches
//!   `io.cpp:200-201` — bool shape, embedded-NUL path falls through to
//!   false silently).
//! * `file_close` / `file_cleanup`: `void`, idempotent on null handle
//!   and null `fp`.

use std::ffi::c_char;
use std::ffi::c_void;

use crate::core::{
    self, alloc_file_handle, cstr, open_nofollow, read_all_growing,
    read_line_via_getline, read_seek_or_chunk, ry_str_bytes, IoErr, Mode, ReadLineOutcome,
    MAX_READ_SIZE,
};
use crate::ffi::{self, IOListHeader, IoFileHandle};
use crate::{catch_or_exit, get_last_error_handle, set_last_error};

// ── Helpers ───────────────────────────────────────────────────────────────

/// Detect an embedded NUL byte inside a Ry str handle's payload.
/// Mirrors `hasEmbeddedNul` in `include/ry/runtime/core/string.hpp:112`
/// (which iterates the `byte_len` payload bytes looking for `\0`). Used
/// at the start of every path-taking entry point to short-circuit
/// before the path reaches `libc::open` (which would interpret the
/// truncated string up to the first NUL — silently opening the wrong
/// file).
///
/// # Safety
/// `handle` must be null or a Ry str handle.
unsafe fn has_embedded_nul(handle: *const c_char) -> bool {
    if handle.is_null() {
        return false;
    }
    ry_str_bytes(handle).contains(&0)
}

/// Return an empty Ry str handle. Used when a function would otherwise
/// return a handle for an empty payload (zero-byte stdin / zero-byte
/// file). Mirrors `makeString("", 0)` — host shim ensures the returned
/// pointer is non-null and points past a valid StringHeader.
unsafe fn empty_str_handle() -> *const c_char {
    ffi::__ry_host_make_string(c"".as_ptr(), 0)
}

/// Return an empty Ry `List<u8>` handle. Wraps `makeEmptyIOList` so
/// the `read_bytes` / `to_bytes` zero-byte paths do not pass a
/// dangling `Vec::as_ptr()` to `__ry_host_make_byte_list` (which
/// would be UB even though the host's `memcpy` would no-op with
/// `len = 0`).
unsafe fn empty_byte_list() -> *mut c_void {
    ffi::__ry_host_make_empty_io_list()
}

/// Build a Ry str handle from a byte slice. Wraps
/// `__ry_host_make_string` so `bytes` of any length (including 0)
/// always lands as a valid handle.
unsafe fn ry_str_from_bytes(bytes: &[u8]) -> *const c_char {
    ffi::__ry_host_make_string(bytes.as_ptr().cast::<c_char>(), bytes.len() as i64)
}

/// Build a Ry `List<u8>` handle from a byte slice, routing `len == 0`
/// through the empty-list shim so `__ry_host_make_byte_list` always
/// sees a non-null `bytes` pointer.
unsafe fn ry_bytes_from_slice(bytes: &[u8]) -> *mut c_void {
    if bytes.is_empty() {
        return empty_byte_list();
    }
    ffi::__ry_host_make_byte_list(bytes.as_ptr(), bytes.len() as i64)
}

// ── Error channel ─────────────────────────────────────────────────────────

#[no_mangle]
pub unsafe extern "C" fn __ry_io_get_last_error() -> *const c_char {
    catch_or_exit(|| get_last_error_handle())
}

// ── Stdin (`io.cpp:63-119`) ───────────────────────────────────────────────

/// Mirrors `io.cpp:63-82`. Tri-state: `0` = line read, `1` = EOF,
/// `-1` = error. On success, writes a Ry str handle through
/// `out_line`; on EOF / error, leaves `*out_line = nullptr`.
#[no_mangle]
pub unsafe extern "C" fn __ry_io_read_line(out_line: *mut *const c_char) -> i64 {
    catch_or_exit(|| {
        if out_line.is_null() {
            set_last_error("readLine: output pointer is null");
            return -1;
        }
        *out_line = std::ptr::null();
        let stream = ry_stdin();
        match read_line_via_getline(stream) {
            ReadLineOutcome::Line(bytes) => {
                *out_line = ry_str_from_bytes(&bytes);
                0
            }
            ReadLineOutcome::Eof => 1,
            ReadLineOutcome::IoError => {
                set_last_error("readLine: I/O error reading from stdin");
                -1
            }
        }
    })
}

/// Mirrors `io.cpp:88-96`. Optional prompt → stdout (fflush), then
/// delegate to `__ry_io_read_line`. Stdout write failures are
/// intentionally swallowed (matches the C++ comment at io.cpp:84-87 —
/// a broken stdout should not kill an interactive read).
#[no_mangle]
pub unsafe extern "C" fn __ry_io_input_prompt(
    prompt: *const c_char,
    out_line: *mut *const c_char,
) -> i64 {
    catch_or_exit(|| {
        if !prompt.is_null() {
            let prompt_bytes = ry_str_bytes(prompt);
            if !prompt_bytes.is_empty() {
                libc::fwrite(
                    prompt_bytes.as_ptr().cast::<c_void>(),
                    1,
                    prompt_bytes.len() as libc::size_t,
                    ry_stdout(),
                );
            }
            libc::fflush(ry_stdout());
        }
        __ry_io_read_line(out_line)
    })
}

/// Mirrors `io.cpp:98-119`. Reads all of stdin into a growable Vec;
/// no `MAX_READ_SIZE` cap (C++ does not bound stdin reads either —
/// only file reads are bounded).
#[no_mangle]
pub unsafe extern "C" fn __ry_io_read_all() -> *const c_char {
    catch_or_exit(|| {
        // `read_all_growing` returns `Err(TooLargeNoSize)` only when
        // given a cap; passing `None` here means the only error path
        // is panic-via-allocation, which would already trap.
        let buf = match read_all_growing(ry_stdin(), None) {
            Ok(buf) => buf,
            Err(_) => unreachable!("stdin read has no cap"),
        };
        ry_str_from_bytes(&buf)
    })
}

// ── File I/O — path-shaped (`io.cpp:123-271`) ─────────────────────────────

/// Mirrors `io.cpp:123-157`.
#[no_mangle]
pub unsafe extern "C" fn __ry_io_read_text(path: *const c_char) -> *const c_char {
    catch_or_exit(|| {
        if path.is_null() {
            set_last_error("read_text: path is null");
            return std::ptr::null();
        }
        if has_embedded_nul(path) {
            set_last_error("read_text: argument contains an embedded NUL byte");
            return std::ptr::null();
        }
        let fp = open_nofollow(path, c"r".as_ptr(), Mode::Read);
        if fp.is_null() {
            set_last_error(&format!(
                "cannot open file '{}' for reading",
                core::path_lossy(ry_str_bytes(path))
            ));
            return std::ptr::null();
        }
        let result = read_text_via_seek(fp, path);
        libc::fclose(fp);
        result
    })
}

/// Inner: seek to end → size check → seek to start → fread. Mirrors
/// `io.cpp:133-156`. Caller closes the FILE*.
unsafe fn read_text_via_seek(fp: *mut libc::FILE, path: *const c_char) -> *const c_char {
    if libc::fseek(fp, 0, libc::SEEK_END) != 0 {
        set_last_error(&format!(
            "cannot seek file '{}'",
            core::path_lossy(ry_str_bytes(path))
        ));
        return std::ptr::null();
    }
    let size = libc::ftell(fp);
    if size < 0 {
        set_last_error(&format!(
            "cannot determine size of file '{}'",
            core::path_lossy(ry_str_bytes(path))
        ));
        return std::ptr::null();
    }
    if size > MAX_READ_SIZE {
        set_last_error(&format!(
            "file '{}' is too large ({} bytes, max {})",
            core::path_lossy(ry_str_bytes(path)),
            size,
            MAX_READ_SIZE,
        ));
        return std::ptr::null();
    }
    libc::fseek(fp, 0, libc::SEEK_SET);
    let mut buf = Vec::<u8>::with_capacity(size as usize);
    let n = libc::fread(
        buf.as_mut_ptr().cast::<c_void>(),
        1,
        size as libc::size_t,
        fp,
    );
    buf.set_len(n);
    ry_str_from_bytes(&buf)
}

/// Mirrors `io.cpp:159-177`. Returns `0` on success, `1` on error.
#[no_mangle]
pub unsafe extern "C" fn __ry_io_write_text(path: *const c_char, content: *const c_char) -> i64 {
    catch_or_exit(|| write_text_inner(path, content, Mode::Write, "write_text", "writing", "write"))
}

/// Mirrors `io.cpp:179-197`. Returns `0` on success, `1` on error.
#[no_mangle]
pub unsafe extern "C" fn __ry_io_append_text(path: *const c_char, content: *const c_char) -> i64 {
    catch_or_exit(|| {
        write_text_inner(
            path,
            content,
            Mode::Append,
            "append_text",
            "appending",
            "append",
        )
    })
}

/// Shared body of `write_text` / `append_text`. The only differences
/// are the mode flag and the three substring fragments in the error
/// messages — the C++ code at io.cpp:159-197 inlines both bodies, but
/// extracting them here cuts the symbol-body line count without
/// changing observable behaviour (every error string matches the C++
/// wording byte-for-byte).
unsafe fn write_text_inner(
    path: *const c_char,
    content: *const c_char,
    mode: Mode,
    label: &str,
    gerund: &str,
    verb: &str,
) -> i64 {
    if path.is_null() {
        set_last_error(&format!("{}: path is null", label));
        return 1;
    }
    if has_embedded_nul(path) {
        set_last_error(&format!("{}: argument contains an embedded NUL byte", label));
        return 1;
    }
    let mode_cstr = match mode {
        Mode::Write => c"w".as_ptr(),
        Mode::Append => c"a".as_ptr(),
        Mode::Read => unreachable!("write_text_inner mode must be Write or Append"),
    };
    let fp = open_nofollow(path, mode_cstr, mode);
    if fp.is_null() {
        set_last_error(&format!(
            "cannot open file '{}' for {}",
            core::path_lossy(ry_str_bytes(path)),
            gerund,
        ));
        return 1;
    }
    let content_bytes = ry_str_bytes(content);
    let written = libc::fwrite(
        content_bytes.as_ptr().cast::<c_void>(),
        1,
        content_bytes.len() as libc::size_t,
        fp,
    );
    let close_rc = libc::fclose(fp);
    if written != content_bytes.len() || close_rc != 0 {
        set_last_error(&format!(
            "failed to {} to file '{}'",
            verb,
            core::path_lossy(ry_str_bytes(path)),
        ));
        return 1;
    }
    0
}

/// Mirrors `io.cpp:199-202`. `1` = exists, `0` = not. No
/// `set_last_error` (bool shape — embedded-NUL path silently returns
/// `0`, matching io.cpp:200).
#[no_mangle]
pub unsafe extern "C" fn __ry_io_exists(path: *const c_char) -> i64 {
    catch_or_exit(|| {
        if path.is_null() || has_embedded_nul(path) {
            return 0;
        }
        if libc::access(path, libc::F_OK) == 0 {
            1
        } else {
            0
        }
    })
}

/// Mirrors `io.cpp:204-214`.
#[no_mangle]
pub unsafe extern "C" fn __ry_io_delete_file(path: *const c_char) -> i64 {
    catch_or_exit(|| {
        if path.is_null() {
            set_last_error("delete_file: path is null");
            return 1;
        }
        if has_embedded_nul(path) {
            set_last_error("delete_file: argument contains an embedded NUL byte");
            return 1;
        }
        if libc::remove(path) != 0 {
            set_last_error(&format!(
                "cannot delete file '{}'",
                core::path_lossy(ry_str_bytes(path))
            ));
            return 1;
        }
        0
    })
}

/// Mirrors `io.cpp:216-251`.
#[no_mangle]
pub unsafe extern "C" fn __ry_io_read_bytes(path: *const c_char) -> *mut c_void {
    catch_or_exit(|| {
        if path.is_null() {
            set_last_error("read_bytes: path is null");
            return std::ptr::null_mut();
        }
        if has_embedded_nul(path) {
            set_last_error("read_bytes: argument contains an embedded NUL byte");
            return std::ptr::null_mut();
        }
        let fp = open_nofollow(path, c"rb".as_ptr(), Mode::Read);
        if fp.is_null() {
            set_last_error(&format!(
                "cannot open file '{}' for reading",
                core::path_lossy(ry_str_bytes(path))
            ));
            return std::ptr::null_mut();
        }
        let result = read_bytes_via_seek(fp, path);
        libc::fclose(fp);
        result
    })
}

unsafe fn read_bytes_via_seek(fp: *mut libc::FILE, path: *const c_char) -> *mut c_void {
    if libc::fseek(fp, 0, libc::SEEK_END) != 0 {
        set_last_error(&format!(
            "cannot seek file '{}'",
            core::path_lossy(ry_str_bytes(path))
        ));
        return std::ptr::null_mut();
    }
    let size = libc::ftell(fp);
    if size < 0 {
        set_last_error(&format!(
            "cannot determine size of file '{}'",
            core::path_lossy(ry_str_bytes(path))
        ));
        return std::ptr::null_mut();
    }
    if size > MAX_READ_SIZE {
        set_last_error(&format!(
            "file '{}' is too large ({} bytes, max {})",
            core::path_lossy(ry_str_bytes(path)),
            size,
            MAX_READ_SIZE,
        ));
        return std::ptr::null_mut();
    }
    libc::fseek(fp, 0, libc::SEEK_SET);
    let mut buf = Vec::<u8>::with_capacity(size as usize);
    let n = libc::fread(
        buf.as_mut_ptr().cast::<c_void>(),
        1,
        size as libc::size_t,
        fp,
    );
    buf.set_len(n);
    ry_bytes_from_slice(&buf)
}

/// Mirrors `io.cpp:253-271`. Returns `0` on success, `1` on error.
#[no_mangle]
pub unsafe extern "C" fn __ry_io_write_bytes(path: *const c_char, list: *mut c_void) -> i64 {
    catch_or_exit(|| {
        if path.is_null() {
            set_last_error("write_bytes: path is null");
            return 1;
        }
        if has_embedded_nul(path) {
            set_last_error("write_bytes: argument contains an embedded NUL byte");
            return 1;
        }
        if list.is_null() {
            set_last_error("write_bytes: list is null");
            return 1;
        }
        let header = &*list.cast::<IOListHeader>();
        let fp = open_nofollow(path, c"wb".as_ptr(), Mode::Write);
        if fp.is_null() {
            set_last_error(&format!(
                "cannot open file '{}' for writing",
                core::path_lossy(ry_str_bytes(path))
            ));
            return 1;
        }
        let written = if header.len > 0 {
            libc::fwrite(
                header.data.cast::<c_void>(),
                1,
                header.len as libc::size_t,
                fp,
            )
        } else {
            0
        };
        libc::fclose(fp);
        if written as i64 != header.len {
            set_last_error(&format!(
                "failed to write all bytes to '{}'",
                core::path_lossy(ry_str_bytes(path))
            ));
            return 1;
        }
        0
    })
}

// ── Byte conversions (`io.cpp:275-283`) ───────────────────────────────────

/// Mirrors `io.cpp:275-277`. Returns an empty IOListHeader for
/// null / empty input (route through the empty-list shim per
/// advisor #2480 review — `ry_bytes_from_slice` handles this).
#[no_mangle]
pub unsafe extern "C" fn __ry_io_to_bytes(s: *const c_char) -> *mut c_void {
    catch_or_exit(|| {
        let bytes = ry_str_bytes(s);
        ry_bytes_from_slice(bytes)
    })
}

/// Mirrors `io.cpp:279-283`.
#[no_mangle]
pub unsafe extern "C" fn __ry_io_bytes_to_str(list: *mut c_void) -> *const c_char {
    catch_or_exit(|| {
        if list.is_null() {
            return empty_str_handle();
        }
        let header = &*list.cast::<IOListHeader>();
        if header.len <= 0 {
            return empty_str_handle();
        }
        ffi::__ry_host_make_string(header.data.cast::<c_char>(), header.len)
    })
}

// ── File handle API (`io.cpp:287-428`) ────────────────────────────────────

/// Mirrors `io.cpp:291-321`. The mode whitelist runs after the NUL
/// safety checks (matches io.cpp ordering at lines 300-312).
#[no_mangle]
pub unsafe extern "C" fn __ry_io_file_open(
    path: *const c_char,
    mode: *const c_char,
) -> *mut c_void {
    catch_or_exit(|| {
        if path.is_null() {
            set_last_error("open: path is null");
            return std::ptr::null_mut();
        }
        if mode.is_null() {
            set_last_error("open: mode is null");
            return std::ptr::null_mut();
        }
        if has_embedded_nul(path) {
            set_last_error("open: path contains an embedded NUL byte");
            return std::ptr::null_mut();
        }
        if has_embedded_nul(mode) {
            set_last_error("open: mode contains an embedded NUL byte");
            return std::ptr::null_mut();
        }
        let mode_bytes = ry_str_bytes(mode);
        let resolved = match mode_bytes {
            b"r" | b"rb" => Mode::Read,
            b"w" | b"wb" => Mode::Write,
            b"a" | b"ab" => Mode::Append,
            _ => {
                set_last_error(&format!(
                    "open: invalid mode '{}' (must be \"r\", \"w\", \"a\", \"rb\", \"wb\", or \"ab\")",
                    core::path_lossy(mode_bytes),
                ));
                return std::ptr::null_mut();
            }
        };
        let fp = open_nofollow(path, mode, resolved);
        if fp.is_null() {
            set_last_error(&format!(
                "open: cannot open '{}' in mode '{}'",
                core::path_lossy(ry_str_bytes(path)),
                core::path_lossy(mode_bytes),
            ));
            return std::ptr::null_mut();
        }
        alloc_file_handle(fp).cast::<c_void>()
    })
}

/// Mirrors `io.cpp:323-368`. Seek-fast-path + chunk fallback live in
/// `core::read_seek_or_chunk`; the abi layer here owns the
/// handle-shape error wording and the `MAX_READ_SIZE` message
/// (different from the path-based version because the message has no
/// path interpolation).
#[no_mangle]
pub unsafe extern "C" fn __ry_io_file_read_all(handle: *mut c_void) -> *const c_char {
    catch_or_exit(|| {
        let h = handle.cast::<IoFileHandle>();
        if h.is_null() || (*h).fp.is_null() {
            set_last_error("readAll: file handle is not open");
            return std::ptr::null();
        }
        match read_seek_or_chunk((*h).fp) {
            Ok(buf) => ry_str_from_bytes(&buf),
            // Seek-fast-path knows the actual size — surface it
            // (matches io.cpp:335 wording).
            Err(IoErr::TooLargeWithSize(size)) => {
                set_last_error(&format!(
                    "readAll: file too large ({} bytes, max {})",
                    size, MAX_READ_SIZE
                ));
                std::ptr::null()
            }
            // Chunk fallback only knows the cap was exceeded
            // (matches io.cpp:361 wording).
            Err(IoErr::TooLargeNoSize) => {
                set_last_error(&format!(
                    "readAll: file too large (max {} bytes)",
                    MAX_READ_SIZE
                ));
                std::ptr::null()
            }
        }
    })
}

/// Mirrors `io.cpp:372-396`. Same tri-state shape as
/// `__ry_io_read_line` but reads from the handle's FILE* instead of
/// stdin.
#[no_mangle]
pub unsafe extern "C" fn __ry_io_file_read_line(
    handle: *mut c_void,
    out_line: *mut *const c_char,
) -> i64 {
    catch_or_exit(|| {
        if out_line.is_null() {
            set_last_error("readLine: output pointer is null");
            return -1;
        }
        *out_line = std::ptr::null();
        let h = handle.cast::<IoFileHandle>();
        if h.is_null() || (*h).fp.is_null() {
            set_last_error("readLine: file handle is not open");
            return -1;
        }
        match read_line_via_getline((*h).fp) {
            ReadLineOutcome::Line(bytes) => {
                *out_line = ry_str_from_bytes(&bytes);
                0
            }
            ReadLineOutcome::Eof => 1,
            ReadLineOutcome::IoError => {
                set_last_error("readLine: I/O error reading file");
                -1
            }
        }
    })
}

/// Mirrors `io.cpp:398-411`. **Error sentinel is `-1`**, not `1`
/// (mirrors C++ io.cpp:402 and io.cpp:408 — do not normalise to the
/// path-shaped `1`).
#[no_mangle]
pub unsafe extern "C" fn __ry_io_file_write_text(handle: *mut c_void, s: *const c_char) -> i64 {
    catch_or_exit(|| {
        let h = handle.cast::<IoFileHandle>();
        if h.is_null() || (*h).fp.is_null() {
            set_last_error("writeText: file handle is not open");
            return -1;
        }
        let bytes = ry_str_bytes(s);
        let written = if !bytes.is_empty() {
            libc::fwrite(
                bytes.as_ptr().cast::<c_void>(),
                1,
                bytes.len() as libc::size_t,
                (*h).fp,
            )
        } else {
            0
        };
        if written != bytes.len() {
            set_last_error("writeText: failed to write to file");
            return -1;
        }
        0
    })
}

/// Mirrors `io.cpp:413-419`. Idempotent — null handle and null `fp`
/// short-circuit. ARC block is NOT freed here (that is the ARC
/// machinery's job, via `__ry_io_file_cleanup` + `__ry_arc_free_counted`).
#[no_mangle]
pub unsafe extern "C" fn __ry_io_file_close(handle: *mut c_void) {
    catch_or_exit(|| {
        if handle.is_null() {
            return;
        }
        let h = &mut *handle.cast::<IoFileHandle>();
        if !h.fp.is_null() {
            libc::fclose(h.fp);
            h.fp = std::ptr::null_mut();
        }
    })
}

/// Mirrors `io.cpp:421-428`. ARC destructor hook — same body as
/// `__ry_io_file_close` but conceptually the last call before
/// `__ry_arc_free_counted` runs. Idempotent.
#[no_mangle]
pub unsafe extern "C" fn __ry_io_file_cleanup(handle: *mut c_void) {
    catch_or_exit(|| {
        if handle.is_null() {
            return;
        }
        let h = &mut *handle.cast::<IoFileHandle>();
        if !h.fp.is_null() {
            libc::fclose(h.fp);
            h.fp = std::ptr::null_mut();
        }
        // ARC machinery calls `__ry_arc_free_counted` after this hook.
    })
}

// ── libc stdin/stdout cross-platform shim ─────────────────────────────────
//
// macOS and Linux name the libc-internal stdin / stdout symbols
// differently (`__stdinp` / `__stdoutp` on macOS, `stdin` / `stdout`
// extern globals on Linux). Both `libc::fdopen(0, "r")` and
// `libc::fdopen(1, "w")` would re-wrap the underlying fd but allocate a
// fresh buffer per call — wrong for stdin where the previous call's
// getline-internal buffer must persist across calls. We re-derive the
// pointers through libc's per-platform extern globals.
#[cfg(target_os = "macos")]
unsafe fn ry_stdin() -> *mut libc::FILE {
    extern "C" {
        static __stdinp: *mut libc::FILE;
    }
    __stdinp
}

#[cfg(target_os = "macos")]
unsafe fn ry_stdout() -> *mut libc::FILE {
    extern "C" {
        static __stdoutp: *mut libc::FILE;
    }
    __stdoutp
}

#[cfg(not(target_os = "macos"))]
unsafe fn ry_stdin() -> *mut libc::FILE {
    extern "C" {
        static mut stdin: *mut libc::FILE;
    }
    stdin
}

#[cfg(not(target_os = "macos"))]
unsafe fn ry_stdout() -> *mut libc::FILE {
    extern "C" {
        static mut stdout: *mut libc::FILE;
    }
    stdout
}

// Silence "unused import" if a platform never exercises a helper.
#[allow(dead_code)]
fn _keep_cstr_import_alive() -> &'static std::ffi::CStr {
    unsafe { cstr(b"\0") }
}
