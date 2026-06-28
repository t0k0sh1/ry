//! Pure / FFI-side primitives that back the `abi.rs` entry points.
//!
//! Everything here mirrors a specific span of the deleted
//! `src/runtime/native/io.cpp`:
//!
//! * `MAX_READ_SIZE` ← `io.cpp:39`
//! * `open_nofollow` ← `io.cpp:41-54`
//! * `read_line_via_getline` / `ReadLineOutcome` ← `io.cpp:63-82` and
//!   `io.cpp:372-396` (shared shape, used by both stdin and File-
//!   handle read_line variants)
//! * `read_all_growing` ← `io.cpp:98-119` (stdin growable read)
//! * `read_seek_or_chunk` ← `io.cpp:323-368` (file_read_all: seek
//!   fast path + chunk fallback)
//! * `alloc_file_handle` ← `arc_alloc(sizeof(IoFileHandle))` at
//!   `io.cpp:318` (header-stamping mirrors
//!   `include/ry/runtime/core/arc.hpp:21-31`)
//! * `Mode` ← the mode-string lookup in `fopen_nofollow`
//!
//! No `set_last_error` calls here: error formatting is the abi layer's
//! job (it owns the `Result<Ok, Err(String)>` shape that calls
//! `lib::set_last_error`). These primitives return `Option` /
//! `Result<_, IoErr>` so the caller can format context-rich messages
//! that match the C++ wording byte-for-byte.

use std::ffi::CStr;
use std::ffi::c_char;

use crate::ffi::{self, IoFileHandle, ARC_HEADER_SIZE};

/// Hard upper bound on bytes a single read_text / read_bytes /
/// file_read_all call may return. Matches `io.cpp:39` (`256 * 1024 *
/// 1024`); the issue (#2480) explicitly keeps the value identical to
/// the C++ side — any retuning is a separate issue.
pub const MAX_READ_SIZE: i64 = 256 * 1024 * 1024;

/// Mode triplet recognised by `open_nofollow`. The text vs binary
/// distinction is a no-op on POSIX (where the C++ code already runs)
/// but we still pass the original mode string through to `fdopen` so
/// that any future platform that distinguishes them (or a `setbuf`
/// difference triggered by the "b" flag) stays byte-identical to the
/// C++ behaviour.
#[derive(Clone, Copy)]
pub enum Mode {
    /// `"r"` or `"rb"` — read-only, file must exist.
    Read,
    /// `"w"` or `"wb"` — write-only, truncate on open, create if
    /// missing (0644).
    Write,
    /// `"a"` or `"ab"` — write-only append, create if missing (0644).
    Append,
}

impl Mode {
    fn open_flags(self) -> libc::c_int {
        let access = match self {
            Mode::Read => libc::O_RDONLY,
            Mode::Write => libc::O_WRONLY | libc::O_CREAT | libc::O_TRUNC,
            Mode::Append => libc::O_WRONLY | libc::O_CREAT | libc::O_APPEND,
        };
        // `O_NOFOLLOW` rejects symlinks at open(2) time — io.cpp:42
        // applies this to every open path, including the file-handle
        // API (per #2480 dual-write rationale: cross-module callers
        // like json::load_file rely on this same posture).
        libc::O_NOFOLLOW | access
    }
}

/// Open `path` with `O_NOFOLLOW` and the file-mode bits derived from
/// `mode`, then wrap the resulting fd in a `FILE *` via `fdopen`.
///
/// Returns `None` on any failure (open, fdopen). The caller is
/// responsible for `set_last_error` formatting in the err arm; this
/// helper never touches the error channel so it stays reusable from
/// non-Result callers (none currently — but the discipline matches
/// `io.cpp:41-54`).
///
/// # Safety
/// `path_cstr` and `mode_cstr` must be NUL-terminated C strings whose
/// pointers remain valid for the duration of this call.
pub unsafe fn open_nofollow(
    path_cstr: *const c_char,
    mode_cstr: *const c_char,
    mode: Mode,
) -> *mut libc::FILE {
    let flags = mode.open_flags();
    // libc::open is variadic — the third arg is consulted only when
    // O_CREAT is set, which it is for Mode::Write / Mode::Append. For
    // Mode::Read the third arg is ignored. 0o644 (rw-r--r--) matches
    // io.cpp:49.
    let fd = libc::open(path_cstr, flags, 0o644 as libc::c_int);
    if fd < 0 {
        return std::ptr::null_mut();
    }
    let fp = libc::fdopen(fd, mode_cstr);
    if fp.is_null() {
        libc::close(fd);
        return std::ptr::null_mut();
    }
    fp
}

/// Outcome of one `read_line_via_getline` call.
pub enum ReadLineOutcome {
    /// Line read successfully. `bytes` is the trailing-`\n`-stripped
    /// line payload (binary-safe — embedded NUL bytes are preserved).
    Line(Vec<u8>),
    /// `getline` returned -1 and `feof(stream)` is true.
    Eof,
    /// `getline` returned -1 and `feof(stream)` is false.
    IoError,
}

/// Read one line from `stream` via `libc::getline`. Strips the trailing
/// `\n` if present (mirrors `io.cpp:78` and `io.cpp:392`). The libc
/// buffer (`line`) is freed via `libc::free` whether `getline`
/// succeeded or not — using `Vec::from_raw_parts` would route the free
/// through Rust's allocator and trip ASan, so we copy the bytes into a
/// fresh `Vec` before freeing.
///
/// # Safety
/// `stream` must be a valid `FILE *` opened for reading.
pub unsafe fn read_line_via_getline(stream: *mut libc::FILE) -> ReadLineOutcome {
    let mut line: *mut c_char = std::ptr::null_mut();
    let mut cap: libc::size_t = 0;
    let nread = libc::getline(&mut line, &mut cap, stream);
    if nread < 0 {
        if !line.is_null() {
            libc::free(line.cast::<libc::c_void>());
        }
        if libc::feof(stream) != 0 {
            return ReadLineOutcome::Eof;
        }
        return ReadLineOutcome::IoError;
    }
    // Strip trailing newline (matches io.cpp:78 / io.cpp:392 —
    // `if (nread > 0 && line[nread-1] == '\n') --nread`).
    let mut len = nread as usize;
    if len > 0 && *line.add(len - 1) == b'\n' as c_char {
        len -= 1;
    }
    let bytes = std::slice::from_raw_parts(line.cast::<u8>(), len).to_vec();
    libc::free(line.cast::<libc::c_void>());
    ReadLineOutcome::Line(bytes)
}

/// Read all bytes available on `stream` into a `Vec<u8>`, doubling
/// the buffer capacity on every refill. Mirrors `io.cpp:98-119`
/// (`__ry_io_read_all`). `cap_limit` enforces an upper bound when
/// `Some(...)` (used by `file_read_all` chunk fallback at
/// `io.cpp:359-363`); when `None`, the read grows without a limit
/// (matches the stdin reader — `io.cpp` does not bound stdin reads).
pub unsafe fn read_all_growing(
    stream: *mut libc::FILE,
    cap_limit: Option<i64>,
) -> Result<Vec<u8>, IoErr> {
    let mut buf = Vec::<u8>::with_capacity(4096);
    loop {
        // Reserve at least one more 4096-byte slot to keep doubling
        // semantics aligned with `io.cpp:104-108` (doubles `cap`
        // before reading).
        if buf.len() == buf.capacity() {
            let next = buf.capacity().saturating_mul(2).max(4096);
            buf.reserve(next - buf.capacity());
        }
        let space = buf.capacity() - buf.len();
        let dst = buf.as_mut_ptr().add(buf.len()).cast::<libc::c_void>();
        let n = libc::fread(dst, 1, space, stream);
        if n == 0 {
            break;
        }
        buf.set_len(buf.len() + n);
        if let Some(limit) = cap_limit {
            if buf.len() as i64 > limit {
                return Err(IoErr::TooLargeNoSize);
            }
        }
    }
    Ok(buf)
}

/// Marker for `read_seek_or_chunk` / `read_all_growing` failure modes
/// that the abi layer translates into byte-identical C++ error
/// messages.
///
/// The seek-fast-path discovers the file's full size up-front and can
/// surface it in the error message (matches io.cpp:335 wording —
/// `"readAll: file too large (%ld bytes, max %ld)"`); the chunk
/// fallback only knows that the accumulator crossed the cap (matches
/// io.cpp:361 wording — `"readAll: file too large (max %ld bytes)"`).
/// The two variants keep that wording difference visible to the abi
/// layer rather than collapsing both messages into the chunk-shape.
pub enum IoErr {
    /// Seek-fast-path detected `size > MAX_READ_SIZE` before reading;
    /// `size` is the value `ftell` returned at SEEK_END.
    TooLargeWithSize(i64),
    /// Chunk fallback accumulator exceeded `MAX_READ_SIZE` — exact
    /// file size unknown (non-seekable handle).
    TooLargeNoSize,
}

/// Read from `stream`'s current cursor to EOF using the same two-path
/// shape as `__ry_io_file_read_all` (`io.cpp:323-368`):
///
/// 1. Seek-based fast path: `ftell` → `fseek(SEEK_END)` → `ftell` to
///    get `size` → `fseek(start)` → single `fread`. Used for regular
///    files; enforces `MAX_READ_SIZE`.
/// 2. Chunk fallback: 4096-byte chunks with doubling capacity, used
///    when seek fails (pipes, sockets, non-seekable fds). Also
///    enforces `MAX_READ_SIZE`.
///
/// # Safety
/// `stream` must be a valid `FILE *` opened for reading.
pub unsafe fn read_seek_or_chunk(stream: *mut libc::FILE) -> Result<Vec<u8>, IoErr> {
    // Seek-based fast path. The C++ code computes `start = ftell()`,
    // then `fseek(SEEK_END)` to compute `size = ftell() - start`,
    // then `fseek(start)` and one `fread`. We follow the same branch
    // shape so the I/O syscall pattern stays observably the same
    // (matters for strace-driven diff testing).
    let start = libc::ftell(stream);
    if start >= 0 && libc::fseek(stream, 0, libc::SEEK_END) == 0 {
        let end = libc::ftell(stream);
        let size = end - start;
        if size > MAX_READ_SIZE {
            return Err(IoErr::TooLargeWithSize(size));
        }
        if size >= 0 {
            libc::fseek(stream, start, libc::SEEK_SET);
            let mut buf = Vec::<u8>::with_capacity(size as usize);
            let n = libc::fread(
                buf.as_mut_ptr().cast::<libc::c_void>(),
                1,
                size as libc::size_t,
                stream,
            );
            buf.set_len(n);
            return Ok(buf);
        }
    }
    // Chunk-based fallback for non-seekable handles. Re-uses the
    // growable read with the same `MAX_READ_SIZE` cap.
    read_all_growing(stream, Some(MAX_READ_SIZE))
}

/// Allocate an ARC-managed `IoFileHandle` block holding `fp`. Returns
/// a pointer to the data area (past the ARC header) — what the C++
/// `arc_alloc(sizeof(IoFileHandle))` returns at `io.cpp:318`.
///
/// Stamps `strong_count = 1`, `weak_count = 0` exactly like the inline
/// `arc_alloc` in `include/ry/runtime/core/arc.hpp:21-31`. The ARC
/// machinery on the host side calls `__ry_io_file_cleanup` (which
/// closes the FILE*) and then `__ry_arc_free_counted(block)` when the
/// last reference drops — Rust does not own the destructor side.
///
/// # Safety
/// `fp` may be null (`open_nofollow` returns null on failure); when
/// non-null, the returned handle takes ownership of the FILE* until
/// either `file_close` / `file_cleanup` closes it explicitly or the
/// ARC machinery's cleanup hook runs.
pub unsafe fn alloc_file_handle(fp: *mut libc::FILE) -> *mut IoFileHandle {
    let total =
        (ARC_HEADER_SIZE + std::mem::size_of::<IoFileHandle>()) as i64;
    let block = ffi::__ry_arc_alloc_counted(total);
    if block.is_null() {
        // The host shim aborts the process via `checked_malloc` on
        // OOM (see `src/runtime/core/alloc.cpp`), so a null return
        // means something is critically wrong — but defensive null
        // check costs nothing and avoids UB if the contract ever
        // relaxes.
        return std::ptr::null_mut();
    }
    // Stamp the ARC header (mirrors arc.hpp:27-29).
    let header = block.cast::<i64>();
    header.write(1); // strong_count
    header.offset(1).write(0); // weak_count
    // Data area starts past the header.
    let handle = block.cast::<u8>().add(ARC_HEADER_SIZE).cast::<IoFileHandle>();
    std::ptr::write(handle, IoFileHandle { fp });
    handle
}

/// Inspect a Ry str handle's bytes without allocating. Returns an empty
/// slice when `handle` is null or when its `byte_len` reads as zero;
/// otherwise returns a slice covering the `byte_len` payload bytes.
///
/// # Safety
/// `handle` must be either null or a valid Ry str handle produced by
/// `makeString` / `makeStringUninit` / `__ry_host_make_string`.
pub unsafe fn ry_str_bytes<'a>(handle: *const c_char) -> &'a [u8] {
    if handle.is_null() {
        return &[];
    }
    let byte_len = ffi::string_byte_len(handle);
    if byte_len <= 0 {
        return &[];
    }
    std::slice::from_raw_parts(handle.cast::<u8>(), byte_len as usize)
}

/// Pretty-print a path byte slice for inclusion in error messages
/// (matches the `'%s'` format specifier the C++ side uses). Any
/// non-UTF-8 byte becomes `U+FFFD` — strictly worse than the C++
/// `printf` which would just emit the raw bytes, but the alternative
/// of emitting raw bytes from Rust into the shared error buffer would
/// invite test-tooling issues (and the existing spec tests only check
/// for ASCII path substrings, so this lossy conversion does not
/// regress any test).
pub fn path_lossy(bytes: &[u8]) -> std::borrow::Cow<'_, str> {
    String::from_utf8_lossy(bytes)
}

/// Read-only marker for `string_byte_len` so the abi layer can stay
/// allocation-free for the common path.
pub use crate::ffi::string_byte_len as ry_string_byte_len;

/// Convenience CStr accessor for callers that pass a NUL-terminated C
/// string (mode strings are always `"r"` / `"w"` / etc. at the abi
/// layer).
pub unsafe fn cstr(bytes_with_nul: &[u8]) -> &CStr {
    CStr::from_bytes_with_nul_unchecked(bytes_with_nul)
}
