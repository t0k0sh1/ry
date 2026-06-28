//! Ry expression lowering bodies. The `#[no_mangle] extern "C"` entries
//! live in `abi.rs`; this module owns the Ry-semantic decisions (suffix →
//! type selection, range validation, error-channel population).
//!
//! Per the migration design (`docs/architecture/upper-codegen-migration.md`
//! §"Stage 1 — primitive literals"), the C++ shim still resolves the LLVM
//! type from the suffix string and passes it across the boundary as
//! `RyTypeRef`; the suffix is also passed as a `SuffixKind` enum so this
//! module can apply the correct range-check arm without re-parsing.
//! Stage 5 will move type registry to Rust and collapse the type +
//! suffix params.
//!
//! The lower crate does NOT stamp metadata on returned constants
//! (`low_level_type_names_` discipline, #311): LLVM constant uniquing
//! shares pointers between identical `(type, value)` pairs, so per-value
//! suffix metadata is propagated through the AST instead (via the C++
//! side's `getExprLowLevelSuffix`).

use crate::emit_extern::{ry_emit_build_arc_global, ry_emit_const_fp, ry_emit_const_int};
use crate::error::set_last_error;
use crate::handles::{RyEmitCtx, RyTypeRef, RyValueId};
use crate::intern;

/// Suffix discriminator passed from the C++ shim. The numeric values are
/// part of the boundary contract — they MUST match the
/// `RyLowerSuffixKind` enum in `include/ry/lower/api.h` exactly.
#[repr(u8)]
#[derive(Copy, Clone, PartialEq, Eq)]
pub(crate) enum SuffixKind {
    None = 0,
    I8 = 1,
    I16 = 2,
    I32 = 3,
    I64 = 4,
    U8 = 5,
    U16 = 6,
    U32 = 7,
    U64 = 8,
    F32 = 9,
    F64 = 10,
}

impl SuffixKind {
    fn from_u8(v: u8) -> Option<Self> {
        match v {
            0 => Some(SuffixKind::None),
            1 => Some(SuffixKind::I8),
            2 => Some(SuffixKind::I16),
            3 => Some(SuffixKind::I32),
            4 => Some(SuffixKind::I64),
            5 => Some(SuffixKind::U8),
            6 => Some(SuffixKind::U16),
            7 => Some(SuffixKind::U32),
            8 => Some(SuffixKind::U64),
            9 => Some(SuffixKind::F32),
            10 => Some(SuffixKind::F64),
            _ => None,
        }
    }

    /// Suffix name as it appears in Ry source (`""` for bare integer).
    /// Used only for diagnostic formatting.
    fn as_str(self) -> &'static str {
        match self {
            SuffixKind::None => "",
            SuffixKind::I8 => "i8",
            SuffixKind::I16 => "i16",
            SuffixKind::I32 => "i32",
            SuffixKind::I64 => "i64",
            SuffixKind::U8 => "u8",
            SuffixKind::U16 => "u16",
            SuffixKind::U32 => "u32",
            SuffixKind::U64 => "u64",
            SuffixKind::F32 => "f32",
            SuffixKind::F64 => "f64",
        }
    }
}

/// Port of `validateIntRange` from `src/codegen_expr.cpp:30-80`. Returns
/// `Ok(())` if the magnitude fits; on overflow returns the diagnostic
/// message to surface via `set_last_error`.
///
/// Mirrors the C++ semantics exactly:
/// - `NumberExpr.value` is the unsigned bit pattern of a non-negative
///   magnitude (negative literals arrive as `UnaryExpr` wrapping a
///   `NumberExpr`, handled by the C++ fast-path).
/// - Signed suffixes accept up to `INT{N}_MAX`. The `INT{N}_MIN`
///   magnitude is only legal via the C++ UnaryExpr fast-path.
/// - For unsigned suffixes the diagnostic displays the bit pattern as
///   unsigned so large `u64` literals don't render as negative numbers.
fn validate_int_range(magnitude: u64, kind: SuffixKind) -> Result<(), String> {
    let fmt = || -> String {
        // Unsigned suffixes: show the bit pattern as unsigned.
        match kind {
            SuffixKind::U8 | SuffixKind::U16 | SuffixKind::U32 | SuffixKind::U64 => {
                magnitude.to_string()
            }
            _ => (magnitude as i64).to_string(),
        }
    };
    match kind {
        SuffixKind::I8 => {
            if magnitude > i8::MAX as u64 {
                return Err(format!("i8 literal out of range: {}", fmt()));
            }
        }
        SuffixKind::I16 => {
            if magnitude > i16::MAX as u64 {
                return Err(format!("i16 literal out of range: {}", fmt()));
            }
        }
        SuffixKind::I32 => {
            if magnitude > i32::MAX as u64 {
                return Err(format!("i32 literal out of range: {}", fmt()));
            }
        }
        SuffixKind::I64 => {
            // Magnitude > INT64_MAX cannot fit in signed i64 except via the
            // UnaryExpr fast-path (which handles the INT64_MIN edge).
            if (magnitude as i64) < 0 {
                return Err(format!("i64 literal out of range: {}", fmt()));
            }
        }
        SuffixKind::U8 => {
            if magnitude > u8::MAX as u64 {
                return Err(format!("u8 literal out of range: {}", fmt()));
            }
        }
        SuffixKind::U16 => {
            if magnitude > u16::MAX as u64 {
                return Err(format!("u16 literal out of range: {}", fmt()));
            }
        }
        SuffixKind::U32 => {
            if magnitude > u32::MAX as u64 {
                return Err(format!("u32 literal out of range: {}", fmt()));
            }
        }
        // `u64`: any 64-bit pattern is valid.
        // `None`: bare-int empty suffix handled separately by `lower_int_const`.
        // `F32`/`F64`: not integer suffixes.
        SuffixKind::U64 | SuffixKind::None | SuffixKind::F32 | SuffixKind::F64 => {}
    }
    Ok(())
}

/// Port of `emitExprVariant(NumberExpr)` from `src/codegen_expr.cpp:88-112`.
///
/// `magnitude` is the parser-provided unsigned bit pattern (negative
/// literals arrive as `UnaryExpr`). `suffix_kind` is the C++ shim's enum
/// mapping of `e.suffix`. `signed_bit` is non-zero for signed contexts
/// (matches `ConstantInt::get(.., isSigned)`).
///
/// On range overflow or bare-int negative bit-pattern, populates the
/// thread-local error slot and returns `0`. The caller checks `id == 0`,
/// retrieves the message via `ry_lower_get_last_error`, and surfaces it
/// through `codegenError`.
///
/// # Safety
///
/// `ctx` and `llvm_ty` must be valid handles (or NULL) — same contract as
/// every other `ry_lower_*` entry.
pub(crate) unsafe fn lower_int_const(
    ctx: *mut RyEmitCtx,
    llvm_ty: RyTypeRef,
    magnitude: u64,
    suffix_kind: u8,
    signed_bit: u8,
) -> RyValueId {
    let Some(kind) = SuffixKind::from_u8(suffix_kind) else {
        set_last_error(&format!(
            "lower: unknown suffix_kind {} for int literal",
            suffix_kind
        ));
        return 0;
    };

    // Mirror the defensive check in `lower_float_const`: a float suffix on
    // the int path would call `ry_emit_const_int` with a float-resolved
    // `llvm_ty` (LLVM undefined behavior). The C++ shim should never send
    // F32/F64 to this entry; reject defensively if it does.
    if matches!(kind, SuffixKind::F32 | SuffixKind::F64) {
        set_last_error(&format!(
            "lower: non-integer suffix '{}' on int literal",
            kind.as_str()
        ));
        return 0;
    }

    if kind == SuffixKind::None {
        // Bare `int` is i64. With the strtoull-based parser a negative bit
        // pattern means the literal exceeds INT64_MAX (negative literals
        // arrive as UnaryExpr, so NumberExpr.value is always a non-negative
        // magnitude). Reject so users must either add a `u64` suffix or a
        // u-type annotation (handled by emitVarDecl suffix injection).
        if (magnitude as i64) < 0 {
            set_last_error(&format!(
                "integer literal out of range for int: {}",
                magnitude
            ));
            return 0;
        }
    } else if let Err(msg) = validate_int_range(magnitude, kind) {
        set_last_error(&msg);
        return 0;
    }

    // No metadata stamping on the result: LLVM's constant uniquing shares
    // the same pointer for identical (type, value) pairs, causing metadata
    // corruption when different suffixes map to the same constant (#311).
    // Suffix info is propagated via the AST (getExprLowLevelSuffix) on the
    // C++ side instead.
    ry_emit_const_int(ctx, llvm_ty, magnitude, signed_bit as core::ffi::c_int)
}

/// Port of `emitExprVariant(FloatExpr)` from `src/codegen_expr.cpp:114-121`.
///
/// `llvm_ty` is the LLVM type the C++ shim selected from the suffix
/// (`f64Ty_` for empty / `f64` suffix, `f32Ty_` for `f32`). `value` is
/// the parser-provided double. `suffix_kind` is included for parity with
/// `lower_int_const`'s signature and for future error-path use, but no
/// validation is performed today — IEEE 754 overflow to infinity is
/// accepted, matching the C++ side.
///
/// The float path has no error channel today; it always succeeds. The
/// `suffix_kind` is sanity-checked only to reject unknown values that
/// would indicate a boundary protocol break.
///
/// # Safety
///
/// `ctx` and `llvm_ty` must be valid handles (or NULL).
pub(crate) unsafe fn lower_float_const(
    ctx: *mut RyEmitCtx,
    llvm_ty: RyTypeRef,
    value: f64,
    suffix_kind: u8,
) -> RyValueId {
    let Some(kind) = SuffixKind::from_u8(suffix_kind) else {
        set_last_error(&format!(
            "lower: unknown suffix_kind {} for float literal",
            suffix_kind
        ));
        return 0;
    };
    // Reject non-float suffix_kind values defensively — the C++ shim
    // should only pass None / F32 / F64 here.
    match kind {
        SuffixKind::None | SuffixKind::F32 | SuffixKind::F64 => {}
        other => {
            set_last_error(&format!(
                "lower: non-float suffix '{}' on float literal",
                other.as_str()
            ));
            return 0;
        }
    }
    // No metadata stamping (#311 discipline) — same reasoning as
    // `lower_int_const`.
    ry_emit_const_fp(ctx, llvm_ty, value)
}

/// Port of `emitExprVariant(StringExpr)` from `src/codegen_expr.cpp:113-115`
/// (Stage 2 of the upper-codegen migration, #2484).
///
/// Builds (or reuses, on cache hit) the StringHeader-prefixed ARC-immortal
/// global for `bytes` and returns its interned `RyValueId`. `name_hint` is
/// the caller-supplied LLVM global name prefix — `.str` for `StringExpr`,
/// or whatever the `cachedGlobalString` caller passed (`.fmt`, `.err_msg`,
/// etc.) — passed by length (not NUL-terminated). The thread-local
/// `STRING_CACHE` mirrors the C++ `global_string_cache_`: same bytes → same
/// id within a single `CodeGen` lifetime. The cache MUST be reset
/// (`reset_module_state`) on `CodeGen` construction; otherwise a second
/// instance on the same thread would resolve stale ids against the previous
/// `EmitCtx`.
///
/// Binary-safe: `bytes` may contain embedded NULs; `Vec<u8>` keying preserves
/// the full byte sequence, matching the C++ `std::string` keying.
///
/// # Safety
///
/// `ctx` must be a valid `RyEmitCtx *` (or NULL); `bytes` and `name_hint`
/// must each be either valid for the supplied length, or empty (len 0,
/// pointer ignored).
pub(crate) unsafe fn lower_string_const(
    ctx: *mut RyEmitCtx,
    bytes: &[u8],
    name_hint: &[u8],
) -> RyValueId {
    // NULL ctx → 0 (boundary contract: a non-zero `RyValueId` MUST be
    // resolvable via `ry_emit_resolve(ctx, id)`). Without this guard, a
    // cache hit would return a non-zero id bound to a prior `EmitCtx`,
    // breaking the contract when the caller's `ctx` is NULL.
    if ctx.is_null() {
        return 0;
    }
    if let Some(id) = intern::lookup_string(bytes) {
        return id;
    }
    let id = ry_emit_build_arc_global(
        ctx,
        bytes.as_ptr(),
        bytes.len(),
        name_hint.as_ptr(),
        name_hint.len(),
    );
    if id != 0 {
        intern::insert_string(bytes.to_vec(), id);
    }
    id
}

/// Port of `emitExprVariant(RegexExpr)` from `src/codegen_expr.cpp:117-124`
/// (Stage 2 of the upper-codegen migration, #2484).
///
/// Independent cache from `lower_string_const` to preserve the regex
/// separation invariant (#306): a string `"hello"` and a regex `/hello/`
/// MUST produce distinct globals so that the C++ shim's
/// `addResourceKind(rk_regex)` does not poison a same-bytes string literal
/// reached elsewhere in the program. The hardcoded `.regex` name hint
/// reproduces the C++ baseline.
///
/// # Safety
///
/// `ctx` must be a valid `RyEmitCtx *` (or NULL).
pub(crate) unsafe fn lower_regex_const(ctx: *mut RyEmitCtx, bytes: &[u8]) -> RyValueId {
    // See `lower_string_const` for the NULL-ctx rationale.
    if ctx.is_null() {
        return 0;
    }
    if let Some(id) = intern::lookup_regex(bytes) {
        return id;
    }
    const NAME: &[u8] = b".regex";
    let id = ry_emit_build_arc_global(ctx, bytes.as_ptr(), bytes.len(), NAME.as_ptr(), NAME.len());
    if id != 0 {
        intern::insert_regex(bytes.to_vec(), id);
    }
    id
}
