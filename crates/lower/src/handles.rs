//! Opaque-pointer handle types shared with the emission boundary
//! (`include/ry/llvm_emit/api.h`). These mirror the C++ side's opaque
//! typedefs as zero-sized `repr(C)` structs / pointer aliases — no field
//! is ever read from Rust, so no layout assertion is needed.
//!
//! Keep this set minimal: only the handles the lower crate's public
//! `ry_lower_*` entries or its `extern "C"` calls into `ry_emit_*` use.

use core::ffi::c_void;

/// Opaque emit-side context. Allocated and destroyed by the `emit`
/// crate; the lower crate only threads the pointer through.
#[repr(C)]
pub(crate) struct RyEmitCtx {
    _unused: [u8; 0],
}

/// Opaque LLVM type handle (`llvm::Type *` on the C++ side, an
/// `LLVMTypeRef`-shaped pointer inside the `emit` crate). The lower
/// crate treats it as a bag-of-bytes pointer and never dereferences.
pub(crate) type RyTypeRef = *mut c_void;

/// Interned value handle. Sentinel `0` is the invalid id; non-zero ids
/// are issued and resolved by the emit-side intern table.
pub(crate) type RyValueId = u32;
