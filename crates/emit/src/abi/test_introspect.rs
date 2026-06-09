//! abi::test_introspect — test-only introspection of the internal header layout.
//!
//! NOT part of the production emission path. It exposes the *same*
//! single-sourced field-kind table that drives `cow_ensure_unique` /
//! `arc_header_type` (`core::header_fields`) so the C++ parity test
//! (`tests/test_header_layout.cpp`) can assert the Rust mirror against C++'s
//! canonical `listHeaderTy_` / `mapHeaderTy_` / `setHeaderTy_` / `arcHeaderTy_`
//! (`src/codegen.cpp:57-61`). This closes the layout-drift hole #2071: a C++
//! field-order change that the Rust mirror does not follow is now caught
//! mechanically (the previous protection was a "Field order MUST stay in sync"
//! comment only).
//!
//! Gate compliance (`scripts/check-emit-abi-no-ir.sh`): this reads only the
//! pure-data `header_fields` slice — it never calls `build_header_struct` or any
//! `LLVMBuild*` / `llvm_sys::core` API, so the abi layer stays IR-free. The ABI
//! size is derived and compared on the C++ side from the reported field kinds.
//!
//! Exported unconditionally (corrosion builds the cdylib once, shared by `ry`
//! and `ry_tests`; a cargo feature would require a second build), hence the
//! `ry_emit_test_` prefix.

use std::ffi::c_int;

use crate::core::{header_fields, HdrField, HeaderKind};

// Header kind selector (the C++ side passes these literals). List/Map/Set match
// RY_COW_LIST / _MAP / _SET (0/1/2); Arc is appended as 3.
pub const RY_TEST_HDR_LIST: c_int = 0;
pub const RY_TEST_HDR_MAP: c_int = 1;
pub const RY_TEST_HDR_SET: c_int = 2;
pub const RY_TEST_HDR_ARC: c_int = 3;

// Per-field type tag: 0 = i64, 1 = ptr.
const FIELD_I64: u8 = 0;
const FIELD_PTR: u8 = 1;

// Flat, C-ABI-stable report of a header's field layout. The first `count` slots
// of `kinds` are valid (the rest stay 0). Eight slots covers the largest header
// (Map = 6 fields). `count == 0` signals an unknown kind selector.
#[repr(C)]
pub struct RyTestHeaderLayout {
    pub count: u32,
    pub kinds: [u8; 8],
}

/// Report the single-sourced field layout of an internal header for the
/// cross-language parity test. Pure data lookup — emits no IR.
#[no_mangle]
pub extern "C" fn ry_emit_test_header_layout(kind: c_int) -> RyTestHeaderLayout {
    let hk = match kind {
        RY_TEST_HDR_LIST => HeaderKind::List,
        RY_TEST_HDR_MAP => HeaderKind::Map,
        RY_TEST_HDR_SET => HeaderKind::Set,
        RY_TEST_HDR_ARC => HeaderKind::Arc,
        _ => {
            return RyTestHeaderLayout {
                count: 0,
                kinds: [0; 8],
            }
        }
    };
    let fields = header_fields(hk);
    let mut kinds = [0u8; 8];
    for (i, f) in fields.iter().enumerate() {
        kinds[i] = match f {
            HdrField::I64 => FIELD_I64,
            HdrField::Ptr => FIELD_PTR,
        };
    }
    RyTestHeaderLayout {
        count: fields.len() as u32,
        kinds,
    }
}
