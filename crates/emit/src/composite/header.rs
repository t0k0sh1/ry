//! composite/header — Ry collection / ARC header struct construction and load
//! helpers. Encodes the Ry layout knowledge that the list/map/set/arc headers
//! exist and that they are laid out per `context::header_fields(kind)`, plus
//! the `load_list_header` interleave-order helper shared by `composite::arc`,
//! `composite::collection`, `composite::reduce`, and `composite::cow`.
//!
//! Keep `build_header_struct` byte-identical to the previous inline
//! `LLVMStructTypeInContext` calls — it feeds a hot codegen path whose IR is
//! golden-pinned (codegen-llvm-ir-conventions.md "IR-byte-identical").
//! Likewise `load_list_header` emits in gep-then-load grouped order (the
//! #2092 instruction-order lesson — distinct from C++ `CodeGen::loadListHeader`'s
//! interleaved order).

use std::ffi::c_char;

use llvm_sys::core::*;
use llvm_sys::prelude::*;
use llvm_sys::LLVMIntPredicate;

use crate::context::header_fields;
use crate::context::{HdrField, HeaderKind, LIST_FIELD_CAP, LIST_FIELD_DATA, LIST_FIELD_LEN};
use crate::primitive::libc::{emit_free, emit_malloc, emit_memcpy};
use crate::primitive::types::{i64_type, ptr_type};
use crate::primitive::util::cname_pfx;

// Build the LLVM literal struct for a header from the single-sourced field
// list (see `crate::context::header_fields`).
pub(crate) unsafe fn build_header_struct(c: LLVMContextRef, kind: HeaderKind) -> LLVMTypeRef {
    let fields = header_fields(kind);
    let mut elems: Vec<LLVMTypeRef> = Vec::with_capacity(fields.len());
    for f in fields {
        elems.push(match f {
            HdrField::I64 => i64_type(c),
            HdrField::Ptr => ptr_type(c),
        });
    }
    LLVMStructTypeInContext(c, elems.as_mut_ptr(), elems.len() as u32, 0)
}

// {i64, i64} ARC header struct (single-sourced via `context::header_fields`).
#[inline]
pub(crate) unsafe fn arc_header_type(c: LLVMContextRef) -> LLVMTypeRef {
    build_header_struct(c, HeaderKind::Arc)
}

// Load a list header's {len, cap, data} via StructGEP + load. `prefix`
// concatenates with _len_ptr / _cap_ptr / _data_ptr / _len / _cap / _data.
//
// Caveat (#2092): this emits the three GEPs first, then the three loads
// (grouped). C++ `CodeGen::loadListHeader` interleaves them (gep/load × 3); the
// two are NOT byte-identical. Callers whose baseline used `loadListHeader`
// must reproduce the interleaved order inline, NOT call this helper.
pub(crate) struct ListHeaderLoad {
    pub(crate) len_ptr: LLVMValueRef,
    pub(crate) cap_ptr: LLVMValueRef,
    pub(crate) data_ptr: LLVMValueRef,
    pub(crate) len: LLVMValueRef,
    pub(crate) cap: LLVMValueRef,
    pub(crate) data: LLVMValueRef,
}

pub(crate) unsafe fn load_list_header(
    b: LLVMBuilderRef,
    list_header_ty: LLVMTypeRef,
    list_ptr: LLVMValueRef,
    i64_ty: LLVMTypeRef,
    ptr_ty: LLVMTypeRef,
    prefix: &[u8],
) -> ListHeaderLoad {
    let len_ptr = LLVMBuildStructGEP2(
        b,
        list_header_ty,
        list_ptr,
        LIST_FIELD_LEN,
        cname_pfx(prefix, b"_len_ptr").as_ptr(),
    );
    let cap_ptr = LLVMBuildStructGEP2(
        b,
        list_header_ty,
        list_ptr,
        LIST_FIELD_CAP,
        cname_pfx(prefix, b"_cap_ptr").as_ptr(),
    );
    let data_ptr = LLVMBuildStructGEP2(
        b,
        list_header_ty,
        list_ptr,
        LIST_FIELD_DATA,
        cname_pfx(prefix, b"_data_ptr").as_ptr(),
    );
    let len = LLVMBuildLoad2(b, i64_ty, len_ptr, cname_pfx(prefix, b"_len").as_ptr());
    let cap = LLVMBuildLoad2(b, i64_ty, cap_ptr, cname_pfx(prefix, b"_cap").as_ptr());
    let data = LLVMBuildLoad2(b, ptr_ty, data_ptr, cname_pfx(prefix, b"_data").as_ptr());
    ListHeaderLoad {
        len_ptr,
        cap_ptr,
        data_ptr,
        len,
        cap,
        data,
    }
}

// Emit the list capacity-grow block: new_cap = max(cap*2, 4), malloc the new
// buffer, memcpy the old elements, free the old buffer, store new data/cap, then
// branch to next_bb. The builder is positioned at grow_bb here. SSA names are
// `prefix`-derived (app_* / ins_*), except the cap>4 compare whose name is
// passed explicitly (append uses `cap_gt4`, insert leaves it unnamed) to keep
// the emitted IR byte-identical to the pre-extraction inline blocks.
#[allow(clippy::too_many_arguments)]
pub(crate) unsafe fn emit_list_grow(
    b: LLVMBuilderRef,
    module: LLVMModuleRef,
    h: &ListHeaderLoad,
    elem_size: u64,
    i64_ty: LLVMTypeRef,
    ptr_ty: LLVMTypeRef,
    void_ty: LLVMTypeRef,
    prefix: &[u8],
    gt4_name: *const c_char,
    grow_bb: LLVMBasicBlockRef,
    next_bb: LLVMBasicBlockRef,
) {
    LLVMPositionBuilderAtEnd(b, grow_bb);
    let four = LLVMConstInt(i64_ty, 4, 0);
    let doubled = LLVMBuildMul(
        b,
        h.cap,
        LLVMConstInt(i64_ty, 2, 0),
        cname_pfx(prefix, b"_doubled").as_ptr(),
    );
    let gt4 = LLVMBuildICmp(b, LLVMIntPredicate::LLVMIntSGT, doubled, four, gt4_name);
    let new_cap = LLVMBuildSelect(
        b,
        gt4,
        doubled,
        four,
        cname_pfx(prefix, b"_new_cap").as_ptr(),
    );
    let new_size = LLVMBuildMul(
        b,
        new_cap,
        LLVMConstInt(i64_ty, elem_size, 0),
        cname_pfx(prefix, b"_new_size").as_ptr(),
    );
    let new_data = emit_malloc(
        b,
        module,
        i64_ty,
        ptr_ty,
        new_size,
        cname_pfx(prefix, b"_new_data").as_ptr(),
    );
    let old_size = LLVMBuildMul(
        b,
        h.len,
        LLVMConstInt(i64_ty, elem_size, 0),
        cname_pfx(prefix, b"_old_size").as_ptr(),
    );
    emit_memcpy(b, module, ptr_ty, i64_ty, new_data, h.data, old_size);
    emit_free(b, module, ptr_ty, void_ty, h.data);
    LLVMBuildStore(b, new_data, h.data_ptr);
    LLVMBuildStore(b, new_cap, h.cap_ptr);
    LLVMBuildBr(b, next_bb);
}
