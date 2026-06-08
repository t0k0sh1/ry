//! Dynamic `any` boxing: `EmitCtx::any_wrap` / `any_unwrap` / `any_try_unwrap`
//! plus the shared retain guard. (core-role: an `impl EmitCtx` using only the
//! core engine, so it is abi-independent and the `core⇏abi` invariant covers
//! this module too. Migrated #2063 — the last legacy module.) The C boundary
//! entry points `ry_emit_any_{wrap,unwrap,try_unwrap}` live in `abi/any.rs`,
//! which resolve the u32 ids, map the C `RyAny*Desc` into the Rust-native
//! `AnyWrap` / `AnyUnwrap` / `AnyTryUnwrap` (the #2062 descriptor-passing
//! pattern), and intern the result. The engine methods take resolved handles,
//! return the raw `ValueRef` (`None` on a guard failure → shell interns 0), and
//! call `arc_retain` directly for the retain guard (no id-bridge round-trip).

use llvm_sys::core::*;
use llvm_sys::prelude::*;
use llvm_sys::target::{LLVMABISizeOfType, LLVMGetModuleDataLayout};
use llvm_sys::LLVMIntPredicate;
use std::ffi::{c_char, CStr};

use crate::core::*;

// Rust-native descriptor for any_wrap. The abi shell (`abi/any.rs`) resolves the
// u32 handle ids to `ValueRef`, maps the `c_int` kind → `AnyWrapKind`, converts
// the flag fields (`c_int` → bool) and the nullable branch handles
// (`descriptor_id` / `box_layout_ty` → `Option<…>`), and builds this before
// calling the engine. The C `RyAnyWrapDesc` stays the locked boundary surface in
// `abi.rs`; this is its core-side counterpart, consumed only here.
pub(crate) struct AnyWrap {
    pub(crate) kind: AnyWrapKind,
    pub(crate) target_tag: i64,
    pub(crate) val: ValueRef,
    pub(crate) do_collection_retain: bool,
    pub(crate) do_str_retain: bool,
    pub(crate) descriptor: Option<ValueRef>,
    pub(crate) box_layout_ty: Option<TypeRef>,
    pub(crate) box_data_size: u64,
    pub(crate) any_ty: TypeRef,
}

// Rust-native descriptor for any_unwrap. The message / global-name fields stay
// `*const c_char` (per the #2059 Section A name convention); the engine calls
// `cstr_bytes` at the point of use, keeping this struct Copy / lifetime-free.
pub(crate) struct AnyUnwrap {
    pub(crate) kind: AnyUnwrapKind,
    pub(crate) any_val: ValueRef,
    pub(crate) any_ty: TypeRef,
    pub(crate) target_ty: Option<TypeRef>,
    pub(crate) expected_tag: i64,
    pub(crate) do_collection_retain: bool,
    pub(crate) do_str_retain: bool,
    pub(crate) mismatch_msg: *const c_char,
    pub(crate) mismatch_global_name: *const c_char,
    pub(crate) expected_desc: Option<ValueRef>,
    pub(crate) box_layout_ty: Option<TypeRef>,
    pub(crate) record_struct_ty: Option<TypeRef>,
    pub(crate) desc_mismatch_msg: *const c_char,
    pub(crate) desc_mismatch_global_name: *const c_char,
}

// Rust-native descriptor for any_try_unwrap.
pub(crate) struct AnyTryUnwrap {
    pub(crate) kind: AnyTryUnwrapKind,
    pub(crate) any_val: ValueRef,
    pub(crate) any_ty: TypeRef,
    pub(crate) res_ty: TypeRef,
    pub(crate) error_ty: TypeRef,
    pub(crate) target_ty: Option<TypeRef>,
    pub(crate) expected_tag: i64,
    pub(crate) do_collection_retain: bool,
    pub(crate) do_str_retain: bool,
    pub(crate) err_msg_str: ValueRef,
}

impl EmitCtx {
    pub(crate) unsafe fn any_wrap(&mut self, d: &AnyWrap) -> Option<ValueRef> {
        let b = self.builder;
        let context = self.context;
        let module = self.module;
        let val = d.val.0;
        let any_ty = d.any_ty.0;
        let i8_ty = i8_type(context);
        let i64_ty = i64_type(context);
        let i1_ty = i1_type(context);

        // RecordBox / EnumBox — heap-box layout `[ ArcHeader | desc ptr | payload ]`.
        if matches!(d.kind, AnyWrapKind::RecordBox | AnyWrapKind::EnumBox) {
            let (Some(layout_ty), Some(descriptor)) = (d.box_layout_ty, d.descriptor) else {
                return None;
            };
            let layout_ty = layout_ty.0;
            let descriptor = descriptor.0;
            let dl = LLVMGetModuleDataLayout(module);
            let expected = LLVMABISizeOfType(dl, layout_ty);
            if d.box_data_size != expected {
                return None;
            }
            let box_data_size_c = LLVMConstInt(i64_ty, d.box_data_size, 0);
            let header_ptr = emit_inline_arc_alloc(self, box_data_size_c);
            let mut dp_gep = [LLVMConstInt(i64_ty, ARC_HEADER_SIZE, 0)];
            let data_ptr = LLVMBuildGEP2(
                b,
                i8_ty,
                header_ptr,
                dp_gep.as_mut_ptr(),
                1,
                c"arc_box_data".as_ptr(),
            );

            // Labels differ between RecordBox ("any.rec.*") and EnumBox ("any.enum.*").
            let (desc_slot_lbl, payload_slot_lbl, tmp_lbl, tag_lbl, data_lbl, val_lbl): (
                &CStr,
                &CStr,
                &CStr,
                &CStr,
                &CStr,
                &CStr,
            ) = if matches!(d.kind, AnyWrapKind::EnumBox) {
                (
                    c"any.enum.desc.slot",
                    c"any.enum.payload.slot",
                    c"any.enum.tmp",
                    c"any.enum.tag",
                    c"any.enum.data",
                    c"any.enum.val",
                )
            } else {
                (
                    c"any.rec.desc.slot",
                    c"any.rec.fields.slot",
                    c"any.rec.tmp",
                    c"any.rec.tag",
                    c"any.rec.data",
                    c"any.rec.val",
                )
            };

            let desc_ptr_slot =
                LLVMBuildStructGEP2(b, layout_ty, data_ptr, 0, desc_slot_lbl.as_ptr());
            LLVMBuildStore(b, descriptor, desc_ptr_slot);
            let payload_slot =
                LLVMBuildStructGEP2(b, layout_ty, data_ptr, 1, payload_slot_lbl.as_ptr());
            LLVMBuildStore(b, val, payload_slot);

            let tmp = LLVMBuildAlloca(b, any_ty, tmp_lbl.as_ptr());
            let tag_slot = LLVMBuildStructGEP2(b, any_ty, tmp, 0, tag_lbl.as_ptr());
            LLVMBuildStore(b, LLVMConstInt(i64_ty, d.target_tag as u64, 0), tag_slot);
            let any_data_slot = LLVMBuildStructGEP2(b, any_ty, tmp, 1, data_lbl.as_ptr());
            LLVMBuildStore(b, data_ptr, any_data_slot);
            let result = LLVMBuildLoad2(b, any_ty, tmp, val_lbl.as_ptr());
            return Some(ValueRef(result));
        }

        // NonBox — retain BEFORE the alloca+store (the two flags are exclusive).
        self.any_retain_guard(i8_ty, i64_ty, val, d.do_collection_retain, d.do_str_retain);

        let mut val_m = val;
        if LLVMTypeOf(val_m) == i1_ty {
            val_m = LLVMBuildZExt(b, val_m, i64_ty, c"any.bool.zext".as_ptr());
        }
        let tmp = LLVMBuildAlloca(b, any_ty, c"any.tmp".as_ptr());
        let tag_ptr = LLVMBuildStructGEP2(b, any_ty, tmp, 0, c"any.tag".as_ptr());
        LLVMBuildStore(b, LLVMConstInt(i64_ty, d.target_tag as u64, 0), tag_ptr);
        let data_ptr = LLVMBuildStructGEP2(b, any_ty, tmp, 1, c"any.data".as_ptr());
        LLVMBuildStore(b, val_m, data_ptr);
        let result = LLVMBuildLoad2(b, any_ty, tmp, c"any.val".as_ptr());
        Some(ValueRef(result))
    }

    pub(crate) unsafe fn any_unwrap(&mut self, d: &AnyUnwrap) -> Option<ValueRef> {
        let b = self.builder;
        let context = self.context;
        let module = self.module;
        let any_val = d.any_val.0;
        let any_ty = d.any_ty.0;
        let i8_ty = i8_type(context);
        let i64_ty = i64_type(context);
        let ptr_ty = ptr_type(context);
        let f64_ty = LLVMDoubleTypeInContext(context);

        let tag = LLVMBuildExtractValue(b, any_val, 0, c"any.tag.val".as_ptr());
        // Builder-derived parent (builder-derived parent rule).
        let fn_v = LLVMGetBasicBlockParent(LLVMGetInsertBlock(b));

        let mismatch_msg = cstr_bytes(d.mismatch_msg);
        let mismatch_name = d.mismatch_global_name;

        // Record — tag check + descriptor chain walk.
        if matches!(d.kind, AnyUnwrapKind::Record) {
            let (Some(layout_ty), Some(record_struct_ty), Some(expected_desc)) =
                (d.box_layout_ty, d.record_struct_ty, d.expected_desc)
            else {
                return None;
            };
            let layout_ty = layout_ty.0;
            let record_struct_ty = record_struct_ty.0;
            let expected_desc = expected_desc.0;
            let desc_mismatch_msg = cstr_bytes(d.desc_mismatch_msg);
            let desc_mismatch_name = d.desc_mismatch_global_name;

            let tag_match_bb =
                LLVMAppendBasicBlockInContext(context, fn_v, c"any.rec.tag_ok".as_ptr());
            let tag_mismatch_bb =
                LLVMAppendBasicBlockInContext(context, fn_v, c"any.rec.tag_err".as_ptr());
            let desc_check_bb =
                LLVMAppendBasicBlockInContext(context, fn_v, c"any.rec.desc_check".as_ptr());
            let desc_mismatch_bb =
                LLVMAppendBasicBlockInContext(context, fn_v, c"any.rec.desc_err".as_ptr());

            let is_record = LLVMBuildICmp(
                b,
                LLVMIntPredicate::LLVMIntEQ,
                tag,
                LLVMConstInt(i64_ty, d.expected_tag as u64, 0),
                c"any.is_record".as_ptr(),
            );
            LLVMBuildCondBr(b, is_record, tag_match_bb, tag_mismatch_bb);

            LLVMPositionBuilderAtEnd(b, tag_mismatch_bb);
            emit_inline_runtime_error(self, mismatch_msg, mismatch_name);

            LLVMPositionBuilderAtEnd(b, tag_match_bb);
            let tmp = LLVMBuildAlloca(b, any_ty, c"any.rec.tmp".as_ptr());
            LLVMBuildStore(b, any_val, tmp);
            let any_data_slot =
                LLVMBuildStructGEP2(b, any_ty, tmp, 1, c"any.rec.data.ptr".as_ptr());
            let data_ptr = LLVMBuildLoad2(b, ptr_ty, any_data_slot, c"any.rec.data".as_ptr());
            let desc_slot =
                LLVMBuildStructGEP2(b, layout_ty, data_ptr, 0, c"any.rec.desc.slot".as_ptr());
            let actual_desc = LLVMBuildLoad2(b, ptr_ty, desc_slot, c"any.rec.desc".as_ptr());

            let mut is_subtype_p = [ptr_ty, ptr_ty];
            let is_subtype_ty = LLVMFunctionType(i64_ty, is_subtype_p.as_mut_ptr(), 2, 0);
            let is_subtype_fn = get_or_insert_function(
                module,
                c"__ry_record_is_subtype_desc".as_ptr(),
                is_subtype_ty,
            );
            let mut is_subtype_a = [actual_desc, expected_desc];
            let chain_ok = LLVMBuildCall2(
                b,
                is_subtype_ty,
                is_subtype_fn,
                is_subtype_a.as_mut_ptr(),
                2,
                c"any.rec.chain.ok".as_ptr(),
            );
            let chain_bool = LLVMBuildICmp(
                b,
                LLVMIntPredicate::LLVMIntNE,
                chain_ok,
                LLVMConstInt(i64_ty, 0, 0),
                c"any.rec.chain.bool".as_ptr(),
            );
            LLVMBuildCondBr(b, chain_bool, desc_check_bb, desc_mismatch_bb);

            LLVMPositionBuilderAtEnd(b, desc_mismatch_bb);
            emit_inline_runtime_error(self, desc_mismatch_msg, desc_mismatch_name);

            LLVMPositionBuilderAtEnd(b, desc_check_bb);
            let fields_slot =
                LLVMBuildStructGEP2(b, layout_ty, data_ptr, 1, c"any.rec.fields.slot".as_ptr());
            let record_val = LLVMBuildLoad2(
                b,
                record_struct_ty,
                fields_slot,
                c"any.rec.unwrap.val".as_ptr(),
            );
            return Some(ValueRef(record_val));
        }

        // F64Promote — 5 BBs; merge PHI(f64).
        if matches!(d.kind, AnyUnwrapKind::F64Promote) {
            let float_bb = LLVMAppendBasicBlockInContext(context, fn_v, c"any.float".as_ptr());
            let check_int_bb =
                LLVMAppendBasicBlockInContext(context, fn_v, c"any.check_int".as_ptr());
            let int_promote_bb =
                LLVMAppendBasicBlockInContext(context, fn_v, c"any.int2float".as_ptr());
            let mismatch_bb =
                LLVMAppendBasicBlockInContext(context, fn_v, c"any.mismatch".as_ptr());
            let merge_bb = LLVMAppendBasicBlockInContext(context, fn_v, c"any.merge".as_ptr());

            let tmp = LLVMBuildAlloca(b, any_ty, c"any.tmp.fp".as_ptr());
            LLVMBuildStore(b, any_val, tmp);
            let data_ptr = LLVMBuildStructGEP2(b, any_ty, tmp, 1, c"any.data.fp".as_ptr());

            let is_float = LLVMBuildICmp(
                b,
                LLVMIntPredicate::LLVMIntEQ,
                tag,
                LLVMConstInt(i64_ty, ANY_TAG_FLOAT as u64, 0),
                c"is.float".as_ptr(),
            );
            LLVMBuildCondBr(b, is_float, float_bb, check_int_bb);

            LLVMPositionBuilderAtEnd(b, check_int_bb);
            let is_int = LLVMBuildICmp(
                b,
                LLVMIntPredicate::LLVMIntEQ,
                tag,
                LLVMConstInt(i64_ty, ANY_TAG_INT as u64, 0),
                c"is.int".as_ptr(),
            );
            LLVMBuildCondBr(b, is_int, int_promote_bb, mismatch_bb);

            LLVMPositionBuilderAtEnd(b, mismatch_bb);
            emit_inline_runtime_error(self, mismatch_msg, mismatch_name);

            LLVMPositionBuilderAtEnd(b, float_bb);
            let float_val = LLVMBuildLoad2(b, f64_ty, data_ptr, c"any.f64".as_ptr());
            LLVMBuildBr(b, merge_bb);

            LLVMPositionBuilderAtEnd(b, int_promote_bb);
            let int_val = LLVMBuildLoad2(b, i64_ty, data_ptr, c"any.i64".as_ptr());
            let promoted = LLVMBuildSIToFP(b, int_val, f64_ty, c"any.i2f".as_ptr());
            LLVMBuildBr(b, merge_bb);

            LLVMPositionBuilderAtEnd(b, merge_bb);
            let phi = LLVMBuildPhi(b, f64_ty, c"any.unwrap.f64".as_ptr());
            let mut vals = [float_val, promoted];
            let mut blocks = [float_bb, int_promote_bb];
            LLVMAddIncoming(phi, vals.as_mut_ptr(), blocks.as_mut_ptr(), 2);
            return Some(ValueRef(phi));
        }

        // Standard — 2-way path.
        let target_ty = d.target_ty?.0;
        let cmp = LLVMBuildICmp(
            b,
            LLVMIntPredicate::LLVMIntEQ,
            tag,
            LLVMConstInt(i64_ty, d.expected_tag as u64, 0),
            c"any.tag.check".as_ptr(),
        );
        let match_bb = LLVMAppendBasicBlockInContext(context, fn_v, c"any.match".as_ptr());
        let mismatch_bb = LLVMAppendBasicBlockInContext(context, fn_v, c"any.mismatch".as_ptr());
        LLVMBuildCondBr(b, cmp, match_bb, mismatch_bb);

        LLVMPositionBuilderAtEnd(b, mismatch_bb);
        emit_inline_runtime_error(self, mismatch_msg, mismatch_name);

        LLVMPositionBuilderAtEnd(b, match_bb);
        let tmp = LLVMBuildAlloca(b, any_ty, c"any.tmp".as_ptr());
        LLVMBuildStore(b, any_val, tmp);
        let data_ptr = LLVMBuildStructGEP2(b, any_ty, tmp, 1, c"any.data.ptr".as_ptr());
        let unwrapped = LLVMBuildLoad2(b, target_ty, data_ptr, c"any.unwrap.val".as_ptr());

        self.any_retain_guard(
            i8_ty,
            i64_ty,
            unwrapped,
            d.do_collection_retain,
            d.do_str_retain,
        );
        Some(ValueRef(unwrapped))
    }

    pub(crate) unsafe fn any_try_unwrap(&mut self, d: &AnyTryUnwrap) -> Option<ValueRef> {
        let b = self.builder;
        let context = self.context;
        let any_val = d.any_val.0;
        let any_ty = d.any_ty.0;
        let res_ty = d.res_ty.0;
        let error_ty = d.error_ty.0;
        let err_msg_str = d.err_msg_str.0;
        let i8_ty = i8_type(context);
        let i64_ty = i64_type(context);
        let i1_ty = i1_type(context);
        let f64_ty = LLVMDoubleTypeInContext(context);

        let tag = LLVMBuildExtractValue(b, any_val, 0, c"tryany.tag".as_ptr());
        let fn_v = LLVMGetBasicBlockParent(LLVMGetInsertBlock(b));

        // Inline the ok / err value builders: retaining the arc source would be a
        // no-op for freshly-extracted loads, so the explicit retain via
        // do_collection_retain / do_str_retain carries it. These capture only Copy
        // LLVM refs (not `self`), so unlike result_branch's C callbacks they live
        // inside this `&mut self` method without aliasing the receiver.
        let build_ok = move |inner: LLVMValueRef| -> LLVMValueRef {
            unsafe {
                let mut v = LLVMConstNull(res_ty);
                v = LLVMBuildInsertValue(b, v, LLVMConstInt(i1_ty, 1, 0), 0, c"res.ok".as_ptr());
                v = LLVMBuildInsertValue(b, v, inner, 1, c"res.ok_val".as_ptr());
                v = LLVMBuildInsertValue(
                    b,
                    v,
                    LLVMConstNull(LLVMStructGetTypeAtIndex(res_ty, 2)),
                    2,
                    c"".as_ptr(),
                );
                v
            }
        };
        let build_err = move || -> LLVMValueRef {
            unsafe {
                let mut err_val = LLVMGetUndef(error_ty);
                err_val = LLVMBuildInsertValue(b, err_val, err_msg_str, 0, c"".as_ptr());
                err_val =
                    LLVMBuildInsertValue(b, err_val, LLVMConstInt(i64_ty, 0, 0), 1, c"".as_ptr());
                let mut v = LLVMConstNull(res_ty);
                v = LLVMBuildInsertValue(b, v, LLVMConstInt(i1_ty, 0, 0), 0, c"res.err".as_ptr());
                v = LLVMBuildInsertValue(
                    b,
                    v,
                    LLVMConstNull(LLVMStructGetTypeAtIndex(res_ty, 1)),
                    1,
                    c"".as_ptr(),
                );
                v = LLVMBuildInsertValue(b, v, err_val, 2, c"res.err_val".as_ptr());
                v
            }
        };

        // F64Promote — both loads share one alloca; Ok arm selects via isFloat.
        if matches!(d.kind, AnyTryUnwrapKind::F64Promote) {
            let tmp = LLVMBuildAlloca(b, any_ty, c"tryany.fp.tmp".as_ptr());
            LLVMBuildStore(b, any_val, tmp);
            let data_ptr = LLVMBuildStructGEP2(b, any_ty, tmp, 1, c"tryany.fp.data".as_ptr());
            let f_val = LLVMBuildLoad2(b, f64_ty, data_ptr, c"tryany.fp.fval".as_ptr());
            let i_val = LLVMBuildLoad2(b, i64_ty, data_ptr, c"tryany.fp.ival".as_ptr());
            let promoted = LLVMBuildSIToFP(b, i_val, f64_ty, c"tryany.fp.i2f".as_ptr());
            let is_float = LLVMBuildICmp(
                b,
                LLVMIntPredicate::LLVMIntEQ,
                tag,
                LLVMConstInt(i64_ty, ANY_TAG_FLOAT as u64, 0),
                c"tryany.fp.is_float".as_ptr(),
            );
            let is_int = LLVMBuildICmp(
                b,
                LLVMIntPredicate::LLVMIntEQ,
                tag,
                LLVMConstInt(i64_ty, ANY_TAG_INT as u64, 0),
                c"tryany.fp.is_int".as_ptr(),
            );
            let is_accept = LLVMBuildOr(b, is_float, is_int, c"tryany.fp.is_accept".as_ptr());
            let is_err = LLVMBuildNot(b, is_accept, c"tryany.fp.is_err".as_ptr());

            let ok_bb = LLVMAppendBasicBlockInContext(context, fn_v, c"res.ok".as_ptr());
            let err_bb = LLVMAppendBasicBlockInContext(context, fn_v, c"res.err".as_ptr());
            let merge_bb = LLVMAppendBasicBlockInContext(context, fn_v, c"res.merge".as_ptr());
            LLVMBuildCondBr(b, is_err, err_bb, ok_bb);

            LLVMPositionBuilderAtEnd(b, ok_bb);
            let chosen = LLVMBuildSelect(b, is_float, f_val, promoted, c"tryany.fp.val".as_ptr());
            let ok_val = build_ok(chosen);
            LLVMBuildBr(b, merge_bb);
            let ok_incoming = LLVMGetInsertBlock(b);

            LLVMPositionBuilderAtEnd(b, err_bb);
            let err_val = build_err();
            LLVMBuildBr(b, merge_bb);
            let err_incoming = LLVMGetInsertBlock(b);

            LLVMPositionBuilderAtEnd(b, merge_bb);
            let phi = LLVMBuildPhi(b, res_ty, c"result".as_ptr());
            let mut vals = [ok_val, err_val];
            let mut blocks = [ok_incoming, err_incoming];
            LLVMAddIncoming(phi, vals.as_mut_ptr(), blocks.as_mut_ptr(), 2);
            return Some(ValueRef(phi));
        }

        // Standard — tag-check primitive arm.
        let target_ty = d.target_ty?.0;
        let cmp = LLVMBuildICmp(
            b,
            LLVMIntPredicate::LLVMIntEQ,
            tag,
            LLVMConstInt(i64_ty, d.expected_tag as u64, 0),
            c"tryany.tag.eq".as_ptr(),
        );
        let is_err = LLVMBuildNot(b, cmp, c"tryany.is_err".as_ptr());

        let ok_bb = LLVMAppendBasicBlockInContext(context, fn_v, c"res.ok".as_ptr());
        let err_bb = LLVMAppendBasicBlockInContext(context, fn_v, c"res.err".as_ptr());
        let merge_bb = LLVMAppendBasicBlockInContext(context, fn_v, c"res.merge".as_ptr());
        LLVMBuildCondBr(b, is_err, err_bb, ok_bb);

        LLVMPositionBuilderAtEnd(b, ok_bb);
        let tmp = LLVMBuildAlloca(b, any_ty, c"tryany.tmp".as_ptr());
        LLVMBuildStore(b, any_val, tmp);
        let data_ptr = LLVMBuildStructGEP2(b, any_ty, tmp, 1, c"tryany.data".as_ptr());
        let unwrapped = LLVMBuildLoad2(b, target_ty, data_ptr, c"tryany.val".as_ptr());
        self.any_retain_guard(
            i8_ty,
            i64_ty,
            unwrapped,
            d.do_collection_retain,
            d.do_str_retain,
        );
        let ok_val = build_ok(unwrapped);
        LLVMBuildBr(b, merge_bb);
        let ok_incoming = LLVMGetInsertBlock(b);

        LLVMPositionBuilderAtEnd(b, err_bb);
        let err_val = build_err();
        LLVMBuildBr(b, merge_bb);
        let err_incoming = LLVMGetInsertBlock(b);

        LLVMPositionBuilderAtEnd(b, merge_bb);
        let phi = LLVMBuildPhi(b, res_ty, c"result".as_ptr());
        let mut vals = [ok_val, err_val];
        let mut blocks = [ok_incoming, err_incoming];
        LLVMAddIncoming(phi, vals.as_mut_ptr(), blocks.as_mut_ptr(), 2);
        Some(ValueRef(phi))
    }

    // Emit the any retain guard: when the boxed value is a heap collection or a
    // str, GEP back from the data pointer to its ARC/String header and ARC-retain
    // it (non-atomic). The two flags are mutually exclusive. SSA names
    // (arc_hdr_from_data / str_hdr_from_data) are identical across wrap / unwrap /
    // try_unwrap, so the only call-site variation is `val_ptr`.
    unsafe fn any_retain_guard(
        &mut self,
        i8_ty: LLVMTypeRef,
        i64_ty: LLVMTypeRef,
        val_ptr: LLVMValueRef,
        do_collection_retain: bool,
        do_str_retain: bool,
    ) {
        let b = self.builder;
        if do_collection_retain {
            let mut gep = [LLVMConstInt(i64_ty, (-(ARC_HEADER_SIZE as i64)) as u64, 0)];
            let hdr = LLVMBuildGEP2(
                b,
                i8_ty,
                val_ptr,
                gep.as_mut_ptr(),
                1,
                c"arc_hdr_from_data".as_ptr(),
            );
            self.arc_retain(ValueRef(hdr), Atomicity::NonAtomic);
        } else if do_str_retain {
            let mut gep = [LLVMConstInt(
                i64_ty,
                (-(STRING_HEADER_SIZE as i64)) as u64,
                0,
            )];
            let hdr = LLVMBuildGEP2(
                b,
                i8_ty,
                val_ptr,
                gep.as_mut_ptr(),
                1,
                c"str_hdr_from_data".as_ptr(),
            );
            self.arc_retain(ValueRef(hdr), Atomicity::NonAtomic);
        }
    }
}
