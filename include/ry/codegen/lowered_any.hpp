#pragma once

#include <cstdint>
#include <string>

namespace llvm {
class Constant;
class StructType;
class Type;
class Value;
}

namespace ry {
class CodeGen;
}

namespace ry::codegen::lowered {

// AnyWrapKind selects the IR shape behind ry_emit_any_wrap.
//   NonBox    — primitive (int/float/bool) / str / collection pointer stored
//               directly in `any.data[8]`. Bool (i1) is ZExt'd to i64 by the
//               boundary ("any.bool.zext") when needed. The do_collection_retain /
//               do_str_retain flags decide whether the boundary emits
//               emitArcGetHeaderFromData / emitStrGetHeaderFromData + ARC
//               retain BEFORE the alloca+store sequence.
//   RecordBox — heap-box layout `[ ArcHeader (16B) | desc ptr (8B) | record
//               struct ]` via emitArcAlloc (counter delta +1, strong=1,
//               weak=0). target_tag must be Record=8.
//   EnumBox   — structurally identical to RecordBox; target_tag must be
//               Enum=9. Carries the per-enum descriptor instead of the
//               per-record descriptor.
//
// CodeGen-side responsibilities (NOT crossed via the boundary):
//   - Type-name resolution (findEnumLikeTypeNameForBoxing /
//     findRecordInfoForType / findRecordTypeName / buildTypeNameFromMeta)
//   - Descriptor lookup (getOrCreateRecordDescriptor /
//     getOrCreateEnumDescriptor)
//   - Layout-type construction (recordBoxLayoutType / enumBoxLayoutType)
//   - Reject decisions (canAnyHoldType, non-record struct, non-collection
//     pointer types)
//   - Typed-collection registration via __ry_any_register_typed_coll
//   - Tag computation (getAnyTypeTagForValue / getAnyTypeTag)
//   - Field-wise retain via emitRecordArcFieldsRetain /
//     emitEnumBoxArcFieldsRetain (RecordBox / EnumBox, gated by the
//     reassignment guard `!isa<CallInst> && !isa<InvokeInst>`)
//   - propagateTypeMeta on the wrap result and arc_any_managed_vars_
//     registration two levels up at the alloca site
enum class AnyWrapKind : int {
    NonBox = 0,
    RecordBox = 1,
    EnumBox = 2,
};

struct AnyWrapOp {
    AnyWrapKind kind;
    int64_t target_tag;
    llvm::Value *val;
    bool do_collection_retain;
    bool do_str_retain;
    llvm::Constant *descriptor;
    llvm::StructType *box_layout_ty;
    uint64_t box_data_size;
    llvm::StructType *any_ty;
};

// AnyUnwrapKind selects the IR shape behind ry_emit_any_unwrap.
//   Standard   — 2-way path. tag check → matchBB(alloca + store + GEP +
//                load + optional ARC retain) or mismatchBB(emitRuntimeError,
//                terminates with unreachable).
//   F64Promote — 5-BB path joining `any.float` (load f64) and
//                `any.int2float` (sitofp(load i64)) into `any.merge` with a
//                PHI(f64). `any.mismatch` is the trap path.
//   Record     — tag check + descriptor chain walk via
//                `__ry_record_is_subtype_desc(actualDesc, expectedDesc)`,
//                then payload GEP/load from box_layout_ty slot 1. The
//                Parent-prefix-of-Child layout invariant
//                (`roundUp(8, alignof(ParentStructTy)) == 8`) lets the
//                target's struct type GEP into the box even when the
//                actual box was constructed for a Child.
//
// CodeGen-side responsibilities (NOT crossed via the boundary):
//   - substituteTypeParamsInName at entry
//   - Generic-substitution guard rejecting non-`any` element types
//   - Enum-target dispatch to unwrapEnumFromAny (stays CodeGen-private)
//   - Record-info / record-typename / record-descriptor / record-layout
//     resolution
//   - Collection / str classification via resolveTypeAlias +
//     isListTypeName / isMapTypeName / isSetTypeName to decide
//     expected_tag and do_{collection,str}_retain
//   - Field-wise retain on the returned record struct
//     (emitRecordArcFieldsRetain) — applied AFTER the boundary returns
enum class AnyUnwrapKind : int {
    Standard = 0,
    F64Promote = 1,
    Record = 2,
};

struct AnyUnwrapOp {
    AnyUnwrapKind kind;
    llvm::Value *any_val;
    llvm::StructType *any_ty;
    llvm::Type *target_ty;
    int64_t expected_tag;
    bool do_collection_retain;
    bool do_str_retain;
    std::string mismatch_msg;
    std::string mismatch_global_name;
    llvm::Constant *expected_desc;
    llvm::StructType *box_layout_ty;
    llvm::StructType *record_struct_ty;
    std::string desc_mismatch_msg;
    std::string desc_mismatch_global_name;
};

// AnyTryUnwrapKind selects the IR shape behind ry_emit_any_try_unwrap.
// Both kinds wrap an emitResultBranch invocation (3 BBs + PHI on resTy).
//   Standard   — tag-check primitive arm covering int (i64), bool (i1),
//                str (ptr + Str tag), and the ARC-pointer + collection-tag
//                cases (List<any> / Map<str,any> / Set<any>).
//                Ok-branch:   alloca + store + GEP + load + optional retain
//                             + buildOkValue (UndefValue + 3 InsertValues).
//                Err-branch:  buildErrValue with inline Error
//                             `{ err_msg_str, 0 }`.
//   F64Promote — f64 target with int auto-promote. Both loads share one
//                alloca; the Ok branch picks via select(isFloat, fVal,
//                sitofp(iVal)).
//
// Typed-arm dispatch (Record / List<T> / Map<str,V> / Option / Result / simple
// enum / Set<T>) stays CodeGen-private and is NOT reached through this op —
// the corresponding sub-helpers (tryUnwrapRecordFromAny / tryUnwrapListFromAny
// / tryUnwrapMapFromAny / tryUnwrapOptionFromAny / unwrapEnumFromAny) handle
// those before falling through to this op.
enum class AnyTryUnwrapKind : int {
    Standard = 0,
    F64Promote = 1,
};

struct AnyTryUnwrapOp {
    AnyTryUnwrapKind kind;
    llvm::Value *any_val;
    llvm::StructType *any_ty;
    llvm::StructType *res_ty;
    llvm::StructType *error_ty;
    llvm::Type *target_ty;
    int64_t expected_tag;
    bool do_collection_retain;
    bool do_str_retain;
    llvm::Value *err_msg_str;
};

} // namespace ry::codegen::lowered

namespace ry::codegen::lowering {

// Passthrough lowerings — no IRBuilder usage, just struct construction.

lowered::AnyWrapOp lowerAnyWrap(CodeGen &cg, lowered::AnyWrapKind kind,
                                int64_t target_tag, llvm::Value *val,
                                bool do_collection_retain, bool do_str_retain,
                                llvm::Constant *descriptor,
                                llvm::StructType *box_layout_ty,
                                uint64_t box_data_size,
                                llvm::StructType *any_ty);

lowered::AnyUnwrapOp lowerAnyUnwrap(
    CodeGen &cg, lowered::AnyUnwrapKind kind, llvm::Value *any_val,
    llvm::StructType *any_ty, llvm::Type *target_ty, int64_t expected_tag,
    bool do_collection_retain, bool do_str_retain, std::string mismatch_msg,
    std::string mismatch_global_name, llvm::Constant *expected_desc,
    llvm::StructType *box_layout_ty, llvm::StructType *record_struct_ty,
    std::string desc_mismatch_msg, std::string desc_mismatch_global_name);

lowered::AnyTryUnwrapOp lowerAnyTryUnwrap(
    CodeGen &cg, lowered::AnyTryUnwrapKind kind, llvm::Value *any_val,
    llvm::StructType *any_ty, llvm::StructType *res_ty,
    llvm::StructType *error_ty, llvm::Type *target_ty, int64_t expected_tag,
    bool do_collection_retain, bool do_str_retain, llvm::Value *err_msg_str);

} // namespace ry::codegen::lowering

namespace ry::codegen::emission {

// Emit AnyWrap via the libemit boundary (ry_emit_any_wrap). Returns the
// final RyAny aggregate value (loaded from the internal alloca).
//
// Precondition: ry_emit_ctx_set_function(cg.emit_ctx_, cg.fn_) must have
// been called before invoking this helper.
//
// Caller responsibilities BEFORE invoking (none of these cross the boundary
// because they depend on ValueMetadata / CodeGen-private state):
//   - typed_coll registration via __ry_any_register_typed_coll for NonBox
//     wraps whose ValueMetadata says the element type is non-any
//   - Field-wise retain (RecordBox via emitRecordArcFieldsRetain, EnumBox
//     via emitEnumBoxArcFieldsRetain) when the reassignment guard
//     `!isa<CallInst> && !isa<InvokeInst>` holds
llvm::Value *emitAnyWrap(CodeGen &cg, const lowered::AnyWrapOp &op);

// Emit AnyUnwrap via the libemit boundary (ry_emit_any_unwrap). Returns
// the unwrapped value (targetTy for Standard / Record arms; f64 for the
// F64Promote arm).
//
// Precondition: ry_emit_ctx_set_function(cg.emit_ctx_, cg.fn_) must have
// been called before invoking this helper.
//
// Caller responsibilities AFTER invoking:
//   - For AnyUnwrapKind::Record: emitRecordArcFieldsRetain on the returned
//     record struct value so the unwrapped record can release independently
//     from the box's destructor at scope exit
llvm::Value *emitAnyUnwrap(CodeGen &cg, const lowered::AnyUnwrapOp &op);

// Emit AnyTryUnwrap via the libemit boundary (ry_emit_any_try_unwrap).
// Returns the Result<targetTy, Error> aggregate value (the PHI joined by
// emitResultBranch).
//
// Precondition: ry_emit_ctx_set_function(cg.emit_ctx_, cg.fn_) must have
// been called before invoking this helper.
llvm::Value *emitAnyTryUnwrap(CodeGen &cg, const lowered::AnyTryUnwrapOp &op);

} // namespace ry::codegen::emission
