// ABI struct-layout verification — C++ side (#1995).
//
// This translation unit is the C++ half of the C++ <-> Rust ABI layout
// contract for the LLVM IR emission shared library. It pins the sizeof,
// alignof, and every field offset of the descriptor structs, plus the
// sizeof/alignof of the opaque handle typedefs declared in
// include/ry/llvm_emit/api.h. The Rust half lives in
// crates/emit/src/lib.rs (the `const _: () = assert!(...)` block);
// both sides assert against the SAME constants, so any incidental drift
// (field reorder, padding, type-width change) on either side breaks the
// build.
//
// Method A (#1995): paired compile-time assertions that add no new
// extern "C" symbol to the ABI surface. The constants below are the
// 64-bit-target layout (x86-64 / arm64); the ABI is not built for 32-bit
// hosts. See docs/architecture/llvm-ir-emission-boundary.md for the
// verification model and the canonical value table.

#include "ry/llvm_emit/api.h"

#include <cstddef>

#include <gtest/gtest.h>

// === Descriptor structs: sizeof + alignof + every field offset ===

// RyCowEnsureUniqueDesc (12 fields) — api.h L429-465.
static_assert(sizeof(RyCowEnsureUniqueDesc) == 64,
              "RyCowEnsureUniqueDesc must be 64 bytes (mirror crates/emit/src/lib.rs)");
static_assert(alignof(RyCowEnsureUniqueDesc) == 8, "RyCowEnsureUniqueDesc must be 8-byte aligned");
static_assert(offsetof(RyCowEnsureUniqueDesc, data_ptr_id) == 0, "RyCowEnsureUniqueDesc.data_ptr_id @ 0");
static_assert(offsetof(RyCowEnsureUniqueDesc, slot_ptr_id) == 4, "RyCowEnsureUniqueDesc.slot_ptr_id @ 4");
static_assert(offsetof(RyCowEnsureUniqueDesc, kind) == 8, "RyCowEnsureUniqueDesc.kind @ 8");
static_assert(offsetof(RyCowEnsureUniqueDesc, atomic) == 12, "RyCowEnsureUniqueDesc.atomic @ 12");
static_assert(offsetof(RyCowEnsureUniqueDesc, elem_size) == 16, "RyCowEnsureUniqueDesc.elem_size @ 16");
static_assert(offsetof(RyCowEnsureUniqueDesc, key_size) == 24, "RyCowEnsureUniqueDesc.key_size @ 24");
static_assert(offsetof(RyCowEnsureUniqueDesc, val_size) == 32, "RyCowEnsureUniqueDesc.val_size @ 32");
static_assert(offsetof(RyCowEnsureUniqueDesc, do_elem_retain) == 40, "RyCowEnsureUniqueDesc.do_elem_retain @ 40");
static_assert(offsetof(RyCowEnsureUniqueDesc, elem_is_str) == 44, "RyCowEnsureUniqueDesc.elem_is_str @ 44");
static_assert(offsetof(RyCowEnsureUniqueDesc, do_key_retain) == 48, "RyCowEnsureUniqueDesc.do_key_retain @ 48");
static_assert(offsetof(RyCowEnsureUniqueDesc, key_is_str) == 52, "RyCowEnsureUniqueDesc.key_is_str @ 52");
static_assert(offsetof(RyCowEnsureUniqueDesc, destructor_callee) == 56,
              "RyCowEnsureUniqueDesc.destructor_callee @ 56");

// RyAnyWrapDesc (9 fields) — api.h L544-570.
static_assert(sizeof(RyAnyWrapDesc) == 56, "RyAnyWrapDesc must be 56 bytes");
static_assert(alignof(RyAnyWrapDesc) == 8, "RyAnyWrapDesc must be 8-byte aligned");
static_assert(offsetof(RyAnyWrapDesc, kind) == 0, "RyAnyWrapDesc.kind @ 0");
static_assert(offsetof(RyAnyWrapDesc, target_tag) == 8, "RyAnyWrapDesc.target_tag @ 8");
static_assert(offsetof(RyAnyWrapDesc, val_id) == 16, "RyAnyWrapDesc.val_id @ 16");
static_assert(offsetof(RyAnyWrapDesc, do_collection_retain) == 20, "RyAnyWrapDesc.do_collection_retain @ 20");
static_assert(offsetof(RyAnyWrapDesc, do_str_retain) == 24, "RyAnyWrapDesc.do_str_retain @ 24");
static_assert(offsetof(RyAnyWrapDesc, descriptor_id) == 28, "RyAnyWrapDesc.descriptor_id @ 28");
static_assert(offsetof(RyAnyWrapDesc, box_layout_ty) == 32, "RyAnyWrapDesc.box_layout_ty @ 32");
static_assert(offsetof(RyAnyWrapDesc, box_data_size) == 40, "RyAnyWrapDesc.box_data_size @ 40");
static_assert(offsetof(RyAnyWrapDesc, any_ty) == 48, "RyAnyWrapDesc.any_ty @ 48");

// RyAnyUnwrapDesc (14 fields) — api.h L595-628.
static_assert(sizeof(RyAnyUnwrapDesc) == 96, "RyAnyUnwrapDesc must be 96 bytes");
static_assert(alignof(RyAnyUnwrapDesc) == 8, "RyAnyUnwrapDesc must be 8-byte aligned");
static_assert(offsetof(RyAnyUnwrapDesc, kind) == 0, "RyAnyUnwrapDesc.kind @ 0");
static_assert(offsetof(RyAnyUnwrapDesc, any_val_id) == 4, "RyAnyUnwrapDesc.any_val_id @ 4");
static_assert(offsetof(RyAnyUnwrapDesc, any_ty) == 8, "RyAnyUnwrapDesc.any_ty @ 8");
static_assert(offsetof(RyAnyUnwrapDesc, target_ty) == 16, "RyAnyUnwrapDesc.target_ty @ 16");
static_assert(offsetof(RyAnyUnwrapDesc, expected_tag) == 24, "RyAnyUnwrapDesc.expected_tag @ 24");
static_assert(offsetof(RyAnyUnwrapDesc, do_collection_retain) == 32, "RyAnyUnwrapDesc.do_collection_retain @ 32");
static_assert(offsetof(RyAnyUnwrapDesc, do_str_retain) == 36, "RyAnyUnwrapDesc.do_str_retain @ 36");
static_assert(offsetof(RyAnyUnwrapDesc, mismatch_msg) == 40, "RyAnyUnwrapDesc.mismatch_msg @ 40");
static_assert(offsetof(RyAnyUnwrapDesc, mismatch_global_name) == 48, "RyAnyUnwrapDesc.mismatch_global_name @ 48");
static_assert(offsetof(RyAnyUnwrapDesc, expected_desc_id) == 56, "RyAnyUnwrapDesc.expected_desc_id @ 56");
static_assert(offsetof(RyAnyUnwrapDesc, box_layout_ty) == 64, "RyAnyUnwrapDesc.box_layout_ty @ 64");
static_assert(offsetof(RyAnyUnwrapDesc, record_struct_ty) == 72, "RyAnyUnwrapDesc.record_struct_ty @ 72");
static_assert(offsetof(RyAnyUnwrapDesc, desc_mismatch_msg) == 80, "RyAnyUnwrapDesc.desc_mismatch_msg @ 80");
static_assert(offsetof(RyAnyUnwrapDesc, desc_mismatch_global_name) == 88,
              "RyAnyUnwrapDesc.desc_mismatch_global_name @ 88");

// RyAnyTryUnwrapDesc (10 fields) — api.h L652-674.
static_assert(sizeof(RyAnyTryUnwrapDesc) == 64, "RyAnyTryUnwrapDesc must be 64 bytes");
static_assert(alignof(RyAnyTryUnwrapDesc) == 8, "RyAnyTryUnwrapDesc must be 8-byte aligned");
static_assert(offsetof(RyAnyTryUnwrapDesc, kind) == 0, "RyAnyTryUnwrapDesc.kind @ 0");
static_assert(offsetof(RyAnyTryUnwrapDesc, any_val_id) == 4, "RyAnyTryUnwrapDesc.any_val_id @ 4");
static_assert(offsetof(RyAnyTryUnwrapDesc, any_ty) == 8, "RyAnyTryUnwrapDesc.any_ty @ 8");
static_assert(offsetof(RyAnyTryUnwrapDesc, res_ty) == 16, "RyAnyTryUnwrapDesc.res_ty @ 16");
static_assert(offsetof(RyAnyTryUnwrapDesc, error_ty) == 24, "RyAnyTryUnwrapDesc.error_ty @ 24");
static_assert(offsetof(RyAnyTryUnwrapDesc, target_ty) == 32, "RyAnyTryUnwrapDesc.target_ty @ 32");
static_assert(offsetof(RyAnyTryUnwrapDesc, expected_tag) == 40, "RyAnyTryUnwrapDesc.expected_tag @ 40");
static_assert(offsetof(RyAnyTryUnwrapDesc, do_collection_retain) == 48, "RyAnyTryUnwrapDesc.do_collection_retain @ 48");
static_assert(offsetof(RyAnyTryUnwrapDesc, do_str_retain) == 52, "RyAnyTryUnwrapDesc.do_str_retain @ 52");
static_assert(offsetof(RyAnyTryUnwrapDesc, err_msg_str_id) == 56, "RyAnyTryUnwrapDesc.err_msg_str_id @ 56");

// === Opaque handle typedefs: sizeof + alignof only (no fields) ===
// All are pointers to incomplete structs -> 8 bytes / 8-byte aligned on
// the 64-bit ABI. Per-field offset is N/A — handles carry no fields.
static_assert(sizeof(RyModuleRef) == 8 && alignof(RyModuleRef) == 8, "RyModuleRef is an 8-byte pointer");
static_assert(sizeof(RyBuilderRef) == 8 && alignof(RyBuilderRef) == 8, "RyBuilderRef is an 8-byte pointer");
static_assert(sizeof(RyContextRef) == 8 && alignof(RyContextRef) == 8, "RyContextRef is an 8-byte pointer");
static_assert(sizeof(RyFunctionRef) == 8 && alignof(RyFunctionRef) == 8, "RyFunctionRef is an 8-byte pointer");
static_assert(sizeof(RyTypeRef) == 8 && alignof(RyTypeRef) == 8, "RyTypeRef is an 8-byte pointer");
static_assert(sizeof(RyFuncTypeRef) == 8 && alignof(RyFuncTypeRef) == 8, "RyFuncTypeRef is an 8-byte pointer");
static_assert(sizeof(RyValueRef) == 8 && alignof(RyValueRef) == 8, "RyValueRef is an 8-byte pointer");
static_assert(sizeof(RyBasicBlockRef) == 8 && alignof(RyBasicBlockRef) == 8, "RyBasicBlockRef is an 8-byte pointer");

// === Scalar intern-handle typedefs (uint32_t) ===
static_assert(sizeof(RyValueId) == 4 && alignof(RyValueId) == 4, "RyValueId is a 4-byte uint32_t");

// === Enum selectors: underlying type is `int` (4 bytes) ===
// The descriptor structs store these as plain `int` fields; locking the
// enum width here documents that the function-argument enums match the
// Rust side's `c_int` representation. (No descriptor field is enum-typed.)
static_assert(sizeof(RyBoundsKind) == sizeof(int), "RyBoundsKind underlying type is int");
static_assert(sizeof(RyArcAtomic) == sizeof(int), "RyArcAtomic underlying type is int");
static_assert(sizeof(RyCowKind) == sizeof(int), "RyCowKind underlying type is int");

// === RyICmpPred cross-language parity (#2143) ===
// The C++ enumerator literals (api.h L985-996) and the Rust `RY_ICMP_*`
// constants (crates/emit/src/abi.rs) were previously protected only by a
// prose "MUST match" comment. Two layered guards now pin them mechanically:
//   1. `static_assert` below pins the C++ side at compile-time — any
//      reorder of the api.h enum that changes a numeric value fires.
//   2. `RyICmpPredRustMirrorMatchesCanonical` below calls the test-only
//      FFI extern `ry_emit_test_icmp_pred_values` (crates/emit/src/abi/
//      test_introspect.rs) which reports the Rust constants by value; a
//      Rust-only edit that drifts from C++ fires the runtime test.
// Following the precedent in .claude/rules/codegen-llvm-ir-conventions.md
// "Rust mirrors of C++ header structs need a cross-language parity guard,
// not a 'keep in sync' comment".
static_assert(RY_ICMP_EQ == 0 && RY_ICMP_NE == 1,
              "RyICmpPred EQ/NE literal mismatch (api.h reorder?)");
static_assert(RY_ICMP_SLT == 2 && RY_ICMP_SLE == 3 && RY_ICMP_SGT == 4 && RY_ICMP_SGE == 5,
              "RyICmpPred signed predicates literal mismatch (api.h reorder?)");
static_assert(RY_ICMP_ULT == 6 && RY_ICMP_ULE == 7 && RY_ICMP_UGT == 8 && RY_ICMP_UGE == 9,
              "RyICmpPred unsigned predicates literal mismatch (api.h reorder?)");

extern "C" {
// Mirror of the Rust #[repr(C)] RyTestICmpPredValues
// (crates/emit/src/abi/test_introspect.rs).
struct RyTestICmpPredValues {
    int eq, ne;
    int slt, sle, sgt, sge;
    int ult, ule, ugt, uge;
};
RyTestICmpPredValues ry_emit_test_icmp_pred_values();
}

TEST(AbiLayout, RyICmpPredRustMirrorMatchesCanonical) {
    const RyTestICmpPredValues rust = ry_emit_test_icmp_pred_values();
    EXPECT_EQ(rust.eq,  static_cast<int>(RY_ICMP_EQ));
    EXPECT_EQ(rust.ne,  static_cast<int>(RY_ICMP_NE));
    EXPECT_EQ(rust.slt, static_cast<int>(RY_ICMP_SLT));
    EXPECT_EQ(rust.sle, static_cast<int>(RY_ICMP_SLE));
    EXPECT_EQ(rust.sgt, static_cast<int>(RY_ICMP_SGT));
    EXPECT_EQ(rust.sge, static_cast<int>(RY_ICMP_SGE));
    EXPECT_EQ(rust.ult, static_cast<int>(RY_ICMP_ULT));
    EXPECT_EQ(rust.ule, static_cast<int>(RY_ICMP_ULE));
    EXPECT_EQ(rust.ugt, static_cast<int>(RY_ICMP_UGT));
    EXPECT_EQ(rust.uge, static_cast<int>(RY_ICMP_UGE));
}

// A no-op runtime test so CTest shows a named green signal confirming this
// TU (and therefore every static_assert above) compiled successfully.
TEST(AbiLayout, LayoutContractCompiled) { SUCCEED(); }
