#pragma once

#include <cstdint>

namespace llvm {
class StructType;
class Type;
class Value;
}

namespace ry {
class CodeGen;
}

namespace ry::codegen::lowered {

// Pre-lowered List append op. `list_header_ty` is CodeGen::listHeaderTy_
// (`{i64 len, i64 cap, ptr data}`); `elem_ty` is the per-element LLVM type and
// `elem_size` its DataLayout-derived allocation size. The op carries no ARC /
// metadata propagation hints — those decisions are caller-side because they
// depend on ValueMetadata that does not cross the llvm_emit boundary.
struct CollectionAppendOp {
    llvm::Value *list_ptr;
    llvm::Value *val;
    llvm::StructType *list_header_ty;
    llvm::Type *elem_ty;
    uint64_t elem_size;
};

// Pre-lowered List insert op. Same shape as append + an `idx` operand. ARC
// retain on `val` is the caller's responsibility (emitted before the boundary
// call) because the type metadata that decides whether a retain is needed
// does not cross the boundary.
struct CollectionInsertOp {
    llvm::Value *list_ptr;
    llvm::Value *idx;
    llvm::Value *val;
    llvm::StructType *list_header_ty;
    llvm::Type *elem_ty;
    uint64_t elem_size;
};

// Pre-lowered List removeAt op. The removed element is returned by the
// emission layer (caller may release it based on element type metadata).
struct CollectionRemoveAtOp {
    llvm::Value *list_ptr;
    llvm::Value *idx;
    llvm::StructType *list_header_ty;
    llvm::Type *elem_ty;
    uint64_t elem_size;
};

// Pre-lowered List slice op. Produces a fresh heap buffer copied from
// `list_ptr.data + start * elem_size` of length `end_excl - start` (both
// endpoints clamped to [0, list.len]). The new ARC header allocation,
// per-element ARC retain loop, and type-metadata propagation are caller-side
// because they need ValueMetadata that does not cross the boundary.
struct ListSliceOp {
    llvm::Value *list_ptr;
    llvm::Value *start;
    llvm::Value *end_excl;
    llvm::StructType *list_header_ty;
    llvm::Type *elem_ty;
    uint64_t elem_size;
};

// Multi-value emission result for ListSliceOp. `count` is the slice length
// (i64), `new_data` is the malloc'd buffer (ptr).
struct ListSliceResult {
    llvm::Value *count;
    llvm::Value *new_data;
};

} // namespace ry::codegen::lowered

namespace ry::codegen::lowering {

// Passthrough lowerings — no IRBuilder usage, just struct construction.
lowered::CollectionAppendOp lowerCollectionAppend(CodeGen &cg,
                                                  llvm::Value *list_ptr,
                                                  llvm::Value *val,
                                                  llvm::StructType *list_header_ty,
                                                  llvm::Type *elem_ty,
                                                  uint64_t elem_size);

lowered::CollectionInsertOp lowerCollectionInsert(CodeGen &cg,
                                                  llvm::Value *list_ptr,
                                                  llvm::Value *idx,
                                                  llvm::Value *val,
                                                  llvm::StructType *list_header_ty,
                                                  llvm::Type *elem_ty,
                                                  uint64_t elem_size);

lowered::CollectionRemoveAtOp lowerCollectionRemoveAt(CodeGen &cg,
                                                     llvm::Value *list_ptr,
                                                     llvm::Value *idx,
                                                     llvm::StructType *list_header_ty,
                                                     llvm::Type *elem_ty,
                                                     uint64_t elem_size);

lowered::ListSliceOp lowerListSlice(CodeGen &cg, llvm::Value *list_ptr,
                                    llvm::Value *start,
                                    llvm::Value *end_excl,
                                    llvm::StructType *list_header_ty,
                                    llvm::Type *elem_ty, uint64_t elem_size);

} // namespace ry::codegen::lowering

namespace ry::codegen::emission {

// Emit a List append via the libry_codegen boundary (ry_emit_collection_append).
// Precondition: ry_emit_ctx_set_function(cg.emit_ctx_, cg.fn_) must have been
// called before invoking this helper because two BBs (`app.grow`, `app.store`)
// are created inside the current function. ARC retain on op.val (if needed by
// the element type) must be emitted BEFORE calling this helper.
void emitCollectionAppend(CodeGen &cg, const lowered::CollectionAppendOp &op);

// Emit a List insert via the libry_codegen boundary (ry_emit_collection_insert).
// Precondition: same as emitCollectionAppend (four BBs created: `ins.err`,
// `ins.ok`, `ins.grow`, `ins.move`). ARC retain on op.val (if needed) must
// be emitted BEFORE invoking.
void emitCollectionInsert(CodeGen &cg, const lowered::CollectionInsertOp &op);

// Emit a List removeAt via ry_emit_collection_remove_at. Returns the removed
// element (caller decides whether to release based on element-type metadata).
// Precondition: same as emitCollectionAppend (`rmat.err`, `rmat.ok` BBs).
llvm::Value *emitCollectionRemoveAt(CodeGen &cg,
                                    const lowered::CollectionRemoveAtOp &op);

// Emit a List slice via ry_emit_list_slice. Branchless (no new BBs), so no
// precondition on ry_emit_ctx_set_function. Returns {count, new_data}; the
// caller is responsible for allocating the new ARC header, storing header
// fields, propagating type metadata, and emitting any ARC retain loop over
// the copied elements.
lowered::ListSliceResult emitListSlice(CodeGen &cg,
                                       const lowered::ListSliceOp &op);

} // namespace ry::codegen::emission
