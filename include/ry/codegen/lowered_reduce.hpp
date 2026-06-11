#pragma once

namespace llvm {
class Type;
class Value;
}

namespace ry {
class CodeGen;
}

// Numeric reduce builtins (sum / min / max, #2092) — the emission-layer wrappers
// over the `ry_emit_reduce_*` boundary. Unlike the collection / cow / bounds ops
// there is no lowering phase (no `lowered::` struct, no `lowering::` passthrough):
// the reduce call sites in codegen_call_higher_order.cpp already hold the operand
// `llvm::Value*`s, so these wrappers just intern → call the boundary → resolve.
//
// Two shapes mirror crates/emit/src/reduce.rs: the list forms emit a whole loop
// in one call; the variadic forms expose a per-step op the C++ fold loop drives
// once per argument (a single coarse "fold the array" op would reorder the
// operand loads and break byte-exact IR).
namespace ry::codegen::emission {

// sum([..]): emit the list-sum loop; return the accumulated result.
// `list_header_ty` is CodeGen::listHeaderTy_. Precondition: builder positioned
// within a function (BBs created inside the boundary).
llvm::Value *emitReduceSumList(CodeGen &cg, llvm::Value *list_ptr,
                               llvm::Type *elem_ty, llvm::Type *list_header_ty);

// sum(a, b, ..): emit one fold step `acc + v` (sum_v). Called once per argument
// after the C++ side seeds the accumulator with arg[0].
llvm::Value *emitReduceSumStep(CodeGen &cg, llvm::Value *acc, llvm::Value *v,
                               llvm::Type *elem_ty);

// min/max([..]): emit the seed + loop at the `mm.ok` block the caller is
// positioned at (the empty-list guard + emitRuntimeError stay C++-side).
// `data` / `len` are the C++-loaded mm_data / mm_len. Returns mm_result.
// Precondition: builder positioned at mm.ok (BBs created inside the boundary).
llvm::Value *emitReduceMinmaxListLoop(CodeGen &cg, llvm::Value *data,
                                      llvm::Value *len, llvm::Type *elem_ty,
                                      bool is_max);

// min/max(a, b, ..): emit one fold step (mm_cmp + mm_best select). Called once
// per argument after the C++ side seeds with arg[0].
llvm::Value *emitReduceMinmaxStep(CodeGen &cg, llvm::Value *best, llvm::Value *v,
                                  llvm::Type *elem_ty, bool is_max);

} // namespace ry::codegen::emission
