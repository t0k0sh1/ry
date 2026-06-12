// Direct-call regression tests for the emit ABI input-validation
// guards (#2028 review hardening). These exercise the NULL / None reject
// branches that are unreachable from the normal CodeGen caller (which always
// passes valid handles and non-null trampolines), so a Ry self-test cannot
// trigger them — the only way to lock them in is to call the extern "C" ABI
// directly. ry_tests links the emit cdylib, so the symbols resolve.
//
// Covered guards:
//   - ctx == NULL  → sentinel (0 / NULL) for build_error_from_runtime,
//     get_runtime_fn, result_branch; early-return for the void-returning
//     arc_retain / arc_release (#2057).
//   - result_branch build_ok / build_err == NULL → sentinel 0 (replaces the
//     former `build_ok.unwrap()` panic across the extern "C" boundary).
//   - #2080 extends the same contract to the remaining entry points
//     (control_flow / collection / bounds / option / lifecycle): ctx == NULL →
//     sentinel / no-op; the reachable non-ctx UB cases (create_phi with NULL
//     incoming arrays, list_slice with NULL out-params, arc retain / release on
//     an id that resolves to a NULL header); plus, with a real ctx, the NULL
//     type / fn handle guards and the `resolve_value → None` reject on a
//     value-returning entry. See the #2080 section below.
//
// Deeper guards that need a *corrupted* ctx (c.module / c.context / c.builder
// NULL) have no supported-API trigger and are documented as defensive in
// .claude/rules/tests-rejection-tdd.md rather than tested here.

#include <gtest/gtest.h>

#include "ry/llvm_emit/api.h"
#include "ry/llvm_emit/cast_helpers.hpp"

#include <llvm/IR/Constants.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Type.h>

#include <memory>

using namespace ry::llvm_emit;

namespace {

// Non-null callback so the ctx-NULL guard (not the callback guard) is the
// trigger in the ResultBranchNullCtx test.
RyValueId dummyBuildValue(void * /*user_ctx*/) { return 0; }

class EmitAbiGuardTest : public ::testing::Test {
protected:
    llvm::LLVMContext llctx_;
    std::unique_ptr<llvm::Module> module_;
    std::unique_ptr<llvm::IRBuilder<>> builder_;
    llvm::StructType *resultTy_ = nullptr; // stands in for Result<T, E>
    llvm::FunctionType *fnTy_ = nullptr;   // stands in for a runtime-fn type

    void SetUp() override {
        module_ = std::make_unique<llvm::Module>("abi_guard_test", llctx_);
        builder_ = std::make_unique<llvm::IRBuilder<>>(llctx_);
        // Variadic StructType::get (derives the context from the first elem)
        // avoids the StructType::get(LLVMContext&, bool) overload trap that an
        // ArrayRef-from-C-array argument falls into.
        resultTy_ = llvm::StructType::get(llvm::Type::getInt1Ty(llctx_),
                                          llvm::Type::getInt64Ty(llctx_),
                                          llvm::PointerType::get(llctx_, 0));
        fnTy_ = llvm::FunctionType::get(llvm::Type::getVoidTy(llctx_), false);
    }

    RyEmitCtx *makeCtx() {
        return ry_emit_ctx_create(asRyModule(module_.get()), asRyBuilder(builder_.get()),
                                  asRyContext(&llctx_));
    }

    // Intern a free-standing i64 constant so a value id resolves to a non-NULL
    // handle — lets a reduce guard test isolate the NULL-type branch (which sits
    // AFTER resolve_value) rather than tripping the resolve_value → None branch.
    RyValueId internDummy(RyEmitCtx *ctx) {
        llvm::Value *v = llvm::ConstantInt::get(llvm::Type::getInt64Ty(llctx_), 0);
        return ry_emit_intern(ctx, asRyValue(v));
    }

    // A BasicBlock inside a throwaway function — gives a test a valid (non-NULL)
    // block handle so it can isolate a guard OTHER than the NULL-block one. The
    // block gets no terminator: these guard tests reject before any IR reaches it.
    llvm::BasicBlock *makeBB() {
        auto *fnTy = llvm::FunctionType::get(llvm::Type::getVoidTy(llctx_), false);
        auto *fn = llvm::Function::Create(fnTy, llvm::Function::ExternalLinkage,
                                          "guard_test_fn", module_.get());
        return llvm::BasicBlock::Create(llctx_, "entry", fn);
    }
};

// --- ctx == NULL → sentinel (directly triggers the new ctx-NULL guard) ---

TEST_F(EmitAbiGuardTest, BuildErrorFromRuntimeNullCtxReturnsZero) {
    EXPECT_EQ(ry_emit_build_error_from_runtime(nullptr, "ry_err_fn", asRyType(resultTy_)), 0u);
}

TEST_F(EmitAbiGuardTest, GetRuntimeFnNullCtxReturnsNull) {
    EXPECT_EQ(ry_emit_get_runtime_fn(nullptr, "ry_runtime_fn", asRyFuncType(fnTy_)), nullptr);
}

TEST_F(EmitAbiGuardTest, ResultBranchNullCtxReturnsZero) {
    EXPECT_EQ(ry_emit_result_branch(nullptr, /*is_err_id=*/0, asRyType(resultTy_),
                                    &dummyBuildValue, &dummyBuildValue, /*user_ctx=*/nullptr),
              0u);
}

// --- result_branch with NULL callbacks → sentinel 0 (Critical: replaces the
//     former unwrap()-panic across the extern "C" boundary) ---

TEST_F(EmitAbiGuardTest, ResultBranchNullCallbacksReturnsZero) {
    RyEmitCtx *ctx = makeCtx();
    ASSERT_NE(ctx, nullptr);
    EXPECT_EQ(ry_emit_result_branch(ctx, /*is_err_id=*/0, asRyType(resultTy_),
                                    /*build_ok=*/nullptr, /*build_err=*/nullptr,
                                    /*user_ctx=*/nullptr),
              0u);
    ry_emit_ctx_destroy(ctx);
}

// --- arc_retain / arc_release return void; a NULL ctx must early-return rather
//     than dereference (UB). These match the prevailing ctx-NULL guard across
//     the other emit entry points (#2057 arc migration). The test passes by the
//     call returning without a crash. ---

TEST_F(EmitAbiGuardTest, ArcRetainNullCtxDoesNotCrash) {
    ry_emit_arc_retain(nullptr, /*header_ptr_id=*/0, RY_ARC_NONATOMIC);
    SUCCEED();
}

TEST_F(EmitAbiGuardTest, ArcReleaseNullCtxDoesNotCrash) {
    ry_emit_arc_release(nullptr, /*header_ptr_id=*/0, RY_ARC_NONATOMIC,
                        /*destructor_callee=*/nullptr, /*gc_visit_fn=*/nullptr);
    SUCCEED();
}

// =============================================================================
// #2080 — uniform input-validation guards across the remaining entry points
// (control_flow / collection / bounds / option / lifecycle), mirroring the
// validated runtime_call / result / any / cow contract via the shared
// `checked_cx` / `resolve_value` / `ffi_slice` helpers.
//
// Same #2028 test-vs-document split: a guard reachable through a *supported*
// boundary call (NULL ctx, NULL FFI array, NULL out-param, or an id that
// resolves to a NULL handle) gets a direct-call case here; the inner
// c.module/context/builder == NULL guards need a *corrupted* ctx (no supported
// call produces one) and stay documented-only in
// .claude/rules/tests-rejection-tdd.md.
//
// Post-fix lock-in: pre-fix these branches dereferenced a NULL ctx / raw FFI
// pointer / out-param and crashed the process, so a clean "red" is impossible —
// the tests are authored against the fixed guards (same convention as the
// ResultBranch* cases above).
// =============================================================================

// --- ctx == NULL → sentinel (value-returning entries) ---

TEST_F(EmitAbiGuardTest, CreateBasicBlockNullCtxReturnsNull) {
    EXPECT_EQ(ry_emit_create_basic_block(nullptr, "bb", /*fn=*/nullptr), nullptr);
}

TEST_F(EmitAbiGuardTest, CreatePhiNullCtxReturnsZero) {
    EXPECT_EQ(ry_emit_create_phi(nullptr, /*ty=*/nullptr, /*incoming_values=*/nullptr,
                                 /*incoming_blocks=*/nullptr, /*count=*/0, /*name_hint=*/nullptr),
              0u);
}

TEST_F(EmitAbiGuardTest, CollectionRemoveAtNullCtxReturnsZero) {
    EXPECT_EQ(ry_emit_collection_remove_at(nullptr, /*list_ptr_id=*/0, /*idx_id=*/0,
                                           /*list_header_ty=*/nullptr, /*elem_ty=*/nullptr,
                                           /*elem_size=*/8),
              0u);
}

TEST_F(EmitAbiGuardTest, BoundsCheckNullCtxReturnsZero) {
    EXPECT_EQ(ry_emit_bounds_check(nullptr, /*idx_id=*/0, /*len_id=*/0, RY_BOUNDS_LIST,
                                   /*global_name=*/"g", /*bb_prefix=*/"p"),
              0u);
}

TEST_F(EmitAbiGuardTest, NegativeIndexWrapNullCtxReturnsZero) {
    EXPECT_EQ(ry_emit_negative_index_wrap(nullptr, /*idx_id=*/0, /*wrap_base_id=*/0,
                                          /*prefix=*/"p"),
              0u);
}

TEST_F(EmitAbiGuardTest, OptionWrapSomeNullCtxReturnsZero) {
    EXPECT_EQ(ry_emit_option_wrap_some(nullptr, /*inner_id=*/0, /*opt_ty=*/nullptr), 0u);
}

TEST_F(EmitAbiGuardTest, OptionWrapNoneNullCtxReturnsZero) {
    EXPECT_EQ(ry_emit_option_wrap_none(nullptr, /*opt_ty=*/nullptr), 0u);
}

TEST_F(EmitAbiGuardTest, InternNullCtxReturnsZero) {
    EXPECT_EQ(ry_emit_intern(nullptr, /*value=*/nullptr), 0u);
}

TEST_F(EmitAbiGuardTest, ResolveNullCtxReturnsNull) {
    EXPECT_EQ(ry_emit_resolve(nullptr, /*id=*/1), nullptr);
}

// --- ctx == NULL → no-op (void entries; the test passes by not crashing) ---

TEST_F(EmitAbiGuardTest, BranchCondNullCtxDoesNotCrash) {
    ry_emit_branch_cond(nullptr, /*cond=*/0, /*true_bb=*/nullptr, /*false_bb=*/nullptr);
    SUCCEED();
}

TEST_F(EmitAbiGuardTest, BranchUncondNullCtxDoesNotCrash) {
    ry_emit_branch_uncond(nullptr, /*target=*/nullptr);
    SUCCEED();
}

TEST_F(EmitAbiGuardTest, CollectionAppendNullCtxDoesNotCrash) {
    ry_emit_collection_append(nullptr, /*list_ptr_id=*/0, /*val_id=*/0, /*list_header_ty=*/nullptr,
                              /*elem_ty=*/nullptr, /*elem_size=*/8);
    SUCCEED();
}

TEST_F(EmitAbiGuardTest, CollectionInsertNullCtxDoesNotCrash) {
    ry_emit_collection_insert(nullptr, /*list_ptr_id=*/0, /*idx_id=*/0, /*val_id=*/0,
                              /*list_header_ty=*/nullptr, /*elem_ty=*/nullptr, /*elem_size=*/8);
    SUCCEED();
}

TEST_F(EmitAbiGuardTest, ListSliceNullCtxDoesNotCrash) {
    ry_emit_list_slice(nullptr, /*list_ptr_id=*/0, /*start_id=*/0, /*end_excl_id=*/0,
                       /*list_header_ty=*/nullptr, /*elem_ty=*/nullptr, /*elem_size=*/8,
                       /*out_count=*/nullptr, /*out_new_data=*/nullptr);
    SUCCEED();
}

TEST_F(EmitAbiGuardTest, BoundsErrorNullCtxDoesNotCrash) {
    ry_emit_bounds_error(nullptr, /*orig_idx_id=*/0, /*len_id=*/0, /*fmt_msg=*/"%lld %lld",
                         /*global_name=*/"g");
    SUCCEED();
}

// --- Reachable non-ctx UB cases (the #2080-specific hardening; each needs a
//     real ctx so the guard — not the ctx-NULL check — is what fires) ---

// create_phi with count > 0 but NULL incoming arrays: pre-fix this raw-deref'd
// `*incoming_values.add(0)`; the ffi_slice guard now rejects to sentinel 0
// before any element access. `resultTy_` is a valid non-NULL type so the ty
// guard is passed and ffi_slice is the trigger.
TEST_F(EmitAbiGuardTest, CreatePhiNullIncomingArraysReturnsZero) {
    RyEmitCtx *ctx = makeCtx();
    ASSERT_NE(ctx, nullptr);
    EXPECT_EQ(ry_emit_create_phi(ctx, asRyType(resultTy_), /*incoming_values=*/nullptr,
                                 /*incoming_blocks=*/nullptr, /*count=*/1, /*name_hint=*/nullptr),
              0u);
    ry_emit_ctx_destroy(ctx);
}

// list_slice with NULL out-params: pre-fix this wrote through `*out_count` /
// `*out_new_data`; the out-param guard now makes it a no-op (the guard fires
// before the engine call, so the unpositioned builder is never touched).
TEST_F(EmitAbiGuardTest, ListSliceNullOutParamsDoesNotCrash) {
    RyEmitCtx *ctx = makeCtx();
    ASSERT_NE(ctx, nullptr);
    ry_emit_list_slice(ctx, /*list_ptr_id=*/0, /*start_id=*/0, /*end_excl_id=*/0,
                       /*list_header_ty=*/nullptr, /*elem_ty=*/nullptr, /*elem_size=*/8,
                       /*out_count=*/nullptr, /*out_new_data=*/nullptr);
    SUCCEED();
    ry_emit_ctx_destroy(ctx);
}

// arc_retain / arc_release with an id that resolves to a NULL header (sentinel
// id 0): the #2080 resolved-header guard makes both a no-op rather than feeding
// a NULL header to the retain / release IR (the issue's explicit arc gap).
TEST_F(EmitAbiGuardTest, ArcRetainNullHeaderDoesNotCrash) {
    RyEmitCtx *ctx = makeCtx();
    ASSERT_NE(ctx, nullptr);
    ry_emit_arc_retain(ctx, /*header_ptr_id=*/0, RY_ARC_NONATOMIC);
    SUCCEED();
    ry_emit_ctx_destroy(ctx);
}

TEST_F(EmitAbiGuardTest, ArcReleaseNullHeaderDoesNotCrash) {
    RyEmitCtx *ctx = makeCtx();
    ASSERT_NE(ctx, nullptr);
    ry_emit_arc_release(ctx, /*header_ptr_id=*/0, RY_ARC_NONATOMIC,
                        /*destructor_callee=*/nullptr, /*gc_visit_fn=*/nullptr);
    SUCCEED();
    ry_emit_ctx_destroy(ctx);
}

// --- valid ctx + NULL type / fn handle → sentinel. The ctx-NULL cases above
//     reject at `checked_cx` before reaching the handle guards, so these use a
//     real ctx to make the handle guard itself the trigger; each returns before
//     any builder use. ---

TEST_F(EmitAbiGuardTest, CreateBasicBlockNullFnReturnsNull) {
    RyEmitCtx *ctx = makeCtx();
    ASSERT_NE(ctx, nullptr);
    EXPECT_EQ(ry_emit_create_basic_block(ctx, "bb", /*fn=*/nullptr), nullptr);
    ry_emit_ctx_destroy(ctx);
}

TEST_F(EmitAbiGuardTest, CreatePhiNullTypeReturnsZero) {
    RyEmitCtx *ctx = makeCtx();
    ASSERT_NE(ctx, nullptr);
    EXPECT_EQ(ry_emit_create_phi(ctx, /*ty=*/nullptr, /*incoming_values=*/nullptr,
                                 /*incoming_blocks=*/nullptr, /*count=*/0, /*name_hint=*/nullptr),
              0u);
    ry_emit_ctx_destroy(ctx);
}

TEST_F(EmitAbiGuardTest, OptionWrapNoneNullTypeReturnsZero) {
    RyEmitCtx *ctx = makeCtx();
    ASSERT_NE(ctx, nullptr);
    EXPECT_EQ(ry_emit_option_wrap_none(ctx, /*opt_ty=*/nullptr), 0u);
    ry_emit_ctx_destroy(ctx);
}

// --- valid ctx + an id that resolves to NULL → sentinel on a value-returning
//     entry. The arc cases above cover the void no-op path; these cover the
//     `resolve_value → None → return 0` path, which fires before the engine /
//     builder is touched. ---

TEST_F(EmitAbiGuardTest, BoundsCheckNullResolvedIdReturnsZero) {
    RyEmitCtx *ctx = makeCtx();
    ASSERT_NE(ctx, nullptr);
    // idx_id 0 resolves to a NULL handle, so the guard fires before bounds_check
    // touches the (unpositioned) builder.
    EXPECT_EQ(ry_emit_bounds_check(ctx, /*idx_id=*/0, /*len_id=*/0, RY_BOUNDS_LIST,
                                   /*global_name=*/"g", /*bb_prefix=*/"p"),
              0u);
    ry_emit_ctx_destroy(ctx);
}

TEST_F(EmitAbiGuardTest, OptionWrapSomeNullResolvedInnerReturnsZero) {
    RyEmitCtx *ctx = makeCtx();
    ASSERT_NE(ctx, nullptr);
    // A valid opt_ty passes the type guard; inner_id 0 then resolves to NULL and
    // the resolve_value guard returns the sentinel.
    EXPECT_EQ(ry_emit_option_wrap_some(ctx, /*inner_id=*/0, asRyType(resultTy_)), 0u);
    ry_emit_ctx_destroy(ctx);
}

// =============================================================================
// #2072 — scalar / memory IR primitive guards ([C] = (ii) boundary move).
// The eleven new `ry_emit_*` primitives (alloca / load / store / gep / icmp /
// and / or / add / sub / select / const_int) share the same `checked_cx` /
// `resolve_value` contract, so a representative sample of each guard SHAPE
// suffices (the bit-exact string-op migration exercises every happy path). The
// `ry_emit_icmp` out-of-range-predicate guard is genuinely new logic
// (`icmp_pred_from → None`) with no precedent above, so it gets its own case.
// Same test-vs-document split: the inner corrupted-ctx guards stay documented in
// .claude/rules/tests-rejection-tdd.md.
// =============================================================================

// --- ctx == NULL → sentinel / no-op ---

TEST_F(EmitAbiGuardTest, AllocaNullCtxReturnsZero) {
    EXPECT_EQ(ry_emit_alloca(nullptr, asRyType(llvm::Type::getInt64Ty(llctx_)), "x"), 0u);
}

TEST_F(EmitAbiGuardTest, StoreNullCtxDoesNotCrash) {
    ry_emit_store(nullptr, /*val_id=*/0, /*ptr_id=*/0);
    SUCCEED();
}

// --- valid ctx + out-of-range icmp predicate → sentinel (new `icmp_pred_from`
//     logic; the guard fires before operand resolution). ---

TEST_F(EmitAbiGuardTest, IcmpUnknownPredicateReturnsZero) {
    RyEmitCtx *ctx = makeCtx();
    ASSERT_NE(ctx, nullptr);
    // 99 is outside the RyICmpPred range [0, 9]; icmp_pred_from returns None
    // before lhs/rhs are resolved, so the (sentinel) operand ids are irrelevant.
    EXPECT_EQ(ry_emit_icmp(ctx, /*predicate=*/99, /*lhs_id=*/0, /*rhs_id=*/0, "x"), 0u);
    ry_emit_ctx_destroy(ctx);
}

// --- valid ctx + a NULL type handle → sentinel (the `ty.is_null()` guard;
//     covers both the value-op and const-materialization shapes). ---

TEST_F(EmitAbiGuardTest, AllocaNullTypeReturnsZero) {
    RyEmitCtx *ctx = makeCtx();
    ASSERT_NE(ctx, nullptr);
    EXPECT_EQ(ry_emit_alloca(ctx, /*ty=*/nullptr, "x"), 0u);
    ry_emit_ctx_destroy(ctx);
}

TEST_F(EmitAbiGuardTest, ConstIntNullTypeReturnsZero) {
    RyEmitCtx *ctx = makeCtx();
    ASSERT_NE(ctx, nullptr);
    EXPECT_EQ(ry_emit_const_int(ctx, /*ty=*/nullptr, /*value=*/42, /*sign_extend=*/0), 0u);
    ry_emit_ctx_destroy(ctx);
}

// --- valid ctx + an operand id that resolves to NULL → sentinel (the
//     `resolve_value → None` guard; binary + ternary shapes). ---

TEST_F(EmitAbiGuardTest, AddNullResolvedOperandReturnsZero) {
    RyEmitCtx *ctx = makeCtx();
    ASSERT_NE(ctx, nullptr);
    // lhs_id 0 resolves to NULL, so the guard fires before the (unpositioned)
    // builder is touched.
    EXPECT_EQ(ry_emit_add(ctx, /*lhs_id=*/0, /*rhs_id=*/0, "x"), 0u);
    ry_emit_ctx_destroy(ctx);
}

TEST_F(EmitAbiGuardTest, SelectNullResolvedOperandReturnsZero) {
    RyEmitCtx *ctx = makeCtx();
    ASSERT_NE(ctx, nullptr);
    EXPECT_EQ(ry_emit_select(ctx, /*cond_id=*/0, /*then_id=*/0, /*else_id=*/0, "x"), 0u);
    ry_emit_ctx_destroy(ctx);
}

// =============================================================================
// #2092 — numeric reduce builtins (sum / min / max). Four value-returning
// entries; each gets the same three-guard contract: ctx == NULL, an operand id
// that resolves to None, and a NULL element-type handle. All return the sentinel
// 0. (No FFI-array params — the variadic forms are per-step ops, not array
// folds — so there is no ffi_slice guard to exercise here.)
// =============================================================================

// --- ctx == NULL → sentinel 0 ---

TEST_F(EmitAbiGuardTest, ReduceSumListNullCtxReturnsZero) {
    EXPECT_EQ(ry_emit_reduce_sum_list(nullptr, /*list_ptr_id=*/0, /*elem_ty=*/nullptr,
                                      /*list_header_ty=*/nullptr),
              0u);
}

TEST_F(EmitAbiGuardTest, ReduceSumStepNullCtxReturnsZero) {
    EXPECT_EQ(ry_emit_reduce_sum_step(nullptr, /*acc_id=*/0, /*val_id=*/0, /*elem_ty=*/nullptr),
              0u);
}

TEST_F(EmitAbiGuardTest, ReduceMinmaxListLoopNullCtxReturnsZero) {
    EXPECT_EQ(ry_emit_reduce_minmax_list_loop(nullptr, /*data_id=*/0, /*len_id=*/0,
                                              /*elem_ty=*/nullptr, /*is_max=*/0),
              0u);
}

TEST_F(EmitAbiGuardTest, ReduceMinmaxStepNullCtxReturnsZero) {
    EXPECT_EQ(ry_emit_reduce_minmax_step(nullptr, /*best_id=*/0, /*val_id=*/0, /*elem_ty=*/nullptr,
                                         /*is_max=*/1),
              0u);
}

// --- valid ctx + an operand id that resolves to None → sentinel 0. id 0 resolves
//     to a NULL handle, and resolve_value sits before the type guard, so a valid
//     elem_ty makes resolve_value the trigger. ---

TEST_F(EmitAbiGuardTest, ReduceSumListNullResolvedListPtrReturnsZero) {
    RyEmitCtx *ctx = makeCtx();
    ASSERT_NE(ctx, nullptr);
    EXPECT_EQ(ry_emit_reduce_sum_list(ctx, /*list_ptr_id=*/0, asRyType(resultTy_),
                                      asRyType(resultTy_)),
              0u);
    ry_emit_ctx_destroy(ctx);
}

TEST_F(EmitAbiGuardTest, ReduceSumStepNullResolvedAccReturnsZero) {
    RyEmitCtx *ctx = makeCtx();
    ASSERT_NE(ctx, nullptr);
    EXPECT_EQ(ry_emit_reduce_sum_step(ctx, /*acc_id=*/0, /*val_id=*/0, asRyType(resultTy_)), 0u);
    ry_emit_ctx_destroy(ctx);
}

TEST_F(EmitAbiGuardTest, ReduceMinmaxListLoopNullResolvedDataReturnsZero) {
    RyEmitCtx *ctx = makeCtx();
    ASSERT_NE(ctx, nullptr);
    EXPECT_EQ(ry_emit_reduce_minmax_list_loop(ctx, /*data_id=*/0, /*len_id=*/0,
                                              asRyType(resultTy_), /*is_max=*/0),
              0u);
    ry_emit_ctx_destroy(ctx);
}

TEST_F(EmitAbiGuardTest, ReduceMinmaxStepNullResolvedBestReturnsZero) {
    RyEmitCtx *ctx = makeCtx();
    ASSERT_NE(ctx, nullptr);
    EXPECT_EQ(ry_emit_reduce_minmax_step(ctx, /*best_id=*/0, /*val_id=*/0, asRyType(resultTy_),
                                         /*is_max=*/1),
              0u);
    ry_emit_ctx_destroy(ctx);
}

// --- valid ctx + resolved operands + NULL element type → sentinel 0. internDummy
//     gives each value id a non-NULL handle so the NULL-elem_ty branch (after
//     resolve_value) is what fires. ---

TEST_F(EmitAbiGuardTest, ReduceSumListNullElemTyReturnsZero) {
    RyEmitCtx *ctx = makeCtx();
    ASSERT_NE(ctx, nullptr);
    RyValueId listId = internDummy(ctx);
    EXPECT_EQ(ry_emit_reduce_sum_list(ctx, listId, /*elem_ty=*/nullptr, asRyType(resultTy_)), 0u);
    ry_emit_ctx_destroy(ctx);
}

TEST_F(EmitAbiGuardTest, ReduceSumStepNullElemTyReturnsZero) {
    RyEmitCtx *ctx = makeCtx();
    ASSERT_NE(ctx, nullptr);
    RyValueId id = internDummy(ctx);
    EXPECT_EQ(ry_emit_reduce_sum_step(ctx, id, id, /*elem_ty=*/nullptr), 0u);
    ry_emit_ctx_destroy(ctx);
}

TEST_F(EmitAbiGuardTest, ReduceMinmaxListLoopNullElemTyReturnsZero) {
    RyEmitCtx *ctx = makeCtx();
    ASSERT_NE(ctx, nullptr);
    RyValueId id = internDummy(ctx);
    EXPECT_EQ(ry_emit_reduce_minmax_list_loop(ctx, id, id, /*elem_ty=*/nullptr, /*is_max=*/0), 0u);
    ry_emit_ctx_destroy(ctx);
}

TEST_F(EmitAbiGuardTest, ReduceMinmaxStepNullElemTyReturnsZero) {
    RyEmitCtx *ctx = makeCtx();
    ASSERT_NE(ctx, nullptr);
    RyValueId id = internDummy(ctx);
    EXPECT_EQ(ry_emit_reduce_minmax_step(ctx, id, id, /*elem_ty=*/nullptr, /*is_max=*/1), 0u);
    ry_emit_ctx_destroy(ctx);
}

// =============================================================================
// #2098 — function definition + indirect call primitive guards
// ([C] = (ii) boundary move). The five new entries (create_function / get_param
// / struct_gep / call_indirect / ret) share the same checked_cx / handle-NULL /
// resolve_value / ffi_slice contract, so each guard SHAPE gets a representative
// case (the bit-exact `take` migration exercises every happy path). The
// create_function unknown-linkage reject is genuinely new logic (`linkage_from →
// None`) with no precedent above, so it gets its own case. The corrupted-ctx
// inner guards stay documented in .claude/rules/tests-rejection-tdd.md.
// =============================================================================

// --- ctx == NULL → sentinel / no-op ---

TEST_F(EmitAbiGuardTest, CreateFunctionNullCtxReturnsNull) {
    EXPECT_EQ(ry_emit_create_function(nullptr, "f", asRyFuncType(fnTy_), RY_LINKAGE_EXTERNAL),
              nullptr);
}

TEST_F(EmitAbiGuardTest, GetParamNullCtxReturnsZero) {
    EXPECT_EQ(ry_emit_get_param(nullptr, /*fn=*/nullptr, /*idx=*/0), 0u);
}

TEST_F(EmitAbiGuardTest, StructGepNullCtxReturnsZero) {
    EXPECT_EQ(
        ry_emit_struct_gep(nullptr, asRyType(resultTy_), /*ptr_id=*/0, /*field_idx=*/0, "x"),
        0u);
}

TEST_F(EmitAbiGuardTest, CallIndirectNullCtxReturnsZero) {
    EXPECT_EQ(ry_emit_call_indirect(nullptr, asRyFuncType(fnTy_), /*callee_id=*/0,
                                    /*arg_ids=*/nullptr, /*arg_count=*/0, /*name=*/nullptr),
              0u);
}

TEST_F(EmitAbiGuardTest, RetNullCtxDoesNotCrash) {
    ry_emit_ret(nullptr, /*val_id=*/0);
    SUCCEED();
}

// A non-zero val_id that fails to resolve is malformed input → no-op, NOT a
// silent `ret void` (#2105 CodeRabbit). With a real ctx whose builder is
// unpositioned, an out-of-range id (999) must return early *before* any
// LLVMBuildRet — reaching this point without a crash proves the early-return
// guard (a stray `ret void` on an unpositioned builder would crash). val_id == 0
// is deliberately excluded: the 0 sentinel intentionally emits `ret void`, which
// requires a positioned builder.
TEST_F(EmitAbiGuardTest, RetUnresolvedNonZeroValDoesNotCrash) {
    RyEmitCtx *ctx = makeCtx();
    ASSERT_NE(ctx, nullptr);
    ry_emit_ret(ctx, /*val_id=*/999);
    SUCCEED();
    ry_emit_ctx_destroy(ctx);
}

// --- valid ctx + NULL type / fn handle → sentinel (the handle-NULL guards) ---

TEST_F(EmitAbiGuardTest, CreateFunctionNullFnTypeReturnsNull) {
    RyEmitCtx *ctx = makeCtx();
    ASSERT_NE(ctx, nullptr);
    EXPECT_EQ(ry_emit_create_function(ctx, "f", /*fn_ty=*/nullptr, RY_LINKAGE_EXTERNAL), nullptr);
    ry_emit_ctx_destroy(ctx);
}

TEST_F(EmitAbiGuardTest, GetParamNullFnReturnsZero) {
    RyEmitCtx *ctx = makeCtx();
    ASSERT_NE(ctx, nullptr);
    EXPECT_EQ(ry_emit_get_param(ctx, /*fn=*/nullptr, /*idx=*/0), 0u);
    ry_emit_ctx_destroy(ctx);
}

TEST_F(EmitAbiGuardTest, StructGepNullTypeReturnsZero) {
    RyEmitCtx *ctx = makeCtx();
    ASSERT_NE(ctx, nullptr);
    EXPECT_EQ(ry_emit_struct_gep(ctx, /*struct_ty=*/nullptr, /*ptr_id=*/0, /*field_idx=*/0, "x"),
              0u);
    ry_emit_ctx_destroy(ctx);
}

TEST_F(EmitAbiGuardTest, CallIndirectNullFnTypeReturnsZero) {
    RyEmitCtx *ctx = makeCtx();
    ASSERT_NE(ctx, nullptr);
    EXPECT_EQ(ry_emit_call_indirect(ctx, /*fn_ty=*/nullptr, /*callee_id=*/0,
                                    /*arg_ids=*/nullptr, /*arg_count=*/0, /*name=*/nullptr),
              0u);
    ry_emit_ctx_destroy(ctx);
}

// --- valid ctx + unknown linkage → NULL (the new `linkage_from → None` logic;
//     fires before LLVMAddFunction so no stray function is added). ---

TEST_F(EmitAbiGuardTest, CreateFunctionUnknownLinkageReturnsNull) {
    RyEmitCtx *ctx = makeCtx();
    ASSERT_NE(ctx, nullptr);
    EXPECT_EQ(ry_emit_create_function(ctx, "f", asRyFuncType(fnTy_), /*linkage=*/99), nullptr);
    ry_emit_ctx_destroy(ctx);
}

// --- valid ctx + an id that resolves to NULL → sentinel (resolve_value → None,
//     fires before the unpositioned builder is touched). ---

TEST_F(EmitAbiGuardTest, StructGepNullResolvedPtrReturnsZero) {
    RyEmitCtx *ctx = makeCtx();
    ASSERT_NE(ctx, nullptr);
    // ptr_id 0 resolves to NULL after the valid struct_ty passes the type guard.
    EXPECT_EQ(ry_emit_struct_gep(ctx, asRyType(resultTy_), /*ptr_id=*/0, /*field_idx=*/0, "x"), 0u);
    ry_emit_ctx_destroy(ctx);
}

TEST_F(EmitAbiGuardTest, CallIndirectNullCalleeReturnsZero) {
    RyEmitCtx *ctx = makeCtx();
    ASSERT_NE(ctx, nullptr);
    // A valid fn_ty passes the type guard; callee_id 0 then resolves to NULL.
    EXPECT_EQ(ry_emit_call_indirect(ctx, asRyFuncType(fnTy_), /*callee_id=*/0,
                                    /*arg_ids=*/nullptr, /*arg_count=*/0, /*name=*/nullptr),
              0u);
    ry_emit_ctx_destroy(ctx);
}

// --- valid ctx + resolvable callee + NULL arg array with count > 0 → sentinel
//     (the ffi_slice NULL-array guard, mirroring CreatePhiNullIncomingArrays).
//     The callee is a function created through the boundary so it resolves
//     non-NULL, isolating the arg-array guard as the trigger. ---

TEST_F(EmitAbiGuardTest, CallIndirectNullArgArrayReturnsZero) {
    RyEmitCtx *ctx = makeCtx();
    ASSERT_NE(ctx, nullptr);
    RyFunctionRef fn =
        ry_emit_create_function(ctx, "ci_dummy", asRyFuncType(fnTy_), RY_LINKAGE_EXTERNAL);
    ASSERT_NE(fn, nullptr);
    RyValueId calleeId = ry_emit_intern(ctx, asRyValue(asLlvmFunction(fn)));
    EXPECT_EQ(ry_emit_call_indirect(ctx, asRyFuncType(fnTy_), calleeId,
                                    /*arg_ids=*/nullptr, /*arg_count=*/1, /*name=*/nullptr),
              0u);
    ry_emit_ctx_destroy(ctx);
}

// =============================================================================
// #2093 — collection copy-generation ops (keys / values / take / appended /
// concat). Three value-returning entries. ry_emit_list_copy_full also has a
// kind-selector guard (list_copy_kind_from → None) and ry_emit_list_concat a
// NULL elem_ty guard; the others share the ctx == NULL + resolve_value → None
// contract. All return the sentinel 0. (No FFI-array params.)
// =============================================================================

// --- ctx == NULL → sentinel 0 ---

TEST_F(EmitAbiGuardTest, ListCopyFullNullCtxReturnsZero) {
    EXPECT_EQ(ry_emit_list_copy_full(nullptr, /*src_data_id=*/0, /*count_id=*/0,
                                     /*elem_size=*/8, RY_LISTCOPY_KEYS),
              0u);
}

TEST_F(EmitAbiGuardTest, ListAppendedNullCtxReturnsZero) {
    EXPECT_EQ(ry_emit_list_appended(nullptr, /*new_len_id=*/0, /*old_len_id=*/0,
                                    /*src_data_id=*/0, /*elem_size=*/8),
              0u);
}

TEST_F(EmitAbiGuardTest, ListConcatNullCtxReturnsZero) {
    EXPECT_EQ(ry_emit_list_concat(nullptr, /*lhs_len_id=*/0, /*lhs_data_id=*/0,
                                  /*rhs_len_id=*/0, /*rhs_data_id=*/0, /*new_len_id=*/0,
                                  /*elem_ty=*/nullptr, /*elem_size=*/8),
              0u);
}

// --- valid ctx + an operand id that resolves to None → sentinel 0. id 0 resolves
//     to a NULL handle; resolve_value sits before the kind / elem_ty guard, so a
//     valid kind / elem_ty makes resolve_value the trigger. ---

TEST_F(EmitAbiGuardTest, ListCopyFullNullResolvedSrcReturnsZero) {
    RyEmitCtx *ctx = makeCtx();
    ASSERT_NE(ctx, nullptr);
    EXPECT_EQ(ry_emit_list_copy_full(ctx, /*src_data_id=*/0, /*count_id=*/0,
                                     /*elem_size=*/8, RY_LISTCOPY_VALUES),
              0u);
    ry_emit_ctx_destroy(ctx);
}

TEST_F(EmitAbiGuardTest, ListAppendedNullResolvedReturnsZero) {
    RyEmitCtx *ctx = makeCtx();
    ASSERT_NE(ctx, nullptr);
    EXPECT_EQ(ry_emit_list_appended(ctx, /*new_len_id=*/0, /*old_len_id=*/0,
                                    /*src_data_id=*/0, /*elem_size=*/8),
              0u);
    ry_emit_ctx_destroy(ctx);
}

TEST_F(EmitAbiGuardTest, ListConcatNullResolvedReturnsZero) {
    RyEmitCtx *ctx = makeCtx();
    ASSERT_NE(ctx, nullptr);
    // A valid elem_ty passes the type guard; lhs_len_id 0 resolves to NULL first.
    EXPECT_EQ(ry_emit_list_concat(ctx, /*lhs_len_id=*/0, /*lhs_data_id=*/0,
                                  /*rhs_len_id=*/0, /*rhs_data_id=*/0, /*new_len_id=*/0,
                                  asRyType(resultTy_), /*elem_size=*/8),
              0u);
    ry_emit_ctx_destroy(ctx);
}

// --- valid ctx + resolved operands + the op-specific guard. copy_full: an
//     out-of-range kind (list_copy_kind_from → None); concat: a NULL elem_ty.
//     internDummy gives each id a non-NULL handle so the op-specific guard (after
//     resolve_value) is what fires. ---

TEST_F(EmitAbiGuardTest, ListCopyFullUnknownKindReturnsZero) {
    RyEmitCtx *ctx = makeCtx();
    ASSERT_NE(ctx, nullptr);
    RyValueId id = internDummy(ctx);
    // 99 is outside the RyListCopyKind range [0, 2]; list_copy_kind_from returns
    // None after the operands resolve.
    EXPECT_EQ(ry_emit_list_copy_full(ctx, id, id, /*elem_size=*/8, /*kind=*/99), 0u);
    ry_emit_ctx_destroy(ctx);
}

TEST_F(EmitAbiGuardTest, ListConcatNullElemTyReturnsZero) {
    RyEmitCtx *ctx = makeCtx();
    ASSERT_NE(ctx, nullptr);
    RyValueId id = internDummy(ctx);
    EXPECT_EQ(ry_emit_list_concat(ctx, id, id, id, id, id, /*elem_ty=*/nullptr,
                                  /*elem_size=*/8),
              0u);
    ry_emit_ctx_destroy(ctx);
}

// =============================================================================
// #2099 — extractvalue primitive guards (closure / FnTypeInfo crossing). Same
// checked_cx / resolve_value contract as the #2098 entries above, so the two
// reachable guard shapes (ctx == NULL, and a valid ctx with an agg id that
// resolves to None) each get a representative case; the bit-exact filter / map
// migration exercises the happy path.
// =============================================================================

TEST_F(EmitAbiGuardTest, ExtractValueNullCtxReturnsZero) {
    EXPECT_EQ(ry_emit_extract_value(nullptr, /*agg_id=*/0, /*idx=*/0, "x"), 0u);
}

TEST_F(EmitAbiGuardTest, ExtractValueUnresolvedAggReturnsZero) {
    RyEmitCtx *ctx = makeCtx();
    ASSERT_NE(ctx, nullptr);
    // agg_id 0 resolves to None (resolve_value → None), the reject fires before
    // any builder use.
    EXPECT_EQ(ry_emit_extract_value(ctx, /*agg_id=*/0, /*idx=*/0, "x"), 0u);
    ry_emit_ctx_destroy(ctx);
}

// =============================================================================
// #2100 — switch construction + array-element GEP primitive guards
// ([B] = (ii) boundary move). create_switch returns the opaque RySwitchRef (NULL
// sentinel, like create_basic_block); switch_add_case is void (no-op on bad
// input); array_gep returns a value id (0 sentinel). Each shares the checked_cx /
// resolve_value / handle-NULL contract, so a representative case per guard SHAPE
// suffices (the bit-exact str(List<enum>) migration exercises the happy path).
// =============================================================================

// --- ry_emit_create_switch: ctx == NULL / unresolvable val / NULL default_bb
//     → NULL (opaque-handle sentinel) ---

TEST_F(EmitAbiGuardTest, CreateSwitchNullCtxReturnsNull) {
    EXPECT_EQ(ry_emit_create_switch(nullptr, /*val=*/0, /*default_bb=*/nullptr, /*num_cases=*/0),
              nullptr);
}

TEST_F(EmitAbiGuardTest, CreateSwitchUnresolvableValReturnsNull) {
    RyEmitCtx *ctx = makeCtx();
    ASSERT_NE(ctx, nullptr);
    // A valid default_bb isolates the unresolvable-val guard: val id 0 resolves
    // to NULL and resolve_value fires (before the default_bb NULL check and
    // before the unpositioned builder is touched).
    EXPECT_EQ(ry_emit_create_switch(ctx, /*val=*/0, asRyBasicBlock(makeBB()), /*num_cases=*/0),
              nullptr);
    ry_emit_ctx_destroy(ctx);
}

TEST_F(EmitAbiGuardTest, CreateSwitchNullDefaultBbReturnsNull) {
    RyEmitCtx *ctx = makeCtx();
    ASSERT_NE(ctx, nullptr);
    // internDummy makes val resolvable so resolve_value passes and the NULL
    // default_bb guard is the trigger.
    RyValueId val = internDummy(ctx);
    EXPECT_EQ(ry_emit_create_switch(ctx, val, /*default_bb=*/nullptr, /*num_cases=*/0), nullptr);
    ry_emit_ctx_destroy(ctx);
}

// --- ry_emit_switch_add_case: void; ctx == NULL / NULL switch → no-op (passes
//     by not crashing) ---

TEST_F(EmitAbiGuardTest, SwitchAddCaseNullCtxDoesNotCrash) {
    ry_emit_switch_add_case(nullptr, /*sw=*/nullptr, /*on_val=*/0, /*dest_bb=*/nullptr);
    SUCCEED();
}

TEST_F(EmitAbiGuardTest, SwitchAddCaseNullSwitchDoesNotCrash) {
    RyEmitCtx *ctx = makeCtx();
    ASSERT_NE(ctx, nullptr);
    // A valid dest_bb isolates the NULL-switch guard (sw.is_null() short-circuits
    // before dest_bb / on_val are inspected).
    ry_emit_switch_add_case(ctx, /*sw=*/nullptr, /*on_val=*/0, asRyBasicBlock(makeBB()));
    SUCCEED();
    ry_emit_ctx_destroy(ctx);
}

// --- ry_emit_array_gep: ctx == NULL / NULL array_ty / unresolvable operand
//     → sentinel 0 ---

TEST_F(EmitAbiGuardTest, ArrayGepNullCtxReturnsZero) {
    EXPECT_EQ(ry_emit_array_gep(nullptr, /*array_ty=*/nullptr, /*base_id=*/0, /*idx_id=*/0, "x"),
              0u);
}

TEST_F(EmitAbiGuardTest, ArrayGepNullArrayTypeReturnsZero) {
    RyEmitCtx *ctx = makeCtx();
    ASSERT_NE(ctx, nullptr);
    // Resolvable operands isolate the NULL-array_ty guard (it fires before the
    // resolve_value checks).
    RyValueId id = internDummy(ctx);
    EXPECT_EQ(ry_emit_array_gep(ctx, /*array_ty=*/nullptr, id, id, "x"), 0u);
    ry_emit_ctx_destroy(ctx);
}

TEST_F(EmitAbiGuardTest, ArrayGepUnresolvableOperandReturnsZero) {
    RyEmitCtx *ctx = makeCtx();
    ASSERT_NE(ctx, nullptr);
    // A valid array type passes the type guard; base_id 0 then resolves to NULL.
    llvm::ArrayType *arrayTy = llvm::ArrayType::get(llvm::Type::getInt64Ty(llctx_), 4);
    EXPECT_EQ(ry_emit_array_gep(ctx, asRyType(arrayTy), /*base_id=*/0, /*idx_id=*/0, "x"), 0u);
    ry_emit_ctx_destroy(ctx);
}

} // namespace
