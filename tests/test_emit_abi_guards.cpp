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
//
// Deeper guards that need a *corrupted* ctx (c.module / c.context / c.builder
// NULL) have no supported-API trigger and are documented as defensive in
// .claude/rules/tests-rejection-tdd.md rather than tested here.

#include <gtest/gtest.h>

#include "ry/llvm_emit/api.h"
#include "ry/llvm_emit/cast_helpers.hpp"

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
                                  asRyContext(&llctx_), /*function=*/nullptr);
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

} // namespace
