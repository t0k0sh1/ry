#pragma once

namespace ry {

// This header is intentionally included only from codegen.hpp after the
// CodeGen class definition so these guard types can access its private state.

class OptionNoneHintGuard {
public:
    explicit OptionNoneHintGuard(CodeGen &cg, llvm::Type *hint)
        : cg_(cg), saved_(cg.option_none_hint_inner_) {
        cg_.option_none_hint_inner_ = hint;
    }

    ~OptionNoneHintGuard() { cg_.option_none_hint_inner_ = saved_; }
    OptionNoneHintGuard(const OptionNoneHintGuard &) = delete;
    OptionNoneHintGuard &operator=(const OptionNoneHintGuard &) = delete;

private:
    CodeGen &cg_;
    llvm::Type *saved_;
};

class DeclAnnotationInnerGuard {
public:
    explicit DeclAnnotationInnerGuard(CodeGen &cg, llvm::Type *inner)
        : cg_(cg), saved_(cg.option_decl_annotation_inner_) {
        cg_.option_decl_annotation_inner_ = inner;
    }

    ~DeclAnnotationInnerGuard() { cg_.option_decl_annotation_inner_ = saved_; }
    DeclAnnotationInnerGuard(const DeclAnnotationInnerGuard &) = delete;
    DeclAnnotationInnerGuard &operator=(const DeclAnnotationInnerGuard &) = delete;

private:
    CodeGen &cg_;
    llvm::Type *saved_;
};

class LiteralAnyHintGuard {
public:
    explicit LiteralAnyHintGuard(CodeGen &cg, CodeGen::LiteralAnyHint hint)
        : cg_(cg), saved_(cg.literal_any_hint_) {
        cg_.literal_any_hint_ = hint;
    }

    ~LiteralAnyHintGuard() { cg_.literal_any_hint_ = saved_; }
    LiteralAnyHintGuard(const LiteralAnyHintGuard &) = delete;
    LiteralAnyHintGuard &operator=(const LiteralAnyHintGuard &) = delete;

private:
    CodeGen &cg_;
    CodeGen::LiteralAnyHint saved_;
};

class ParallelForScope {
public:
    explicit ParallelForScope(CodeGen &cg) : cg_(cg) { ++cg_.parallel_for_depth_; }

    ~ParallelForScope() { --cg_.parallel_for_depth_; }
    ParallelForScope(const ParallelForScope &) = delete;
    ParallelForScope &operator=(const ParallelForScope &) = delete;

private:
    CodeGen &cg_;
};

} // namespace ry
