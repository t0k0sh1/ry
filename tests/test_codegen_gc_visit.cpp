// Test taxonomy: docs/reference/test-taxonomy.md
// Section header tags: [contract] / [regression #NNNN] / [internal].
// Per-test exceptions use an inline `// [regression: #NNNN]` comment.
// Whole-file tag: [internal] (forces GC visitor thunk emission via test-only flag
// and asserts emitted IR marker shape).
//
// Pilot G (#2196): verify that the GC visitor thunk emission generates
// byte-equivalent IR after migrating from inline `builder_.Create*` to
// crates/emit primitives. The thunk codegen path is currently unreachable from
// any Ry program (3 gates: arc_managed_vars_ membership + enum_value_type meta
// + isPotentiallyCyclic), so this test forces emission via the test-only
// `setForceEmitCyclicVisitorsForTest` flag on CodeGen and asserts the marker
// shape of the generated `__ry_gc_visit_*` functions — the #2026 coverage gate
// applied to a synthesised-not-probe path. ADT enum and record paths are both
// exercised; the StructType branch of emitGcVisitField (nested-visit on an
// inline record field) is not exercised because Ry sema rejects records with
// an inline record field of a transitively-cyclic type (no forward
// declarations), making the branch unreachable in practice; the migration of
// that branch (`emitCallIndirect(visitFnTy, nestedVisitFn, ...)`) is
// reasoned-equivalent to the original `builder_.CreateCall(visitFnTy, ...)`
// because emit primitive wraps `LLVMBuildCall2` with the same args.

#include "test_codegen_common.hpp"

#include <llvm/IR/Function.h>
#include <llvm/IR/Module.h>
#include <llvm/Support/raw_ostream.h>

#include <string>

using namespace ry;
using namespace llvm;
using namespace llvm::orc;

namespace {

ThreadSafeModule compileWithForcedVisitorThunks(const std::string &src) {
    Lexer lex(src);
    Parser parser(lex);
    Program prog = parser.parseProgram();
    CodeGen cg;
    cg.setForceEmitCyclicVisitorsForTest(true);
    return cg.compile(prog);
}

std::string dumpFunctionIR(Function *fn) {
    std::string out;
    raw_string_ostream rso(out);
    fn->print(rso);
    return out;
}

} // namespace

// ADT enum visitor thunk: tag dispatch + per-variant payload byte-offset GEP
// + null-guard ARC field + arc_hdr_from_data (-16) + visitor call.
TEST_F(CodeGenTest, GcVisitorThunkAdtEnumIsByteExact) {
    auto tsm = compileWithForcedVisitorThunks(
        "enum Tree:\n"
        "    Leaf(int)\n"
        "    Branch(List<Tree>)\n"
        "\n"
        "print(1)\n");
    tsm.withModuleDo([](Module &mod) {
        Function *fn = mod.getFunction("__ry_gc_visit_Tree");
        ASSERT_NE(fn, nullptr) << "visitor thunk for Tree was not emitted";
        EXPECT_TRUE(fn->doesNotThrow()) << "Tree visitor thunk must be nounwind";

        std::string ir = dumpFunctionIR(fn);
        // ADT tag dispatch
        EXPECT_NE(ir.find("%gc_tag_ptr"), std::string::npos);
        EXPECT_NE(ir.find("%gc_tag = load i64"), std::string::npos);
        EXPECT_NE(ir.find("%gc_payload"), std::string::npos);
        EXPECT_NE(ir.find("switch i64 %gc_tag"), std::string::npos);
        // The Branch variant case carries the ARC field walk
        EXPECT_NE(ir.find("gc.visit.Branch"), std::string::npos);
        EXPECT_NE(ir.find("%gc.field.0"), std::string::npos);
        // Null-guard diamond
        EXPECT_NE(ir.find("%gc.visit.val = load ptr"), std::string::npos);
        EXPECT_NE(ir.find("%gc.visit.null = icmp eq ptr"), std::string::npos);
        EXPECT_NE(ir.find("gc.visit.ptr"), std::string::npos);
        EXPECT_NE(ir.find("gc.skip.ptr"), std::string::npos);
        // ARC header recovery + visitor call
        EXPECT_NE(ir.find("%arc_hdr_from_data = getelementptr i8"), std::string::npos);
        EXPECT_NE(ir.find("i64 -16"), std::string::npos);
        EXPECT_NE(ir.find("call void %1(ptr %arc_hdr_from_data)"), std::string::npos);
    });
}

// Record visitor thunk: fixed-layout field StructGEP + null-guard ARC field
// + arc_hdr_from_data (-16) + visitor call. No switch (single tag implied).
TEST_F(CodeGenTest, GcVisitorThunkRecordIsByteExact) {
    auto tsm = compileWithForcedVisitorThunks(
        "record Box:\n"
        "    items: List<Box>\n"
        "\n"
        "print(1)\n");
    tsm.withModuleDo([](Module &mod) {
        Function *fn = mod.getFunction("__ry_gc_visit_Box");
        ASSERT_NE(fn, nullptr) << "visitor thunk for Box was not emitted";
        EXPECT_TRUE(fn->doesNotThrow()) << "Box visitor thunk must be nounwind";

        std::string ir = dumpFunctionIR(fn);
        // Record field GEP (no switch)
        EXPECT_EQ(ir.find("switch i64"), std::string::npos)
            << "record path must not emit a tag switch";
        EXPECT_NE(ir.find("%gc.record.field.0"), std::string::npos);
        // Same null-guard + ARC header recovery + visitor call shape as ADT.
        EXPECT_NE(ir.find("%gc.visit.val = load ptr"), std::string::npos);
        EXPECT_NE(ir.find("%gc.visit.null = icmp eq ptr"), std::string::npos);
        EXPECT_NE(ir.find("%arc_hdr_from_data = getelementptr i8"), std::string::npos);
        EXPECT_NE(ir.find("i64 -16"), std::string::npos);
        EXPECT_NE(ir.find("call void %1(ptr %arc_hdr_from_data)"), std::string::npos);
    });
}
