// Test taxonomy: docs/reference/test-taxonomy.md
// Section header tags: [contract] / [regression #NNNN] / [internal].
//
// Whole-file tag: [regression: #2484] — upper-codegen Stage 2 migration of
// `emitExprVariant(StringExpr)` / `emitExprVariant(RegexExpr)` and the
// `global_string_cache_` / `regex_global_cache_` pair to the Rust lower
// crate.
//
// Coverage:
//   - StringConstDedup: three references to the same byte content share
//     one global (cache hit invariant — the C++ `global_string_cache_`
//     contract preserved by the new Rust-side `STRING_CACHE`).
//   - StringRegexSeparation: same-content string + regex emit TWO distinct
//     globals (#306 separation invariant: `addResourceKind(rk_regex)` would
//     otherwise poison a string global of identical bytes).
//   - StringCachePerInstance: sequential `CodeGen` instances on the same
//     thread do not leak `RyValueId`s from the prior `RyEmitCtx`. Guards
//     the `ry_lower_reset_module_state` call wired into the `CodeGen`
//     constructor; without that reset, the second compilation would
//     resolve cached ids against the freed prior context and crash or
//     miscompile.

#include "test_codegen_common.hpp"

#include <llvm/IR/Module.h>
#include <llvm/Support/raw_ostream.h>

#include <string>

using namespace ry;
using namespace llvm;
using namespace llvm::orc;

namespace {

std::string dumpModuleIR(Module &mod) {
    std::string out;
    raw_string_ostream rso(out);
    mod.print(rso, /*AssemblyAnnotationWriter=*/nullptr);
    return out;
}

int countOccurrences(const std::string &haystack, const std::string &needle) {
    int count = 0;
    size_t pos = 0;
    while ((pos = haystack.find(needle, pos)) != std::string::npos) {
        ++count;
        pos += needle.size();
    }
    return count;
}

} // namespace

// ===== [regression: #2484] String literal dedup =====
//
// Three `"hello"` references in one program emit exactly one `.str.arc`
// global. The Rust-side `STRING_CACHE` (thread_local) replaces the C++
// `global_string_cache_`; this test locks the dedup invariant at the IR
// level so any future regression (e.g. accidentally re-emitting on every
// call) trips here, not just in indirect runtime behavior.
TEST_F(CodeGenTest, StringConstDedupStage2_2484) {
    auto tsm = compileSource(
        "fn probe() -> int:\n"
        "    a = \"hello\"\n"
        "    b = \"hello\"\n"
        "    c = \"hello\"\n"
        "    return len(a)\n"
        "fn main():\n"
        "    _ = probe()\n");
    tsm.withModuleDo([](Module &mod) {
        std::string ir = dumpModuleIR(mod);
        // Exactly one ARC-immortal global for the byte content "hello".
        // Any second global emission would indicate the cache was bypassed.
        int helloGlobals = countOccurrences(ir, "c\"hello\\00\"");
        EXPECT_EQ(helloGlobals, 1)
            << "Expected exactly one deduped \"hello\" ARC global; got "
            << helloGlobals << " in IR:\n" << ir;
    });
}

// ===== [regression: #2484, #306] String / regex separation invariant =====
//
// A string `"hello"` and a regex `/hello/` with identical byte content
// MUST emit TWO distinct `.arc` globals. Folding them into one would let
// `addResourceKind(gs, rk_regex)` (still applied in the C++ regex shim)
// poison the string global, breaking `isStrLike()` discrimination on
// any other `cachedGlobalString` call with the same content.
TEST_F(CodeGenTest, StringRegexSeparationStage2_2484) {
    // `isMatch` is a stdlib regex builtin available in the codegen test
    // harness without an explicit import (the harness skips ModuleLoader;
    // mirroring `RegexLiteralMatch` in test_codegen_regex.cpp).
    auto tsm = compileSource(
        "fn probe() -> bool:\n"
        "    pat = /hello/\n"
        "    s = \"hello\"\n"
        "    return isMatch(s, pat)\n"
        "fn main():\n"
        "    _ = probe()\n");
    tsm.withModuleDo([](Module &mod) {
        std::string ir = dumpModuleIR(mod);
        // The regex and string globals are named `.regex.arc` and `.str.arc`
        // respectively. Both must appear — distinct globals despite shared
        // byte content.
        EXPECT_NE(ir.find("@.str.arc"), std::string::npos)
            << "Expected @.str.arc global in IR:\n" << ir;
        EXPECT_NE(ir.find("@.regex.arc"), std::string::npos)
            << "Expected @.regex.arc global in IR:\n" << ir;
        // Two distinct ARC-immortal globals with the byte content "hello"
        // — string + regex, even though the bytes match.
        int helloGlobals = countOccurrences(ir, "c\"hello\\00\"");
        EXPECT_EQ(helloGlobals, 2)
            << "Expected exactly two ARC globals for the byte content "
               "\"hello\" (one .str.arc, one .regex.arc); got "
            << helloGlobals << " in IR:\n" << ir;
    });
}

// ===== [regression: #2484] Per-instance cache lifecycle =====
//
// Two sequential `CodeGen` instances on the same thread must not share
// `RyValueId`s through the lower-side thread_local cache. The reset hook
// in the `CodeGen` constructor (`ry_lower_reset_module_state(emit_ctx_)`)
// flushes the cache; without it the second compilation would resolve
// cached ids against the freed prior `RyEmitCtx`, producing either a
// miscompile or a null-pointer crash inside `ry_emit_resolve`.
//
// Verified end-to-end: both programs run and print the expected output.
TEST_F(CodeGenTest, StringCachePerInstanceStage2_2484) {
    EXPECT_EQ(runSource("print(\"xyzzy\")"), "xyzzy\n");
    // Same string content, fresh CodeGen — the cache reset in the
    // constructor must clear the prior entry so the new compilation
    // emits a fresh global against the new RyEmitCtx.
    EXPECT_EQ(runSource("print(\"xyzzy\")"), "xyzzy\n");
}

// ===== [regression: #2484] Empty + distinct-second-literal probes =====
//
// Verification table item: the empty literal `""` must emit its own
// ARC-immortal global (NOT collapse to the same global as any other
// content), and a distinct second literal must lay out separately. Both
// are folded into the Rust-side `STRING_CACHE`, so this test exercises
// the cache miss path with content that the dedup probe (above) does not.
TEST_F(CodeGenTest, StringConstEmptyAndDistinctStage2_2484) {
    auto tsm = compileSource(
        "fn probe() -> int:\n"
        "    a = \"\"\n"
        "    b = \"world\"\n"
        "    return len(a) + len(b)\n"
        "fn main():\n"
        "    _ = probe()\n");
    tsm.withModuleDo([](Module &mod) {
        std::string ir = dumpModuleIR(mod);
        // The empty-string ARC global: byte_len = 0, single-byte payload
        // (LLVMConstStringInContext2 appends the trailing NUL; LLVM
        // renders a single-NUL array as `zeroinitializer` rather than
        // `c"\00"`).
        EXPECT_NE(ir.find("i64 0, [1 x i8] zeroinitializer"),
                  std::string::npos)
            << "Expected empty-string ARC global in IR:\n" << ir;
        // The distinct second literal lives in its own global.
        EXPECT_NE(ir.find("c\"world\\00\""), std::string::npos)
            << "Expected distinct \"world\" ARC global in IR:\n" << ir;
    });
}

// ===== [regression: #2484] NUL-containing literal binary safety =====
//
// The C++ side keys the cache on `std::string` (NUL-safe), and the Rust
// side keys on `Vec<u8>` with `len` as the authoritative byte count
// — never `strlen`. A literal with an embedded NUL must therefore
// preserve its full byte count both in the cache key and in the LLVM
// global's `byte_len` field (slot 2 of the StringHeader struct).
//
// `print(s)` calls into `__ry_print_str` which uses the `byte_len`
// field (handle - 8) to bound the output; if `byte_len` were 1 (the
// strlen-collapsed value) only the byte before the NUL would print.
// The Ry source-level escape `\x00` is binary-safe by spec.
TEST_F(CodeGenTest, StringConstNulByteBinarySafetyStage2_2484) {
    auto tsm = compileSource(
        "fn probe() -> int:\n"
        "    s = \"a\\x00b\"\n"
        "    return len(s)\n"
        "fn main():\n"
        "    _ = probe()\n");
    tsm.withModuleDo([](Module &mod) {
        std::string ir = dumpModuleIR(mod);
        // ARC global with byte_len = 3 and a 4-byte payload (3 + NUL).
        // The presence of `c"a\00b\00"` proves the NUL byte survived the
        // boundary (Rust-side cache keying + emit-side
        // `LLVMConstStringInContext2` byte length).
        EXPECT_NE(ir.find("i64 3, [4 x i8] c\"a\\00b\\00\""),
                  std::string::npos)
            << "Expected NUL-embedded byte payload preserved in IR:\n"
            << ir;
    });
}
