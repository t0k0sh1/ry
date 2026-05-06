#include "ry/jit_runner.hpp"
#include "ry/lexer.hpp"
#include "ry/parser.hpp"
#include "ry/module_loader.hpp"
#include "ry/codegen.hpp"
#include "ry/source_manager.hpp"
#include "ry/diagnostic.hpp"
#include "ry/jit.hpp"
#include "ry/test_runtime.hpp"
#include "ry/project_config.hpp"
#include "ry/self_update.hpp"
#include "ry/paths.hpp"
#include "ry/coverage_runtime.hpp"
#include "ry/trace.hpp"
#include "ry/dotenv.hpp"
#include <csignal>
#include <filesystem>
#include <algorithm>
#include <iterator>
#include <unistd.h>
#include <llvm/ExecutionEngine/Orc/LLJIT.h>
#include <llvm/ExecutionEngine/Orc/ExecutorProcessControl.h>
#include <llvm/ExecutionEngine/Orc/RTDyldObjectLinkingLayer.h>
#include <llvm/Support/MemoryBuffer.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/ADT/ScopeExit.h>

using namespace llvm;
using namespace llvm::orc;

namespace fs = std::filesystem;

namespace ry {
extern const char *__ry_test_current_it_name();
} // namespace ry

extern "C" int64_t  __ry_runtime_internal_arc_live_count();

static void test_timeout_handler(int) {
    const char msg[] = "\nTest timed out: ";
    (void)write(STDERR_FILENO, msg, sizeof(msg) - 1);
    const char *name = ry::__ry_test_current_it_name();
    if (name && name[0]) {
        size_t len = 0;
        while (name[len]) ++len;
        (void)write(STDERR_FILENO, name, len);
    }
    const char nl[] = "\n";
    (void)write(STDERR_FILENO, nl, 1);
    _exit(124);
}

namespace ry {

void resetCoverageState(CoverageState &cs) {
    __ry_coverage_reset();
    cs.file_id_offset = 0;
    cs.file_count = 0;
    cs.filenames.clear();
}

void emitCoverageReport(const CoverageState &cs) {
    if (cs.file_count <= 0) return;
    std::vector<const char*> ptrs(static_cast<size_t>(cs.file_count));
    for (size_t i = 0; i < static_cast<size_t>(cs.file_count); ++i)
        ptrs[i] = cs.filenames[i].c_str();
    __ry_coverage_report_summary(ptrs.data(), cs.file_count);
}

int runRySource(const std::string &src, const std::string &source_name,
                const std::string &referrer_dir, bool test_mode,
                const char *argv0, bool skip_global_lib,
                bool coverage_mode, bool outline_mode,
                CoverageState *cs, bool emit_llvm_ir) {
    SourceLocation sourceLoc{1, 1, 0};
    if (ry::traceEnabled()) {
        ry::emitTraceEvent("source.loaded", "compile", &sourceLoc,
                           {ry::TraceField("file", source_name),
                            ry::TraceField("size_bytes", static_cast<int64_t>(src.size()))});
    }

    std::string project_root;
    // Load .env from project root (once per root per process)
    {
        if (auto root = findProjectRoot(referrer_dir)) {
            project_root = *root;
            static std::string loaded_root;
            if (loaded_root != *root) {
                const char *ry_env = std::getenv("RY_ENV");
                ry::loadDotEnv(*root, ry_env ? ry_env : "");
                loaded_root = *root;
            }
        }
    }

    // Source manager for rich error messages
    SourceManager sm;
    int fileId = sm.addSource(source_name, src);
    sourceLoc.file_id = fileId;

    // Lex -> Parse
    Program prog;
    {
        Lexer lexer(src);
        Parser parser(lexer, &sm, fileId);
        try {
            if (ry::traceEnabled())
                ry::emitTraceEvent("parse.start", "compile", &sourceLoc,
                                   {ry::TraceField("file", source_name)});
            prog = parser.parseProgram();
            if (ry::traceEnabled())
                ry::emitTraceEvent("parse.done", "compile", &sourceLoc,
                                   {ry::TraceField("file", source_name)});
        } catch (const DiagnosticError &e) {
            ry::emitTraceDiagnostic("parse.error", "compile", &e.diagnostic().loc, e.diagnostic().message);
            throw;
        }
    }

    // Set up search paths with share/ for stdlib
    std::vector<std::string> search_paths;
    std::string share_dir = ry::find_share_dir(argv0, project_root, skip_global_lib).string();
    if (!share_dir.empty()) {
        search_paths.push_back(share_dir);
        // Enable flattened imports: "from math" resolves to share/std/math/
        search_paths.push_back((fs::path(share_dir) / "std").string());
    }

    ModuleLoader loader(search_paths, &sm);

    // Implicit `from std` import (like Java's java.lang)
    // Only insert if std can be found (graceful degradation without stdlib)
    try {
        Program std_prog;
        std_prog.push_back(ImportStmt{"std", {}, {0, 0, fileId}});
        // Resolve against stdlib search paths only (not referrer_dir)
        // to prevent local std/ from shadowing the bundled stdlib
        std_prog = loader.resolveImports(std_prog, "");
        prog.insert(prog.begin(),
            std::make_move_iterator(std_prog.begin()),
            std::make_move_iterator(std_prog.end()));
    } catch (const std::runtime_error &) { // NOLINT(bugprone-empty-catch): stdlib is optional, absence is expected
        // stdlib not installed — continue without it
    }

    // Resolve remaining imports
    try {
        prog = loader.resolveImports(prog, referrer_dir);
    } catch (const DiagnosticError &) {
        throw;
    } catch (const std::exception &e) {
        ry::emitTraceEvent("import.resolve.error", "compile", &sourceLoc,
                           {ry::TraceField("file", source_name),
                            ry::TraceField("detail", e.what())});
        throw;
    }

    // CodeGen -> ThreadSafeModule
    int coverage_offset = cs ? cs->file_id_offset : 0;
    if (ry::traceEnabled())
        ry::emitTraceEvent("codegen.start", "compile", &sourceLoc,
                           {ry::TraceField("file", source_name)});
    CodeGen cg(test_mode, &sm, coverage_mode, coverage_offset, outline_mode);
    cg.setTestingIntrinsicsImported(loader.importedTestingIntrinsics());
    ThreadSafeModule tsm = [&]() {
        try {
            auto module = cg.compile(prog);
            if (ry::traceEnabled())
                ry::emitTraceEvent("codegen.done", "compile", &sourceLoc,
                                   {ry::TraceField("file", source_name)});
            return module;
        } catch (const DiagnosticError &e) {
            ry::emitTraceDiagnostic("codegen.error", "compile", &e.diagnostic().loc, e.diagnostic().message);
            throw;
        }
    }();

    // Must precede the emit_llvm_ir early return below to avoid swallowing warnings.
    {
        std::vector<std::string> seen;
        for (const auto &w : cg.getWarnings()) {
            if (std::find(seen.begin(), seen.end(), w) == seen.end()) {
                seen.push_back(w);
                errs() << w << "\n";
            }
        }
    }

    if (emit_llvm_ir) {
        tsm.getModuleUnlocked()->print(llvm::outs(), nullptr);
        return 0;
    }

    // Build LLJIT
    if (ry::traceEnabled())
        ry::emitTraceEvent("jit.start", "jit", &sourceLoc,
                           {ry::TraceField("file", source_name)});
    auto jitOrErr = LLJITBuilder().create();
    if (!jitOrErr) {
        errs() << "Failed to create JIT: ";
        logAllUnhandledErrors(jitOrErr.takeError(), errs());
        ry::emitTraceEvent("runtime.error", "jit", &sourceLoc,
                           {ry::TraceField("detail", "Failed to create JIT")});
        return 1;
    }
    auto &jit = *jitOrErr;

    jit->getIRTransformLayer().setTransform(ry::optimizeModule);

    auto &mainJD = jit->getMainJITDylib();
    auto dlsg = DynamicLibrarySearchGenerator::GetForCurrentProcess(
        jit->getDataLayout().getGlobalPrefix());
    if (!dlsg) {
        errs() << "Failed to create DynamicLibrarySearchGenerator: ";
        logAllUnhandledErrors(dlsg.takeError(), errs());
        ry::emitTraceEvent("runtime.error", "jit", &sourceLoc,
                           {ry::TraceField("detail", "Failed to create DynamicLibrarySearchGenerator")});
        return 1;
    }
    mainJD.addGenerator(std::move(*dlsg));

    // Load dynamic libraries required by @native("libname") declarations
    {
        // Resolve the executable path for reliable exe-relative library search.
        // argv0 may be a bare name (e.g. "ry" from PATH), so resolve it first.
        std::string resolvedExe = ry::self_update::detail::get_executable_path();
        if (resolvedExe.empty()) resolvedExe = argv0;
        auto requiredLibs = cg.getRequiredLibraries();
        for (const auto &libName : requiredLibs) {
            auto libPath = ry::find_native_library(resolvedExe, libName);
            if (libPath.empty()) {
                errs() << "Error: native library '" << libName
                       << "' not found (searched exe/../lib/, exe/lib/, $RY_HOME/lib/)\n";
                ry::emitTraceEvent("runtime.error", "jit", &sourceLoc,
                                   {ry::TraceField("detail", "native library not found: " + libName)});
                return 1;
            }
            if (ry::traceEnabled())
                ry::emitTraceEvent("jit.load_library", "jit", &sourceLoc,
                                   {ry::TraceField("library", libName),
                                    ry::TraceField("path", libPath.string())});
            auto libGen = DynamicLibrarySearchGenerator::Load(
                libPath.string().c_str(),
                jit->getDataLayout().getGlobalPrefix());
            if (!libGen) {
                errs() << "Failed to load native library '" << libName << "': ";
                logAllUnhandledErrors(libGen.takeError(), errs());
                ry::emitTraceEvent("runtime.error", "jit", &sourceLoc,
                                   {ry::TraceField("detail", "Failed to load library: " + libName)});
                return 1;
            }
            mainJD.addGenerator(std::move(*libGen));
        }
    }

    // Register test runtime symbols if in test mode
    if (test_mode) {
        auto &es = jit->getExecutionSession();
        SymbolMap testSymbols;
        testSymbols[es.intern("__ry_test_describe_begin")] =
            {ExecutorAddr::fromPtr(&__ry_test_describe_begin), JITSymbolFlags::Exported};
        testSymbols[es.intern("__ry_test_describe_end")] =
            {ExecutorAddr::fromPtr(&__ry_test_describe_end), JITSymbolFlags::Exported};
        testSymbols[es.intern("__ry_test_it_begin")] =
            {ExecutorAddr::fromPtr(&__ry_test_it_begin), JITSymbolFlags::Exported};
        testSymbols[es.intern("__ry_test_it_end")] =
            {ExecutorAddr::fromPtr(&__ry_test_it_end), JITSymbolFlags::Exported};
        testSymbols[es.intern("__ry_test_expect_fail")] =
            {ExecutorAddr::fromPtr(&__ry_test_expect_fail), JITSymbolFlags::Exported};
        testSymbols[es.intern("__ry_test_summary")] =
            {ExecutorAddr::fromPtr(&__ry_test_summary), JITSymbolFlags::Exported};
        testSymbols[es.intern("__ry_mock_set")] =
            {ExecutorAddr::fromPtr(&__ry_mock_set), JITSymbolFlags::Exported};
        testSymbols[es.intern("__ry_mock_get")] =
            {ExecutorAddr::fromPtr(&__ry_mock_get), JITSymbolFlags::Exported};
        testSymbols[es.intern("__ry_mock_get_call_count")] =
            {ExecutorAddr::fromPtr(&__ry_mock_get_call_count), JITSymbolFlags::Exported};
        testSymbols[es.intern("__ry_mock_increment_call")] =
            {ExecutorAddr::fromPtr(&__ry_mock_increment_call), JITSymbolFlags::Exported};
        testSymbols[es.intern("__ry_mock_clear_all")] =
            {ExecutorAddr::fromPtr(&__ry_mock_clear_all), JITSymbolFlags::Exported};
        testSymbols[es.intern("__ry_test_prop_init_rng")] =
            {ExecutorAddr::fromPtr(&__ry_test_prop_init_rng), JITSymbolFlags::Exported};
        testSymbols[es.intern("__ry_test_rand_int")] =
            {ExecutorAddr::fromPtr(&__ry_test_rand_int), JITSymbolFlags::Exported};
        testSymbols[es.intern("__ry_test_rand_float")] =
            {ExecutorAddr::fromPtr(&__ry_test_rand_float), JITSymbolFlags::Exported};
        testSymbols[es.intern("__ry_test_rand_bool")] =
            {ExecutorAddr::fromPtr(&__ry_test_rand_bool), JITSymbolFlags::Exported};
        testSymbols[es.intern("__ry_test_rand_str")] =
            {ExecutorAddr::fromPtr(&__ry_test_rand_str), JITSymbolFlags::Exported};
        testSymbols[es.intern("__ry_test_it_is_failed")] =
            {ExecutorAddr::fromPtr(&__ry_test_it_is_failed), JITSymbolFlags::Exported};
        if (auto err = mainJD.define(absoluteSymbols(std::move(testSymbols)))) {
            errs() << "Failed to define test symbols: ";
            logAllUnhandledErrors(std::move(err), errs());
            ry::emitTraceEvent("runtime.error", "jit", &sourceLoc,
                               {ry::TraceField("detail", "Failed to define test symbols")});
            return 1;
        }
    }

    // Register coverage runtime symbols if in coverage mode
    if (coverage_mode) {
        auto &es = jit->getExecutionSession();
        SymbolMap covSymbols;
        covSymbols[es.intern("__ry_coverage_hit")] =
            {ExecutorAddr::fromPtr(&__ry_coverage_hit), JITSymbolFlags::Exported};
        if (auto err = mainJD.define(absoluteSymbols(std::move(covSymbols)))) {
            errs() << "Failed to define coverage symbols: ";
            logAllUnhandledErrors(std::move(err), errs());
            ry::emitTraceEvent("runtime.error", "jit", &sourceLoc,
                               {ry::TraceField("detail", "Failed to define coverage symbols")});
            return 1;
        }
    }

    // Register runtime_internal symbols. arcLiveCount() is called from Ry
    // test code via `from runtime_internal import arcLiveCount`.  Explicit
    // registration ensures reliable JIT resolution on Linux where
    // DynamicLibrarySearchGenerator may not find static-library symbols.
    {
        auto &es = jit->getExecutionSession();
        SymbolMap runtimeInternalSymbols;
        runtimeInternalSymbols[es.intern("__ry_runtime_internal_arc_live_count")] =
            {ExecutorAddr::fromPtr(&__ry_runtime_internal_arc_live_count), JITSymbolFlags::Exported};
        if (auto err = mainJD.define(absoluteSymbols(std::move(runtimeInternalSymbols)))) {
            errs() << "Failed to define runtime_internal symbols: ";
            logAllUnhandledErrors(std::move(err), errs());
            return 1;
        }
    }

    // Add module via ResourceTracker so we can explicitly remove it before
    // ~LLJIT() runs — avoids a heap-corruption crash on Linux ELF teardown
    // (observed during ~LLJIT() → endSession() when the module contains
    // ConstantExpr GEP relocations for ARC-managed globals).
    auto RT = jit->getMainJITDylib().createResourceTracker();
    if (auto err = jit->addIRModule(RT, std::move(tsm))) {
        errs() << "Failed to add IR module: ";
        logAllUnhandledErrors(std::move(err), errs());
        ry::emitTraceEvent("runtime.error", "jit", &sourceLoc,
                           {ry::TraceField("detail", "Failed to add IR module")});
        return 1;
    }

    // RAII guard: always remove JIT resources on function exit (normal or early return).
    auto rtCleanup = llvm::make_scope_exit([&RT]() {
        if (auto err = RT->remove())
            llvm::consumeError(std::move(err));
    });

    if (ry::traceEnabled())
        ry::emitTraceEvent("jit.ready", "jit", &sourceLoc,
                           {ry::TraceField("file", source_name)});

    // Lookup and run __ry_main__
    auto symOrErr = jit->lookup("__ry_main__");
    if (!symOrErr) {
        errs() << "Failed to lookup __ry_main__: ";
        logAllUnhandledErrors(symOrErr.takeError(), errs());
        ry::emitTraceEvent("runtime.error", "jit", &sourceLoc,
                           {ry::TraceField("detail", "Failed to lookup __ry_main__")});
        return 1;
    }
    auto *fn = symOrErr->toPtr<int(*)()>();

    if (test_mode) {
        std::signal(SIGALRM, test_timeout_handler);
    }
    if (ry::traceEnabled())
        ry::emitTraceEvent("exec.start", "runtime", &sourceLoc,
                           {ry::TraceField("file", source_name)});
    int result = fn();
    if (ry::traceEnabled())
        ry::emitTraceEvent("exec.end", "runtime", &sourceLoc,
                           {ry::TraceField("file", source_name),
                            ry::TraceField("exit_code", result)});

    // Record filenames for coverage reporting (accumulates across calls).
    // Exclude stdlib files (those under share_dir) from the report.
    if (coverage_mode && cs) {
        int fc = sm.getFileCount();
        int new_total = cs->file_id_offset + fc;
        if (new_total > cs->file_count) {
            std::string canonical_share;
            if (!share_dir.empty())
                canonical_share = fs::weakly_canonical(share_dir).string();
            cs->filenames.resize(static_cast<size_t>(new_total));
            for (int i = 0; i < fc; ++i) {
                size_t gid = static_cast<size_t>(cs->file_id_offset) + static_cast<size_t>(i);
                const std::string &fname = sm.getFilename(i);
                bool is_stdlib = false;
                if (!canonical_share.empty()) {
                    auto canonical = fs::weakly_canonical(fname).string();
                    is_stdlib = canonical.find(canonical_share) == 0;
                }
                cs->filenames[gid] = is_stdlib ? "" : fname;
            }
            cs->file_count = new_total;
        }
        cs->file_id_offset += fc;
    }

#if defined(__linux__) || defined(__APPLE__)
    // Cancel rtCleanup before leaking the LLJIT.  Otherwise scope_exit fires
    // RT->remove() during stack unwind, which walks the same JITLink wrapper-
    // call deallocation path that crashes inside ~LLJIT() — only suppressing
    // one frame in the family leaves the second frame (removeResourceTracker)
    // intermittently aborting under parallel load (#1187).  The OS reclaims
    // memory on process exit since the LLJIT is leaked below.
    rtCleanup.release();

    // Intentionally leak the LLJIT to prevent an intermittent crash in
    // ~LLJIT() during JIT teardown.  On Linux: ~ExecutorProcessControl() →
    // __libc_free due to JIT relocation side-effects on ELF+JITLink (#742).
    // On macOS: same intermittent crash signature observed in parallel test
    // mode (ry test -p) — ~40 % failure rate; exit code 128+N (#1088).
    // The OS reclaims memory on process exit.  Affects both subprocess and
    // sequential modes; since ry always exits after run/test, the per-
    // invocation leak is bounded by the process lifetime.
    // TODO(#742): Investigate root cause; fix or file upstream LLVM bug.
    (void)jit.release(); // NOLINT(bugprone-unused-return-value)
#endif

    return result > 0 ? 1 : 0;
}

int runRyFile(const std::string &filepath, bool test_mode,
              const char *argv0, bool skip_global_lib,
              bool coverage_mode, bool outline_mode,
              CoverageState *cs, bool emit_llvm_ir) {
    auto bufOrErr = MemoryBuffer::getFile(filepath);
    if (!bufOrErr) {
        errs() << "Error reading file: " << filepath << "\n";
        return 1;
    }
    std::string src = (*bufOrErr)->getBuffer().str();
    std::string referrer_dir = fs::path(filepath).parent_path().string();
    return runRySource(src, filepath, referrer_dir, test_mode, argv0, skip_global_lib,
                       coverage_mode, outline_mode, cs, emit_llvm_ir);
}

} // namespace ry
