#include "ry/jit.hpp"
#include <llvm/IR/Module.h>
#include <llvm/Passes/PassBuilder.h>

using namespace llvm;
using namespace llvm::orc;

namespace ry {

Expected<ThreadSafeModule>
optimizeModule(ThreadSafeModule TSM,
               const MaterializationResponsibility & /*R*/) {
    TSM.withModuleDo([](Module &M) {
        LoopAnalysisManager LAM;
        FunctionAnalysisManager FAM;
        CGSCCAnalysisManager CGAM;
        ModuleAnalysisManager MAM;

        PassBuilder PB;
        PB.registerModuleAnalyses(MAM);
        PB.registerCGSCCAnalyses(CGAM);
        PB.registerFunctionAnalyses(FAM);
        PB.registerLoopAnalyses(LAM);
        PB.crossRegisterProxies(LAM, FAM, CGAM, MAM);

        ModulePassManager MPM =
            std::getenv("RY_NO_OPT")
                ? PB.buildPerModuleDefaultPipeline(OptimizationLevel::O0)
                : PB.buildPerModuleDefaultPipeline(OptimizationLevel::O2);
        MPM.run(M, MAM);
#ifndef __clang_analyzer__
        if (std::getenv("RY_DUMP_OPT_IR"))
            M.print(llvm::errs(), nullptr);
#endif
    });
    return std::move(TSM);
}

} // namespace ry
