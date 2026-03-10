#pragma once

#include <llvm/ExecutionEngine/Orc/Core.h>
#include <llvm/ExecutionEngine/Orc/ThreadSafeModule.h>

namespace ry {

/// Apply O2 optimization pipeline to a ThreadSafeModule (in-place transform).
llvm::Expected<llvm::orc::ThreadSafeModule>
optimizeModule(llvm::orc::ThreadSafeModule TSM,
               const llvm::orc::MaterializationResponsibility &R);

} // namespace ry
