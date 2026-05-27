#pragma once

#include "ry/source_manager/source_location.hpp"
#include "ry/source_manager/source_manager.hpp"
#include <stdexcept>
#include <string>


namespace ry {

enum class DiagLevel { Error, Warning, Note };

struct Diagnostic {
    DiagLevel level = DiagLevel::Error;
    SourceLocation loc;
    std::string message;
    std::string label;
};

std::string formatDiagnostic(const Diagnostic &diag, const SourceManager *sm);

class DiagnosticError : public std::runtime_error {
public:
    DiagnosticError(Diagnostic diag, const SourceManager *sm);
    DiagnosticError(const SourceLocation &loc, const std::string &msg, const SourceManager *sm);
    const Diagnostic& diagnostic() const { return diag_; }

private:
    Diagnostic diag_;
};

} // namespace ry
