#include "ry/diagnostic.hpp"
#include <sstream>

static const char* levelStr(DiagLevel level) {
    switch (level) {
        case DiagLevel::Error:   return "error";
        case DiagLevel::Warning: return "warning";
        case DiagLevel::Note:    return "note";
    }
    return "error";
}

std::string formatDiagnostic(const Diagnostic &diag, const SourceManager *sm) {
    std::ostringstream os;

    os << levelStr(diag.level) << ": " << diag.message << "\n";

    if (!diag.loc.isValid() || !sm) {
        return os.str();
    }

    const auto &filename = sm->getFilename(diag.loc.file_id);
    auto srcLine = sm->getLine(diag.loc.file_id, diag.loc.line);

    int lineNum = diag.loc.line;
    int lineWidth = 1;
    {
        int n = lineNum;
        while (n >= 10) { n /= 10; ++lineWidth; }
    }

    os << std::string(lineWidth + 1, ' ') << "--> " << filename
       << ":" << lineNum << ":" << diag.loc.col << "\n";

    os << std::string(lineWidth + 1, ' ') << "|\n";

    os << lineNum << " | " << srcLine << "\n";

    os << std::string(lineWidth + 1, ' ') << "| ";

    if (diag.loc.col > 0) {
        // Replicate whitespace from the source line for alignment
        for (int i = 0; i < diag.loc.col - 1 && i < static_cast<int>(srcLine.size()); ++i) {
            os << (srcLine[i] == '\t' ? '\t' : ' ');
        }
        os << "^";
    }

    if (!diag.label.empty()) {
        os << " " << diag.label;
    }
    os << "\n";

    return os.str();
}

DiagnosticError::DiagnosticError(Diagnostic diag, const SourceManager *sm)
    : std::runtime_error(formatDiagnostic(diag, sm)), diag_(std::move(diag)) {}

DiagnosticError::DiagnosticError(const SourceLocation &loc, const std::string &msg, const SourceManager *sm)
    : DiagnosticError(Diagnostic{DiagLevel::Error, loc, msg, msg}, sm) {}
