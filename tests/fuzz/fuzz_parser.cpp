#include "ry/diagnostic.hpp"
#include "ry/lexer.hpp"
#include "ry/parser.hpp"

#include <cstddef>
#include <cstdint>
#include <stdexcept>
#include <string>

extern "C" int LLVMFuzzerTestOneInput(const uint8_t *data, size_t size) {
    try {
        ry::Lexer lex(std::string(reinterpret_cast<const char *>(data), size));
        ry::Parser parser(lex);
        parser.parseProgram();
    } catch (const ry::DiagnosticError &) { // NOLINT(bugprone-empty-catch)
        // Expected: parser diagnostics on malformed input.
    } catch (const std::runtime_error &) { // NOLINT(bugprone-empty-catch)
        // Expected: lexer hard errors (e.g., invalid UTF-8, unterminated string).
    }
    return 0;
}
