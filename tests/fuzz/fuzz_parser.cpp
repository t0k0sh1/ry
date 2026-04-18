#include "ry/lexer.hpp"
#include "ry/parser.hpp"

#include <cstddef>
#include <cstdint>
#include <exception>
#include <string>

extern "C" int LLVMFuzzerTestOneInput(const uint8_t *data, size_t size) {
    try {
        ry::Lexer lex(std::string(reinterpret_cast<const char *>(data), size));
        ry::Parser parser(lex);
        parser.parseProgram();
    } catch (const std::exception &e) {
        // Expected: DiagnosticError (parse errors), std::runtime_error (lexer hard errors).
        // Any other exception or an abort() is a bug.
        (void)e;
    }
    return 0;
}
