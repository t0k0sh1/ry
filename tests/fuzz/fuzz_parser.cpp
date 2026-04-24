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
    } catch (const std::exception &) { // NOLINT(bugprone-empty-catch)
        // Any parser/lexer exception is a valid rejection for the fuzzer:
        //   - ry::DiagnosticError (parser diagnostics; derives from std::runtime_error)
        //   - std::runtime_error (lexer hard errors: invalid UTF-8, unterminated string)
        //   - std::out_of_range / other std::logic_error subtypes
        //     (parseFloatLiteral / parseIntLiteral on malformed numeric literals
        //      such as "0B1f32" — see #1275)
    }
    return 0;
}
