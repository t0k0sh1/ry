#include "test_codegen_common.hpp"

#include <fstream>
#include <iterator>
#include <string>


using namespace ry;
namespace {

std::string readFile(const std::string &path) {
    for (const std::string &candidate : {path, "../" + path}) {
        std::ifstream in(candidate);
        if (in) {
            return std::string((std::istreambuf_iterator<char>(in)),
                               std::istreambuf_iterator<char>());
        }
    }
    throw std::runtime_error("failed to open spec file: " + path);
}

} // namespace

// Previously disabled under ASan (commit fb010ea, 2026-03-31) because
// in-process parallel-for tests caused a deadlock: non-atomic ARC
// retain/release ops raced with ASan's shadow-memory interceptors.
// Re-enabled after #630's P0 fix made all ARC ops inside @parallel for
// thunks use atomicrmw — the root cause is gone.  Verified: passes in
// 55 ms on macOS (ASAN_OPTIONS=detect_container_overflow=0).
TEST_F(CodeGenTest, ConcurrencySpecSuite) {
    EXPECT_NO_THROW(runTestSource(withStdlibDirectiveDecls(stripTestingImport(readFile("tests/spec/concurrency.test.ry")))));
}
