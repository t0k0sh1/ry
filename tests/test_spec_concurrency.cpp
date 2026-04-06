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

// ASan builds hang on in-process parallel-for / async tests due to
// thread-sanitizer overhead.  The same tests run reliably via
// `ry test -p` (subprocess) and the asan-trace CI job.
#if defined(__SANITIZE_ADDRESS__) || (defined(__has_feature) && __has_feature(address_sanitizer))
TEST_F(CodeGenTest, DISABLED_ConcurrencySpecSuite) {
#else
TEST_F(CodeGenTest, ConcurrencySpecSuite) {
#endif
    EXPECT_NO_THROW(runTestSource(readFile("tests/spec/concurrency.test.ry")));
}
