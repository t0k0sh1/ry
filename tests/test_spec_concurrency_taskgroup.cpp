#include "test_codegen_common.hpp"

#include <fstream>
#include <iterator>
#include <string>

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

TEST_F(CodeGenTest, ConcurrencyTaskGroupSpecSuite) {
    try {
        runTestSource(readFile("tests/spec/concurrency_taskgroup.test.ry"));
    } catch (const std::exception &e) {
        FAIL() << "Exception: " << e.what();
    } catch (...) {
        FAIL() << "Unknown exception";
    }
}
