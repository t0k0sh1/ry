#include <gtest/gtest.h>
#include "ry/builtin_stdlib_registry.hpp"
#include <filesystem>
#include <fstream>
#include <sstream>
#include <string>

namespace fs = std::filesystem;

namespace {

fs::path repo_root() {
    return fs::path(__FILE__).parent_path().parent_path();
}

std::string read_text(const fs::path &path) {
    std::ifstream file(path);
    std::ostringstream out;
    out << file.rdbuf();
    return out.str();
}

} // namespace

TEST(BuiltinStdlibRegistry, DeclarationFilesExist) {
#define RY_EXPECT_DECL_EXISTS(pkg, decl, method) \
    EXPECT_TRUE(fs::exists(repo_root() / decl)) << decl;
    RY_BUILTIN_STDLIB_PACKAGES(RY_EXPECT_DECL_EXISTS)
#undef RY_EXPECT_DECL_EXISTS
}

TEST(BuiltinStdlibRegistry, NativeConstantsAreDeclaredInStdlib) {
#define RY_EXPECT_CONST_DECL(pkg, name, kind, value)                                      \
    do {                                                                                  \
        const fs::path decl_path = repo_root() / "lib/std/" #pkg "/" #pkg ".ry";         \
        const std::string content = read_text(decl_path);                                 \
        EXPECT_NE(content.find("@const\n" #name ":"), std::string::npos) << decl_path;   \
    } while (false);
    RY_BUILTIN_STDLIB_CONSTANTS(RY_EXPECT_CONST_DECL)
#undef RY_EXPECT_CONST_DECL
}
