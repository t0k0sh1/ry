#include <gtest/gtest.h>
#include "ry/stdlib_registry.hpp"
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
    for (auto &pkg : StdlibRegistry::instance().packages()) {
        EXPECT_TRUE(fs::exists(repo_root() / pkg.decl_path)) << pkg.decl_path;
    }
}

// Regression test: net must be dispatched before http because net::listen
// (2-arg) falls through to http::listen (3+-arg) via a nullptr return.
// If http were dispatched first, http::listen would grab all listen() calls
// including the 2-arg TCP variant.
TEST(BuiltinStdlibRegistry, NetDispatchedBeforeHttp) {
    auto &pkgs = StdlibRegistry::instance().packages();
    int net_idx = -1, http_idx = -1;
    for (int i = 0; i < static_cast<int>(pkgs.size()); ++i) {
        if (std::string(pkgs[i].package_name) == "net") net_idx = i;
        if (std::string(pkgs[i].package_name) == "http") http_idx = i;
    }
    ASSERT_NE(net_idx, -1) << "net package not registered";
    ASSERT_NE(http_idx, -1) << "http package not registered";
    EXPECT_LT(net_idx, http_idx)
        << "net (index " << net_idx << ") must be dispatched before http (index "
        << http_idx << ") so that net::listen can fall through to http::listen";
}

TEST(BuiltinStdlibRegistry, NativeConstantsAreDeclaredInStdlib) {
    auto &constants = StdlibRegistry::instance().constants();
    // All math constants must be declared in math.ry
    const fs::path math_decl = repo_root() / "share/std/math/math.ry";
    const std::string content = read_text(math_decl);
    for (auto &[name, entry] : constants) {
        EXPECT_NE(content.find("@const\n" + name + ":"), std::string::npos)
            << "constant " << name << " not found in " << math_decl;
    }
}
