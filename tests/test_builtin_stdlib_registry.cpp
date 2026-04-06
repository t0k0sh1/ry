#include <gtest/gtest.h>
#include "ry/stdlib_registry.hpp"
#include <filesystem>
#include <fstream>
#include <sstream>
#include <string>
#include <unordered_set>


using namespace ry;
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

TEST(BuiltinStdlibRegistry, ExpectedPackagesRegistered) {
    auto &pkgs = StdlibRegistry::instance().packages();
    std::unordered_set<std::string> names;
    for (auto &pkg : pkgs)
        names.insert(pkg.package_name);
    for (auto *expected : {"math", "io", "net", "http", "json", "path", "thread"}) {
        EXPECT_TRUE(names.count(expected))
            << expected << " package not registered — check CMakeLists.txt ry_lib sources";
    }
}

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
    EXPECT_FALSE(constants.empty()) << "no native constants registered";
    // Verify each constant is declared in at least one package .ry file
    for (auto &[name, entry] : constants) {
        bool found = false;
        for (auto &pkg : StdlibRegistry::instance().packages()) {
            const std::string content = read_text(repo_root() / pkg.decl_path);
            if (content.find("@const\n" + name + ":") != std::string::npos) {
                found = true;
                break;
            }
        }
        EXPECT_TRUE(found) << "constant " << name
                           << " not found in any registered package";
    }
}
