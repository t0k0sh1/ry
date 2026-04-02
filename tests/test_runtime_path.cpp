#include <gtest/gtest.h>
#include <climits>
#include <cstdlib>
#include <cstring>
#include <string>
#include <unistd.h>

// Forward-declare the runtime path functions under test.
// They are compiled into ry_lib and linked via whole-archive.
extern "C" {
const char *__ry_path_join2(const char *a, const char *b);
const char *__ry_path_join3(const char *a, const char *b, const char *c);
const char *__ry_path_join4(const char *a, const char *b, const char *c, const char *d);
const char *__ry_path_basename(const char *p);
const char *__ry_path_dirname(const char *p);
const char *__ry_path_extension(const char *p);
const char *__ry_path_resolve(const char *p);
int64_t     __ry_path_is_absolute(const char *p);
}

// RAII wrapper to free malloc'd strings returned by the runtime.
struct FreeStr {
    const char *p;
    explicit FreeStr(const char *s) : p(s) {}
    ~FreeStr() { std::free(const_cast<char *>(p)); }
    operator std::string() const { return p ? std::string(p) : ""; }
    bool operator==(const char *s) const { return p && std::strcmp(p, s) == 0; }
};

// ===== __ry_path_join2 =====

TEST(PathJoin2, BasicJoin) {
    FreeStr r(__ry_path_join2("/home/user", "docs"));
    EXPECT_EQ(std::string(r), "/home/user/docs");
}

TEST(PathJoin2, TrailingSlashOnA) {
    FreeStr r(__ry_path_join2("/home/user/", "docs"));
    EXPECT_EQ(std::string(r), "/home/user/docs");
}

TEST(PathJoin2, MultipleTrailingSlashesOnA) {
    FreeStr r(__ry_path_join2("/home/user///", "docs"));
    EXPECT_EQ(std::string(r), "/home/user/docs");
}

TEST(PathJoin2, BIsAbsoluteReturnB) {
    // If b is absolute, it overrides a (POSIX behaviour).
    FreeStr r(__ry_path_join2("/home/user", "/etc/passwd"));
    EXPECT_EQ(std::string(r), "/etc/passwd");
}

TEST(PathJoin2, EmptyAReturnsB) {
    FreeStr r(__ry_path_join2("", "docs"));
    EXPECT_EQ(std::string(r), "docs");
}

TEST(PathJoin2, EmptyBReturnsA) {
    FreeStr r(__ry_path_join2("/home/user", ""));
    EXPECT_EQ(std::string(r), "/home/user");
}

TEST(PathJoin2, RelativeJoin) {
    FreeStr r(__ry_path_join2("a/b", "c"));
    EXPECT_EQ(std::string(r), "a/b/c");
}

TEST(PathJoin2, RootA) {
    // Root path "/" + "etc" should give "/etc", not "//etc".
    FreeStr r(__ry_path_join2("/", "etc"));
    EXPECT_EQ(std::string(r), "/etc");
}

// ===== __ry_path_join3 =====

TEST(PathJoin3, ThreeComponents) {
    FreeStr r(__ry_path_join3("/a", "b", "c"));
    EXPECT_EQ(std::string(r), "/a/b/c");
}

TEST(PathJoin3, LastComponentAbsolute) {
    FreeStr r(__ry_path_join3("/a", "b", "/root"));
    EXPECT_EQ(std::string(r), "/root");
}

TEST(PathJoin3, MiddleComponentAbsolute) {
    FreeStr r(__ry_path_join3("/a", "/b", "c"));
    EXPECT_EQ(std::string(r), "/b/c");
}

// ===== __ry_path_join4 =====

TEST(PathJoin4, FourComponents) {
    FreeStr r(__ry_path_join4("/a", "b", "c", "d"));
    EXPECT_EQ(std::string(r), "/a/b/c/d");
}

TEST(PathJoin4, LastComponentAbsolute) {
    FreeStr r(__ry_path_join4("a", "b", "c", "/abs"));
    EXPECT_EQ(std::string(r), "/abs");
}

// ===== __ry_path_basename =====

TEST(PathBasename, SimpleFile) {
    FreeStr r(__ry_path_basename("/home/user/file.txt"));
    EXPECT_EQ(std::string(r), "file.txt");
}

TEST(PathBasename, NoDirectory) {
    FreeStr r(__ry_path_basename("file.txt"));
    EXPECT_EQ(std::string(r), "file.txt");
}

TEST(PathBasename, TrailingSlash) {
    FreeStr r(__ry_path_basename("/home/user/"));
    EXPECT_EQ(std::string(r), "user");
}

TEST(PathBasename, RootPath) {
    FreeStr r(__ry_path_basename("/"));
    // Root has no basename component.
    EXPECT_EQ(std::string(r), "");
}

TEST(PathBasename, EmptyString) {
    FreeStr r(__ry_path_basename(""));
    EXPECT_EQ(std::string(r), "");
}

TEST(PathBasename, DeepPath) {
    FreeStr r(__ry_path_basename("/a/b/c/d/e.cpp"));
    EXPECT_EQ(std::string(r), "e.cpp");
}

// ===== __ry_path_dirname =====

TEST(PathDirname, SimpleFile) {
    FreeStr r(__ry_path_dirname("/home/user/file.txt"));
    EXPECT_EQ(std::string(r), "/home/user");
}

TEST(PathDirname, NoDirectory) {
    FreeStr r(__ry_path_dirname("file.txt"));
    EXPECT_EQ(std::string(r), ".");
}

TEST(PathDirname, TrailingSlash) {
    FreeStr r(__ry_path_dirname("/home/user/"));
    EXPECT_EQ(std::string(r), "/home");
}

TEST(PathDirname, RootPath) {
    FreeStr r(__ry_path_dirname("/"));
    EXPECT_EQ(std::string(r), "/");
}

TEST(PathDirname, EmptyString) {
    FreeStr r(__ry_path_dirname(""));
    EXPECT_EQ(std::string(r), ".");
}

TEST(PathDirname, DirectlyUnderRoot) {
    FreeStr r(__ry_path_dirname("/file.txt"));
    EXPECT_EQ(std::string(r), "/");
}

TEST(PathDirname, DeepPath) {
    FreeStr r(__ry_path_dirname("/a/b/c/d/e.cpp"));
    EXPECT_EQ(std::string(r), "/a/b/c/d");
}

// ===== __ry_path_extension =====

TEST(PathExtension, SimpleExtension) {
    FreeStr r(__ry_path_extension("file.txt"));
    EXPECT_EQ(std::string(r), ".txt");
}

TEST(PathExtension, LongPath) {
    FreeStr r(__ry_path_extension("/a/b/c/archive.tar.gz"));
    // Should return the last extension only.
    EXPECT_EQ(std::string(r), ".gz");
}

TEST(PathExtension, NoExtension) {
    FreeStr r(__ry_path_extension("/usr/bin/ls"));
    EXPECT_EQ(std::string(r), "");
}

TEST(PathExtension, HiddenFile) {
    // Dotfiles like .gitignore have no extension (leading dot is not an extension).
    FreeStr r(__ry_path_extension(".gitignore"));
    EXPECT_EQ(std::string(r), "");
}

TEST(PathExtension, HiddenFileWithExtension) {
    FreeStr r(__ry_path_extension(".hidden.txt"));
    EXPECT_EQ(std::string(r), ".txt");
}

TEST(PathExtension, EmptyString) {
    FreeStr r(__ry_path_extension(""));
    EXPECT_EQ(std::string(r), "");
}

TEST(PathExtension, TrailingDot) {
    // A trailing dot should be treated as an empty extension name but still
    // a dot, so the extension is ".".
    FreeStr r(__ry_path_extension("file."));
    EXPECT_EQ(std::string(r), ".");
}

// ===== __ry_path_is_absolute =====

TEST(PathIsAbsolute, AbsolutePath) {
    EXPECT_EQ(__ry_path_is_absolute("/home/user"), 1);
}

TEST(PathIsAbsolute, RelativePath) {
    EXPECT_EQ(__ry_path_is_absolute("relative/path"), 0);
}

TEST(PathIsAbsolute, DotPath) {
    EXPECT_EQ(__ry_path_is_absolute("./foo"), 0);
}

TEST(PathIsAbsolute, RootPath) {
    EXPECT_EQ(__ry_path_is_absolute("/"), 1);
}

TEST(PathIsAbsolute, Nullptr) {
    EXPECT_EQ(__ry_path_is_absolute(nullptr), 0);
}

TEST(PathIsAbsolute, EmptyString) {
    EXPECT_EQ(__ry_path_is_absolute(""), 0);
}

// ===== __ry_path_resolve =====

TEST(PathResolve, ExistingDirectory) {
    // Resolve the current working directory — it must exist.
    char cwd[PATH_MAX];
    ASSERT_NE(getcwd(cwd, sizeof(cwd)), nullptr);

    FreeStr r(__ry_path_resolve(cwd));
    ASSERT_NE(r.p, nullptr);
    // A resolved path must be absolute.
    EXPECT_EQ(__ry_path_is_absolute(r.p), 1);
}

TEST(PathResolve, NonExistentPath) {
    // Resolving a path that does not exist should return nullptr.
    const char *r = __ry_path_resolve("/this/path/definitely/does/not/exist/abc123");
    EXPECT_EQ(r, nullptr);
    if (r) std::free(const_cast<char *>(r));
}

TEST(PathResolve, EmptyPath) {
    // Empty path should fail.
    const char *r = __ry_path_resolve("");
    EXPECT_EQ(r, nullptr);
    if (r) std::free(const_cast<char *>(r));
}

// Regression: join2 then basename should compose correctly.
TEST(PathCompose, JoinThenBasename) {
    FreeStr joined(__ry_path_join2("/usr/lib", "libm.so"));
    FreeStr base(__ry_path_basename(joined.p));
    EXPECT_EQ(std::string(base), "libm.so");
}

// Regression: join2 then dirname should compose correctly.
TEST(PathCompose, JoinThenDirname) {
    FreeStr joined(__ry_path_join2("/usr/lib", "libm.so"));
    FreeStr dir(__ry_path_dirname(joined.p));
    EXPECT_EQ(std::string(dir), "/usr/lib");
}