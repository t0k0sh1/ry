// Test taxonomy: docs/reference/test-taxonomy.md
// Section header tags: [contract] / [regression #NNNN] / [internal].
// Per-test exceptions use an inline `// [regression: #NNNN]` comment.

#include "test_codegen_common.hpp"
#include <cstdio>
#include <cstdlib>
#include <string>


using namespace ry;
// Helper to create a unique temp file path
static std::string tmpPath(const std::string &suffix) {
    return "/tmp/ry_io_test_" + suffix + ".txt";
}

// Cleanup helper
static void removeIfExists(const std::string &path) {
    std::remove(path.c_str());
}

// @native fn declarations needed for IO functions. Post-#2338 these must
// carry the explicit `@native("io")` tag — descriptor-driven dispatch
// indexes by library name, and bare `@native` without a containing
// module cannot resolve to libry_io.dylib (runSource skips ModuleLoader
// so rule (b) module-keyed inference does not apply). Mirrors the
// share/std/io/io.ry surface that ModuleLoader normally injects.
static const std::string IO_DECLS = R"(
@native("io")
fn readText(path: str) -> Result<str, Error>
@native("io")
fn writeText(path: str, content: str) -> Result<Unit, Error>
@native("io")
fn appendText(path: str, content: str) -> Result<Unit, Error>
@native("io")
fn exists(path: str) -> bool
@native("io")
fn deleteFile(path: str) -> Result<Unit, Error>
@native("io")
fn readBytes(path: str) -> Result<List<u8>, Error>
@native("io")
fn writeBytes(path: str, data: List<u8>) -> Result<Unit, Error>
@native("io")
fn toBytes(s: str) -> List<u8>
@native("io")
fn bytesToStr(bs: List<u8>) -> Result<str, Error>
)";

// ===== [contract] writeText / readText round-trip =====

TEST_F(CodeGenTest, IOWriteReadText) {
    std::string path = tmpPath("write_read");
    removeIfExists(path);
    EXPECT_EQ(runSource(IO_DECLS + R"(
case writeText(")" + path + R"(", "hello world"):
    Ok(_):
        case readText(")" + path + R"("):
            Ok(s):
                print(s)
            Err(e):
                print(e.message)
    Err(e):
        print(e.message)
)"), "hello world\n");
    removeIfExists(path);
}

// ===== [contract] appendText =====

TEST_F(CodeGenTest, IOAppendText) {
    std::string path = tmpPath("append");
    removeIfExists(path);
    EXPECT_EQ(runSource(IO_DECLS + R"(
case writeText(")" + path + R"(", "hello"):
    Ok(_):
        case appendText(")" + path + R"(", " world"):
            Ok(_):
                case readText(")" + path + R"("):
                    Ok(s):
                        print(s)
                    Err(e):
                        print(e.message)
            Err(e):
                print(e.message)
    Err(e):
        print(e.message)
)"), "hello world\n");
    removeIfExists(path);
}

// ===== [contract] exists =====

TEST_F(CodeGenTest, IOFileExistsTrue) {
    std::string path = tmpPath("exists");
    removeIfExists(path);
    EXPECT_EQ(runSource(IO_DECLS + R"(
case writeText(")" + path + R"(", "test"):
    Ok(_):
        print(exists(")" + path + R"("))
    Err(e):
        print(e.message)
)"), "true\n");
    removeIfExists(path);
}

TEST_F(CodeGenTest, IOFileExistsFalse) {
    std::string path = tmpPath("not_exists");
    removeIfExists(path);
    EXPECT_EQ(runSource(IO_DECLS + R"(
print(exists(")" + path + R"("))
)"), "false\n");
}

// ===== [contract] deleteFile =====

TEST_F(CodeGenTest, IODeleteFile) {
    std::string path = tmpPath("delete");
    removeIfExists(path);
    EXPECT_EQ(runSource(IO_DECLS + R"(
case writeText(")" + path + R"(", "temp"):
    Ok(_):
        case deleteFile(")" + path + R"("):
            Ok(_):
                print(exists(")" + path + R"("))
            Err(e):
                print(e.message)
    Err(e):
        print(e.message)
)"), "false\n");
}

// ===== [contract] toBytes / bytesToStr =====

TEST_F(CodeGenTest, IOStrToBytes) {
    EXPECT_EQ(runSource(IO_DECLS + R"(
bs = toBytes("ABC")
print(len(bs))
print(bs[0])
print(bs[1])
print(bs[2])
)"), "3\n65\n66\n67\n");
}

TEST_F(CodeGenTest, IOBytesToStr) {
    EXPECT_EQ(runSource(IO_DECLS + R"(
bs = toBytes("hello")
case bytesToStr(bs):
    Ok(s):
        print(s)
    Err(e):
        print(e.message)
)"), "hello\n");
}

// ===== [contract] readBytes / writeBytes round-trip =====

TEST_F(CodeGenTest, IOWriteReadBytes) {
    std::string path = tmpPath("bytes");
    removeIfExists(path);
    EXPECT_EQ(runSource(IO_DECLS + R"(
bs = toBytes("binary data")
case writeBytes(")" + path + R"(", bs):
    Ok(_):
        case readBytes(")" + path + R"("):
            Ok(rb):
                case bytesToStr(rb):
                    Ok(s):
                        print(s)
                        print(len(rb))
                    Err(e):
                        print(e.message)
            Err(e):
                print(e.message)
    Err(e):
        print(e.message)
)"), "binary data\n11\n");
    removeIfExists(path);
}

// ===== [contract] Error case: readText on non-existent file returns Err =====

TEST_F(CodeGenTest, IOReadTextNotFound) {
    EXPECT_EQ(runSource(IO_DECLS + R"(
case readText("/tmp/ry_io_test_no_such_file_ever.txt"):
    Ok(s):
        print("unexpected success")
    Err(e):
        print("error")
)"), "error\n");
}
