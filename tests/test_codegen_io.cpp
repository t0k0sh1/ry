#include "test_codegen_common.hpp"
#include <cstdio>
#include <cstdlib>
#include <fstream>
#include <string>

// Helper to create a unique temp file path
static std::string tmpPath(const std::string &suffix) {
    return "/tmp/ry_io_test_" + suffix + ".txt";
}

// Cleanup helper
static void removeIfExists(const std::string &path) {
    std::remove(path.c_str());
}

// @native fn declarations needed for IO functions
static const std::string IO_DECLS = R"(
@native
fn read_text(path: str) -> str
@native
fn write_text(path: str, content: str) -> Unit
@native
fn append_text(path: str, content: str) -> Unit
@native
fn file_exists(path: str) -> bool
@native
fn delete_file(path: str) -> Unit
@native
fn read_bytes(path: str) -> List<byte>
@native
fn write_bytes(path: str, data: List<byte>) -> Unit
@native
fn str_to_bytes(s: str) -> List<byte>
@native
fn bytes_to_str(bs: List<byte>) -> str
)";

// ============================================================
// write_text / read_text round-trip
// ============================================================

TEST_F(CodeGenTest, IOWriteReadText) {
    std::string path = tmpPath("write_read");
    removeIfExists(path);
    EXPECT_EQ(runSource(IO_DECLS + R"(
write_text(")" + path + R"(", "hello world")
let s = read_text(")" + path + R"(")
print(s)
)"), "hello world\n");
    removeIfExists(path);
}

// ============================================================
// append_text
// ============================================================

TEST_F(CodeGenTest, IOAppendText) {
    std::string path = tmpPath("append");
    removeIfExists(path);
    EXPECT_EQ(runSource(IO_DECLS + R"(
write_text(")" + path + R"(", "hello")
append_text(")" + path + R"(", " world")
let s = read_text(")" + path + R"(")
print(s)
)"), "hello world\n");
    removeIfExists(path);
}

// ============================================================
// file_exists
// ============================================================

TEST_F(CodeGenTest, IOFileExistsTrue) {
    std::string path = tmpPath("exists");
    removeIfExists(path);
    EXPECT_EQ(runSource(IO_DECLS + R"(
write_text(")" + path + R"(", "test")
print(file_exists(")" + path + R"("))
)"), "true\n");
    removeIfExists(path);
}

TEST_F(CodeGenTest, IOFileExistsFalse) {
    std::string path = tmpPath("not_exists");
    removeIfExists(path);
    EXPECT_EQ(runSource(IO_DECLS + R"(
print(file_exists(")" + path + R"("))
)"), "false\n");
}

// ============================================================
// delete_file
// ============================================================

TEST_F(CodeGenTest, IODeleteFile) {
    std::string path = tmpPath("delete");
    removeIfExists(path);
    EXPECT_EQ(runSource(IO_DECLS + R"(
write_text(")" + path + R"(", "temp")
delete_file(")" + path + R"(")
print(file_exists(")" + path + R"("))
)"), "false\n");
}

// ============================================================
// str_to_bytes / bytes_to_str
// ============================================================

TEST_F(CodeGenTest, IOStrToBytes) {
    EXPECT_EQ(runSource(IO_DECLS + R"(
let bs = str_to_bytes("ABC")
print(len(bs))
print(bs[0])
print(bs[1])
print(bs[2])
)"), "3\n65\n66\n67\n");
}

TEST_F(CodeGenTest, IOBytesToStr) {
    EXPECT_EQ(runSource(IO_DECLS + R"(
let bs = str_to_bytes("hello")
let s = bytes_to_str(bs)
print(s)
)"), "hello\n");
}

// ============================================================
// read_bytes / write_bytes round-trip
// ============================================================

TEST_F(CodeGenTest, IOWriteReadBytes) {
    std::string path = tmpPath("bytes");
    removeIfExists(path);
    EXPECT_EQ(runSource(IO_DECLS + R"(
let bs = str_to_bytes("binary data")
write_bytes(")" + path + R"(", bs)
let rb = read_bytes(")" + path + R"(")
let s = bytes_to_str(rb)
print(s)
print(len(rb))
)"), "binary data\n11\n");
    removeIfExists(path);
}

// ============================================================
// Error case: read_text on non-existent file
// ============================================================

TEST_F(CodeGenTest, IOReadTextNotFound) {
    EXPECT_EXIT(
        runSource(IO_DECLS + R"(
let s = read_text("/tmp/ry_io_test_no_such_file_ever.txt")
print(s)
)"),
        ::testing::ExitedWithCode(1),
        "runtime error: cannot open file"
    );
}
