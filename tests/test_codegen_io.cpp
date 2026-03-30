#include "test_codegen_common.hpp"
#include <cstdio>
#include <cstdlib>
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
fn read_text(path: str) -> Result<str, Error>
@native
fn write_text(path: str, content: str) -> Result<Unit, Error>
@native
fn append_text(path: str, content: str) -> Result<Unit, Error>
@native
fn file_exists(path: str) -> bool
@native
fn delete_file(path: str) -> Result<Unit, Error>
@native
fn read_bytes(path: str) -> Result<List<u8>, Error>
@native
fn write_bytes(path: str, data: List<u8>) -> Result<Unit, Error>
@native
fn str_to_bytes(s: str) -> List<u8>
@native
fn bytes_to_str(bs: List<u8>) -> Result<str, Error>
)";

// ============================================================
// write_text / read_text round-trip
// ============================================================

TEST_F(CodeGenTest, IOWriteReadText) {
    std::string path = tmpPath("write_read");
    removeIfExists(path);
    EXPECT_EQ(runSource(IO_DECLS + R"(
when write_text(")" + path + R"(", "hello world"):
    case Ok(_):
        when read_text(")" + path + R"("):
            case Ok(s):
                print(s)
            case Err(e):
                print(e.message)
    case Err(e):
        print(e.message)
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
when write_text(")" + path + R"(", "hello"):
    case Ok(_):
        when append_text(")" + path + R"(", " world"):
            case Ok(_):
                when read_text(")" + path + R"("):
                    case Ok(s):
                        print(s)
                    case Err(e):
                        print(e.message)
            case Err(e):
                print(e.message)
    case Err(e):
        print(e.message)
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
when write_text(")" + path + R"(", "test"):
    case Ok(_):
        print(file_exists(")" + path + R"("))
    case Err(e):
        print(e.message)
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
when write_text(")" + path + R"(", "temp"):
    case Ok(_):
        when delete_file(")" + path + R"("):
            case Ok(_):
                print(file_exists(")" + path + R"("))
            case Err(e):
                print(e.message)
    case Err(e):
        print(e.message)
)"), "false\n");
}

// ============================================================
// str_to_bytes / bytes_to_str
// ============================================================

TEST_F(CodeGenTest, IOStrToBytes) {
    EXPECT_EQ(runSource(IO_DECLS + R"(
bs = str_to_bytes("ABC")
print(length(bs))
print(bs[0])
print(bs[1])
print(bs[2])
)"), "3\n65\n66\n67\n");
}

TEST_F(CodeGenTest, IOBytesToStr) {
    EXPECT_EQ(runSource(IO_DECLS + R"(
bs = str_to_bytes("hello")
when bytes_to_str(bs):
    case Ok(s):
        print(s)
    case Err(e):
        print(e.message)
)"), "hello\n");
}

// ============================================================
// read_bytes / write_bytes round-trip
// ============================================================

TEST_F(CodeGenTest, IOWriteReadBytes) {
    std::string path = tmpPath("bytes");
    removeIfExists(path);
    EXPECT_EQ(runSource(IO_DECLS + R"(
bs = str_to_bytes("binary data")
when write_bytes(")" + path + R"(", bs):
    case Ok(_):
        when read_bytes(")" + path + R"("):
            case Ok(rb):
                when bytes_to_str(rb):
                    case Ok(s):
                        print(s)
                        print(length(rb))
                    case Err(e):
                        print(e.message)
            case Err(e):
                print(e.message)
    case Err(e):
        print(e.message)
)"), "binary data\n11\n");
    removeIfExists(path);
}

// ============================================================
// Error case: read_text on non-existent file returns Err
// ============================================================

TEST_F(CodeGenTest, IOReadTextNotFound) {
    EXPECT_EQ(runSource(IO_DECLS + R"(
when read_text("/tmp/ry_io_test_no_such_file_ever.txt"):
    case Ok(s):
        print("unexpected success")
    case Err(e):
        print("error")
)"), "error\n");
}
