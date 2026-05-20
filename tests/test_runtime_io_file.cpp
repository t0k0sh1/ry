#include "ry/runtime_io.hpp"
#include "ry/runtime_string.hpp"
#include "ry/runtime_arc.hpp"
#include <gtest/gtest.h>
#include <cstdio>
#include <cstdlib>
#include <cstring>

using namespace ry;

extern "C" {
void *__ry_io_file_open(const char *path, const char *mode);
const char *__ry_io_file_read_all(void *handle);
int64_t __ry_io_file_read_line(void *handle, const char **out_line);
int64_t __ry_io_file_write_text(void *handle, const char *s);
void __ry_io_file_close(void *handle);
void __ry_io_file_cleanup(void *handle);
const char *__ry_get_last_error();
}

// __ry_io_file_open / __ry_io_file_write_text expect Ry string handles
// (StringHeader-backed pointers from makeString), not raw C strings.
static const char *ryStr(const char *s) {
    return makeString(s, strlen(s));
}

static std::string tmpPath(const char *suffix) {
    return std::string("/tmp/ry_rt_io_file_") + suffix;
}

TEST(RuntimeIoFileTest, OpenNullPathReturnsNull) {
    void *h = __ry_io_file_open(nullptr, ryStr("r"));
    EXPECT_EQ(h, nullptr);
}

TEST(RuntimeIoFileTest, OpenNonexistentPathReturnsNull) {
    const char *path = ryStr("/tmp/ry_rt_io_no_such_file_xyzzy.txt");
    void *h = __ry_io_file_open(path, ryStr("r"));
    EXPECT_EQ(h, nullptr);
    const char *err = __ry_get_last_error();
    EXPECT_NE(err, nullptr);
    EXPECT_NE(std::strstr(err, "open"), nullptr);
}

TEST(RuntimeIoFileTest, OpenInvalidModeReturnsNull) {
    std::string p = tmpPath("badmode.txt");
    FILE *f = fopen(p.c_str(), "w");
    if (f) fclose(f);
    void *h = __ry_io_file_open(ryStr(p.c_str()), ryStr("z"));
    EXPECT_EQ(h, nullptr);
    std::remove(p.c_str());
}

TEST(RuntimeIoFileTest, OpenWriteReadRoundtrip) {
    std::string p = tmpPath("rw.txt");
    std::remove(p.c_str());

    void *hw = __ry_io_file_open(ryStr(p.c_str()), ryStr("w"));
    ASSERT_NE(hw, nullptr);
    EXPECT_EQ(__ry_io_file_write_text(hw, ryStr("hello")), 0);
    __ry_io_file_close(hw);
    arc_free(hw);

    void *hr = __ry_io_file_open(ryStr(p.c_str()), ryStr("r"));
    ASSERT_NE(hr, nullptr);
    const char *content = __ry_io_file_read_all(hr);
    ASSERT_NE(content, nullptr);
    EXPECT_EQ(std::strncmp(content, "hello", 5), 0);
    __ry_io_file_close(hr);
    arc_free(hr);

    std::remove(p.c_str());
}

TEST(RuntimeIoFileTest, ReadLineEofOnEmptyFile) {
    std::string p = tmpPath("eof.txt");
    FILE *f = fopen(p.c_str(), "w");
    ASSERT_NE(f, nullptr);
    fclose(f);

    void *h = __ry_io_file_open(ryStr(p.c_str()), ryStr("r"));
    ASSERT_NE(h, nullptr);
    const char *line = nullptr;
    int64_t status = __ry_io_file_read_line(h, &line);
    EXPECT_EQ(status, 1);  // EOF
    EXPECT_EQ(line, nullptr);
    __ry_io_file_close(h);
    arc_free(h);
    std::remove(p.c_str());
}

TEST(RuntimeIoFileTest, ReadLineReturnsLines) {
    std::string p = tmpPath("lines.txt");
    FILE *f = fopen(p.c_str(), "w");
    ASSERT_NE(f, nullptr);
    fputs("alpha\nbeta\n", f);
    fclose(f);

    void *h = __ry_io_file_open(ryStr(p.c_str()), ryStr("r"));
    ASSERT_NE(h, nullptr);

    const char *line1 = nullptr;
    EXPECT_EQ(__ry_io_file_read_line(h, &line1), 0);
    ASSERT_NE(line1, nullptr);
    EXPECT_EQ(std::strncmp(line1, "alpha", 5), 0);

    const char *line2 = nullptr;
    EXPECT_EQ(__ry_io_file_read_line(h, &line2), 0);
    ASSERT_NE(line2, nullptr);
    EXPECT_EQ(std::strncmp(line2, "beta", 4), 0);

    const char *eof_line = nullptr;
    EXPECT_EQ(__ry_io_file_read_line(h, &eof_line), 1);  // EOF
    EXPECT_EQ(eof_line, nullptr);

    __ry_io_file_close(h);
    arc_free(h);
    std::remove(p.c_str());
}

TEST(RuntimeIoFileTest, DoubleCleanupIsSafe) {
    std::string p = tmpPath("dblclose.txt");
    FILE *f = fopen(p.c_str(), "w");
    ASSERT_NE(f, nullptr);
    fclose(f);

    void *h = __ry_io_file_open(ryStr(p.c_str()), ryStr("r"));
    ASSERT_NE(h, nullptr);
    __ry_io_file_cleanup(h);
    __ry_io_file_cleanup(h);  // second call must be a no-op
    arc_free(h);
    std::remove(p.c_str());
}
