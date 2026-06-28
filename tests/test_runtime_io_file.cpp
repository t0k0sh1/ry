// Test taxonomy: docs/reference/test-taxonomy.md
// Section header tags: [contract] / [regression #NNNN] / [internal].
// Per-test exceptions use an inline `// [regression: #NNNN]` comment.
// Whole-file dominant tag: [internal] — drives __ry_io_file_* runtime entry
// points directly.

#include "ry/runtime/native/io.hpp"
#include "ry/runtime/core/string.hpp"
#include "ry/runtime/core/arc.hpp"
#include <gtest/gtest.h>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <unistd.h>

using namespace ry;

extern "C" {
void *__ry_io_file_open(const char *path, const char *mode);
const char *__ry_io_file_read_all(void *handle);
int64_t __ry_io_file_read_line(void *handle, const char **out_line);
int64_t __ry_io_file_write_text(void *handle, const char *s);
void __ry_io_file_close(void *handle);
void __ry_io_file_cleanup(void *handle);
const char *__ry_io_get_last_error();
}

// __ry_io_file_open / __ry_io_file_write_text expect Ry string handles
// (StringHeader-backed pointers from makeString), not raw C strings.
static const char *ryStr(const char *s) {
    return makeString(s, strlen(s));
}

// Release a Ry str handle produced by makeString. The test harness has no
// ARC codegen wrapping these, so the StringHeader slot must be released
// manually to keep the test memory-clean under ASan.
static void freeRyStr(const char *s) {
    if (s) freeStringSlot(const_cast<char *>(s));
}

static std::string tmpPath(const char *suffix) {
    return std::string("/tmp/ry_rt_io_file_") + suffix;
}

TEST(RuntimeIoFileTest, OpenNullPathReturnsNull) {
    const char *mode = ryStr("r");
    void *h = __ry_io_file_open(nullptr, mode);
    EXPECT_EQ(h, nullptr);
    freeRyStr(mode);
}

TEST(RuntimeIoFileTest, OpenNonexistentPathReturnsNull) {
    const char *path = ryStr("/tmp/ry_rt_io_no_such_file_xyzzy.txt");
    const char *mode = ryStr("r");
    void *h = __ry_io_file_open(path, mode);
    EXPECT_EQ(h, nullptr);
    const char *err = __ry_io_get_last_error();
    EXPECT_NE(err, nullptr);
    EXPECT_NE(std::strstr(err, "open"), nullptr);
    freeRyStr(err);
    freeRyStr(path);
    freeRyStr(mode);
}

TEST(RuntimeIoFileTest, OpenInvalidModeReturnsNull) {
    std::string p = tmpPath("badmode.txt");
    FILE *f = fopen(p.c_str(), "w");
    if (f) fclose(f);
    const char *path = ryStr(p.c_str());
    const char *mode = ryStr("z");
    void *h = __ry_io_file_open(path, mode);
    EXPECT_EQ(h, nullptr);
    freeRyStr(path);
    freeRyStr(mode);
    std::remove(p.c_str());
}

TEST(RuntimeIoFileTest, OpenWriteReadRoundtrip) {
    std::string p = tmpPath("rw.txt");
    std::remove(p.c_str());

    const char *path_w = ryStr(p.c_str());
    const char *mode_w = ryStr("w");
    void *hw = __ry_io_file_open(path_w, mode_w);
    ASSERT_NE(hw, nullptr);
    const char *payload = ryStr("hello");
    EXPECT_EQ(__ry_io_file_write_text(hw, payload), 0);
    __ry_io_file_close(hw);
    arc_free(hw);
    freeRyStr(path_w);
    freeRyStr(mode_w);
    freeRyStr(payload);

    const char *path_r = ryStr(p.c_str());
    const char *mode_r = ryStr("r");
    void *hr = __ry_io_file_open(path_r, mode_r);
    ASSERT_NE(hr, nullptr);
    const char *content = __ry_io_file_read_all(hr);
    ASSERT_NE(content, nullptr);
    EXPECT_STREQ(content, "hello");
    __ry_io_file_close(hr);
    arc_free(hr);
    freeRyStr(path_r);
    freeRyStr(mode_r);
    freeRyStr(content);

    std::remove(p.c_str());
}

TEST(RuntimeIoFileTest, ReadLineEofOnEmptyFile) {
    std::string p = tmpPath("eof.txt");
    FILE *f = fopen(p.c_str(), "w");
    ASSERT_NE(f, nullptr);
    fclose(f);

    const char *path = ryStr(p.c_str());
    const char *mode = ryStr("r");
    void *h = __ry_io_file_open(path, mode);
    ASSERT_NE(h, nullptr);
    const char *line = nullptr;
    int64_t status = __ry_io_file_read_line(h, &line);
    EXPECT_EQ(status, 1);  // EOF
    EXPECT_EQ(line, nullptr);
    __ry_io_file_close(h);
    arc_free(h);
    freeRyStr(path);
    freeRyStr(mode);
    std::remove(p.c_str());
}

TEST(RuntimeIoFileTest, ReadLineReturnsLines) {
    std::string p = tmpPath("lines.txt");
    FILE *f = fopen(p.c_str(), "w");
    ASSERT_NE(f, nullptr);
    fputs("alpha\nbeta\n", f);
    fclose(f);

    const char *path = ryStr(p.c_str());
    const char *mode = ryStr("r");
    void *h = __ry_io_file_open(path, mode);
    ASSERT_NE(h, nullptr);

    const char *line1 = nullptr;
    EXPECT_EQ(__ry_io_file_read_line(h, &line1), 0);
    ASSERT_NE(line1, nullptr);
    EXPECT_STREQ(line1, "alpha");

    const char *line2 = nullptr;
    EXPECT_EQ(__ry_io_file_read_line(h, &line2), 0);
    ASSERT_NE(line2, nullptr);
    EXPECT_STREQ(line2, "beta");

    const char *eof_line = nullptr;
    EXPECT_EQ(__ry_io_file_read_line(h, &eof_line), 1);  // EOF
    EXPECT_EQ(eof_line, nullptr);

    __ry_io_file_close(h);
    arc_free(h);
    freeRyStr(path);
    freeRyStr(mode);
    freeRyStr(line1);
    freeRyStr(line2);
    std::remove(p.c_str());
}

TEST(RuntimeIoFileTest, DoubleCleanupIsSafe) {
    std::string p = tmpPath("dblclose.txt");
    FILE *f = fopen(p.c_str(), "w");
    ASSERT_NE(f, nullptr);
    fclose(f);

    const char *path = ryStr(p.c_str());
    const char *mode = ryStr("r");
    void *h = __ry_io_file_open(path, mode);
    ASSERT_NE(h, nullptr);
    __ry_io_file_cleanup(h);
    __ry_io_file_cleanup(h);  // second call must be a no-op
    arc_free(h);
    freeRyStr(path);
    freeRyStr(mode);
    std::remove(p.c_str());
}

// [regression: #2480] Dual-write error channel — io owns BOTH a module-
// local `io_last_error_buf` (read via __ry_io_get_last_error) AND mirrors
// every error into the host-shared `__ry_set_last_error` buffer (read via
// __ry_get_last_error) so cross-module callers like json::load_file /
// json5::dump_file surface io failures through their own
// __ry_get_last_error reads. After the #2480 Rust port the dual-write
// happens in `crates/native_io/src/lib.rs::set_last_error` instead of the
// deleted io.cpp's `setLastError` helper — this test pins the contract.
TEST(RuntimeIoFileTest, OpenErrorPopulatesBothErrorBuffers) {
    const char *path = ryStr("/tmp/ry_rt_io_dualwrite_xyzzy.txt");
    const char *mode = ryStr("r");
    void *h = __ry_io_file_open(path, mode);
    EXPECT_EQ(h, nullptr);

    const char *io_err = __ry_io_get_last_error();
    const char *shared_err = __ry_get_last_error();
    ASSERT_NE(io_err, nullptr);
    ASSERT_NE(shared_err, nullptr);
    // Both channels must hold a non-empty message after the failed open.
    EXPECT_GT(std::strlen(io_err), 0u);
    EXPECT_GT(std::strlen(shared_err), 0u);
    // Both messages must mention the failing path/mode tuple this call
    // formatted (matches `__ry_io_file_open`'s err-arm wording: "open:
    // cannot open '<path>' in mode '<mode>'"). Asserting on the unique
    // file basename rather than just the word "open" rules out a
    // false-pass when an earlier test left an "open" error in either
    // thread-local buffer.
    EXPECT_NE(std::strstr(io_err, "ry_rt_io_dualwrite_xyzzy.txt"), nullptr);
    EXPECT_NE(std::strstr(shared_err, "ry_rt_io_dualwrite_xyzzy.txt"), nullptr);

    freeRyStr(io_err);
    freeRyStr(shared_err);
    freeRyStr(path);
    freeRyStr(mode);
}

// [regression: #2480] O_NOFOLLOW posture — `open_nofollow` (io.cpp:41-54
// in C++, `core::open_nofollow` in Rust) sets O_NOFOLLOW on every open
// path so dangling symlinks and symlinks pointing at sensitive files
// can't be followed by stdlib callers. The Rust port forwards the same
// flag via `libc::O_NOFOLLOW`; this test confirms the behaviour
// survives the abi layer.
TEST(RuntimeIoFileTest, OpenRejectsSymlinkViaONoFollow) {
    std::string target = tmpPath("nofollow_target.txt");
    std::string link = tmpPath("nofollow_link.txt");
    std::remove(target.c_str());
    std::remove(link.c_str());

    // Create the real file with known content.
    FILE *f = fopen(target.c_str(), "w");
    ASSERT_NE(f, nullptr);
    fputs("real-content", f);
    fclose(f);

    // Create the symlink pointing at it; if symlink(2) fails (no
    // permission on the temp dir, exotic FS), skip rather than fail —
    // there is nothing useful to verify.
    if (symlink(target.c_str(), link.c_str()) != 0) {
        std::remove(target.c_str());
        GTEST_SKIP() << "symlink(2) unavailable; cannot verify O_NOFOLLOW";
    }

    const char *path = ryStr(link.c_str());
    const char *mode = ryStr("r");
    void *h = __ry_io_file_open(path, mode);
    EXPECT_EQ(h, nullptr);  // open must refuse to follow the symlink
    if (h != nullptr) {
        __ry_io_file_close(h);
        arc_free(h);
    }

    freeRyStr(path);
    freeRyStr(mode);
    std::remove(link.c_str());
    std::remove(target.c_str());
}
