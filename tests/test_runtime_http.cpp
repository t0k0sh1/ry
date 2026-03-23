#include <gtest/gtest.h>
#include <cstdlib>
#include <cstring>
#include <string>
#include <sys/socket.h>
#include <unistd.h>

extern "C" {
int64_t __ry_http_parse_content_length(const char *value);
void *__ry_http_read_request(void *stream);
const char *__ry_http_method(void *req);
const char *__ry_http_path(void *req);
const char *__ry_http_body(void *req);
void __ry_http_request_free(void *req);
}

// TcpStreamHandle layout must match runtime_http.cpp
struct TcpStreamHandle {
    int fd;
};

// --- Unit tests for __ry_http_parse_content_length ---

TEST(RuntimeHttp, ParseContentLengthValid) {
    EXPECT_EQ(__ry_http_parse_content_length("0"), 0);
    EXPECT_EQ(__ry_http_parse_content_length("42"), 42);
    EXPECT_EQ(__ry_http_parse_content_length("1024"), 1024);
}

TEST(RuntimeHttp, ParseContentLengthNull) {
    EXPECT_EQ(__ry_http_parse_content_length(nullptr), -1);
}

TEST(RuntimeHttp, ParseContentLengthInvalid) {
    EXPECT_EQ(__ry_http_parse_content_length("abc"), -2);
    EXPECT_EQ(__ry_http_parse_content_length("-1"), -2);
    EXPECT_EQ(__ry_http_parse_content_length("12abc"), -2);
    EXPECT_EQ(__ry_http_parse_content_length(""), -2);
}

TEST(RuntimeHttp, ParseContentLengthExceedsMax) {
    EXPECT_EQ(__ry_http_parse_content_length("10485761"), -3);
    EXPECT_EQ(__ry_http_parse_content_length("999999999999"), -3);
}

TEST(RuntimeHttp, ParseContentLengthBoundary) {
    // Exactly MAX_BODY_SIZE (10 MB) should be valid
    EXPECT_EQ(__ry_http_parse_content_length("10485760"), 10485760);
}

TEST(RuntimeHttp, ParseContentLengthWhitespace) {
    EXPECT_EQ(__ry_http_parse_content_length(" 42"), 42);
    EXPECT_EQ(__ry_http_parse_content_length("42 "), 42);
    EXPECT_EQ(__ry_http_parse_content_length("  100  "), 100);
}

// --- socketpair integration tests for __ry_http_read_request ---

// Helper: write data to fd and close the write end
static void send_and_close(int fd, const std::string &data) {
    size_t sent = 0;
    while (sent < data.size()) {
        ssize_t n = ::write(fd, data.c_str() + sent, data.size() - sent);
        if (n <= 0) break;
        sent += (size_t)n;
    }
    ::close(fd);
}

TEST(RuntimeHttp, ReadRequestValidGet) {
    int fds[2];
    ASSERT_EQ(socketpair(AF_UNIX, SOCK_STREAM, 0, fds), 0);

    std::string req = "GET /hello HTTP/1.1\r\nHost: localhost\r\n\r\n";
    send_and_close(fds[1], req);

    auto *handle = (TcpStreamHandle *)malloc(sizeof(TcpStreamHandle));
    handle->fd = fds[0];
    void *result = __ry_http_read_request(handle);
    ASSERT_NE(result, nullptr);
    EXPECT_STREQ(__ry_http_method(result), "GET");
    EXPECT_STREQ(__ry_http_path(result), "/hello");
    EXPECT_STREQ(__ry_http_body(result), "");
    __ry_http_request_free(result);
    ::close(fds[0]);
    free(handle);
}

TEST(RuntimeHttp, ReadRequestValidPostWithBody) {
    int fds[2];
    ASSERT_EQ(socketpair(AF_UNIX, SOCK_STREAM, 0, fds), 0);

    std::string req = "POST /data HTTP/1.1\r\nContent-Length: 5\r\n\r\nhello";
    send_and_close(fds[1], req);

    auto *handle = (TcpStreamHandle *)malloc(sizeof(TcpStreamHandle));
    handle->fd = fds[0];
    void *result = __ry_http_read_request(handle);
    ASSERT_NE(result, nullptr);
    EXPECT_STREQ(__ry_http_method(result), "POST");
    EXPECT_STREQ(__ry_http_body(result), "hello");
    __ry_http_request_free(result);
    ::close(fds[0]);
    free(handle);
}

TEST(RuntimeHttp, ReadRequestBodyLongerThanContentLength) {
    int fds[2];
    ASSERT_EQ(socketpair(AF_UNIX, SOCK_STREAM, 0, fds), 0);

    // Content-Length says 5, but 10 bytes of body are sent
    std::string req =
        "POST /data HTTP/1.1\r\nContent-Length: 5\r\n\r\nhelloEXTRA";
    send_and_close(fds[1], req);

    auto *handle = (TcpStreamHandle *)malloc(sizeof(TcpStreamHandle));
    handle->fd = fds[0];
    void *result = __ry_http_read_request(handle);
    ASSERT_NE(result, nullptr);
    EXPECT_STREQ(__ry_http_method(result), "POST");
    EXPECT_STREQ(__ry_http_body(result), "hello");
    __ry_http_request_free(result);
    ::close(fds[0]);
    free(handle);
}

TEST(RuntimeHttp, ReadRequestTruncatedBody) {
    int fds[2];
    ASSERT_EQ(socketpair(AF_UNIX, SOCK_STREAM, 0, fds), 0);

    // Content-Length says 100, but only 5 bytes of body
    std::string req = "POST /data HTTP/1.1\r\nContent-Length: 100\r\n\r\nshort";
    send_and_close(fds[1], req);

    auto *handle = (TcpStreamHandle *)malloc(sizeof(TcpStreamHandle));
    handle->fd = fds[0];
    void *result = __ry_http_read_request(handle);
    EXPECT_EQ(result, nullptr);
    ::close(fds[0]);
    free(handle);
}

TEST(RuntimeHttp, ReadRequestInvalidContentLength) {
    int fds[2];
    ASSERT_EQ(socketpair(AF_UNIX, SOCK_STREAM, 0, fds), 0);

    std::string req = "POST /data HTTP/1.1\r\nContent-Length: abc\r\n\r\n";
    send_and_close(fds[1], req);

    auto *handle = (TcpStreamHandle *)malloc(sizeof(TcpStreamHandle));
    handle->fd = fds[0];
    void *result = __ry_http_read_request(handle);
    EXPECT_EQ(result, nullptr);
    ::close(fds[0]);
    free(handle);
}

TEST(RuntimeHttp, ReadRequestNegativeContentLength) {
    int fds[2];
    ASSERT_EQ(socketpair(AF_UNIX, SOCK_STREAM, 0, fds), 0);

    std::string req = "POST /data HTTP/1.1\r\nContent-Length: -1\r\n\r\n";
    send_and_close(fds[1], req);

    auto *handle = (TcpStreamHandle *)malloc(sizeof(TcpStreamHandle));
    handle->fd = fds[0];
    void *result = __ry_http_read_request(handle);
    EXPECT_EQ(result, nullptr);
    ::close(fds[0]);
    free(handle);
}

TEST(RuntimeHttp, ReadRequestOversizedContentLength) {
    int fds[2];
    ASSERT_EQ(socketpair(AF_UNIX, SOCK_STREAM, 0, fds), 0);

    std::string req = "POST /data HTTP/1.1\r\nContent-Length: 999999999999\r\n\r\n";
    send_and_close(fds[1], req);

    auto *handle = (TcpStreamHandle *)malloc(sizeof(TcpStreamHandle));
    handle->fd = fds[0];
    void *result = __ry_http_read_request(handle);
    EXPECT_EQ(result, nullptr);
    ::close(fds[0]);
    free(handle);
}

TEST(RuntimeHttp, ReadRequestPostBodySplitAcrossRecv) {
    int fds[2];
    ASSERT_EQ(socketpair(AF_UNIX, SOCK_STREAM, 0, fds), 0);

    // Send headers first, then body in a second write
    std::string headers = "POST /split HTTP/1.1\r\nContent-Length: 11\r\n\r\n";
    std::string body = "hello world";
    ::write(fds[1], headers.c_str(), headers.size());
    ::write(fds[1], body.c_str(), body.size());
    ::close(fds[1]);

    auto *handle = (TcpStreamHandle *)malloc(sizeof(TcpStreamHandle));
    handle->fd = fds[0];
    void *result = __ry_http_read_request(handle);
    ASSERT_NE(result, nullptr);
    EXPECT_STREQ(__ry_http_body(result), "hello world");
    __ry_http_request_free(result);
    ::close(fds[0]);
    free(handle);
}
