#include <gtest/gtest.h>
#include <cstdlib>
#include <cstring>
#include <string>
#include <sys/socket.h>
#include <unistd.h>

extern "C" {
int64_t __ry_http_parse_content_length(const char *value);
const char *__ry_http_reason_phrase(int64_t status);
void *__ry_http_read_request(void *stream);
const char *__ry_http_method(void *req);
const char *__ry_http_path(void *req);
const char *__ry_http_body(void *req);
const char *__ry_http_query(void *req, const char *key);
void *__ry_http_query_all(void *req);
const char *__ry_http_cookie(void *req, const char *name);
void *__ry_http_cookies(void *req);
void __ry_http_request_free(void *req);
}

// TcpStreamHandle layout must match runtime_http.cpp
struct TcpStreamHandle {
    int fd;
};

// --- Unit tests for __ry_http_reason_phrase ---

TEST(RuntimeHttp, ReasonPhrase1xx) {
    EXPECT_STREQ(__ry_http_reason_phrase(100), "Continue");
    EXPECT_STREQ(__ry_http_reason_phrase(101), "Switching Protocols");
}

TEST(RuntimeHttp, ReasonPhrase2xx) {
    EXPECT_STREQ(__ry_http_reason_phrase(200), "OK");
    EXPECT_STREQ(__ry_http_reason_phrase(201), "Created");
    EXPECT_STREQ(__ry_http_reason_phrase(202), "Accepted");
    EXPECT_STREQ(__ry_http_reason_phrase(203), "Non-Authoritative Information");
    EXPECT_STREQ(__ry_http_reason_phrase(204), "No Content");
    EXPECT_STREQ(__ry_http_reason_phrase(205), "Reset Content");
    EXPECT_STREQ(__ry_http_reason_phrase(206), "Partial Content");
}

TEST(RuntimeHttp, ReasonPhrase3xx) {
    EXPECT_STREQ(__ry_http_reason_phrase(300), "Multiple Choices");
    EXPECT_STREQ(__ry_http_reason_phrase(301), "Moved Permanently");
    EXPECT_STREQ(__ry_http_reason_phrase(302), "Found");
    EXPECT_STREQ(__ry_http_reason_phrase(303), "See Other");
    EXPECT_STREQ(__ry_http_reason_phrase(304), "Not Modified");
    EXPECT_STREQ(__ry_http_reason_phrase(307), "Temporary Redirect");
    EXPECT_STREQ(__ry_http_reason_phrase(308), "Permanent Redirect");
}

TEST(RuntimeHttp, ReasonPhrase4xx) {
    EXPECT_STREQ(__ry_http_reason_phrase(400), "Bad Request");
    EXPECT_STREQ(__ry_http_reason_phrase(401), "Unauthorized");
    EXPECT_STREQ(__ry_http_reason_phrase(402), "Payment Required");
    EXPECT_STREQ(__ry_http_reason_phrase(403), "Forbidden");
    EXPECT_STREQ(__ry_http_reason_phrase(404), "Not Found");
    EXPECT_STREQ(__ry_http_reason_phrase(405), "Method Not Allowed");
    EXPECT_STREQ(__ry_http_reason_phrase(406), "Not Acceptable");
    EXPECT_STREQ(__ry_http_reason_phrase(407), "Proxy Authentication Required");
    EXPECT_STREQ(__ry_http_reason_phrase(408), "Request Timeout");
    EXPECT_STREQ(__ry_http_reason_phrase(409), "Conflict");
    EXPECT_STREQ(__ry_http_reason_phrase(410), "Gone");
    EXPECT_STREQ(__ry_http_reason_phrase(411), "Length Required");
    EXPECT_STREQ(__ry_http_reason_phrase(412), "Precondition Failed");
    EXPECT_STREQ(__ry_http_reason_phrase(413), "Content Too Large");
    EXPECT_STREQ(__ry_http_reason_phrase(414), "URI Too Long");
    EXPECT_STREQ(__ry_http_reason_phrase(415), "Unsupported Media Type");
    EXPECT_STREQ(__ry_http_reason_phrase(416), "Range Not Satisfiable");
    EXPECT_STREQ(__ry_http_reason_phrase(417), "Expectation Failed");
    EXPECT_STREQ(__ry_http_reason_phrase(418), "I'm a teapot");
    EXPECT_STREQ(__ry_http_reason_phrase(421), "Misdirected Request");
    EXPECT_STREQ(__ry_http_reason_phrase(422), "Unprocessable Content");
    EXPECT_STREQ(__ry_http_reason_phrase(425), "Too Early");
    EXPECT_STREQ(__ry_http_reason_phrase(426), "Upgrade Required");
    EXPECT_STREQ(__ry_http_reason_phrase(428), "Precondition Required");
    EXPECT_STREQ(__ry_http_reason_phrase(429), "Too Many Requests");
    EXPECT_STREQ(__ry_http_reason_phrase(431), "Request Header Fields Too Large");
    EXPECT_STREQ(__ry_http_reason_phrase(451), "Unavailable For Legal Reasons");
}

TEST(RuntimeHttp, ReasonPhrase5xx) {
    EXPECT_STREQ(__ry_http_reason_phrase(500), "Internal Server Error");
    EXPECT_STREQ(__ry_http_reason_phrase(501), "Not Implemented");
    EXPECT_STREQ(__ry_http_reason_phrase(502), "Bad Gateway");
    EXPECT_STREQ(__ry_http_reason_phrase(503), "Service Unavailable");
    EXPECT_STREQ(__ry_http_reason_phrase(504), "Gateway Timeout");
    EXPECT_STREQ(__ry_http_reason_phrase(505), "HTTP Version Not Supported");
    EXPECT_STREQ(__ry_http_reason_phrase(507), "Insufficient Storage");
    EXPECT_STREQ(__ry_http_reason_phrase(508), "Loop Detected");
    EXPECT_STREQ(__ry_http_reason_phrase(511), "Network Authentication Required");
}

TEST(RuntimeHttp, ReasonPhraseUnknown) {
    EXPECT_STREQ(__ry_http_reason_phrase(0), "Unknown");
    EXPECT_STREQ(__ry_http_reason_phrase(999), "Unknown");
    EXPECT_STREQ(__ry_http_reason_phrase(299), "Unknown");
}

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

// --- Query parameter tests ---

TEST(RuntimeHttp, QueryParamsBasic) {
    int fds[2];
    ASSERT_EQ(socketpair(AF_UNIX, SOCK_STREAM, 0, fds), 0);

    std::string req = "GET /search?q=hello&page=2 HTTP/1.1\r\nHost: localhost\r\n\r\n";
    send_and_close(fds[1], req);

    auto *handle = (TcpStreamHandle *)malloc(sizeof(TcpStreamHandle));
    handle->fd = fds[0];
    void *result = __ry_http_read_request(handle);
    ASSERT_NE(result, nullptr);
    EXPECT_STREQ(__ry_http_path(result), "/search");
    EXPECT_STREQ(__ry_http_query(result, "q"), "hello");
    EXPECT_STREQ(__ry_http_query(result, "page"), "2");
    EXPECT_EQ(__ry_http_query(result, "missing"), nullptr);
    __ry_http_request_free(result);
    ::close(fds[0]);
    free(handle);
}

TEST(RuntimeHttp, QueryParamsNone) {
    int fds[2];
    ASSERT_EQ(socketpair(AF_UNIX, SOCK_STREAM, 0, fds), 0);

    std::string req = "GET /hello HTTP/1.1\r\nHost: localhost\r\n\r\n";
    send_and_close(fds[1], req);

    auto *handle = (TcpStreamHandle *)malloc(sizeof(TcpStreamHandle));
    handle->fd = fds[0];
    void *result = __ry_http_read_request(handle);
    ASSERT_NE(result, nullptr);
    EXPECT_STREQ(__ry_http_path(result), "/hello");
    EXPECT_EQ(__ry_http_query(result, "anything"), nullptr);
    __ry_http_request_free(result);
    ::close(fds[0]);
    free(handle);
}

TEST(RuntimeHttp, QueryParamsEmpty) {
    int fds[2];
    ASSERT_EQ(socketpair(AF_UNIX, SOCK_STREAM, 0, fds), 0);

    std::string req = "GET /path? HTTP/1.1\r\nHost: localhost\r\n\r\n";
    send_and_close(fds[1], req);

    auto *handle = (TcpStreamHandle *)malloc(sizeof(TcpStreamHandle));
    handle->fd = fds[0];
    void *result = __ry_http_read_request(handle);
    ASSERT_NE(result, nullptr);
    EXPECT_STREQ(__ry_http_path(result), "/path");
    EXPECT_EQ(__ry_http_query(result, "anything"), nullptr);
    __ry_http_request_free(result);
    ::close(fds[0]);
    free(handle);
}

TEST(RuntimeHttp, QueryParamsUrlDecode) {
    int fds[2];
    ASSERT_EQ(socketpair(AF_UNIX, SOCK_STREAM, 0, fds), 0);

    std::string req = "GET /search?q=hello%20world&tag=c%2B%2B HTTP/1.1\r\nHost: localhost\r\n\r\n";
    send_and_close(fds[1], req);

    auto *handle = (TcpStreamHandle *)malloc(sizeof(TcpStreamHandle));
    handle->fd = fds[0];
    void *result = __ry_http_read_request(handle);
    ASSERT_NE(result, nullptr);
    EXPECT_STREQ(__ry_http_query(result, "q"), "hello world");
    EXPECT_STREQ(__ry_http_query(result, "tag"), "c++");
    __ry_http_request_free(result);
    ::close(fds[0]);
    free(handle);
}

TEST(RuntimeHttp, QueryParamsPlusAsSpace) {
    int fds[2];
    ASSERT_EQ(socketpair(AF_UNIX, SOCK_STREAM, 0, fds), 0);

    std::string req = "GET /search?q=hello+world HTTP/1.1\r\nHost: localhost\r\n\r\n";
    send_and_close(fds[1], req);

    auto *handle = (TcpStreamHandle *)malloc(sizeof(TcpStreamHandle));
    handle->fd = fds[0];
    void *result = __ry_http_read_request(handle);
    ASSERT_NE(result, nullptr);
    EXPECT_STREQ(__ry_http_query(result, "q"), "hello world");
    __ry_http_request_free(result);
    ::close(fds[0]);
    free(handle);
}

TEST(RuntimeHttp, QueryParamsNoValue) {
    int fds[2];
    ASSERT_EQ(socketpair(AF_UNIX, SOCK_STREAM, 0, fds), 0);

    std::string req = "GET /path?flag&key= HTTP/1.1\r\nHost: localhost\r\n\r\n";
    send_and_close(fds[1], req);

    auto *handle = (TcpStreamHandle *)malloc(sizeof(TcpStreamHandle));
    handle->fd = fds[0];
    void *result = __ry_http_read_request(handle);
    ASSERT_NE(result, nullptr);
    EXPECT_STREQ(__ry_http_query(result, "flag"), "");
    EXPECT_STREQ(__ry_http_query(result, "key"), "");
    __ry_http_request_free(result);
    ::close(fds[0]);
    free(handle);
}

TEST(RuntimeHttp, QueryParamsDuplicateFirstWins) {
    int fds[2];
    ASSERT_EQ(socketpair(AF_UNIX, SOCK_STREAM, 0, fds), 0);

    std::string req = "GET /path?a=1&a=2 HTTP/1.1\r\nHost: localhost\r\n\r\n";
    send_and_close(fds[1], req);

    auto *handle = (TcpStreamHandle *)malloc(sizeof(TcpStreamHandle));
    handle->fd = fds[0];
    void *result = __ry_http_read_request(handle);
    ASSERT_NE(result, nullptr);
    EXPECT_STREQ(__ry_http_query(result, "a"), "1");
    __ry_http_request_free(result);
    ::close(fds[0]);
    free(handle);
}

// MapHeader layout must match codegen
struct MapHeader {
    int64_t len;
    int64_t cap;
    char **keys;
    char **vals;
    int64_t bucket_count;
    void *buckets;
};

TEST(RuntimeHttp, QueryAllBasic) {
    int fds[2];
    ASSERT_EQ(socketpair(AF_UNIX, SOCK_STREAM, 0, fds), 0);

    std::string req = "GET /search?q=hello%20world&page=2 HTTP/1.1\r\nHost: localhost\r\n\r\n";
    send_and_close(fds[1], req);

    auto *handle = (TcpStreamHandle *)malloc(sizeof(TcpStreamHandle));
    handle->fd = fds[0];
    void *result = __ry_http_read_request(handle);
    ASSERT_NE(result, nullptr);

    auto *map = (MapHeader *)__ry_http_query_all(result);
    ASSERT_NE(map, nullptr);
    EXPECT_EQ(map->len, 2);
    EXPECT_GE(map->bucket_count, 4);
    ASSERT_NE(map->keys, nullptr);
    ASSERT_NE(map->vals, nullptr);

    // Verify decoded key/value pairs exist (order matches insertion)
    EXPECT_STREQ(map->keys[0], "q");
    EXPECT_STREQ(map->vals[0], "hello world");
    EXPECT_STREQ(map->keys[1], "page");
    EXPECT_STREQ(map->vals[1], "2");

    // Cleanup map
    for (int64_t i = 0; i < map->len; i++) {
        free(map->keys[i]);
        free(map->vals[i]);
    }
    free(map->keys);
    free(map->vals);
    free(map->buckets);
    free(map);

    __ry_http_request_free(result);
    ::close(fds[0]);
    free(handle);
}

TEST(RuntimeHttp, QueryAllEmpty) {
    int fds[2];
    ASSERT_EQ(socketpair(AF_UNIX, SOCK_STREAM, 0, fds), 0);

    std::string req = "GET /hello HTTP/1.1\r\nHost: localhost\r\n\r\n";
    send_and_close(fds[1], req);

    auto *handle = (TcpStreamHandle *)malloc(sizeof(TcpStreamHandle));
    handle->fd = fds[0];
    void *result = __ry_http_read_request(handle);
    ASSERT_NE(result, nullptr);

    auto *map = (MapHeader *)__ry_http_query_all(result);
    ASSERT_NE(map, nullptr);
    EXPECT_EQ(map->len, 0);
    EXPECT_GE(map->bucket_count, 4);

    free(map->buckets);
    free(map);

    __ry_http_request_free(result);
    ::close(fds[0]);
    free(handle);
}

TEST(RuntimeHttp, QueryAllDuplicateFirstWins) {
    int fds[2];
    ASSERT_EQ(socketpair(AF_UNIX, SOCK_STREAM, 0, fds), 0);

    std::string req = "GET /path?a=1&b=2&a=3 HTTP/1.1\r\nHost: localhost\r\n\r\n";
    send_and_close(fds[1], req);

    auto *handle = (TcpStreamHandle *)malloc(sizeof(TcpStreamHandle));
    handle->fd = fds[0];
    void *result = __ry_http_read_request(handle);
    ASSERT_NE(result, nullptr);

    auto *map = (MapHeader *)__ry_http_query_all(result);
    ASSERT_NE(map, nullptr);
    EXPECT_EQ(map->len, 2);
    EXPECT_STREQ(map->keys[0], "a");
    EXPECT_STREQ(map->vals[0], "1");
    EXPECT_STREQ(map->keys[1], "b");
    EXPECT_STREQ(map->vals[1], "2");

    for (int64_t i = 0; i < map->len; i++) {
        free(map->keys[i]);
        free(map->vals[i]);
    }
    free(map->keys);
    free(map->vals);
    free(map->buckets);
    free(map);

    __ry_http_request_free(result);
    ::close(fds[0]);
    free(handle);
}

// --- Cookie tests ---

TEST(RuntimeHttp, CookieBasic) {
    int fds[2];
    ASSERT_EQ(socketpair(AF_UNIX, SOCK_STREAM, 0, fds), 0);

    std::string req = "GET / HTTP/1.1\r\nHost: localhost\r\nCookie: session_id=abc123; theme=dark\r\n\r\n";
    send_and_close(fds[1], req);

    auto *handle = (TcpStreamHandle *)malloc(sizeof(TcpStreamHandle));
    handle->fd = fds[0];
    void *result = __ry_http_read_request(handle);
    ASSERT_NE(result, nullptr);
    EXPECT_STREQ(__ry_http_cookie(result, "session_id"), "abc123");
    EXPECT_STREQ(__ry_http_cookie(result, "theme"), "dark");
    __ry_http_request_free(result);
    ::close(fds[0]);
    free(handle);
}

TEST(RuntimeHttp, CookieNotFound) {
    int fds[2];
    ASSERT_EQ(socketpair(AF_UNIX, SOCK_STREAM, 0, fds), 0);

    std::string req = "GET / HTTP/1.1\r\nHost: localhost\r\nCookie: session_id=abc123\r\n\r\n";
    send_and_close(fds[1], req);

    auto *handle = (TcpStreamHandle *)malloc(sizeof(TcpStreamHandle));
    handle->fd = fds[0];
    void *result = __ry_http_read_request(handle);
    ASSERT_NE(result, nullptr);
    EXPECT_EQ(__ry_http_cookie(result, "nonexistent"), nullptr);
    __ry_http_request_free(result);
    ::close(fds[0]);
    free(handle);
}

TEST(RuntimeHttp, CookieNoCookieHeader) {
    int fds[2];
    ASSERT_EQ(socketpair(AF_UNIX, SOCK_STREAM, 0, fds), 0);

    std::string req = "GET / HTTP/1.1\r\nHost: localhost\r\n\r\n";
    send_and_close(fds[1], req);

    auto *handle = (TcpStreamHandle *)malloc(sizeof(TcpStreamHandle));
    handle->fd = fds[0];
    void *result = __ry_http_read_request(handle);
    ASSERT_NE(result, nullptr);
    EXPECT_EQ(__ry_http_cookie(result, "session_id"), nullptr);
    __ry_http_request_free(result);
    ::close(fds[0]);
    free(handle);
}

TEST(RuntimeHttp, CookieValueWithEquals) {
    int fds[2];
    ASSERT_EQ(socketpair(AF_UNIX, SOCK_STREAM, 0, fds), 0);

    std::string req = "GET / HTTP/1.1\r\nHost: localhost\r\nCookie: token=abc=def=ghi\r\n\r\n";
    send_and_close(fds[1], req);

    auto *handle = (TcpStreamHandle *)malloc(sizeof(TcpStreamHandle));
    handle->fd = fds[0];
    void *result = __ry_http_read_request(handle);
    ASSERT_NE(result, nullptr);
    EXPECT_STREQ(__ry_http_cookie(result, "token"), "abc=def=ghi");
    __ry_http_request_free(result);
    ::close(fds[0]);
    free(handle);
}

TEST(RuntimeHttp, CookiesAll) {
    int fds[2];
    ASSERT_EQ(socketpair(AF_UNIX, SOCK_STREAM, 0, fds), 0);

    std::string req = "GET / HTTP/1.1\r\nHost: localhost\r\nCookie: a=1; b=2; c=3\r\n\r\n";
    send_and_close(fds[1], req);

    auto *handle = (TcpStreamHandle *)malloc(sizeof(TcpStreamHandle));
    handle->fd = fds[0];
    void *result = __ry_http_read_request(handle);
    ASSERT_NE(result, nullptr);

    auto *map = (MapHeader *)__ry_http_cookies(result);
    ASSERT_NE(map, nullptr);
    EXPECT_EQ(map->len, 3);
    EXPECT_GE(map->bucket_count, 4);
    ASSERT_NE(map->keys, nullptr);
    ASSERT_NE(map->vals, nullptr);
    EXPECT_STREQ(map->keys[0], "a");
    EXPECT_STREQ(map->vals[0], "1");
    EXPECT_STREQ(map->keys[1], "b");
    EXPECT_STREQ(map->vals[1], "2");
    EXPECT_STREQ(map->keys[2], "c");
    EXPECT_STREQ(map->vals[2], "3");

    for (int64_t i = 0; i < map->len; i++) {
        free(map->keys[i]);
        free(map->vals[i]);
    }
    free(map->keys);
    free(map->vals);
    free(map->buckets);
    free(map);

    __ry_http_request_free(result);
    ::close(fds[0]);
    free(handle);
}

TEST(RuntimeHttp, CookiesAllEmpty) {
    int fds[2];
    ASSERT_EQ(socketpair(AF_UNIX, SOCK_STREAM, 0, fds), 0);

    std::string req = "GET / HTTP/1.1\r\nHost: localhost\r\n\r\n";
    send_and_close(fds[1], req);

    auto *handle = (TcpStreamHandle *)malloc(sizeof(TcpStreamHandle));
    handle->fd = fds[0];
    void *result = __ry_http_read_request(handle);
    ASSERT_NE(result, nullptr);

    auto *map = (MapHeader *)__ry_http_cookies(result);
    ASSERT_NE(map, nullptr);
    EXPECT_EQ(map->len, 0);
    EXPECT_GE(map->bucket_count, 4);

    free(map->buckets);
    free(map);

    __ry_http_request_free(result);
    ::close(fds[0]);
    free(handle);
}

TEST(RuntimeHttp, CookiesAllDuplicateFirstWins) {
    int fds[2];
    ASSERT_EQ(socketpair(AF_UNIX, SOCK_STREAM, 0, fds), 0);

    std::string req = "GET / HTTP/1.1\r\nHost: localhost\r\nCookie: a=1; b=2; a=3\r\n\r\n";
    send_and_close(fds[1], req);

    auto *handle = (TcpStreamHandle *)malloc(sizeof(TcpStreamHandle));
    handle->fd = fds[0];
    void *result = __ry_http_read_request(handle);
    ASSERT_NE(result, nullptr);

    auto *map = (MapHeader *)__ry_http_cookies(result);
    ASSERT_NE(map, nullptr);
    EXPECT_EQ(map->len, 2);
    EXPECT_STREQ(map->keys[0], "a");
    EXPECT_STREQ(map->vals[0], "1");
    EXPECT_STREQ(map->keys[1], "b");
    EXPECT_STREQ(map->vals[1], "2");

    for (int64_t i = 0; i < map->len; i++) {
        free(map->keys[i]);
        free(map->vals[i]);
    }
    free(map->keys);
    free(map->vals);
    free(map->buckets);
    free(map);

    __ry_http_request_free(result);
    ::close(fds[0]);
    free(handle);
}
