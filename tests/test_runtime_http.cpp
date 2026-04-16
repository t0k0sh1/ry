#include <gtest/gtest.h>
#include <cstdlib>
#include <cstring>
#include <string>
#include <thread>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <unistd.h>
#include "ry/runtime_net.hpp"
#include "ry/runtime_http_types.hpp"
#include "ry/runtime_io.hpp"
#include "ry/runtime_arc.hpp"


using namespace ry;
extern "C" {
const char *__ry_http_reason_phrase(int64_t status);
void *__ry_http_read_request(void *stream);
const char *__ry_http_method(void *req);
const char *__ry_http_path(void *req);
const char *__ry_http_body(void *req);
void *__ry_http_body_bytes(void *req);
const char *__ry_http_query(void *req, const char *key);
void *__ry_http_query_all(void *req);
const char *__ry_http_cookie(void *req, const char *name);
void *__ry_http_cookies(void *req);
const char *__ry_http_form_field(void *req, const char *name);
void *__ry_http_form_file(void *req, const char *name);
void *__ry_http_form_fields(void *req);
void __ry_http_request_free(void *req);
void *__ry_http_response_create(int64_t status, void *headers_map, const char *body);
void __ry_http_send_response(void *stream, void *response);
void __ry_http_response_free(void *resp);

// HTTP client functions
void *__ry_http_parse_url(const char *url);
void __ry_http_parsed_url_free(void *parsed);
void *__ry_http_client_request(const char *method, const char *url,
                                void *headers_map, const char *body);
void *__ry_http_get(const char *url);
void *__ry_http_post(const char *url, const char *body, void *headers_map);
int64_t __ry_http_client_status(void *resp);
const char *__ry_http_client_body(void *resp);
void *__ry_http_client_body_bytes(void *resp);
const char *__ry_http_client_header(void *resp, const char *key);
void __ry_http_client_response_free(void *resp);
}

// RAII guard to temporarily allow private/loopback connections for tests
// that need localhost access, restoring the original env on destruction.
class AllowPrivateHTTPGuard {
public:
    AllowPrivateHTTPGuard() {
        const char *prev = std::getenv("RY_ALLOW_PRIVATE_HTTP");
        had_prev_ = (prev != nullptr);
        if (had_prev_) prev_value_ = prev;
        ::setenv("RY_ALLOW_PRIVATE_HTTP", "1", 1);
    }
    ~AllowPrivateHTTPGuard() {
        if (had_prev_)
            ::setenv("RY_ALLOW_PRIVATE_HTTP", prev_value_.c_str(), 1);
        else
            ::unsetenv("RY_ALLOW_PRIVATE_HTTP");
    }
private:
    bool had_prev_ = false;
    std::string prev_value_;
};

// TcpStreamHandle layout must match runtime_http.cpp
struct TcpStreamHandle {
    int fd;
};

// --- Merged unit test for __ry_http_reason_phrase ---

TEST(RuntimeHttp, ReasonPhraseAll) {
    // 1xx
    EXPECT_STREQ(__ry_http_reason_phrase(100), "Continue");
    EXPECT_STREQ(__ry_http_reason_phrase(101), "Switching Protocols");

    // 2xx
    EXPECT_STREQ(__ry_http_reason_phrase(200), "OK");
    EXPECT_STREQ(__ry_http_reason_phrase(201), "Created");
    EXPECT_STREQ(__ry_http_reason_phrase(202), "Accepted");
    EXPECT_STREQ(__ry_http_reason_phrase(203), "Non-Authoritative Information");
    EXPECT_STREQ(__ry_http_reason_phrase(204), "No Content");
    EXPECT_STREQ(__ry_http_reason_phrase(205), "Reset Content");
    EXPECT_STREQ(__ry_http_reason_phrase(206), "Partial Content");

    // 3xx
    EXPECT_STREQ(__ry_http_reason_phrase(300), "Multiple Choices");
    EXPECT_STREQ(__ry_http_reason_phrase(301), "Moved Permanently");
    EXPECT_STREQ(__ry_http_reason_phrase(302), "Found");
    EXPECT_STREQ(__ry_http_reason_phrase(303), "See Other");
    EXPECT_STREQ(__ry_http_reason_phrase(304), "Not Modified");
    EXPECT_STREQ(__ry_http_reason_phrase(307), "Temporary Redirect");
    EXPECT_STREQ(__ry_http_reason_phrase(308), "Permanent Redirect");

    // 4xx
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

    // 5xx
    EXPECT_STREQ(__ry_http_reason_phrase(500), "Internal Server Error");
    EXPECT_STREQ(__ry_http_reason_phrase(501), "Not Implemented");
    EXPECT_STREQ(__ry_http_reason_phrase(502), "Bad Gateway");
    EXPECT_STREQ(__ry_http_reason_phrase(503), "Service Unavailable");
    EXPECT_STREQ(__ry_http_reason_phrase(504), "Gateway Timeout");
    EXPECT_STREQ(__ry_http_reason_phrase(505), "HTTP Version Not Supported");
    EXPECT_STREQ(__ry_http_reason_phrase(507), "Insufficient Storage");
    EXPECT_STREQ(__ry_http_reason_phrase(508), "Loop Detected");
    EXPECT_STREQ(__ry_http_reason_phrase(511), "Network Authentication Required");

    // Unknown
    EXPECT_STREQ(__ry_http_reason_phrase(0), "Unknown");
    EXPECT_STREQ(__ry_http_reason_phrase(999), "Unknown");
    EXPECT_STREQ(__ry_http_reason_phrase(299), "Unknown");
}

// --- Merged unit test for __ry_http_parse_content_length ---

TEST(RuntimeHttp, ParseContentLength) {
    // Valid
    EXPECT_EQ(__ry_http_parse_content_length("0"), 0);
    EXPECT_EQ(__ry_http_parse_content_length("42"), 42);
    EXPECT_EQ(__ry_http_parse_content_length("1024"), 1024);

    // Null
    EXPECT_EQ(__ry_http_parse_content_length(nullptr), -1);

    // Invalid
    EXPECT_EQ(__ry_http_parse_content_length("abc"), -2);
    EXPECT_EQ(__ry_http_parse_content_length("-1"), -2);
    EXPECT_EQ(__ry_http_parse_content_length("12abc"), -2);
    EXPECT_EQ(__ry_http_parse_content_length(""), -2);

    // Exceeds max
    EXPECT_EQ(__ry_http_parse_content_length("10485761"), -3);
    EXPECT_EQ(__ry_http_parse_content_length("999999999999"), -3);

    // Boundary: exactly MAX_BODY_SIZE (10 MB) should be valid
    EXPECT_EQ(__ry_http_parse_content_length("10485760"), 10485760);

    // Whitespace
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

// --- Merged URL parsing tests ---

TEST(RuntimeHttpClient, ParseUrlValid) {
    // Basic
    auto *u = (ParsedUrl *)__ry_http_parse_url("http://example.com/path");
    ASSERT_NE(u, nullptr);
    EXPECT_STREQ(u->host, "example.com");
    EXPECT_EQ(u->port, 80);
    EXPECT_STREQ(u->path, "/path");
    __ry_http_parsed_url_free(u);

    // With port
    u = (ParsedUrl *)__ry_http_parse_url("http://localhost:8080/api/data");
    ASSERT_NE(u, nullptr);
    EXPECT_STREQ(u->host, "localhost");
    EXPECT_EQ(u->port, 8080);
    EXPECT_STREQ(u->path, "/api/data");
    __ry_http_parsed_url_free(u);

    // No path
    u = (ParsedUrl *)__ry_http_parse_url("http://example.com");
    ASSERT_NE(u, nullptr);
    EXPECT_STREQ(u->host, "example.com");
    EXPECT_EQ(u->port, 80);
    EXPECT_STREQ(u->path, "/");
    __ry_http_parsed_url_free(u);

    // Trailing slash
    u = (ParsedUrl *)__ry_http_parse_url("http://example.com/");
    ASSERT_NE(u, nullptr);
    EXPECT_STREQ(u->host, "example.com");
    EXPECT_EQ(u->port, 80);
    EXPECT_STREQ(u->path, "/");
    __ry_http_parsed_url_free(u);

    // With query
    u = (ParsedUrl *)__ry_http_parse_url("http://example.com/search?q=hello");
    ASSERT_NE(u, nullptr);
    EXPECT_STREQ(u->host, "example.com");
    EXPECT_STREQ(u->path, "/search?q=hello");
    __ry_http_parsed_url_free(u);

    // HTTPS accepted
    u = (ParsedUrl *)__ry_http_parse_url("https://example.com");
    ASSERT_NE(u, nullptr);
    EXPECT_STREQ(u->host, "example.com");
    EXPECT_EQ(u->port, 443);
    EXPECT_STREQ(u->path, "/");
    EXPECT_TRUE(u->is_https);
    __ry_http_parsed_url_free(u);

    // HTTPS with port
    u = (ParsedUrl *)__ry_http_parse_url("https://example.com:8443/api");
    ASSERT_NE(u, nullptr);
    EXPECT_STREQ(u->host, "example.com");
    EXPECT_EQ(u->port, 8443);
    EXPECT_STREQ(u->path, "/api");
    EXPECT_TRUE(u->is_https);
    __ry_http_parsed_url_free(u);

    // HTTP is not HTTPS
    u = (ParsedUrl *)__ry_http_parse_url("http://example.com");
    ASSERT_NE(u, nullptr);
    EXPECT_FALSE(u->is_https);
    __ry_http_parsed_url_free(u);

    // Port no path
    u = (ParsedUrl *)__ry_http_parse_url("http://localhost:3000");
    ASSERT_NE(u, nullptr);
    EXPECT_STREQ(u->host, "localhost");
    EXPECT_EQ(u->port, 3000);
    EXPECT_STREQ(u->path, "/");
    __ry_http_parsed_url_free(u);

    // Query without path
    u = (ParsedUrl *)__ry_http_parse_url("http://example.com?x=1&y=2");
    ASSERT_NE(u, nullptr);
    EXPECT_STREQ(u->host, "example.com");
    EXPECT_EQ(u->port, 80);
    EXPECT_STREQ(u->path, "/?x=1&y=2");
    __ry_http_parsed_url_free(u);
}

TEST(RuntimeHttpClient, ParseUrlInvalid) {
    // No scheme
    EXPECT_EQ(__ry_http_parse_url("example.com/path"), nullptr);

    // Empty string
    EXPECT_EQ(__ry_http_parse_url(""), nullptr);

    // Null
    EXPECT_EQ(__ry_http_parse_url(nullptr), nullptr);

    // Empty host
    EXPECT_EQ(__ry_http_parse_url("http:///path"), nullptr);

    // Bad port
    EXPECT_EQ(__ry_http_parse_url("http://example.com:99999/path"), nullptr);
    EXPECT_EQ(__ry_http_parse_url("http://example.com:0/path"), nullptr);
    EXPECT_EQ(__ry_http_parse_url("http://example.com:abc/path"), nullptr);
}

// --- HTTP client integration tests ---

static void mock_http_server(int fd, const std::string &response) {
    char buf[4096];
    ssize_t n = ::recv(fd, buf, sizeof(buf), 0);
    (void)n;
    size_t sent = 0;
    while (sent < response.size()) {
        ssize_t w = ::write(fd, response.c_str() + sent, response.size() - sent);
        if (w <= 0) break;
        sent += (size_t)w;
    }
    ::close(fd);
}

static std::string mock_http_server_capture(int fd, const std::string &response) {
    char buf[8192];
    std::string request;
    struct timeval tv = {2, 0};
    ::setsockopt(fd, SOL_SOCKET, SO_RCVTIMEO, &tv, sizeof(tv));
    while (true) {
        ssize_t n = ::recv(fd, buf, sizeof(buf), 0);
        if (n <= 0) break;
        request.append(buf, (size_t)n);
        if (request.find("\r\n\r\n") != std::string::npos) break;
    }
    size_t sent = 0;
    while (sent < response.size()) {
        ssize_t w = ::write(fd, response.c_str() + sent, response.size() - sent);
        if (w <= 0) break;
        sent += (size_t)w;
    }
    ::close(fd);
    return request;
}

static int start_mock_server() {
    int srv = ::socket(AF_INET, SOCK_STREAM, 0);
    if (srv < 0) return -1;
    int opt = 1;
    ::setsockopt(srv, SOL_SOCKET, SO_REUSEADDR, &opt, sizeof(opt));
    struct sockaddr_in addr{};
    addr.sin_family = AF_INET;
    addr.sin_addr.s_addr = htonl(INADDR_LOOPBACK);
    addr.sin_port = 0;
    if (::bind(srv, (struct sockaddr *)&addr, sizeof(addr)) != 0) { ::close(srv); return -1; }
    if (::listen(srv, 1) != 0) { ::close(srv); return -1; }
    return srv;
}

static int get_server_port(int srv) {
    struct sockaddr_in addr{};
    socklen_t len = sizeof(addr);
    ::getsockname(srv, (struct sockaddr *)&addr, &len);
    return ntohs(addr.sin_port);
}

// RAII guard to ensure std::thread is always joined, even if ASSERT_* triggers early return.
struct JoinGuard {
    std::thread &t;
    explicit JoinGuard(std::thread &t) : t(t) {}
    ~JoinGuard() { if (t.joinable()) t.join(); }
};

class RuntimeHttpClientTest : public ::testing::Test {
protected:
    AllowPrivateHTTPGuard allow_private_;
};

TEST_F(RuntimeHttpClientTest, HttpGetBasic) {
    std::string response = "HTTP/1.1 200 OK\r\nContent-Length: 5\r\nX-Custom: test-value\r\n\r\nhello";
    int srv = start_mock_server();
    if (srv < 0) GTEST_SKIP() << "could not create mock server (network unavailable)";
    int port = get_server_port(srv);

    std::thread server_thread([&]() {
        struct sockaddr_in client_addr{};
        socklen_t client_len = sizeof(client_addr);
        int conn = ::accept(srv, (struct sockaddr *)&client_addr, &client_len);
        mock_http_server(conn, response);
    });
    JoinGuard jg(server_thread);

    std::string url = "http://127.0.0.1:" + std::to_string(port) + "/test";
    void *resp = __ry_http_get(url.c_str());
    ASSERT_NE(resp, nullptr);
    EXPECT_EQ(__ry_http_client_status(resp), 200);
    EXPECT_STREQ(__ry_http_client_body(resp), "hello");
    EXPECT_STREQ(__ry_http_client_header(resp, "X-Custom"), "test-value");
    EXPECT_EQ(__ry_http_client_header(resp, "Missing"), nullptr);
    __ry_http_client_response_free(resp);

    ::close(srv);
}

TEST_F(RuntimeHttpClientTest, HttpPostWithBody) {
    std::string response = "HTTP/1.1 201 Created\r\nContent-Length: 2\r\n\r\nok";
    int srv = start_mock_server();
    if (srv < 0) GTEST_SKIP() << "could not create mock server (network unavailable)";
    int port = get_server_port(srv);

    std::string captured_request;
    std::thread server_thread([&]() {
        struct sockaddr_in client_addr{};
        socklen_t client_len = sizeof(client_addr);
        int conn = ::accept(srv, (struct sockaddr *)&client_addr, &client_len);
        captured_request = mock_http_server_capture(conn, response);
    });
    JoinGuard jg(server_thread);

    std::string url = "http://127.0.0.1:" + std::to_string(port) + "/data";
    void *resp = __ry_http_post(url.c_str(), "test body", nullptr);
    ASSERT_NE(resp, nullptr);
    EXPECT_EQ(__ry_http_client_status(resp), 201);
    EXPECT_STREQ(__ry_http_client_body(resp), "ok");
    __ry_http_client_response_free(resp);

    ::close(srv);
    server_thread.join(); // ensure captured_request is fully written

    EXPECT_NE(captured_request.find("POST /data HTTP/1.1"), std::string::npos);
    EXPECT_NE(captured_request.find("Content-Length: 9"), std::string::npos);
}

TEST_F(RuntimeHttpClientTest, HttpGetConnectionRefused) {
    void *resp = __ry_http_get("http://127.0.0.1:1/nonexistent");
    EXPECT_EQ(resp, nullptr);
}

TEST_F(RuntimeHttpClientTest, HttpGetNoContentLength) {
    std::string response = "HTTP/1.1 200 OK\r\nConnection: close\r\n\r\nno content length body";
    int srv = start_mock_server();
    if (srv < 0) GTEST_SKIP() << "could not create mock server (network unavailable)";
    int port = get_server_port(srv);

    std::thread server_thread([&]() {
        struct sockaddr_in client_addr{};
        socklen_t client_len = sizeof(client_addr);
        int conn = ::accept(srv, (struct sockaddr *)&client_addr, &client_len);
        mock_http_server(conn, response);
    });
    JoinGuard jg(server_thread);

    std::string url = "http://127.0.0.1:" + std::to_string(port) + "/";
    void *resp = __ry_http_get(url.c_str());
    ASSERT_NE(resp, nullptr);
    EXPECT_EQ(__ry_http_client_status(resp), 200);
    EXPECT_STREQ(__ry_http_client_body(resp), "no content length body");
    __ry_http_client_response_free(resp);

    ::close(srv);
}

TEST_F(RuntimeHttpClientTest, HttpRequestCustomMethod) {
    std::string response = "HTTP/1.1 204 No Content\r\nContent-Length: 0\r\n\r\n";
    int srv = start_mock_server();
    if (srv < 0) GTEST_SKIP() << "could not create mock server (network unavailable)";
    int port = get_server_port(srv);

    std::string captured_request;
    std::thread server_thread([&]() {
        struct sockaddr_in client_addr{};
        socklen_t client_len = sizeof(client_addr);
        int conn = ::accept(srv, (struct sockaddr *)&client_addr, &client_len);
        captured_request = mock_http_server_capture(conn, response);
    });
    JoinGuard jg(server_thread);

    std::string url = "http://127.0.0.1:" + std::to_string(port) + "/resource";
    void *resp = __ry_http_client_request("DELETE", url.c_str(), nullptr, "");
    ASSERT_NE(resp, nullptr);
    EXPECT_EQ(__ry_http_client_status(resp), 204);
    __ry_http_client_response_free(resp);

    ::close(srv);
    server_thread.join(); // ensure captured_request is fully written

    EXPECT_NE(captured_request.find("DELETE /resource HTTP/1.1"), std::string::npos);
}

TEST_F(RuntimeHttpClientTest, HttpClientHeaderCaseInsensitive) {
    std::string response = "HTTP/1.1 200 OK\r\ncontent-type: application/json\r\nContent-Length: 2\r\n\r\n{}";
    int srv = start_mock_server();
    if (srv < 0) GTEST_SKIP() << "could not create mock server (network unavailable)";
    int port = get_server_port(srv);

    std::thread server_thread([&]() {
        struct sockaddr_in client_addr{};
        socklen_t client_len = sizeof(client_addr);
        int conn = ::accept(srv, (struct sockaddr *)&client_addr, &client_len);
        mock_http_server(conn, response);
    });
    JoinGuard jg(server_thread);

    std::string url = "http://127.0.0.1:" + std::to_string(port) + "/";
    void *resp = __ry_http_get(url.c_str());
    ASSERT_NE(resp, nullptr);
    EXPECT_STREQ(__ry_http_client_header(resp, "Content-Type"), "application/json");
    EXPECT_STREQ(__ry_http_client_header(resp, "content-type"), "application/json");
    __ry_http_client_response_free(resp);

    ::close(srv);
}

TEST_F(RuntimeHttpClientTest, HopByHopHeadersFiltered) {
    std::string response = "HTTP/1.1 200 OK\r\nContent-Length: 2\r\n\r\nok";
    int srv = start_mock_server();
    if (srv < 0) GTEST_SKIP() << "could not create mock server (network unavailable)";
    int port = get_server_port(srv);

    std::string captured_request;
    std::thread server_thread([&]() {
        struct sockaddr_in client_addr{};
        socklen_t client_len = sizeof(client_addr);
        int conn = ::accept(srv, (struct sockaddr *)&client_addr, &client_len);
        captured_request = mock_http_server_capture(conn, response);
    });
    JoinGuard jg(server_thread);

    // Build a MapHeader with hop-by-hop headers and a normal custom header
    auto *map = (MapHeader *)malloc(sizeof(MapHeader));
    memset(map, 0, sizeof(MapHeader));
    map->len = 8;
    map->cap = 8;
    map->keys = (char **)malloc(sizeof(char *) * 8);
    map->vals = (char **)malloc(sizeof(char *) * 8);
    map->keys[0] = strdup("Host");
    map->vals[0] = strdup("evil.com");
    map->keys[1] = strdup("Connection");
    map->vals[1] = strdup("keep-alive");
    map->keys[2] = strdup("Keep-Alive");
    map->vals[2] = strdup("timeout=5");
    map->keys[3] = strdup("Transfer-Encoding");
    map->vals[3] = strdup("chunked");
    map->keys[4] = strdup("TE");
    map->vals[4] = strdup("trailers");
    map->keys[5] = strdup("Upgrade");
    map->vals[5] = strdup("websocket");
    map->keys[6] = strdup("Proxy-Connection");
    map->vals[6] = strdup("keep-alive");
    map->keys[7] = strdup("X-Custom");
    map->vals[7] = strdup("allowed-value");
    map->bucket_count = 16;
    map->buckets = calloc(16, sizeof(int64_t));

    std::string url = "http://127.0.0.1:" + std::to_string(port) + "/test";
    void *resp = __ry_http_client_request("GET", url.c_str(), map, "");
    ASSERT_NE(resp, nullptr);
    EXPECT_EQ(__ry_http_client_status(resp), 200);
    __ry_http_client_response_free(resp);

    ::close(srv);
    server_thread.join();

    // Hop-by-hop headers must NOT appear in the request
    EXPECT_EQ(captured_request.find("Host: evil.com"), std::string::npos);
    EXPECT_EQ(captured_request.find("Connection: keep-alive"), std::string::npos);
    EXPECT_EQ(captured_request.find("Keep-Alive:"), std::string::npos);
    EXPECT_EQ(captured_request.find("Transfer-Encoding:"), std::string::npos);
    EXPECT_EQ(captured_request.find("TE:"), std::string::npos);
    EXPECT_EQ(captured_request.find("Upgrade:"), std::string::npos);
    EXPECT_EQ(captured_request.find("Proxy-Connection:"), std::string::npos);

    // The auto-generated Host header should be present (not the user-injected one)
    EXPECT_NE(captured_request.find("Host: 127.0.0.1:"), std::string::npos);

    // Normal custom header must still be present
    EXPECT_NE(captured_request.find("X-Custom: allowed-value"), std::string::npos);

    for (int64_t i = 0; i < map->len; i++) {
        free(map->keys[i]);
        free(map->vals[i]);
    }
    free(map->keys);
    free(map->vals);
    free(map->buckets);
    free(map);
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

// --- Chunked transfer encoding tests ---

TEST(RuntimeHttp, ChunkedRequestBasic) {
    int fds[2];
    ASSERT_EQ(socketpair(AF_UNIX, SOCK_STREAM, 0, fds), 0);

    std::string req =
        "POST /data HTTP/1.1\r\n"
        "Host: localhost\r\n"
        "Transfer-Encoding: chunked\r\n"
        "\r\n"
        "5\r\nhello\r\n"
        "6\r\n world\r\n"
        "0\r\n\r\n";
    send_and_close(fds[1], req);

    auto *handle = (TcpStreamHandle *)malloc(sizeof(TcpStreamHandle));
    handle->fd = fds[0];
    void *result = __ry_http_read_request(handle);
    ASSERT_NE(result, nullptr);
    EXPECT_STREQ(__ry_http_method(result), "POST");
    EXPECT_STREQ(__ry_http_path(result), "/data");
    EXPECT_STREQ(__ry_http_body(result), "hello world");

    __ry_http_request_free(result);
    ::close(fds[0]);
    free(handle);
}

TEST(RuntimeHttp, ChunkedRequestSingleChunk) {
    int fds[2];
    ASSERT_EQ(socketpair(AF_UNIX, SOCK_STREAM, 0, fds), 0);

    std::string req =
        "POST /single HTTP/1.1\r\n"
        "Host: localhost\r\n"
        "Transfer-Encoding: chunked\r\n"
        "\r\n"
        "d\r\nHello, World!\r\n"
        "0\r\n\r\n";
    send_and_close(fds[1], req);

    auto *handle = (TcpStreamHandle *)malloc(sizeof(TcpStreamHandle));
    handle->fd = fds[0];
    void *result = __ry_http_read_request(handle);
    ASSERT_NE(result, nullptr);
    EXPECT_STREQ(__ry_http_body(result), "Hello, World!");

    __ry_http_request_free(result);
    ::close(fds[0]);
    free(handle);
}

TEST(RuntimeHttp, ChunkedRequestEmptyBody) {
    int fds[2];
    ASSERT_EQ(socketpair(AF_UNIX, SOCK_STREAM, 0, fds), 0);

    std::string req =
        "POST /empty HTTP/1.1\r\n"
        "Host: localhost\r\n"
        "Transfer-Encoding: chunked\r\n"
        "\r\n"
        "0\r\n\r\n";
    send_and_close(fds[1], req);

    auto *handle = (TcpStreamHandle *)malloc(sizeof(TcpStreamHandle));
    handle->fd = fds[0];
    void *result = __ry_http_read_request(handle);
    ASSERT_NE(result, nullptr);
    EXPECT_STREQ(__ry_http_body(result), "");

    __ry_http_request_free(result);
    ::close(fds[0]);
    free(handle);
}

TEST(RuntimeHttp, ChunkedRequestHexSize) {
    int fds[2];
    ASSERT_EQ(socketpair(AF_UNIX, SOCK_STREAM, 0, fds), 0);

    // 1A = 26 bytes
    std::string chunk_data = "abcdefghijklmnopqrstuvwxyz";
    std::string req =
        "POST /hex HTTP/1.1\r\n"
        "Host: localhost\r\n"
        "Transfer-Encoding: chunked\r\n"
        "\r\n"
        "1A\r\n" + chunk_data + "\r\n"
        "0\r\n\r\n";
    send_and_close(fds[1], req);

    auto *handle = (TcpStreamHandle *)malloc(sizeof(TcpStreamHandle));
    handle->fd = fds[0];
    void *result = __ry_http_read_request(handle);
    ASSERT_NE(result, nullptr);
    EXPECT_STREQ(__ry_http_body(result), "abcdefghijklmnopqrstuvwxyz");

    __ry_http_request_free(result);
    ::close(fds[0]);
    free(handle);
}

TEST(RuntimeHttp, ChunkedRequestWithExtensions) {
    int fds[2];
    ASSERT_EQ(socketpair(AF_UNIX, SOCK_STREAM, 0, fds), 0);

    std::string req =
        "POST /ext HTTP/1.1\r\n"
        "Host: localhost\r\n"
        "Transfer-Encoding: chunked\r\n"
        "\r\n"
        "5;ext=val\r\nhello\r\n"
        "0\r\n\r\n";
    send_and_close(fds[1], req);

    auto *handle = (TcpStreamHandle *)malloc(sizeof(TcpStreamHandle));
    handle->fd = fds[0];
    void *result = __ry_http_read_request(handle);
    ASSERT_NE(result, nullptr);
    EXPECT_STREQ(__ry_http_body(result), "hello");

    __ry_http_request_free(result);
    ::close(fds[0]);
    free(handle);
}

TEST(RuntimeHttp, ChunkedRequestRejectsWithContentLength) {
    int fds[2];
    ASSERT_EQ(socketpair(AF_UNIX, SOCK_STREAM, 0, fds), 0);

    std::string req =
        "POST /conflict HTTP/1.1\r\n"
        "Host: localhost\r\n"
        "Transfer-Encoding: chunked\r\n"
        "Content-Length: 5\r\n"
        "\r\n"
        "5\r\nhello\r\n"
        "0\r\n\r\n";
    send_and_close(fds[1], req);

    auto *handle = (TcpStreamHandle *)malloc(sizeof(TcpStreamHandle));
    handle->fd = fds[0];
    void *result = __ry_http_read_request(handle);
    EXPECT_EQ(result, nullptr);

    ::close(fds[0]);
    free(handle);
}

TEST(RuntimeHttp, ChunkedResponseSend) {
    int fds[2];
    ASSERT_EQ(socketpair(AF_UNIX, SOCK_STREAM, 0, fds), 0);

    // Build a MapHeader with Transfer-Encoding: chunked
    auto *map = (MapHeader *)malloc(sizeof(MapHeader));
    map->len = 1;
    map->cap = 1;
    map->keys = (char **)malloc(sizeof(char *));
    map->vals = (char **)malloc(sizeof(char *));
    map->keys[0] = strdup("Transfer-Encoding");
    map->vals[0] = strdup("chunked");
    map->bucket_count = 4;
    map->buckets = calloc(4, sizeof(int64_t));

    void *resp = __ry_http_response_create(200, map, "hello world");

    auto *handle = (TcpStreamHandle *)malloc(sizeof(TcpStreamHandle));
    handle->fd = fds[0];
    __ry_http_send_response(handle, resp);
    ::close(fds[0]);

    // Read what was sent
    char buf[4096];
    std::string received;
    while (true) {
        ssize_t n = ::read(fds[1], buf, sizeof(buf));
        if (n <= 0) break;
        received.append(buf, (size_t)n);
    }
    ::close(fds[1]);

    // Verify chunked format
    EXPECT_NE(received.find("Transfer-Encoding: chunked"), std::string::npos);
    EXPECT_EQ(received.find("Content-Length"), std::string::npos);
    // Body should contain chunk: "b\r\nhello world\r\n0\r\n\r\n"
    EXPECT_NE(received.find("b\r\nhello world\r\n0\r\n\r\n"), std::string::npos);

    __ry_http_response_free(resp);
    free(map->keys[0]);
    free(map->vals[0]);
    free(map->keys);
    free(map->vals);
    free(map->buckets);
    free(map);
    free(handle);
}

TEST_F(RuntimeHttpClientTest, ChunkedClientResponse) {
    std::string response =
        "HTTP/1.1 200 OK\r\n"
        "Transfer-Encoding: chunked\r\n"
        "\r\n"
        "5\r\nhello\r\n"
        "6\r\n world\r\n"
        "0\r\n\r\n";
    int srv = start_mock_server();
    if (srv < 0) GTEST_SKIP() << "could not create mock server (network unavailable)";
    int port = get_server_port(srv);

    std::thread server_thread([&]() {
        struct sockaddr_in client_addr{};
        socklen_t client_len = sizeof(client_addr);
        int conn = ::accept(srv, (struct sockaddr *)&client_addr, &client_len);
        mock_http_server(conn, response);
    });
    JoinGuard jg(server_thread);

    std::string url = "http://127.0.0.1:" + std::to_string(port) + "/chunked";
    void *resp = __ry_http_get(url.c_str());
    ASSERT_NE(resp, nullptr);
    EXPECT_EQ(__ry_http_client_status(resp), 200);
    EXPECT_STREQ(__ry_http_client_body(resp), "hello world");
    __ry_http_client_response_free(resp);

    ::close(srv);
}

TEST_F(RuntimeHttpClientTest, ChunkedClientResponseMultipleChunks) {
    std::string response =
        "HTTP/1.1 200 OK\r\n"
        "Transfer-Encoding: chunked\r\n"
        "\r\n"
        "4\r\nWiki\r\n"
        "5\r\npedia\r\n"
        "e\r\n in\r\n\r\nchunks.\r\n"
        "0\r\n\r\n";
    int srv = start_mock_server();
    if (srv < 0) GTEST_SKIP() << "could not create mock server (network unavailable)";
    int port = get_server_port(srv);

    std::thread server_thread([&]() {
        struct sockaddr_in client_addr{};
        socklen_t client_len = sizeof(client_addr);
        int conn = ::accept(srv, (struct sockaddr *)&client_addr, &client_len);
        mock_http_server(conn, response);
    });
    JoinGuard jg(server_thread);

    std::string url = "http://127.0.0.1:" + std::to_string(port) + "/chunked-multi";
    void *resp = __ry_http_get(url.c_str());
    ASSERT_NE(resp, nullptr);
    EXPECT_EQ(__ry_http_client_status(resp), 200);
    EXPECT_STREQ(__ry_http_client_body(resp), "Wikipedia in\r\n\r\nchunks.");
    __ry_http_client_response_free(resp);

    ::close(srv);
}

// --- SSRF protection tests ---

TEST(HttpSSRF, PrivateHostLoopback) {
    EXPECT_TRUE(__ry_is_private_host("127.0.0.1", 80));
}

TEST(HttpSSRF, PrivateHostLocalhost) {
    EXPECT_TRUE(__ry_is_private_host("localhost", 80));
}

TEST(HttpSSRF, PrivateHost10Network) {
    EXPECT_TRUE(__ry_is_private_host("10.0.0.1", 80));
}

TEST(HttpSSRF, PrivateHost192168) {
    EXPECT_TRUE(__ry_is_private_host("192.168.1.1", 80));
}

TEST(HttpSSRF, PrivateHostLinkLocal) {
    EXPECT_TRUE(__ry_is_private_host("169.254.169.254", 80));
}

TEST(HttpSSRF, PublicHostAllowed) {
    EXPECT_FALSE(__ry_is_private_host("8.8.8.8", 80));
}

// --- IPv4-mapped IPv6 SSRF tests ---

TEST(HttpSSRF, PrivateHostIPv4MappedLoopback) {
    EXPECT_TRUE(__ry_is_private_host("::ffff:127.0.0.1", 80));
}

TEST(HttpSSRF, PrivateHostIPv4Mapped192168) {
    EXPECT_TRUE(__ry_is_private_host("::ffff:192.168.1.1", 80));
}

TEST(HttpSSRF, PublicHostIPv4MappedPublic) {
    EXPECT_FALSE(__ry_is_private_host("::ffff:8.8.8.8", 80));
}

// --- DNS resolve and private addrinfo tests ---

TEST(HttpSSRF, ResolveLocalhost) {
    struct addrinfo *result = nullptr;
    ASSERT_EQ(__ry_resolve("localhost", 80, &result), 0);
    ASSERT_NE(result, nullptr);
    EXPECT_TRUE(__ry_is_private_addrinfo(result));
    ::freeaddrinfo(result);
}

TEST(HttpSSRF, ResolveInvalidHost) {
    struct addrinfo *result = nullptr;
    EXPECT_EQ(__ry_resolve("this-host-does-not-exist.invalid", 80, &result), -1);
    EXPECT_EQ(result, nullptr);
}

TEST(HttpSSRF, PrivateAddrInfoSynthetic) {
    // Build a synthetic addrinfo with loopback address
    struct sockaddr_in sin{};
    sin.sin_family = AF_INET;
    sin.sin_addr.s_addr = htonl(INADDR_LOOPBACK);
    sin.sin_port = htons(80);

    struct addrinfo ai{};
    ai.ai_family = AF_INET;
    ai.ai_socktype = SOCK_STREAM;
    ai.ai_addr = (struct sockaddr *)&sin;
    ai.ai_addrlen = sizeof(sin);

    EXPECT_TRUE(__ry_is_private_addrinfo(&ai));
}

TEST(HttpSSRF, PublicAddrInfoSynthetic) {
    // Build a synthetic addrinfo with public address 8.8.8.8
    struct sockaddr_in sin{};
    sin.sin_family = AF_INET;
    sin.sin_addr.s_addr = htonl(0x08080808);
    sin.sin_port = htons(80);

    struct addrinfo ai{};
    ai.ai_family = AF_INET;
    ai.ai_socktype = SOCK_STREAM;
    ai.ai_addr = (struct sockaddr *)&sin;
    ai.ai_addrlen = sizeof(sin);

    EXPECT_FALSE(__ry_is_private_addrinfo(&ai));
}

// --- Additional SSRF range tests (IPv4) ---

TEST(HttpSSRF, PrivateHostCarrierGradeNAT) {
    EXPECT_TRUE(__ry_is_private_host("100.64.0.1", 80));
}

TEST(HttpSSRF, PrivateHostCarrierGradeNATLowerEdge) {
    EXPECT_TRUE(__ry_is_private_host("100.64.0.0", 80));
}

TEST(HttpSSRF, PrivateHostCarrierGradeNATUpper) {
    EXPECT_TRUE(__ry_is_private_host("100.127.255.255", 80));
}

TEST(HttpSSRF, PrivateHostBenchmarking) {
    EXPECT_TRUE(__ry_is_private_host("198.18.0.1", 80));
}

TEST(HttpSSRF, PrivateHostBenchmarkingLowerEdge) {
    EXPECT_TRUE(__ry_is_private_host("198.18.0.0", 80));
}

TEST(HttpSSRF, PrivateHostBenchmarkingUpper) {
    EXPECT_TRUE(__ry_is_private_host("198.19.255.255", 80));
}

TEST(HttpSSRF, PrivateHostMulticast) {
    EXPECT_TRUE(__ry_is_private_host("224.0.0.1", 80));
}

TEST(HttpSSRF, PrivateHostMulticastLowerEdge) {
    EXPECT_TRUE(__ry_is_private_host("224.0.0.0", 80));
}

TEST(HttpSSRF, PrivateHostMulticastUpper) {
    EXPECT_TRUE(__ry_is_private_host("239.255.255.255", 80));
}

TEST(HttpSSRF, PrivateHostReserved) {
    EXPECT_TRUE(__ry_is_private_host("240.0.0.1", 80));
}

TEST(HttpSSRF, PrivateHostReservedLowerEdge) {
    EXPECT_TRUE(__ry_is_private_host("240.0.0.0", 80));
}

TEST(HttpSSRF, PrivateHostBroadcast) {
    EXPECT_TRUE(__ry_is_private_host("255.255.255.255", 80));
}

// Boundary tests (should remain public / not blocked)

TEST(HttpSSRF, PublicHostBelowCarrierGradeNAT) {
    EXPECT_FALSE(__ry_is_private_host("100.63.255.255", 80));
}

TEST(HttpSSRF, PublicHostAboveCarrierGradeNAT) {
    EXPECT_FALSE(__ry_is_private_host("100.128.0.0", 80));
}

TEST(HttpSSRF, PublicHostBelowBenchmarking) {
    EXPECT_FALSE(__ry_is_private_host("198.17.255.255", 80));
}

TEST(HttpSSRF, PublicHostAboveBenchmarking) {
    EXPECT_FALSE(__ry_is_private_host("198.20.0.0", 80));
}

TEST(HttpSSRF, PublicHostBelowMulticast) {
    EXPECT_FALSE(__ry_is_private_host("223.255.255.255", 80));
}

// --- Additional SSRF range tests (IPv6 synthetic) ---

TEST(HttpSSRF, PrivateAddrIPv6Unspecified) {
    struct sockaddr_in6 sin6{};
    sin6.sin6_family = AF_INET6;
    EXPECT_TRUE(__ry_is_private_addr((struct sockaddr *)&sin6));
}

TEST(HttpSSRF, PrivateAddrIPv6Multicast) {
    struct sockaddr_in6 sin6{};
    sin6.sin6_family = AF_INET6;
    sin6.sin6_addr.s6_addr[0] = 0xFF;
    sin6.sin6_addr.s6_addr[1] = 0x02;
    sin6.sin6_addr.s6_addr[15] = 0x01; // ff02::1
    EXPECT_TRUE(__ry_is_private_addr((struct sockaddr *)&sin6));
}

TEST(HttpSSRF, PrivateAddrNullSockaddr) {
    EXPECT_FALSE(__ry_is_private_addr(nullptr));
}

// --- Multipart form-data tests ---

TEST(RuntimeHttp, FormFieldBasic) {
    int fds[2];
    ASSERT_EQ(socketpair(AF_UNIX, SOCK_STREAM, 0, fds), 0);

    std::string body =
        "--boundary123\r\n"
        "Content-Disposition: form-data; name=\"username\"\r\n"
        "\r\n"
        "alice\r\n"
        "--boundary123\r\n"
        "Content-Disposition: form-data; name=\"email\"\r\n"
        "\r\n"
        "alice@example.com\r\n"
        "--boundary123--\r\n";

    std::string req =
        "POST /submit HTTP/1.1\r\n"
        "Host: localhost\r\n"
        "Content-Type: multipart/form-data; boundary=boundary123\r\n"
        "Content-Length: " + std::to_string(body.size()) + "\r\n"
        "\r\n" + body;
    send_and_close(fds[1], req);

    auto *handle = (TcpStreamHandle *)malloc(sizeof(TcpStreamHandle));
    handle->fd = fds[0];
    void *result = __ry_http_read_request(handle);
    ASSERT_NE(result, nullptr);
    EXPECT_STREQ(__ry_http_form_field(result, "username"), "alice");
    EXPECT_STREQ(__ry_http_form_field(result, "email"), "alice@example.com");
    EXPECT_EQ(__ry_http_form_field(result, "missing"), nullptr);
    __ry_http_request_free(result);
    ::close(fds[0]);
    free(handle);
}

TEST(RuntimeHttp, FormFieldQuotedBoundary) {
    int fds[2];
    ASSERT_EQ(socketpair(AF_UNIX, SOCK_STREAM, 0, fds), 0);

    std::string body =
        "--myboundary\r\n"
        "Content-Disposition: form-data; name=\"field1\"\r\n"
        "\r\n"
        "value1\r\n"
        "--myboundary--\r\n";

    std::string req =
        "POST /submit HTTP/1.1\r\n"
        "Host: localhost\r\n"
        "Content-Type: multipart/form-data; boundary=\"myboundary\"\r\n"
        "Content-Length: " + std::to_string(body.size()) + "\r\n"
        "\r\n" + body;
    send_and_close(fds[1], req);

    auto *handle = (TcpStreamHandle *)malloc(sizeof(TcpStreamHandle));
    handle->fd = fds[0];
    void *result = __ry_http_read_request(handle);
    ASSERT_NE(result, nullptr);
    EXPECT_STREQ(__ry_http_form_field(result, "field1"), "value1");
    __ry_http_request_free(result);
    ::close(fds[0]);
    free(handle);
}

TEST(RuntimeHttp, FormFileBasic) {
    int fds[2];
    ASSERT_EQ(socketpair(AF_UNIX, SOCK_STREAM, 0, fds), 0);

    std::string body =
        "--fileboundary\r\n"
        "Content-Disposition: form-data; name=\"avatar\"; filename=\"photo.png\"\r\n"
        "Content-Type: image/png\r\n"
        "\r\n"
        "FAKE_PNG_DATA\r\n"
        "--fileboundary--\r\n";

    std::string req =
        "POST /upload HTTP/1.1\r\n"
        "Host: localhost\r\n"
        "Content-Type: multipart/form-data; boundary=fileboundary\r\n"
        "Content-Length: " + std::to_string(body.size()) + "\r\n"
        "\r\n" + body;
    send_and_close(fds[1], req);

    auto *handle = (TcpStreamHandle *)malloc(sizeof(TcpStreamHandle));
    handle->fd = fds[0];
    void *result = __ry_http_read_request(handle);
    ASSERT_NE(result, nullptr);

    auto *map = (MapHeader *)__ry_http_form_file(result, "avatar");
    ASSERT_NE(map, nullptr);
    EXPECT_EQ(map->len, 3);

    // Find values by key
    const char *filename = nullptr;
    const char *content_type = nullptr;
    const char *data = nullptr;
    for (int64_t i = 0; i < map->len; i++) {
        if (strcmp(map->keys[i], "filename") == 0) filename = map->vals[i];
        else if (strcmp(map->keys[i], "content_type") == 0) content_type = map->vals[i];
        else if (strcmp(map->keys[i], "data") == 0) data = map->vals[i];
    }
    EXPECT_STREQ(filename, "photo.png");
    EXPECT_STREQ(content_type, "image/png");
    EXPECT_STREQ(data, "FAKE_PNG_DATA");

    for (int64_t i = 0; i < map->len; i++) {
        free(map->keys[i]);
        free(map->vals[i]);
    }
    free(map->keys);
    free(map->vals);
    free(map->buckets);
    free(map);

    // Missing file returns nullptr (None)
    EXPECT_EQ(__ry_http_form_file(result, "missing"), nullptr);

    __ry_http_request_free(result);
    ::close(fds[0]);
    free(handle);
}

TEST(RuntimeHttp, FormMixedFieldsAndFiles) {
    int fds[2];
    ASSERT_EQ(socketpair(AF_UNIX, SOCK_STREAM, 0, fds), 0);

    std::string body =
        "--mixbnd\r\n"
        "Content-Disposition: form-data; name=\"title\"\r\n"
        "\r\n"
        "My Document\r\n"
        "--mixbnd\r\n"
        "Content-Disposition: form-data; name=\"doc\"; filename=\"readme.txt\"\r\n"
        "Content-Type: text/plain\r\n"
        "\r\n"
        "Hello World\r\n"
        "--mixbnd\r\n"
        "Content-Disposition: form-data; name=\"description\"\r\n"
        "\r\n"
        "A test file\r\n"
        "--mixbnd--\r\n";

    std::string req =
        "POST /upload HTTP/1.1\r\n"
        "Host: localhost\r\n"
        "Content-Type: multipart/form-data; boundary=mixbnd\r\n"
        "Content-Length: " + std::to_string(body.size()) + "\r\n"
        "\r\n" + body;
    send_and_close(fds[1], req);

    auto *handle = (TcpStreamHandle *)malloc(sizeof(TcpStreamHandle));
    handle->fd = fds[0];
    void *result = __ry_http_read_request(handle);
    ASSERT_NE(result, nullptr);

    // Text fields
    EXPECT_STREQ(__ry_http_form_field(result, "title"), "My Document");
    EXPECT_STREQ(__ry_http_form_field(result, "description"), "A test file");

    // File
    auto *map = (MapHeader *)__ry_http_form_file(result, "doc");
    ASSERT_NE(map, nullptr);
    const char *data = nullptr;
    for (int64_t i = 0; i < map->len; i++) {
        if (strcmp(map->keys[i], "data") == 0) data = map->vals[i];
    }
    EXPECT_STREQ(data, "Hello World");

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

TEST(RuntimeHttp, FormFieldsAll) {
    int fds[2];
    ASSERT_EQ(socketpair(AF_UNIX, SOCK_STREAM, 0, fds), 0);

    std::string body =
        "--bnd\r\n"
        "Content-Disposition: form-data; name=\"a\"\r\n"
        "\r\n"
        "1\r\n"
        "--bnd\r\n"
        "Content-Disposition: form-data; name=\"b\"\r\n"
        "\r\n"
        "2\r\n"
        "--bnd--\r\n";

    std::string req =
        "POST /form HTTP/1.1\r\n"
        "Host: localhost\r\n"
        "Content-Type: multipart/form-data; boundary=bnd\r\n"
        "Content-Length: " + std::to_string(body.size()) + "\r\n"
        "\r\n" + body;
    send_and_close(fds[1], req);

    auto *handle = (TcpStreamHandle *)malloc(sizeof(TcpStreamHandle));
    handle->fd = fds[0];
    void *result = __ry_http_read_request(handle);
    ASSERT_NE(result, nullptr);

    auto *map = (MapHeader *)__ry_http_form_fields(result);
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

TEST(RuntimeHttp, FormFieldNonMultipart) {
    int fds[2];
    ASSERT_EQ(socketpair(AF_UNIX, SOCK_STREAM, 0, fds), 0);

    std::string req =
        "POST /data HTTP/1.1\r\n"
        "Host: localhost\r\n"
        "Content-Type: application/json\r\n"
        "Content-Length: 2\r\n"
        "\r\n{}";
    send_and_close(fds[1], req);

    auto *handle = (TcpStreamHandle *)malloc(sizeof(TcpStreamHandle));
    handle->fd = fds[0];
    void *result = __ry_http_read_request(handle);
    ASSERT_NE(result, nullptr);
    EXPECT_EQ(__ry_http_form_field(result, "anything"), nullptr);
    EXPECT_EQ(__ry_http_form_file(result, "anything"), nullptr);

    auto *map = (MapHeader *)__ry_http_form_fields(result);
    ASSERT_NE(map, nullptr);
    EXPECT_EQ(map->len, 0);
    free(map->buckets);
    free(map);

    __ry_http_request_free(result);
    ::close(fds[0]);
    free(handle);
}

TEST(RuntimeHttp, FormFieldNoBody) {
    int fds[2];
    ASSERT_EQ(socketpair(AF_UNIX, SOCK_STREAM, 0, fds), 0);

    std::string req = "GET / HTTP/1.1\r\nHost: localhost\r\n\r\n";
    send_and_close(fds[1], req);

    auto *handle = (TcpStreamHandle *)malloc(sizeof(TcpStreamHandle));
    handle->fd = fds[0];
    void *result = __ry_http_read_request(handle);
    ASSERT_NE(result, nullptr);
    EXPECT_EQ(__ry_http_form_field(result, "anything"), nullptr);
    __ry_http_request_free(result);
    ::close(fds[0]);
    free(handle);
}

// --- NUL byte regression tests (#281) ---

// Helper: write raw bytes (including NUL) to fd and close
static void send_raw_and_close(int fd, const char *data, size_t len) {
    size_t sent = 0;
    while (sent < len) {
        ssize_t n = ::write(fd, data + sent, len - sent);
        if (n <= 0) break;
        sent += (size_t)n;
    }
    ::close(fd);
}

TEST(RuntimeHttp, ReadRequestBodyWithNulByte) {
    int fds[2];
    ASSERT_EQ(socketpair(AF_UNIX, SOCK_STREAM, 0, fds), 0);

    // Body: "ab\0cd" (5 bytes)
    std::string body_data("ab\0cd", 5);
    std::string req =
        "POST /data HTTP/1.1\r\n"
        "Content-Length: 5\r\n"
        "\r\n";
    req += body_data;
    send_raw_and_close(fds[1], req.data(), req.size());

    auto *handle = (TcpStreamHandle *)malloc(sizeof(TcpStreamHandle));
    handle->fd = fds[0];
    void *result = __ry_http_read_request(handle);
    ASSERT_NE(result, nullptr);

    // __ry_http_body returns const char *, so C-string truncation at NUL is expected
    const char *body = __ry_http_body(result);
    EXPECT_STREQ(body, "ab");

    // body_bytes preserves the full binary content including NUL
    void *bytes = __ry_http_body_bytes(result);
    ASSERT_NE(bytes, nullptr);
    auto *bheader = (IOListHeader *)bytes;
    EXPECT_EQ(bheader->len, 5);
    EXPECT_EQ(bheader->data[0], 'a');
    EXPECT_EQ(bheader->data[1], 'b');
    EXPECT_EQ(bheader->data[2], '\0');
    EXPECT_EQ(bheader->data[3], 'c');
    EXPECT_EQ(bheader->data[4], 'd');
    free(bheader->data);
    arc_free(bheader);

    __ry_http_request_free(result);
    ::close(fds[0]);
    free(handle);
}

TEST(RuntimeHttp, ResponseSendBodyWithNulByte) {
    // Verify body_len (not strlen) is used for Content-Length when sending responses.
    int fds[2];
    ASSERT_EQ(socketpair(AF_UNIX, SOCK_STREAM, 0, fds), 0);

    auto *map = (MapHeader *)malloc(sizeof(MapHeader));
    memset(map, 0, sizeof(MapHeader));
    map->bucket_count = 4;
    map->buckets = calloc(4, sizeof(int64_t));

    void *resp = __ry_http_response_create(200, map, "hello world");

    auto *handle = (TcpStreamHandle *)malloc(sizeof(TcpStreamHandle));
    handle->fd = fds[0];
    __ry_http_send_response(handle, resp);
    ::close(fds[0]);

    char buf[4096];
    std::string received;
    while (true) {
        ssize_t n = ::read(fds[1], buf, sizeof(buf));
        if (n <= 0) break;
        received.append(buf, (size_t)n);
    }
    ::close(fds[1]);

    // Verify Content-Length is 11 (strlen("hello world"))
    EXPECT_NE(received.find("Content-Length: 11"), std::string::npos);
    // Verify body is present
    EXPECT_NE(received.find("hello world"), std::string::npos);

    __ry_http_response_free(resp);
    free(map->buckets);
    free(map);
    free(handle);
}

TEST(RuntimeHttp, MultipartFileDataWithNulByte) {
    int fds[2];
    ASSERT_EQ(socketpair(AF_UNIX, SOCK_STREAM, 0, fds), 0);

    // File data: "X\0Y" (3 bytes) — contains embedded NUL
    std::string file_content("X\0Y", 3);
    std::string body =
        "--bnd\r\n"
        "Content-Disposition: form-data; name=\"file\"; filename=\"test.bin\"\r\n"
        "Content-Type: application/octet-stream\r\n"
        "\r\n" +
        file_content +
        "\r\n"
        "--bnd\r\n"
        "Content-Disposition: form-data; name=\"field1\"\r\n"
        "\r\n"
        "after_nul_field\r\n"
        "--bnd--\r\n";

    std::string req =
        "POST /upload HTTP/1.1\r\n"
        "Host: localhost\r\n"
        "Content-Type: multipart/form-data; boundary=bnd\r\n"
        "Content-Length: " + std::to_string(body.size()) + "\r\n"
        "\r\n";
    req += body;
    send_raw_and_close(fds[1], req.data(), req.size());

    auto *handle = (TcpStreamHandle *)malloc(sizeof(TcpStreamHandle));
    handle->fd = fds[0];
    void *result = __ry_http_read_request(handle);
    ASSERT_NE(result, nullptr);

    // memmem (not strstr) allows boundary search past NUL bytes
    EXPECT_STREQ(__ry_http_form_field(result, "field1"), "after_nul_field");

    // The file should also be found
    auto *file_map = (MapHeader *)__ry_http_form_file(result, "file");
    ASSERT_NE(file_map, nullptr);
    EXPECT_EQ(file_map->len, 3);

    const char *data = nullptr;
    for (int64_t i = 0; i < file_map->len; i++) {
        if (strcmp(file_map->keys[i], "data") == 0) data = file_map->vals[i];
    }
    ASSERT_NE(data, nullptr);
    EXPECT_EQ(data[0], 'X');

    for (int64_t i = 0; i < file_map->len; i++) {
        free(file_map->keys[i]);
        free(file_map->vals[i]);
    }
    free(file_map->keys);
    free(file_map->vals);
    free(file_map->buckets);
    free(file_map);

    __ry_http_request_free(result);
    ::close(fds[0]);
    free(handle);
}

TEST_F(RuntimeHttpClientTest, ClientResponseBodyWithNulByte) {
    // Server sends response with body containing a NUL byte.
    // Body: "he\0lo" (5 bytes)
    std::string body_bytes("he\0lo", 5);
    std::string response =
        "HTTP/1.1 200 OK\r\n"
        "Content-Length: 5\r\n"
        "\r\n" +
        body_bytes;

    int srv = start_mock_server();
    if (srv < 0) GTEST_SKIP() << "could not create mock server (network unavailable)";
    int port = get_server_port(srv);

    std::thread server_thread([&]() {
        struct sockaddr_in client_addr{};
        socklen_t client_len = sizeof(client_addr);
        int conn = ::accept(srv, (struct sockaddr *)&client_addr, &client_len);
        // Send raw bytes including NUL
        size_t sent = 0;
        while (sent < response.size()) {
            ssize_t w = ::write(conn, response.data() + sent, response.size() - sent);
            if (w <= 0) break;
            sent += (size_t)w;
        }
        ::close(conn);
    });
    JoinGuard jg(server_thread);

    std::string url = "http://127.0.0.1:" + std::to_string(port) + "/";
    void *resp = __ry_http_get(url.c_str());
    ASSERT_NE(resp, nullptr);
    EXPECT_EQ(__ry_http_client_status(resp), 200);

    const char *body = __ry_http_client_body(resp);
    EXPECT_STREQ(body, "he");  // C-string truncation at NUL

    // body_bytes preserves the full binary content including NUL
    void *bytes = __ry_http_client_body_bytes(resp);
    ASSERT_NE(bytes, nullptr);
    auto *header = (IOListHeader *)bytes;
    EXPECT_EQ(header->len, 5);
    EXPECT_EQ(header->data[0], 'h');
    EXPECT_EQ(header->data[1], 'e');
    EXPECT_EQ(header->data[2], '\0');
    EXPECT_EQ(header->data[3], 'l');
    EXPECT_EQ(header->data[4], 'o');
    free(header->data);
    arc_free(header);

    __ry_http_client_response_free(resp);
    ::close(srv);
}

TEST_F(RuntimeHttpClientTest, ClientBodyBytesEmptyBody) {
    // Server sends response with empty body.
    std::string response =
        "HTTP/1.1 200 OK\r\n"
        "Content-Length: 0\r\n"
        "\r\n";

    int srv = start_mock_server();
    if (srv < 0) GTEST_SKIP() << "could not create mock server (network unavailable)";
    int port = get_server_port(srv);

    std::thread server_thread([&]() {
        struct sockaddr_in client_addr{};
        socklen_t client_len = sizeof(client_addr);
        int conn = ::accept(srv, (struct sockaddr *)&client_addr, &client_len);
        ::write(conn, response.data(), response.size());
        ::close(conn);
    });
    JoinGuard jg(server_thread);

    std::string url = "http://127.0.0.1:" + std::to_string(port) + "/";
    void *resp = __ry_http_get(url.c_str());
    ASSERT_NE(resp, nullptr);

    void *bytes = __ry_http_client_body_bytes(resp);
    ASSERT_NE(bytes, nullptr);
    auto *header = (IOListHeader *)bytes;
    EXPECT_EQ(header->len, 0);
    free(header->data);
    arc_free(header);

    __ry_http_client_response_free(resp);
    ::close(srv);
}

TEST(RuntimeHttp, FormFileDefaultContentType) {
    int fds[2];
    ASSERT_EQ(socketpair(AF_UNIX, SOCK_STREAM, 0, fds), 0);

    std::string body =
        "--bnd\r\n"
        "Content-Disposition: form-data; name=\"file\"; filename=\"data.bin\"\r\n"
        "\r\n"
        "binary stuff\r\n"
        "--bnd--\r\n";

    std::string req =
        "POST /upload HTTP/1.1\r\n"
        "Host: localhost\r\n"
        "Content-Type: multipart/form-data; boundary=bnd\r\n"
        "Content-Length: " + std::to_string(body.size()) + "\r\n"
        "\r\n" + body;
    send_and_close(fds[1], req);

    auto *handle = (TcpStreamHandle *)malloc(sizeof(TcpStreamHandle));
    handle->fd = fds[0];
    void *result = __ry_http_read_request(handle);
    ASSERT_NE(result, nullptr);

    auto *map = (MapHeader *)__ry_http_form_file(result, "file");
    ASSERT_NE(map, nullptr);
    const char *ct = nullptr;
    for (int64_t i = 0; i < map->len; i++) {
        if (strcmp(map->keys[i], "content_type") == 0) ct = map->vals[i];
    }
    EXPECT_STREQ(ct, "application/octet-stream");

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

// --- Response header CRLF injection tests ---

// Helper to build a MapHeader for response CRLF tests
static MapHeader *build_response_headers(
    std::initializer_list<std::pair<const char *, const char *>> entries) {
    auto *map = (MapHeader *)malloc(sizeof(MapHeader));
    auto count = (int64_t)entries.size();
    map->len = count;
    map->cap = count;
    map->keys = count > 0 ? (char **)malloc(sizeof(char *) * (size_t)count) : nullptr;
    map->vals = count > 0 ? (char **)malloc(sizeof(char *) * (size_t)count) : nullptr;
    map->bucket_count = 4;
    map->buckets = calloc(4, sizeof(int64_t));
    int64_t i = 0;
    for (auto &[k, v] : entries) {
        map->keys[i] = strdup(k);
        map->vals[i] = strdup(v);
        i++;
    }
    return map;
}

static void free_response_headers(MapHeader *map) {
    for (int64_t i = 0; i < map->len; i++) {
        free(map->keys[i]);
        free(map->vals[i]);
    }
    free(map->keys);
    free(map->vals);
    free(map->buckets);
    free(map);
}

TEST(RuntimeHttp, ResponseHeaderCRLFInKeyIsSkipped) {
    auto *map = build_response_headers({
        {"X-Safe", "ok"},
        {"X-Evil\r\nInjected: bad", "value"},
        {"X-Also-Safe", "fine"},
    });

    void *resp_ptr = __ry_http_response_create(200, map, "body");
    auto *resp = (HttpResponseHandle *)resp_ptr;

    ASSERT_EQ(resp->header_count, 2);
    EXPECT_STREQ(resp->header_keys[0], "X-Safe");
    EXPECT_STREQ(resp->header_values[0], "ok");
    EXPECT_STREQ(resp->header_keys[1], "X-Also-Safe");
    EXPECT_STREQ(resp->header_values[1], "fine");

    __ry_http_response_free(resp_ptr);
    free_response_headers(map);
}

TEST(RuntimeHttp, ResponseHeaderCRLFInValueIsSkipped) {
    auto *map = build_response_headers({
        {"X-Safe", "ok"},
        {"X-Inject", "val\r\nEvil-Header: injected"},
        {"X-Also-Safe", "fine"},
    });

    void *resp_ptr = __ry_http_response_create(200, map, "body");
    auto *resp = (HttpResponseHandle *)resp_ptr;

    ASSERT_EQ(resp->header_count, 2);
    EXPECT_STREQ(resp->header_keys[0], "X-Safe");
    EXPECT_STREQ(resp->header_keys[1], "X-Also-Safe");

    __ry_http_response_free(resp_ptr);
    free_response_headers(map);
}

TEST(RuntimeHttp, ResponseHeaderLFOnlyIsSkipped) {
    auto *map = build_response_headers({
        {"X-LF-Key\n", "val"},
        {"X-Normal", "good\ninjection"},
        {"X-Clean", "safe"},
    });

    void *resp_ptr = __ry_http_response_create(200, map, "body");
    auto *resp = (HttpResponseHandle *)resp_ptr;

    ASSERT_EQ(resp->header_count, 1);
    EXPECT_STREQ(resp->header_keys[0], "X-Clean");
    EXPECT_STREQ(resp->header_values[0], "safe");

    __ry_http_response_free(resp_ptr);
    free_response_headers(map);
}

TEST(RuntimeHttp, ResponseHeaderSafeHeadersUnaffected) {
    auto *map = build_response_headers({
        {"Content-Type", "text/html"},
        {"X-Custom", "value123"},
        {"Cache-Control", "no-cache"},
    });

    void *resp_ptr = __ry_http_response_create(200, map, "body");
    auto *resp = (HttpResponseHandle *)resp_ptr;

    ASSERT_EQ(resp->header_count, 3);
    EXPECT_STREQ(resp->header_keys[0], "Content-Type");
    EXPECT_STREQ(resp->header_values[0], "text/html");
    EXPECT_STREQ(resp->header_keys[1], "X-Custom");
    EXPECT_STREQ(resp->header_values[1], "value123");
    EXPECT_STREQ(resp->header_keys[2], "Cache-Control");
    EXPECT_STREQ(resp->header_values[2], "no-cache");

    __ry_http_response_free(resp_ptr);
    free_response_headers(map);
}
