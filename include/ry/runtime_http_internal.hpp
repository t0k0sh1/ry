#pragma once

// Internal shared header for runtime_http*.cpp files that need network transport.
// Includes OpenSSL and HttpTransport. For code that only needs struct definitions
// and helpers (e.g., multipart parsing), use runtime_http_types.hpp instead.

#include "ry/runtime_http_types.hpp"

#include <sys/socket.h>
#include <unistd.h>

#include <openssl/ssl.h>

#include "ry/runtime_net.hpp"
#include "ry/runtime_tls.hpp"

// ---------------------------------------------------------------------------
// Constants (transport-specific)
// ---------------------------------------------------------------------------
inline constexpr int MAX_REDIRECTS = 10;

// ---------------------------------------------------------------------------
// Network transport abstraction
// ---------------------------------------------------------------------------

struct HttpTransport {
    int fd;
    SSL *ssl;  // nullptr for plain TCP

    ssize_t do_recv(void *buf, size_t len) {
        if (ssl) {
            int n = SSL_read(ssl, buf, (int)len);
            if (n > 0) return (ssize_t)n;
            int err = SSL_get_error(ssl, n);
            if (err == SSL_ERROR_WANT_READ || err == SSL_ERROR_WANT_WRITE)
                return 0;
            if (err == SSL_ERROR_ZERO_RETURN)
                return 0;
            return -1;
        }
        return ::recv(fd, buf, len, 0);
    }

    ssize_t do_send(const void *buf, size_t len) {
        if (ssl) return __ry_tls_send_all(ssl, buf, len);
        return __ry_send_all(fd, buf, len);
    }

    void close_transport() {
        if (ssl) {
            SSL_shutdown(ssl);
            SSL_free(ssl);
            ssl = nullptr;
        }
        if (fd >= 0) {
            ::close(fd);
            fd = -1;
        }
    }
};

// ---------------------------------------------------------------------------
// Network I/O helpers
// ---------------------------------------------------------------------------

std::string recv_all(HttpTransport &t, size_t max_bytes);
bool recv_into_buf(HttpTransport &t, std::string &buf);

// Legacy overloads for server-side code that still uses raw fd
std::string recv_all(int fd, size_t max_bytes);
bool recv_into_buf(int fd, std::string &buf);

// ---------------------------------------------------------------------------
// Transfer-Encoding helpers
// ---------------------------------------------------------------------------

bool te_value_is_chunked(const char *value);

template <typename GetKey>
inline bool has_transfer_encoding_impl(int64_t count, GetKey get_key) {
    for (int64_t i = 0; i < count; i++) {
        if (strcasecmp(get_key(i), "Transfer-Encoding") == 0)
            return true;
    }
    return false;
}

template <typename GetKey, typename GetVal>
inline bool has_chunked_encoding_impl(int64_t count, GetKey get_key, GetVal get_val) {
    for (int64_t i = 0; i < count; i++) {
        if (strcasecmp(get_key(i), "Transfer-Encoding") == 0 &&
            te_value_is_chunked(get_val(i)))
            return true;
    }
    return false;
}

bool has_transfer_encoding(char **keys, int64_t count);
bool has_transfer_encoding(const std::vector<HeaderPair> &headers);
bool has_chunked_encoding(char **keys, char **values, int64_t count);
bool has_chunked_encoding(const std::vector<HeaderPair> &headers);

std::string read_chunked_body(HttpTransport &t, std::string &buf, size_t pos, bool &ok);
std::string read_chunked_body(int fd, std::string &buf, size_t pos, bool &ok);
