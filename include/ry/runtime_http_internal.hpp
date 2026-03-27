#pragma once

// Internal shared header for runtime_http*.cpp files.
// NOT part of the public API — do not include from codegen or other modules.

#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <string>
#include <strings.h>
#include <vector>

#include <sys/socket.h>
#include <unistd.h>

#include <openssl/ssl.h>

#include "ry/runtime_net.hpp"
#include "ry/runtime_tls.hpp"

// Forward declarations for runtime functions used by HTTP code
extern "C" {
int64_t __ry_http_parse_content_length(const char *value);
int64_t *__ry_ht_rehash_str(const char **keys, int64_t count,
                             int64_t newBucketCount);
}

// ---------------------------------------------------------------------------
// Constants
// ---------------------------------------------------------------------------
inline constexpr int64_t MAX_BODY_SIZE = 10 * 1024 * 1024; // 10 MB
inline constexpr int MAX_REDIRECTS = 10;
inline constexpr size_t kRecvBufSize = 4096;
inline constexpr size_t kMaxHeaderSize = 8192;

// ---------------------------------------------------------------------------
// Struct definitions — layouts must match codegen expectations
// ---------------------------------------------------------------------------

struct HttpRequestHandle {
    char *method;
    char *path;
    char **header_keys;
    char **header_values;
    int64_t header_count;
    char *body;
    int64_t body_len;
    char **query_keys;
    char **query_values;
    int64_t query_count;
    char **cookie_keys;
    char **cookie_values;
    int64_t cookie_count;
    // Multipart form data (parsed lazily on first access)
    bool form_parsed;
    char **form_field_keys;
    char **form_field_values;
    int64_t form_field_count;
    char **form_file_keys;
    char **form_file_filenames;
    char **form_file_types;
    char **form_file_data;
    int64_t *form_file_data_lens;
    int64_t form_file_count;
};

struct HttpResponseHandle {
    int64_t status;
    char **header_keys;
    char **header_values;
    int64_t header_count;
    char *body;
    int64_t body_len;
};

// MapHeader layout must match codegen: {len, cap, keys, vals, bucket_count, buckets}
struct MapHeader {
    int64_t len;
    int64_t cap;
    char **keys;
    char **vals;
    int64_t bucket_count;
    void *buckets;
};

struct HeaderPair { char *key; char *val; };

struct HttpClientResponseHandle {
    int64_t status;
    char **header_keys;
    char **header_values;
    int64_t header_count;
    char *body;
    int64_t body_len;
};

struct ParsedUrl {
    char *host;
    int64_t port;
    char *path;
    bool is_https;
};

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
// OOM-safe allocation helpers
// ---------------------------------------------------------------------------

[[noreturn]] inline void oom_abort() {
    fprintf(stderr, "ry: out of memory\n");
    abort();
}

inline char *checked_strndup(const char *s, size_t n) {
    char *r = strndup(s, n);
    if (!r) oom_abort();
    return r;
}

inline char *checked_strdup(const char *s) {
    char *r = strdup(s);
    if (!r) oom_abort();
    return r;
}

inline char *checked_memdup(const void *src, size_t len) {
    char *r = (char *)malloc(len + 1);
    if (!r) oom_abort();
    memcpy(r, src, len);
    r[len] = '\0';  // NUL-terminate for str compatibility
    return r;
}

inline void *checked_malloc(size_t n) {
    void *r = malloc(n);
    if (!r) oom_abort();
    return r;
}

// ---------------------------------------------------------------------------
// Header parsing helpers
// ---------------------------------------------------------------------------

std::vector<HeaderPair> parse_raw_headers(const std::string &raw,
                                           size_t start, size_t end);

inline void assign_headers(const std::vector<HeaderPair> &headers,
                           char ***keys_out, char ***values_out,
                           int64_t *count_out) {
    int64_t count = (int64_t)headers.size();
    *count_out = count;
    if (count > 0) {
        *keys_out = (char **)checked_malloc(sizeof(char *) * (size_t)count);
        *values_out = (char **)checked_malloc(sizeof(char *) * (size_t)count);
        for (int64_t i = 0; i < count; i++) {
            (*keys_out)[i] = headers[(size_t)i].key;
            (*values_out)[i] = headers[(size_t)i].val;
        }
    } else {
        *keys_out = nullptr;
        *values_out = nullptr;
    }
}

// Assign parallel key/value arrays from a vector, using accessor lambdas.
template <typename T, typename KeyFn, typename ValFn>
inline void assign_kv_pairs(const std::vector<T> &items,
                            char ***keys_out, char ***values_out,
                            int64_t *count_out,
                            KeyFn get_key, ValFn get_val) {
    auto count = (int64_t)items.size();
    *count_out = count;
    if (count > 0) {
        *keys_out = (char **)checked_malloc(sizeof(char *) * (size_t)count);
        *values_out = (char **)checked_malloc(sizeof(char *) * (size_t)count);
        for (size_t i = 0; i < items.size(); i++) {
            (*keys_out)[i] = get_key(items[i]);
            (*values_out)[i] = get_val(items[i]);
        }
    } else {
        *keys_out = nullptr;
        *values_out = nullptr;
    }
}

void *build_str_map(char **keys, char **vals, int64_t count);

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
