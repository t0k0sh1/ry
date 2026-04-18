#pragma once

// Lightweight internal header for runtime_http*.cpp files.
// Contains struct definitions, OOM helpers, and header parsing utilities.
// Does NOT include OpenSSL or network transport — use runtime_http_internal.hpp
// for code that needs HttpTransport or SSL.

#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <string>
#include <strings.h>
#include <vector>

#include "ry/runtime_alloc.hpp"
#include "ry/runtime_string.hpp"


namespace ry {

// Forward declarations for runtime functions used by HTTP code
extern "C" {
int64_t __ry_http_parse_content_length(const char *value);
int64_t *__ry_ht_rehash_str(const char **keys, int64_t count,
                             int64_t newBucketCount);
}

// ---------------------------------------------------------------------------
// Constants
// ---------------------------------------------------------------------------
inline constexpr int64_t MAX_BODY_SIZE = INT64_C(10) * 1024 * 1024; // 10 MB
inline constexpr size_t kRecvBufSize = 4096;
inline constexpr size_t kMaxHeaderSize = 8192;

// ---------------------------------------------------------------------------
// Struct definitions — layouts must match codegen expectations
// ---------------------------------------------------------------------------

struct FormFieldEntry {
    char *key;
    char *value;
};

struct FormFileEntry {
    char *name;
    char *filename;
    char *content_type;
    char *data;
    int64_t data_len;
};

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
    FormFieldEntry *form_fields;
    int64_t form_field_count;
    FormFileEntry *form_files;
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

// ---------------------------------------------------------------------------
// Parallel key/value array helpers
// ---------------------------------------------------------------------------

inline void free_kv_pairs(char **keys, char **vals, int64_t count) {
    for (int64_t i = 0; i < count; i++) {
        freeStringSlot(keys[i]);
        freeStringSlot(vals[i]);
    }
    free(keys);
    free(vals);
}

// Case-insensitive search in parallel key/value arrays
inline const char *find_in_kv_pairs_ci(char **keys, char **vals, int64_t count,
                                        const char *target) {
    for (int64_t i = 0; i < count; i++) {
        if (strcasecmp(keys[i], target) == 0)
            return vals[i];
    }
    return nullptr;
}

// Case-sensitive search in parallel key/value arrays
inline const char *find_in_kv_pairs(char **keys, char **vals, int64_t count,
                                     const char *target) {
    for (int64_t i = 0; i < count; i++) {
        if (strcmp(keys[i], target) == 0)
            return vals[i];
    }
    return nullptr;
}

// ---------------------------------------------------------------------------
// CRLF validation — used by both client and server paths to prevent
// HTTP request/response splitting via injected CR/LF in headers.
// ---------------------------------------------------------------------------

inline bool has_crlf(const char *s) {
    if (!s) return false;
    for (; *s; s++) {
        if (*s == '\r' || *s == '\n') return true;
    }
    return false;
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
void *build_str_map_copy(char **keys, char **vals, int64_t count);

} // namespace ry
