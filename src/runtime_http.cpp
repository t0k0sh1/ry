#include "ry/runtime_http.hpp"
#include "ry/runtime_net.hpp"

#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <strings.h>
#include <string>
#include <unordered_set>
#include <vector>
#include <sys/socket.h>
#include <sys/time.h>
#include <unistd.h>

static const int64_t MAX_BODY_SIZE = 10 * 1024 * 1024; // 10 MB
static const int MAX_REDIRECTS = 10;

// Forward declaration: TcpStreamHandle from runtime_net.cpp has fd at offset 0
struct TcpStreamHandle {
    int fd;
};

struct HttpRequestHandle {
    char *method;
    char *path;
    char **header_keys;
    char **header_values;
    int64_t header_count;
    char *body;
    char **query_keys;
    char **query_values;
    int64_t query_count;
    char **cookie_keys;
    char **cookie_values;
    int64_t cookie_count;
};

struct HttpResponseHandle {
    int64_t status;
    char **header_keys;
    char **header_values;
    int64_t header_count;
    char *body;
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

static std::vector<HeaderPair> parse_raw_headers(const std::string &raw,
                                                  size_t start, size_t end) {
    std::vector<HeaderPair> headers;
    size_t pos = start;
    while (pos < end) {
        size_t eol = raw.find("\r\n", pos);
        if (eol == std::string::npos || eol > end) eol = end;
        if (eol == pos) break;
        size_t colon = raw.find(':', pos);
        if (colon != std::string::npos && colon < eol) {
            size_t vstart = colon + 1;
            while (vstart < eol && (raw[vstart] == ' ' || raw[vstart] == '\t'))
                vstart++;
            headers.push_back({
                strndup(raw.c_str() + pos, colon - pos),
                strndup(raw.c_str() + vstart, eol - vstart)
            });
        }
        pos = eol + 2;
    }
    return headers;
}

static void assign_headers(const std::vector<HeaderPair> &headers,
                            char ***keys_out, char ***values_out,
                            int64_t *count_out) {
    int64_t count = (int64_t)headers.size();
    *count_out = count;
    if (count > 0) {
        *keys_out = (char **)malloc(sizeof(char *) * (size_t)count);
        *values_out = (char **)malloc(sizeof(char *) * (size_t)count);
        for (int64_t i = 0; i < count; i++) {
            (*keys_out)[i] = headers[(size_t)i].key;
            (*values_out)[i] = headers[(size_t)i].val;
        }
    } else {
        *keys_out = nullptr;
        *values_out = nullptr;
    }
}

// Forward declaration for hash table construction
extern "C" int64_t *__ry_ht_rehash_str(const char **keys, int64_t count,
                                        int64_t newBucketCount);

// Build a MapHeader by adopting the provided keys/vals arrays directly.
// The MapHeader takes ownership of both the arrays and the strings they point to.
// The caller must NOT free keys, vals, or their elements after this call.
static void *build_str_map(char **keys, char **vals, int64_t count) {
    auto *map = (MapHeader *)malloc(sizeof(MapHeader));
    map->len = count;
    map->cap = count;
    if (count > 0) {
        map->keys = keys;
        map->vals = vals;
        int64_t bc = 4;
        while (bc < count * 2) bc *= 2;
        map->bucket_count = bc;
        map->buckets = __ry_ht_rehash_str((const char **)map->keys, count, bc);
    } else {
        map->keys = nullptr;
        map->vals = nullptr;
        map->bucket_count = 4;
        map->buckets = __ry_ht_rehash_str(nullptr, 0, 4);
    }
    return map;
}

static int hex_digit(char c) {
    if (c >= '0' && c <= '9') return c - '0';
    if (c >= 'A' && c <= 'F') return c - 'A' + 10;
    if (c >= 'a' && c <= 'f') return c - 'a' + 10;
    return -1;
}

static std::string url_decode(const std::string &src) {
    std::string out;
    out.reserve(src.size());
    for (size_t i = 0; i < src.size(); i++) {
        if (src[i] == '+') {
            out += ' ';
        } else if (src[i] == '%' && i + 2 < src.size()) {
            int hi = hex_digit(src[i + 1]);
            int lo = hex_digit(src[i + 2]);
            if (hi >= 0 && lo >= 0) {
                char decoded = static_cast<char>(hi * 16 + lo);
                if (decoded == '\0') {
                    out += '%';
                    out += src[i + 1];
                    out += src[i + 2];
                } else {
                    out += decoded;
                }
                i += 2;
            } else {
                out += '%';
            }
        } else {
            out += src[i];
        }
    }
    return out;
}

static void parse_query_string(const std::string &qs, HttpRequestHandle *req) {
    if (qs.empty()) {
        req->query_keys = nullptr;
        req->query_values = nullptr;
        req->query_count = 0;
        return;
    }

    struct QParam { char *key; char *val; };
    std::vector<QParam> params;
    std::unordered_set<std::string> seen_keys;

    size_t pos = 0;
    while (pos < qs.size()) {
        size_t amp = qs.find('&', pos);
        if (amp == std::string::npos) amp = qs.size();
        if (amp > pos) {
            std::string pair = qs.substr(pos, amp - pos);
            size_t eq = pair.find('=');
            std::string key, val;
            if (eq != std::string::npos) {
                key = url_decode(pair.substr(0, eq));
                val = url_decode(pair.substr(eq + 1));
            } else {
                key = url_decode(pair);
            }
            // First-value-wins for duplicate keys
            if (seen_keys.insert(key).second)
                params.push_back({strdup(key.c_str()), strdup(val.c_str())});
        }
        pos = amp + 1;
    }

    int64_t count = (int64_t)params.size();
    req->query_count = count;
    if (count > 0) {
        req->query_keys = (char **)malloc(sizeof(char *) * (size_t)count);
        req->query_values = (char **)malloc(sizeof(char *) * (size_t)count);
        for (int64_t i = 0; i < count; i++) {
            req->query_keys[i] = params[(size_t)i].key;
            req->query_values[i] = params[(size_t)i].val;
        }
    } else {
        req->query_keys = nullptr;
        req->query_values = nullptr;
    }
}

// Parse the Cookie header value and store results in req->cookie_keys/values/count.
// Cookie format: "name1=value1; name2=value2; ..."
// Only the first '=' splits name from value (values may contain '=').
static void parse_cookie_header(HttpRequestHandle *req) {
    // Find Cookie header (case-insensitive)
    const char *cookie_str = nullptr;
    for (int64_t i = 0; i < req->header_count; i++) {
        if (strcasecmp(req->header_keys[i], "Cookie") == 0) {
            cookie_str = req->header_values[i];
            break;
        }
    }
    if (!cookie_str || !*cookie_str) {
        req->cookie_keys = nullptr;
        req->cookie_values = nullptr;
        req->cookie_count = 0;
        return;
    }

    struct CookiePair { char *key; char *val; };
    std::vector<CookiePair> pairs;
    std::unordered_set<std::string> seen_keys;

    const char *p = cookie_str;
    while (*p) {
        while (*p == ' ' || *p == '\t') p++;
        if (!*p) break;

        const char *semi = strchr(p, ';');
        size_t pair_len = semi ? (size_t)(semi - p) : strlen(p);

        const char *eq = (const char *)memchr(p, '=', pair_len);
        if (eq) {
            const char *key_end = eq;
            while (key_end > p && (*(key_end - 1) == ' ' || *(key_end - 1) == '\t'))
                key_end--;
            size_t key_len = (size_t)(key_end - p);

            const char *val_start = eq + 1;
            const char *val_end = p + pair_len;
            while (val_start < val_end && (*val_start == ' ' || *val_start == '\t'))
                val_start++;
            while (val_end > val_start && (*(val_end - 1) == ' ' || *(val_end - 1) == '\t'))
                val_end--;
            size_t val_len = (size_t)(val_end - val_start);

            if (key_len > 0) {
                std::string key_s(p, key_len);
                if (seen_keys.insert(key_s).second)
                    pairs.push_back({strndup(p, key_len), strndup(val_start, val_len)});
            }
        }

        p += pair_len;
        if (*p == ';') p++;
    }

    int64_t count = (int64_t)pairs.size();
    req->cookie_count = count;
    if (count > 0) {
        req->cookie_keys = (char **)malloc(sizeof(char *) * (size_t)count);
        req->cookie_values = (char **)malloc(sizeof(char *) * (size_t)count);
        for (int64_t i = 0; i < count; i++) {
            req->cookie_keys[i] = pairs[(size_t)i].key;
            req->cookie_values[i] = pairs[(size_t)i].val;
        }
    } else {
        req->cookie_keys = nullptr;
        req->cookie_values = nullptr;
    }
}

extern "C" int64_t __ry_http_parse_content_length(const char *value) {
    if (!value) return -1;

    // Skip leading whitespace
    const char *p = value;
    while (*p == ' ' || *p == '\t') p++;

    // Must start with a digit
    if (*p < '0' || *p > '9') return -2;

    char *endptr = nullptr;
    long long parsed = strtoll(p, &endptr, 10);

    // Reject trailing non-whitespace characters
    if (endptr) {
        const char *q = endptr;
        while (*q == ' ' || *q == '\t') q++;
        if (*q != '\0') return -2;
    }

    if (parsed < 0) return -2;
    if (parsed > MAX_BODY_SIZE) return -3;
    return (int64_t)parsed;
}

static std::string recv_all(int fd, size_t max_bytes) {
    std::string buf;
    buf.resize(max_bytes);
    size_t total = 0;
    while (total < max_bytes) {
        ssize_t n = ::recv(fd, &buf[total], max_bytes - total, 0);
        if (n <= 0) break;
        total += (size_t)n;
        // Search for header/body boundary starting near the new data
        size_t search_from = (total > n + 3) ? total - (size_t)n - 3 : 0;
        if (buf.find("\r\n\r\n", search_from) != std::string::npos)
            break;
    }
    buf.resize(total);
    return buf;
}

// Append data from fd into buf. Returns false on connection close/error.
static bool recv_into_buf(int fd, std::string &buf) {
    char tmp[4096];
    ssize_t n = ::recv(fd, tmp, sizeof(tmp), 0);
    if (n <= 0) return false;
    buf.append(tmp, (size_t)n);
    return true;
}

// Check if a Transfer-Encoding value has "chunked" as the last coding.
// Handles comma-separated lists (e.g. "gzip, chunked") per RFC 9112.
static bool te_value_is_chunked(const char *value) {
    // Find the last non-whitespace token after the last comma
    const char *last_token = value;
    const char *p = value;
    while (*p) {
        // Skip whitespace
        while (*p == ' ' || *p == '\t') p++;
        if (*p == '\0') break;
        last_token = p;
        // Advance to next comma or end
        while (*p && *p != ',') p++;
        if (*p == ',') p++;
    }
    // Compare last token, ignoring trailing whitespace
    size_t len = strlen(last_token);
    // Trim trailing whitespace
    while (len > 0 && (last_token[len - 1] == ' ' || last_token[len - 1] == '\t'))
        len--;
    return len == 7 && strncasecmp(last_token, "chunked", 7) == 0;
}

// Check if Transfer-Encoding header is present (regardless of value).
static bool has_transfer_encoding(char **keys, int64_t count) {
    for (int64_t i = 0; i < count; i++) {
        if (strcasecmp(keys[i], "Transfer-Encoding") == 0)
            return true;
    }
    return false;
}

static bool has_transfer_encoding(const std::vector<HeaderPair> &headers) {
    for (auto &h : headers) {
        if (strcasecmp(h.key, "Transfer-Encoding") == 0)
            return true;
    }
    return false;
}

static bool has_chunked_encoding(char **keys, char **values, int64_t count) {
    for (int64_t i = 0; i < count; i++) {
        if (strcasecmp(keys[i], "Transfer-Encoding") == 0 &&
            te_value_is_chunked(values[i]))
            return true;
    }
    return false;
}

static bool has_chunked_encoding(const std::vector<HeaderPair> &headers) {
    for (auto &h : headers) {
        if (strcasecmp(h.key, "Transfer-Encoding") == 0 &&
            te_value_is_chunked(h.val))
            return true;
    }
    return false;
}

// Decode a chunked transfer-encoded body from fd.
// `buf` is the read buffer (may already contain data after headers);
// `pos` is the current offset into buf.
// Sets `ok` to false on parse error or size overflow.
static std::string read_chunked_body(int fd, std::string &buf, size_t pos, bool &ok) {
    ok = true;
    std::string body;

    auto ensure = [&](size_t needed) -> bool {
        while (buf.size() - pos < needed) {
            if (!recv_into_buf(fd, buf)) return false;
        }
        return true;
    };

    // Max chunk-size line length to prevent DoS from extremely long lines without CRLF
    static const size_t MAX_CHUNK_LINE = 4096;

    while (true) {
        size_t crlf;
        while (true) {
            crlf = buf.find("\r\n", pos);
            if (crlf != std::string::npos) break;
            if (buf.size() - pos > MAX_CHUNK_LINE) { ok = false; return ""; }
            if (!recv_into_buf(fd, buf)) { ok = false; return ""; }
        }

        // Parse hex chunk size, ignoring extensions after ';'
        const char *size_start = buf.c_str() + pos;
        size_t size_len = crlf - pos;
        const char *semi = (const char *)memchr(size_start, ';', size_len);
        if (semi) size_len = (size_t)(semi - size_start);

        char *endptr = nullptr;
        unsigned long long chunk_size = strtoull(size_start, &endptr, 16);
        if (!endptr || endptr == size_start) { ok = false; return ""; }
        // Reject trailing non-whitespace after hex digits (RFC 9112: chunk-size = 1*HEXDIG)
        for (const char *t = endptr; t < size_start + size_len; t++) {
            if (*t != ' ' && *t != '\t') { ok = false; return ""; }
        }

        pos = crlf + 2;

        if (chunk_size == 0) {
            // Consume optional trailers and final CRLF after terminal chunk.
            // Trailers end with an empty line (\r\n), so we read until \r\n\r\n.
            while (true) {
                size_t end = buf.find("\r\n", pos);
                if (end == std::string::npos) {
                    if (!recv_into_buf(fd, buf)) break;
                    continue;
                }
                if (end == pos) { pos += 2; break; } // empty line = end of trailers
                pos = end + 2; // skip trailer line
            }
            break;
        }

        // Reject chunk sizes that would exceed MAX_BODY_SIZE (also prevents overflow)
        if (chunk_size > (unsigned long long)MAX_BODY_SIZE ||
            (int64_t)(body.size() + chunk_size) > MAX_BODY_SIZE) {
            ok = false; return "";
        }

        if (!ensure((size_t)chunk_size + 2)) { ok = false; return ""; }
        body.append(buf, pos, (size_t)chunk_size);
        pos += (size_t)chunk_size;

        if (buf[pos] != '\r' || buf[pos + 1] != '\n') { ok = false; return ""; }
        pos += 2;

        // Discard consumed data to avoid unbounded buffer growth
        if (pos > 8192) {
            buf.erase(0, pos);
            pos = 0;
        }
    }

    return body;
}

extern "C" void *__ry_http_read_request(void *stream) {
    auto *handle = (TcpStreamHandle *)stream;
    struct timeval tv = {5, 0};
    ::setsockopt(handle->fd, SOL_SOCKET, SO_RCVTIMEO, &tv, sizeof(tv));
    std::string raw = recv_all(handle->fd, 8192);
    if (raw.empty()) return nullptr;

    auto *req = (HttpRequestHandle *)malloc(sizeof(HttpRequestHandle));
    memset(req, 0, sizeof(HttpRequestHandle));

    // Parse request line: METHOD PATH HTTP/x.x\r\n
    size_t line_end = raw.find("\r\n");
    if (line_end == std::string::npos) {
        free(req);
        return nullptr;
    }
    std::string request_line = raw.substr(0, line_end);

    // Parse method
    size_t sp1 = request_line.find(' ');
    if (sp1 == std::string::npos) {
        free(req);
        return nullptr;
    }
    std::string method = request_line.substr(0, sp1);
    req->method = strdup(method.c_str());

    // Parse path and query string
    size_t sp2 = request_line.find(' ', sp1 + 1);
    std::string full_path;
    if (sp2 != std::string::npos)
        full_path = request_line.substr(sp1 + 1, sp2 - sp1 - 1);
    else
        full_path = request_line.substr(sp1 + 1);

    size_t qmark = full_path.find('?');
    if (qmark != std::string::npos) {
        req->path = strdup(full_path.substr(0, qmark).c_str());
        parse_query_string(full_path.substr(qmark + 1), req);
    } else {
        req->path = strdup(full_path.c_str());
        parse_query_string("", req);
    }

    // Parse headers
    size_t headers_start = line_end + 2;
    size_t headers_end = raw.find("\r\n\r\n", headers_start);
    if (headers_end == std::string::npos) {
        __ry_http_request_free(req);
        return nullptr;
    }

    // Parse headers using shared helper
    auto parsed_headers = parse_raw_headers(raw, headers_start, headers_end);
    assign_headers(parsed_headers, &req->header_keys, &req->header_values, &req->header_count);

    // Pre-parse Cookie header into req->cookie_keys/values/count
    parse_cookie_header(req);

    size_t body_start = headers_end + 4;
    const char *cl_value = nullptr;
    for (int64_t i = 0; i < req->header_count; i++) {
        if (strcasecmp(req->header_keys[i], "Content-Length") == 0) {
            cl_value = req->header_values[i];
            break;
        }
    }
    bool has_te = has_transfer_encoding(req->header_keys, req->header_count);
    bool is_chunked = has_te && has_chunked_encoding(
        req->header_keys, req->header_values, req->header_count);

    // RFC 9112 §6.1: reject if both Transfer-Encoding and Content-Length are present
    if (is_chunked && cl_value != nullptr) {
        __ry_http_request_free(req);
        return nullptr;
    }

    // Reject unsupported transfer codings (e.g. gzip without chunked)
    if (has_te && !is_chunked) {
        __ry_http_request_free(req);
        return nullptr;
    }

    if (is_chunked) {
        // Chunked transfer encoding
        bool ok;
        std::string body_data = read_chunked_body(handle->fd, raw, body_start, ok);
        if (!ok) {
            __ry_http_request_free(req);
            return nullptr;
        }
        req->body = strdup(body_data.c_str());
        return req;
    }

    // Content-Length based body parsing
    int64_t content_length = __ry_http_parse_content_length(cl_value);
    if (content_length == -2 || content_length == -3) {
        __ry_http_request_free(req);
        return nullptr;
    }

    std::string body_data;
    if (body_start < raw.size())
        body_data = raw.substr(body_start);

    if (content_length > 0 && (int64_t)body_data.size() < content_length) {
        size_t remaining = (size_t)content_length - body_data.size();
        char *extra = (char *)malloc(remaining);
        size_t got = 0;
        while (got < remaining) {
            ssize_t n = ::recv(handle->fd, extra + got, remaining - got, 0);
            if (n <= 0) break;
            got += (size_t)n;
        }
        body_data.append(extra, got);
        free(extra);

        if ((int64_t)body_data.size() < content_length) {
            __ry_http_request_free(req);
            return nullptr;
        }
    }

    if (content_length >= 0 && (int64_t)body_data.size() > content_length)
        body_data.resize((size_t)content_length);

    req->body = strdup(body_data.c_str());

    return req;
}

extern "C" const char *__ry_http_method(void *r) {
    auto *req = (HttpRequestHandle *)r;
    return req->method;
}

extern "C" const char *__ry_http_path(void *r) {
    auto *req = (HttpRequestHandle *)r;
    return req->path;
}

extern "C" const char *__ry_http_header(void *r, const char *key) {
    auto *req = (HttpRequestHandle *)r;
    for (int64_t i = 0; i < req->header_count; i++) {
        if (strcasecmp(req->header_keys[i], key) == 0)
            return req->header_values[i];
    }
    return nullptr;
}

extern "C" const char *__ry_http_body(void *r) {
    auto *req = (HttpRequestHandle *)r;
    return req->body;
}

extern "C" const char *__ry_http_query(void *r, const char *key) {
    auto *req = (HttpRequestHandle *)r;
    for (int64_t i = 0; i < req->query_count; i++) {
        if (strcmp(req->query_keys[i], key) == 0)
            return req->query_values[i];
    }
    return nullptr;
}

extern "C" void *__ry_http_query_all(void *r) {
    auto *req = (HttpRequestHandle *)r;
    char **dup_keys = nullptr;
    char **dup_vals = nullptr;
    if (req->query_count > 0) {
        dup_keys = (char **)malloc(sizeof(char *) * (size_t)req->query_count);
        dup_vals = (char **)malloc(sizeof(char *) * (size_t)req->query_count);
        for (int64_t i = 0; i < req->query_count; i++) {
            dup_keys[i] = strdup(req->query_keys[i]);
            dup_vals[i] = strdup(req->query_values[i]);
        }
    }
    return build_str_map(dup_keys, dup_vals, req->query_count);
}

extern "C" const char *__ry_http_cookie(void *r, const char *name) {
    auto *req = (HttpRequestHandle *)r;
    for (int64_t i = 0; i < req->cookie_count; i++) {
        if (strcmp(req->cookie_keys[i], name) == 0)
            return req->cookie_values[i];
    }
    return nullptr;
}

extern "C" void *__ry_http_cookies(void *r) {
    auto *req = (HttpRequestHandle *)r;
    char **dup_keys = nullptr;
    char **dup_vals = nullptr;
    if (req->cookie_count > 0) {
        dup_keys = (char **)malloc(sizeof(char *) * (size_t)req->cookie_count);
        dup_vals = (char **)malloc(sizeof(char *) * (size_t)req->cookie_count);
        for (int64_t i = 0; i < req->cookie_count; i++) {
            dup_keys[i] = strdup(req->cookie_keys[i]);
            dup_vals[i] = strdup(req->cookie_values[i]);
        }
    }
    return build_str_map(dup_keys, dup_vals, req->cookie_count);
}

extern "C" void *__ry_http_response_create(int64_t status, void *headers_map, const char *body) {
    auto *resp = (HttpResponseHandle *)malloc(sizeof(HttpResponseHandle));
    resp->status = status;
    resp->body = strdup(body ? body : "");

    auto *map = (MapHeader *)headers_map;
    resp->header_count = map->len;
    if (map->len > 0) {
        resp->header_keys = (char **)malloc(sizeof(char *) * (size_t)map->len);
        resp->header_values = (char **)malloc(sizeof(char *) * (size_t)map->len);
        for (int64_t i = 0; i < map->len; i++) {
            resp->header_keys[i] = strdup(map->keys[i]);
            resp->header_values[i] = strdup(map->vals[i]);
        }
    } else {
        resp->header_keys = nullptr;
        resp->header_values = nullptr;
    }

    return resp;
}

extern "C" const char *__ry_http_reason_phrase(int64_t status) {
    switch (status) {
        case 100: return "Continue";
        case 101: return "Switching Protocols";
        case 200: return "OK";
        case 201: return "Created";
        case 202: return "Accepted";
        case 203: return "Non-Authoritative Information";
        case 204: return "No Content";
        case 205: return "Reset Content";
        case 206: return "Partial Content";
        case 300: return "Multiple Choices";
        case 301: return "Moved Permanently";
        case 302: return "Found";
        case 303: return "See Other";
        case 304: return "Not Modified";
        case 307: return "Temporary Redirect";
        case 308: return "Permanent Redirect";
        case 400: return "Bad Request";
        case 401: return "Unauthorized";
        case 402: return "Payment Required";
        case 403: return "Forbidden";
        case 404: return "Not Found";
        case 405: return "Method Not Allowed";
        case 406: return "Not Acceptable";
        case 407: return "Proxy Authentication Required";
        case 408: return "Request Timeout";
        case 409: return "Conflict";
        case 410: return "Gone";
        case 411: return "Length Required";
        case 412: return "Precondition Failed";
        case 413: return "Content Too Large";
        case 414: return "URI Too Long";
        case 415: return "Unsupported Media Type";
        case 416: return "Range Not Satisfiable";
        case 417: return "Expectation Failed";
        case 418: return "I'm a teapot";
        case 421: return "Misdirected Request";
        case 422: return "Unprocessable Content";
        case 425: return "Too Early";
        case 426: return "Upgrade Required";
        case 428: return "Precondition Required";
        case 429: return "Too Many Requests";
        case 431: return "Request Header Fields Too Large";
        case 451: return "Unavailable For Legal Reasons";
        case 500: return "Internal Server Error";
        case 501: return "Not Implemented";
        case 502: return "Bad Gateway";
        case 503: return "Service Unavailable";
        case 504: return "Gateway Timeout";
        case 505: return "HTTP Version Not Supported";
        case 507: return "Insufficient Storage";
        case 508: return "Loop Detected";
        case 511: return "Network Authentication Required";
        default: return "Unknown";
    }
}

extern "C" void __ry_http_send_response(void *stream, void *response) {
    auto *handle = (TcpStreamHandle *)stream;
    auto *resp = (HttpResponseHandle *)response;

    const char *reason = __ry_http_reason_phrase(resp->status);

    // Estimate response size to avoid repeated reallocations
    size_t body_len = strlen(resp->body);
    size_t estimated = 64 + body_len; // status line + CRLF overhead
    for (int64_t i = 0; i < resp->header_count; i++)
        estimated += strlen(resp->header_keys[i]) + strlen(resp->header_values[i]) + 4;

    std::string out;
    out.reserve(estimated);
    out += "HTTP/1.1 ";
    out += std::to_string(resp->status);
    out += ' ';
    out += reason;
    out += "\r\n";

    bool has_content_length = false;
    bool is_chunked = has_chunked_encoding(
        resp->header_keys, resp->header_values, resp->header_count);
    for (int64_t i = 0; i < resp->header_count; i++) {
        if (strcasecmp(resp->header_keys[i], "Content-Length") == 0) {
            has_content_length = true;
            if (is_chunked) continue; // suppress Content-Length when using chunked
        }
        out += resp->header_keys[i];
        out += ": ";
        out += resp->header_values[i];
        out += "\r\n";
    }

    if (!is_chunked && !has_content_length) {
        out += "Content-Length: ";
        out += std::to_string(body_len);
        out += "\r\n";
    }

    out += "\r\n";

    if (is_chunked) {
        if (body_len > 0) {
            char size_buf[20];
            snprintf(size_buf, sizeof(size_buf), "%llx", (unsigned long long)body_len);
            out += size_buf;
            out += "\r\n";
            out.append(resp->body, body_len);
            out += "\r\n";
        }
        out += "0\r\n\r\n";
    } else {
        out.append(resp->body, body_len);
    }

    // Send full response
    __ry_send_all(handle->fd, out.c_str(), out.size());
}

extern "C" void __ry_http_request_free(void *r) {
    if (!r) return;
    auto *req = (HttpRequestHandle *)r;
    free(req->method);
    free(req->path);
    for (int64_t i = 0; i < req->header_count; i++) {
        free(req->header_keys[i]);
        free(req->header_values[i]);
    }
    free(req->header_keys);
    free(req->header_values);
    for (int64_t i = 0; i < req->query_count; i++) {
        free(req->query_keys[i]);
        free(req->query_values[i]);
    }
    free(req->query_keys);
    free(req->query_values);
    for (int64_t i = 0; i < req->cookie_count; i++) {
        free(req->cookie_keys[i]);
        free(req->cookie_values[i]);
    }
    free(req->cookie_keys);
    free(req->cookie_values);
    free(req->body);
    free(req);
}

// =====================================================================
// HTTP Client
// =====================================================================

struct HttpClientResponseHandle {
    int64_t status;
    char **header_keys;
    char **header_values;
    int64_t header_count;
    char *body;
};

struct ParsedUrl {
    char *host;
    int64_t port;
    char *path;
};

static ParsedUrl *parse_url(const char *url) {
    if (!url || !*url) return nullptr;

    // Must start with "http://"
    if (strncmp(url, "http://", 7) != 0) return nullptr;

    const char *authority = url + 7;
    if (!*authority || *authority == '/' || *authority == ':') return nullptr;

    // Find end of host: ':' (port), '/' (path), '?' (query), '#' (fragment), or end
    const char *p = authority;
    while (*p && *p != ':' && *p != '/' && *p != '?' && *p != '#') p++;

    std::string host(authority, p);
    if (host.empty()) return nullptr;

    int64_t port = 80;
    if (*p == ':') {
        p++;
        const char *port_start = p;
        while (*p >= '0' && *p <= '9') p++;
        if (p == port_start) return nullptr; // no digits
        std::string port_str(port_start, p);
        long long pv = strtoll(port_str.c_str(), nullptr, 10);
        if (pv < 1 || pv > 65535) return nullptr;
        port = (int64_t)pv;
    }

    std::string path;
    if (*p == '/') {
        path = p;
    } else if (*p == '?') {
        path = "/";
        path += p;
    } else if (*p == '#' || *p == '\0') {
        path = "/";
    } else {
        return nullptr;
    }

    // Strip fragment — fragments must not be sent on the wire
    size_t frag = path.find('#');
    if (frag != std::string::npos)
        path.resize(frag);

    auto *result = (ParsedUrl *)malloc(sizeof(ParsedUrl));
    result->host = strdup(host.c_str());
    result->port = port;
    result->path = strdup(path.c_str());
    return result;
}

extern "C" void *__ry_http_parse_url(const char *url) {
    return parse_url(url);
}

extern "C" void __ry_http_parsed_url_free(void *parsed) {
    if (!parsed) return;
    auto *u = (ParsedUrl *)parsed;
    free(u->host);
    free(u->path);
    free(u);
}

static HttpClientResponseHandle *read_http_response(int fd) {
    // Set receive timeout
    struct timeval tv = {30, 0};
    ::setsockopt(fd, SOL_SOCKET, SO_RCVTIMEO, &tv, sizeof(tv));

    // Read headers
    std::string raw = recv_all(fd, 8192);
    if (raw.empty()) return nullptr;

    // Find header/body boundary
    size_t hdr_end = raw.find("\r\n\r\n");
    if (hdr_end == std::string::npos) return nullptr;

    // Parse status line: HTTP/1.x STATUS REASON\r\n
    size_t line_end = raw.find("\r\n");
    if (line_end == std::string::npos) return nullptr;

    std::string status_line = raw.substr(0, line_end);
    // Must start with "HTTP/"
    if (status_line.size() < 12 || status_line.substr(0, 5) != "HTTP/") return nullptr;

    size_t sp1 = status_line.find(' ');
    if (sp1 == std::string::npos) return nullptr;
    size_t sp2 = status_line.find(' ', sp1 + 1);

    std::string status_str;
    if (sp2 != std::string::npos)
        status_str = status_line.substr(sp1 + 1, sp2 - sp1 - 1);
    else
        status_str = status_line.substr(sp1 + 1);

    char *endptr = nullptr;
    long long status_code = strtoll(status_str.c_str(), &endptr, 10);
    if (!endptr || endptr == status_str.c_str() || status_code < 100 || status_code > 599)
        return nullptr;

    // Parse headers using shared helper
    size_t headers_start = line_end + 2;
    auto parsed_headers = parse_raw_headers(raw, headers_start, hdr_end);

    const char *cl_value = nullptr;
    for (auto &h : parsed_headers) {
        if (strcasecmp(h.key, "Content-Length") == 0) {
            cl_value = h.val;
            break;
        }
    }
    bool has_te = has_transfer_encoding(parsed_headers);
    bool is_chunked = has_te && has_chunked_encoding(parsed_headers);

    // Reject unsupported transfer codings (e.g. gzip without chunked)
    if (has_te && !is_chunked) {
        for (auto &h : parsed_headers) { free(h.key); free(h.val); }
        return nullptr;
    }

    size_t body_start = hdr_end + 4;
    std::string body_data;

    if (is_chunked) {
        bool ok;
        body_data = read_chunked_body(fd, raw, body_start, ok);
        if (!ok) {
            for (auto &h : parsed_headers) { free(h.key); free(h.val); }
            return nullptr;
        }
    } else {
        if (body_start < raw.size())
            body_data = raw.substr(body_start);

        int64_t content_length = __ry_http_parse_content_length(cl_value);
        if (content_length == -2 || content_length == -3) {
            for (auto &h : parsed_headers) { free(h.key); free(h.val); }
            return nullptr;
        }
        if (content_length >= 0) {
            if ((int64_t)body_data.size() < content_length) {
                size_t remaining = (size_t)content_length - body_data.size();
                char *extra = (char *)malloc(remaining);
                size_t got = 0;
                while (got < remaining) {
                    ssize_t n = ::recv(fd, extra + got, remaining - got, 0);
                    if (n <= 0) break;
                    got += (size_t)n;
                }
                body_data.append(extra, got);
                free(extra);
            }
            if ((int64_t)body_data.size() < content_length) {
                for (auto &h : parsed_headers) { free(h.key); free(h.val); }
                return nullptr;
            }
            if ((int64_t)body_data.size() > content_length)
                body_data.resize((size_t)content_length);
        } else {
            char buf[4096];
            while (true) {
                ssize_t n = ::recv(fd, buf, sizeof(buf), 0);
                if (n <= 0) break;
                body_data.append(buf, (size_t)n);
                if ((int64_t)body_data.size() > MAX_BODY_SIZE) {
                    body_data.resize((size_t)MAX_BODY_SIZE);
                    break;
                }
            }
        }
    }

    // Build response handle
    auto *resp = (HttpClientResponseHandle *)malloc(sizeof(HttpClientResponseHandle));
    resp->status = (int64_t)status_code;
    resp->body = strdup(body_data.c_str());

    assign_headers(parsed_headers, &resp->header_keys, &resp->header_values, &resp->header_count);

    return resp;
}

// Determine whether a redirect should change the method to GET.
// RFC 9110: 301/302 change POST→GET; 303 always changes to GET.
// 307/308 preserve the original method.
static bool should_redirect_as_get(int status, const char *method) {
    if (status == 303) return true;
    if ((status == 301 || status == 302) && strcasecmp(method, "POST") == 0)
        return true;
    return false;
}

// Resolve a Location header value against the base URL.
// Supports: absolute URL (http://...), protocol-relative (//...),
// absolute path (/...), and relative path.
// Returns a malloc'd string or nullptr on failure.
static char *resolve_redirect_url(const char *base_url, const char *location) {
    if (!location || !*location) return nullptr;

    // Absolute URL
    if (strncmp(location, "http://", 7) == 0) {
        return strdup(location);
    }

    // Protocol-relative URL
    if (location[0] == '/' && location[1] == '/') {
        std::string resolved = "http:";
        resolved += location;
        return strdup(resolved.c_str());
    }

    // Need scheme+authority from base_url
    // base_url is like "http://host:port/path..."
    if (strncmp(base_url, "http://", 7) != 0) return nullptr;

    const char *authority_start = base_url + 7;
    const char *origin_end = strpbrk(authority_start, "/?#");

    std::string origin(base_url, origin_end ? (size_t)(origin_end - base_url)
                                            : strlen(base_url));

    const char *path_start = (origin_end && *origin_end == '/') ? origin_end : nullptr;

    // Absolute path
    if (location[0] == '/') {
        std::string resolved = origin + location;
        return strdup(resolved.c_str());
    }

    // Relative path: append to directory of current path
    if (path_start) {
        const char *last_slash = strrchr(path_start, '/');
        std::string base_dir(base_url,
                             (size_t)(last_slash + 1 - base_url));
        std::string resolved = base_dir + location;
        return strdup(resolved.c_str());
    }

    // No path in base — append /location
    std::string resolved = origin + "/" + location;
    return strdup(resolved.c_str());
}

static bool is_sensitive_header(const char *name) {
    return strcasecmp(name, "Authorization") == 0 ||
           strcasecmp(name, "Proxy-Authorization") == 0 ||
           strcasecmp(name, "Cookie") == 0;
}

static bool is_cross_origin(const char *url_a, const char *url_b) {
    ParsedUrl *a = parse_url(url_a);
    ParsedUrl *b = parse_url(url_b);
    if (!a || !b) {
        __ry_http_parsed_url_free(a);
        __ry_http_parsed_url_free(b);
        return true;
    }
    bool cross = (a->port != b->port || strcasecmp(a->host, b->host) != 0);
    __ry_http_parsed_url_free(a);
    __ry_http_parsed_url_free(b);
    return cross;
}

static bool is_redirect_status(int status) {
    return status == 301 || status == 302 || status == 303 ||
           status == 307 || status == 308;
}

// Reject strings containing CR or LF to prevent HTTP request splitting
static bool has_crlf(const char *s) {
    if (!s) return false;
    for (; *s; s++) {
        if (*s == '\r' || *s == '\n') return true;
    }
    return false;
}

extern "C" void *__ry_http_client_request(const char *method, const char *url,
                                           void *headers_map, const char *body) {
    if (has_crlf(method)) return nullptr;

    // Owned copies — only allocated when redirects occur.
    char *owned_url = nullptr;
    char *owned_method = nullptr;
    const char *current_url = url;
    const char *current_method = method;
    const char *current_body = body ? body : "";
    int redirect_count = 0;
    bool strip_sensitive = false;
    void *result = nullptr;

    for (;;) {
        ParsedUrl *parsed = parse_url(current_url);
        if (!parsed) break;

        if (has_crlf(parsed->host) || has_crlf(parsed->path)) {
            __ry_http_parsed_url_free(parsed);
            break;
        }

        void *stream = __ry_connect(parsed->host, parsed->port);
        if (!stream) {
            __ry_http_parsed_url_free(parsed);
            break;
        }

        auto *tcp = (TcpStreamHandle *)stream;

        // Build HTTP/1.1 request
        std::string request;
        request.reserve(256);
        request += current_method;
        request += ' ';
        request += parsed->path;
        request += " HTTP/1.1\r\n";

        // Host header
        request += "Host: ";
        request += parsed->host;
        if (parsed->port != 80) {
            request += ':';
            request += std::to_string(parsed->port);
        }
        request += "\r\n";

        // User-provided headers
        if (headers_map) {
            auto *map = (MapHeader *)headers_map;
            for (int64_t i = 0; i < map->len; i++) {
                if (has_crlf(map->keys[i]) || has_crlf(map->vals[i])) continue;
                if (strcasecmp(map->keys[i], "Content-Length") == 0) continue;
                if (strip_sensitive && is_sensitive_header(map->keys[i])) continue;
                request += map->keys[i];
                request += ": ";
                request += map->vals[i];
                request += "\r\n";
            }
        }

        size_t body_len = (current_body && *current_body) ? strlen(current_body) : 0;
        request += "Content-Length: ";
        request += std::to_string(body_len);
        request += "\r\n";

        request += "Connection: close\r\n";
        request += "\r\n";

        if (body_len > 0) {
            request.append(current_body, body_len);
        }

        ssize_t sent = __ry_send_all(tcp->fd, request.c_str(), request.size());
        if (sent < 0) {
            __ry_tcp_close(tcp);
            __ry_http_parsed_url_free(parsed);
            break;
        }

        HttpClientResponseHandle *resp = read_http_response(tcp->fd);

        __ry_tcp_close(tcp);
        __ry_http_parsed_url_free(parsed);

        if (!resp) break;

        // Check for redirect
        if (is_redirect_status((int)resp->status)) {
            const char *location = __ry_http_client_header(resp, "Location");

            if (location && *location) {
                if (redirect_count >= MAX_REDIRECTS) {
                    __ry_http_client_response_free(resp);
                    break;
                }

                char *new_url = resolve_redirect_url(current_url, location);
                if (!new_url) {
                    result = resp;
                    break;
                }

                if (should_redirect_as_get((int)resp->status, current_method)) {
                    free(owned_method);
                    owned_method = strdup("GET");
                    current_method = owned_method;
                    current_body = "";
                }

                if (!strip_sensitive && is_cross_origin(current_url, new_url)) {
                    strip_sensitive = true;
                }

                __ry_http_client_response_free(resp);
                free(owned_url);
                owned_url = new_url;
                current_url = owned_url;
                redirect_count++;
                continue;
            }
        }

        result = resp;
        break;
    }

    free(owned_url);
    free(owned_method);
    return result;
}

extern "C" void *__ry_http_get(const char *url) {
    return __ry_http_client_request("GET", url, nullptr, "");
}

extern "C" void *__ry_http_post(const char *url, const char *body, void *headers_map) {
    return __ry_http_client_request("POST", url, headers_map, body);
}

extern "C" int64_t __ry_http_client_status(void *r) {
    if (!r) return -1;
    return ((HttpClientResponseHandle *)r)->status;
}

extern "C" const char *__ry_http_client_body(void *r) {
    if (!r) return "";
    return ((HttpClientResponseHandle *)r)->body;
}

extern "C" const char *__ry_http_client_header(void *r, const char *key) {
    if (!r) return nullptr;
    auto *resp = (HttpClientResponseHandle *)r;
    for (int64_t i = 0; i < resp->header_count; i++) {
        if (strcasecmp(resp->header_keys[i], key) == 0)
            return resp->header_values[i];
    }
    return nullptr;
}

extern "C" void __ry_http_client_response_free(void *r) {
    if (!r) return;
    auto *resp = (HttpClientResponseHandle *)r;
    for (int64_t i = 0; i < resp->header_count; i++) {
        free(resp->header_keys[i]);
        free(resp->header_values[i]);
    }
    free(resp->header_keys);
    free(resp->header_values);
    free(resp->body);
    free(resp);
}

extern "C" void __ry_http_response_free(void *r) {
    if (!r) return;
    auto *resp = (HttpResponseHandle *)r;
    for (int64_t i = 0; i < resp->header_count; i++) {
        free(resp->header_keys[i]);
        free(resp->header_values[i]);
    }
    free(resp->header_keys);
    free(resp->header_values);
    free(resp->body);
    free(resp);
}
