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

    // Single-pass header parsing with dynamic storage
    struct HeaderPair { char *key; char *val; };
    std::vector<HeaderPair> parsed_headers;

    size_t pos = headers_start;
    while (pos < headers_end) {
        size_t eol = raw.find("\r\n", pos);
        if (eol == std::string::npos || eol > headers_end) eol = headers_end;
        if (eol == pos) break;
        size_t colon = raw.find(':', pos);
        if (colon != std::string::npos && colon < eol) {
            // Trim leading whitespace from value
            size_t vstart = colon + 1;
            while (vstart < eol && (raw[vstart] == ' ' || raw[vstart] == '\t'))
                vstart++;
            parsed_headers.push_back({
                strndup(raw.c_str() + pos, colon - pos),
                strndup(raw.c_str() + vstart, eol - vstart)
            });
        }
        pos = eol + 2;
    }

    int64_t count = (int64_t)parsed_headers.size();
    req->header_count = count;
    if (count > 0) {
        req->header_keys = (char **)malloc(sizeof(char *) * (size_t)count);
        req->header_values = (char **)malloc(sizeof(char *) * (size_t)count);
        for (int64_t i = 0; i < count; i++) {
            req->header_keys[i] = parsed_headers[(size_t)i].key;
            req->header_values[i] = parsed_headers[(size_t)i].val;
        }
    } else {
        req->header_keys = nullptr;
        req->header_values = nullptr;
    }

    // Pre-parse Cookie header into req->cookie_keys/values/count
    parse_cookie_header(req);

    // Validate Content-Length before body parsing
    size_t body_start = headers_end + 4;
    const char *cl_value = nullptr;
    for (int64_t i = 0; i < req->header_count; i++) {
        if (strcasecmp(req->header_keys[i], "Content-Length") == 0) {
            cl_value = req->header_values[i];
            break;
        }
    }
    int64_t content_length = __ry_http_parse_content_length(cl_value);
    if (content_length == -2 || content_length == -3) {
        // Invalid or oversized Content-Length — reject as malformed
        __ry_http_request_free(req);
        return nullptr;
    }

    // Parse body
    std::string body_data;
    if (body_start < raw.size())
        body_data = raw.substr(body_start);

    if (content_length > 0 && (int64_t)body_data.size() < content_length) {
        // Need to read more body data
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

        // Reject truncated body
        if ((int64_t)body_data.size() < content_length) {
            __ry_http_request_free(req);
            return nullptr;
        }
    }

    // Truncate body to exactly Content-Length to prevent request smuggling
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

    // Check if Content-Length is already provided
    bool has_content_length = false;
    for (int64_t i = 0; i < resp->header_count; i++) {
        if (strcasecmp(resp->header_keys[i], "Content-Length") == 0)
            has_content_length = true;
        out += resp->header_keys[i];
        out += ": ";
        out += resp->header_values[i];
        out += "\r\n";
    }

    if (!has_content_length) {
        out += "Content-Length: ";
        out += std::to_string(body_len);
        out += "\r\n";
    }

    out += "\r\n";
    out.append(resp->body, body_len);

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
