#include "ry/runtime_http_internal.hpp"
#include "ry/runtime_http.hpp"
#include "ry/runtime_io.hpp"
#include "ry/runtime_net_utils.hpp"
#include "ry/runtime_arc.hpp"

#include <openssl/err.h>

#include <sys/socket.h>
#include <sys/time.h>


namespace ry {

// ---------------------------------------------------------------------------
// Non-inline shared helpers (definitions used by other runtime_http_*.cpp)
// ---------------------------------------------------------------------------

std::vector<HeaderPair> parse_raw_headers(const std::string &raw,
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
                checked_strndup(raw.c_str() + pos, colon - pos),
                checked_strndup(raw.c_str() + vstart, eol - vstart)
            });
        }
        pos = eol + 2;
    }
    return headers;
}

void *build_str_map(char **keys, char **vals, int64_t count) {
    auto *map = (MapHeader *)checked_malloc(sizeof(MapHeader));
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

void *build_str_map_copy(char **keys, char **vals, int64_t count) {
    char **dup_keys = nullptr;
    char **dup_vals = nullptr;
    if (count > 0) {
        dup_keys = (char **)checked_malloc(sizeof(char *) * (size_t)count);
        dup_vals = (char **)checked_malloc(sizeof(char *) * (size_t)count);
        for (int64_t i = 0; i < count; i++) {
            dup_keys[i] = checked_strdup(keys[i]);
            dup_vals[i] = checked_strdup(vals[i]);
        }
    }
    return build_str_map(dup_keys, dup_vals, count);
}

static int hex_digit(char c) {
    if (c >= '0' && c <= '9') return c - '0';
    if (c >= 'A' && c <= 'F') return c - 'A' + 10;
    if (c >= 'a' && c <= 'f') return c - 'a' + 10;
    return -1;
}

static std::string url_decode(const char *src, size_t len) {
    std::string out;
    out.reserve(len);
    for (size_t i = 0; i < len; i++) {
        if (src[i] == '+') {
            out += ' ';
        } else if (src[i] == '%' && i + 2 < len) {
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

static void parse_query_string(const char *qs, size_t qs_len, HttpRequestHandle *req) {
    if (!qs || qs_len == 0) {
        req->query_keys = nullptr;
        req->query_values = nullptr;
        req->query_count = 0;
        return;
    }

    struct QParam { char *key; char *val; };
    std::vector<QParam> params;

    size_t pos = 0;
    while (pos < qs_len) {
        const char *seg = qs + pos;
        const char *amp = (const char *)memchr(seg, '&', qs_len - pos);
        size_t seg_len = amp ? (size_t)(amp - seg) : qs_len - pos;
        if (seg_len > 0) {
            const char *eq = (const char *)memchr(seg, '=', seg_len);
            std::string key, val;
            if (eq) {
                size_t key_raw_len = (size_t)(eq - seg);
                key = url_decode(seg, key_raw_len);
                val = url_decode(eq + 1, seg_len - key_raw_len - 1);
            } else {
                key = url_decode(seg, seg_len);
            }
            // First-value-wins: linear scan (query params are typically few)
            bool dup = false;
            for (size_t j = 0; j < params.size(); j++) {
                if (strcmp(params[j].key, key.c_str()) == 0) { dup = true; break; }
            }
            if (!dup)
                params.push_back({checked_strdup(key.c_str()), checked_strdup(val.c_str())});
        }
        pos += seg_len + 1;
    }

    assign_kv_pairs(params, &req->query_keys, &req->query_values, &req->query_count,
                    [](const QParam &p) { return p.key; },
                    [](const QParam &p) { return p.val; });
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

    const char *p = cookie_str;
    while (*p) {
        while (*p == ' ' || *p == '\t') p++;
        if (!*p) break;

        const char *semi = strchr(p, ';');
        size_t pair_len = semi ? (size_t)(semi - p) : strlen(p);

        const char *eq = static_cast<const char *>(memchr(p, '=', pair_len)); // NOLINT(bugprone-not-null-terminated-result)
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
                // Linear scan for duplicate detection (cookies are typically few)
                bool dup = false;
                for (size_t j = 0; j < pairs.size(); j++) {
                    if (strlen(pairs[j].key) == key_len && memcmp(pairs[j].key, p, key_len) == 0) {
                        dup = true; break;
                    }
                }
                if (!dup)
                    pairs.push_back({checked_strndup(p, key_len), checked_strndup(val_start, val_len)});
            }
        }

        p += pair_len;
        if (*p == ';') p++;
    }

    assign_kv_pairs(pairs, &req->cookie_keys, &req->cookie_values, &req->cookie_count,
                    [](const CookiePair &p) { return p.key; },
                    [](const CookiePair &p) { return p.val; });
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

// ---------------------------------------------------------------------------
// Non-inline shared helpers (network I/O + transfer-encoding)
// ---------------------------------------------------------------------------

std::string recv_all(HttpTransport &t, size_t max_bytes) {
    std::string buf;
    buf.resize(max_bytes);
    size_t total = 0;
    while (total < max_bytes) {
        ssize_t n = t.do_recv(&buf[total], max_bytes - total);
        if (n <= 0) break;
        total += (size_t)n;
        size_t search_from = (total > (size_t)n + 3) ? total - (size_t)n - 3 : 0;
        if (buf.find("\r\n\r\n", search_from) != std::string::npos)
            break;
    }
    buf.resize(total);
    return buf;
}

bool recv_into_buf(HttpTransport &t, std::string &buf) {
    char tmp[kRecvBufSize];
    ssize_t n = t.do_recv(tmp, sizeof(tmp));
    if (n <= 0) return false;
    buf.append(tmp, (size_t)n);
    return true;
}

std::string recv_all(int fd, size_t max_bytes) {
    HttpTransport t{fd, nullptr};
    return recv_all(t, max_bytes);
}

bool recv_into_buf(int fd, std::string &buf) {
    HttpTransport t{fd, nullptr};
    return recv_into_buf(t, buf);
}

bool te_value_is_chunked(const char *value) {
    const char *last_token = value;
    const char *p = value;
    while (*p) {
        while (*p == ' ' || *p == '\t') p++;
        if (*p == '\0') break;
        last_token = p;
        while (*p && *p != ',') p++;
        if (*p == ',') p++;
    }
    size_t len = strlen(last_token);
    while (len > 0 && (last_token[len - 1] == ' ' || last_token[len - 1] == '\t'))
        len--;
    return len == 7 && strncasecmp(last_token, "chunked", 7) == 0;
}

bool has_transfer_encoding(char **keys, int64_t count) {
    return has_transfer_encoding_impl(count, [keys](int64_t i) { return keys[i]; });
}

bool has_transfer_encoding(const std::vector<HeaderPair> &headers) {
    return has_transfer_encoding_impl((int64_t)headers.size(),
        [&headers](int64_t i) { return headers[(size_t)i].key; });
}

bool has_chunked_encoding(char **keys, char **values, int64_t count) {
    return has_chunked_encoding_impl(count,
        [keys](int64_t i) { return keys[i]; },
        [values](int64_t i) { return values[i]; });
}

bool has_chunked_encoding(const std::vector<HeaderPair> &headers) {
    return has_chunked_encoding_impl((int64_t)headers.size(),
        [&headers](int64_t i) { return headers[(size_t)i].key; },
        [&headers](int64_t i) { return headers[(size_t)i].val; });
}

std::string read_chunked_body(int fd, std::string &buf, size_t pos, bool &ok) {
    HttpTransport t{fd, nullptr};
    return read_chunked_body(t, buf, pos, ok);
}

std::string read_chunked_body(HttpTransport &t, std::string &buf, size_t pos, bool &ok) {
    ok = true;
    std::string body;

    auto ensure = [&](size_t needed) -> bool {
        while (buf.size() - pos < needed) {
            if (!recv_into_buf(t, buf)) return false;
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
            if (!recv_into_buf(t, buf)) { ok = false; return ""; }
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
        for (const char *cp = endptr; cp < size_start + size_len; cp++) {
            if (*cp != ' ' && *cp != '\t') { ok = false; return ""; }
        }

        pos = crlf + 2;

        if (chunk_size == 0) {
            // Consume optional trailers and final CRLF after terminal chunk.
            // Trailers end with an empty line (\r\n), so we read until \r\n\r\n.
            while (true) {
                size_t end = buf.find("\r\n", pos);
                if (end == std::string::npos) {
                    if (!recv_into_buf(t, buf)) break;
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

// Parse "METHOD PATH HTTP/x.x" request line and populate req->method, path, query.
// On success, sets line_end_out to the position of the first \r\n.
static bool parse_request_line(const std::string &raw, HttpRequestHandle *req,
                                 size_t &line_end_out) {
    size_t line_end = raw.find("\r\n");
    if (line_end == std::string::npos) return false;

    const char *line = raw.c_str();

    // Find first space (end of method)
    const char *sp1 = (const char *)memchr(line, ' ', line_end);
    if (!sp1) return false;

    req->method = checked_strndup(line, (size_t)(sp1 - line));

    // Find second space (end of path, before HTTP/x.x)
    size_t remaining = line_end - (size_t)(sp1 + 1 - line);
    const char *sp2 = (const char *)memchr(sp1 + 1, ' ', remaining);
    const char *path_start = sp1 + 1;
    size_t path_len = sp2 ? (size_t)(sp2 - path_start) : line_end - (size_t)(path_start - line);

    // Find '?' within path to split path and query string
    const char *qmark = (const char *)memchr(path_start, '?', path_len);
    if (qmark) {
        req->path = checked_strndup(path_start, (size_t)(qmark - path_start));
        const char *qs = qmark + 1;
        size_t qs_len = path_len - (size_t)(qs - path_start);
        parse_query_string(qs, qs_len, req);
    } else {
        req->path = checked_strndup(path_start, path_len);
        parse_query_string(nullptr, 0, req);
    }
    line_end_out = line_end;
    return true;
}

// Read request body based on Content-Length or chunked Transfer-Encoding.
static bool read_request_body(int fd, std::string &raw, size_t body_start,
                                HttpRequestHandle *req) {
    const char *cl_value = find_in_kv_pairs_ci(
        req->header_keys, req->header_values, req->header_count, "Content-Length");
    bool has_te = has_transfer_encoding(req->header_keys, req->header_count);
    bool is_chunked = has_te && has_chunked_encoding(
        req->header_keys, req->header_values, req->header_count);

    // RFC 9112 §6.1: reject if both Transfer-Encoding and Content-Length are present
    if (is_chunked && cl_value != nullptr) return false;
    // Reject unsupported transfer codings (e.g. gzip without chunked)
    if (has_te && !is_chunked) return false;

    if (is_chunked) {
        bool ok;
        std::string body_data = read_chunked_body(fd, raw, body_start, ok);
        if (!ok) return false;
        req->body_len = (int64_t)body_data.size();
        req->body = checked_memdup(body_data.data(), body_data.size());
        return true;
    }

    int64_t content_length = __ry_http_parse_content_length(cl_value);
    if (content_length == -2 || content_length == -3) return false;

    size_t initial_len = (body_start < raw.size()) ? raw.size() - body_start : 0;

    if (content_length > 0) {
        // Allocate final buffer directly to avoid intermediate string copy
        size_t cl = (size_t)content_length;
        char *body = (char *)checked_malloc(cl + 1);
        size_t have = (initial_len < cl) ? initial_len : cl;
        if (have > 0)
            memcpy(body, raw.c_str() + body_start, have);

        // Recv remaining bytes directly into the final buffer
        size_t got = have;
        while (got < cl) {
            ssize_t n = ::recv(fd, body + got, cl - got, 0);
            if (n <= 0) break;
            got += (size_t)n;
        }

        if ((int64_t)got < content_length) { free(body); return false; }

        body[cl] = '\0';
        req->body_len = content_length;
        req->body = body;
    } else if (content_length == 0) {
        req->body_len = 0;
        req->body = checked_memdup("", 0);
    } else {
        // No Content-Length: use whatever data is available after headers
        req->body_len = (int64_t)initial_len;
        req->body = (initial_len > 0)
            ? checked_memdup(raw.c_str() + body_start, initial_len)
            : checked_memdup("", 0);
    }
    return true;
}

extern "C" void *__ry_http_read_request(void *stream) {
    auto *handle = (TcpStreamHandle *)stream;
    struct timeval tv = {5, 0};
    ::setsockopt(handle->fd, SOL_SOCKET, SO_RCVTIMEO, &tv, sizeof(tv));
    std::string raw = recv_all(handle->fd, kMaxHeaderSize);
    if (raw.empty()) return nullptr;

    void *req_mem = arc_alloc(sizeof(HttpRequestHandle));
    if (!req_mem) return nullptr;
    auto *req = new (req_mem) HttpRequestHandle{};

    size_t line_end;
    if (!parse_request_line(raw, req, line_end)) {
        arc_free(req);
        return nullptr;
    }

    size_t headers_start = line_end + 2;
    size_t headers_end = raw.find("\r\n\r\n", headers_start);
    if (headers_end == std::string::npos) {
        __ry_http_request_free(req);
        return nullptr;
    }

    auto parsed_headers = parse_raw_headers(raw, headers_start, headers_end);
    assign_headers(parsed_headers, &req->header_keys, &req->header_values, &req->header_count);
    parse_cookie_header(req);

    size_t body_start = headers_end + 4;
    if (!read_request_body(handle->fd, raw, body_start, req)) {
        __ry_http_request_free(req);
        return nullptr;
    }

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
    return find_in_kv_pairs_ci(req->header_keys, req->header_values, req->header_count, key);
}

extern "C" const char *__ry_http_body(void *r) {
    auto *req = (HttpRequestHandle *)r;
    return req->body;
}

extern "C" void *__ry_http_body_bytes(void *r) {
    auto *req = (HttpRequestHandle *)r;
    return makeByteList((const uint8_t *)req->body, req->body_len);
}

extern "C" const char *__ry_http_query(void *r, const char *key) {
    auto *req = (HttpRequestHandle *)r;
    return find_in_kv_pairs(req->query_keys, req->query_values, req->query_count, key);
}

extern "C" void *__ry_http_query_all(void *r) {
    auto *req = (HttpRequestHandle *)r;
    return build_str_map_copy(req->query_keys, req->query_values, req->query_count);
}

extern "C" const char *__ry_http_cookie(void *r, const char *name) {
    auto *req = (HttpRequestHandle *)r;
    return find_in_kv_pairs(req->cookie_keys, req->cookie_values, req->cookie_count, name);
}

extern "C" void *__ry_http_cookies(void *r) {
    auto *req = (HttpRequestHandle *)r;
    return build_str_map_copy(req->cookie_keys, req->cookie_values, req->cookie_count);
}

extern "C" void *__ry_http_response_create(int64_t status, void *headers_map, const char *body) {
    void *resp_mem = arc_alloc(sizeof(HttpResponseHandle));
    if (!resp_mem) return nullptr;
    auto *resp = new (resp_mem) HttpResponseHandle{};
    resp->status = status;
    const char *b = body ? body : "";
    resp->body_len = (int64_t)strlen(b);
    resp->body = checked_memdup(b, (size_t)resp->body_len);

    auto *map = (MapHeader *)headers_map;
    if (map->len > 0) {
        resp->header_keys = (char **)checked_malloc(sizeof(char *) * (size_t)map->len);
        resp->header_values = (char **)checked_malloc(sizeof(char *) * (size_t)map->len);
        int64_t actual = 0;
        for (int64_t i = 0; i < map->len; i++) {
            if (has_crlf(map->keys[i]) || has_crlf(map->vals[i])) continue;
            resp->header_keys[actual] = checked_strdup(map->keys[i]);
            resp->header_values[actual] = checked_strdup(map->vals[i]);
            actual++;
        }
        resp->header_count = actual;
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

extern "C" int64_t __ry_http_should_keep_alive(void *request) {
    const char *conn = __ry_http_header(request, "Connection");
    if (!conn) return 1; // HTTP/1.1 default: keep-alive
    if (strcasecmp(conn, "close") == 0) return 0;
    return 1;
}

extern "C" void __ry_http_send_response(void *stream, void *response, int64_t keep_alive) {
    auto *handle = (TcpStreamHandle *)stream;
    auto *resp = (HttpResponseHandle *)response;

    const char *reason = __ry_http_reason_phrase(resp->status);

    // Use stored body length (not strlen) to handle NUL bytes correctly
    size_t body_len = (size_t)resp->body_len;
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
    bool has_connection = false;
    bool is_chunked = has_chunked_encoding(
        resp->header_keys, resp->header_values, resp->header_count);
    for (int64_t i = 0; i < resp->header_count; i++) {
        if (strcasecmp(resp->header_keys[i], "Content-Length") == 0) {
            has_content_length = true;
            if (is_chunked) continue; // suppress Content-Length when using chunked
        }
        if (strcasecmp(resp->header_keys[i], "Connection") == 0) {
            has_connection = true;
        }
        out += resp->header_keys[i];
        out += ": ";
        out += resp->header_values[i];
        out += "\r\n";
    }

    if (!has_connection) {
        out += "Connection: ";
        out += (keep_alive ? "keep-alive" : "close");
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
    ry_net_send_all(handle->fd, out.c_str(), out.size());
}

// Free internal fields of an HttpRequestHandle but NOT the handle memory itself.
// Used by ARC weak-reference cleanup paths.
extern "C" void __ry_http_request_cleanup(void *r) {
    if (!r) return;
    auto *req = (HttpRequestHandle *)r;
    free(req->method);
    free(req->path);
    free_kv_pairs(req->header_keys, req->header_values, req->header_count);
    free_kv_pairs(req->query_keys, req->query_values, req->query_count);
    free_kv_pairs(req->cookie_keys, req->cookie_values, req->cookie_count);
    for (int64_t i = 0; i < req->form_field_count; i++) {
        free(req->form_fields[i].key);
        free(req->form_fields[i].value);
    }
    free(req->form_fields);
    for (int64_t i = 0; i < req->form_file_count; i++) {
        free(req->form_files[i].name);
        free(req->form_files[i].filename);
        free(req->form_files[i].content_type);
        free(req->form_files[i].data);
    }
    free(req->form_files);
    free(req->body);
}

extern "C" void __ry_http_request_free(void *r) {
    if (!r) return;
    __ry_http_request_cleanup(r);
    arc_free(r);
}

// Free internal fields of an HttpResponseHandle but NOT the handle memory itself.
// Used by ARC weak-reference cleanup paths.
extern "C" void __ry_http_response_cleanup(void *r) {
    if (!r) return;
    auto *resp = (HttpResponseHandle *)r;
    free_kv_pairs(resp->header_keys, resp->header_values, resp->header_count);
    free(resp->body);
}

extern "C" void __ry_http_response_free(void *r) {
    if (!r) return;
    __ry_http_response_cleanup(r);
    arc_free(r);
}

} // namespace ry
