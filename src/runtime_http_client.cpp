#include "ry/runtime_http_internal.hpp"
#include "ry/runtime_http.hpp"
#include "ry/runtime_io.hpp"
#include "ry/runtime_net_utils.hpp"
#include "ry/runtime_arc.hpp"

// =====================================================================
// HTTP Client — URL parsing, response reading, redirect handling
// =====================================================================

static ParsedUrl *parse_url(const char *url) {
    if (!url || !*url) return nullptr;

    bool is_https = false;
    const char *authority = nullptr;
    if (strncmp(url, "https://", 8) == 0) {
        is_https = true;
        authority = url + 8;
    } else if (strncmp(url, "http://", 7) == 0) {
        authority = url + 7;
    } else {
        return nullptr;
    }

    if (!*authority || *authority == '/' || *authority == ':') return nullptr;

    // Find end of host: ':' (port), '/' (path), '?' (query), '#' (fragment), or end
    const char *p = authority;
    while (*p && *p != ':' && *p != '/' && *p != '?' && *p != '#') p++;

    size_t host_len = (size_t)(p - authority);
    if (host_len == 0) return nullptr;

    int64_t port = is_https ? 443 : 80;
    if (*p == ':') {
        p++;
        const char *port_start = p;
        while (*p >= '0' && *p <= '9') p++;
        if (p == port_start) return nullptr; // no digits
        // Parse port in-place without copying, enforcing numeric range 1..65535
        long long pv = 0;
        for (const char *q = port_start; q < p; q++) {
            int digit = *q - '0';
            if (pv > 65535 / 10 || (pv == 65535 / 10 && digit > 65535 % 10))
                return nullptr;
            pv = pv * 10 + digit;
        }
        if (pv < 1) return nullptr;
        port = (int64_t)pv;
    }

    // Build path directly into malloc'd buffer
    char *path_result;
    if (*p == '/') {
        // Find fragment to strip
        const char *frag = strchr(p, '#');
        size_t path_len = frag ? (size_t)(frag - p) : strlen(p);
        path_result = checked_strndup(p, path_len);
    } else if (*p == '?') {
        const char *frag = strchr(p, '#');
        size_t suffix_len = frag ? (size_t)(frag - p) : strlen(p);
        path_result = (char *)checked_malloc(1 + suffix_len + 1);
        path_result[0] = '/';
        memcpy(path_result + 1, p, suffix_len);
        path_result[1 + suffix_len] = '\0';
    } else if (*p == '#' || *p == '\0') {
        path_result = checked_strdup("/");
    } else {
        return nullptr;
    }

    auto *result = (ParsedUrl *)checked_malloc(sizeof(ParsedUrl));
    result->host = checked_strndup(authority, host_len);
    result->port = port;
    result->is_https = is_https;
    result->path = path_result;
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

// Parse "HTTP/1.x STATUS REASON" and return status code, or -1 on failure.
// Sets headers_start to the position after the status line.
static int64_t parse_status_line(const std::string &raw, size_t &headers_start) {
    size_t line_end = raw.find("\r\n");
    if (line_end == std::string::npos) return -1;

    const char *line = raw.c_str();
    if (line_end < 12 || memcmp(line, "HTTP/", 5) != 0) return -1;

    const char *sp1 = (const char *)memchr(line, ' ', line_end);
    if (!sp1) return -1;

    char *endptr = nullptr;
    long long status_code = strtoll(sp1 + 1, &endptr, 10);
    if (!endptr || endptr == sp1 + 1 || status_code < 100 || status_code > 599)
        return -1;

    headers_start = line_end + 2;
    return (int64_t)status_code;
}

// Read response body based on Transfer-Encoding or Content-Length.
// On success, sets body_ptr to a malloc'd buffer and body_len_out to its size.
// Caller takes ownership of body_ptr.
static bool read_response_body(HttpTransport &t, std::string &raw, size_t body_start,
                                 const std::vector<HeaderPair> &headers,
                                 char *&body_ptr, size_t &body_len_out) {
    bool has_te = has_transfer_encoding(headers);
    bool is_chunked = has_te && has_chunked_encoding(headers);

    // Reject unsupported transfer codings (e.g. gzip without chunked)
    if (has_te && !is_chunked) return false;

    if (is_chunked) {
        bool ok;
        std::string body_data = read_chunked_body(t, raw, body_start, ok);
        if (!ok) return false;
        body_len_out = body_data.size();
        body_ptr = (char *)checked_memdup(body_data.data(), body_data.size());
        return true;
    }

    size_t initial_len = (body_start < raw.size()) ? raw.size() - body_start : 0;

    const char *cl_value = nullptr;
    for (auto &h : headers) {
        if (strcasecmp(h.key, "Content-Length") == 0) {
            cl_value = h.val;
            break;
        }
    }

    int64_t content_length = __ry_http_parse_content_length(cl_value);
    if (content_length == -2 || content_length == -3) return false;

    if (content_length >= 0) {
        // Known Content-Length: allocate final buffer directly
        size_t cl = (size_t)content_length;
        char *body = (char *)checked_malloc(cl + 1);
        size_t have = (initial_len < cl) ? initial_len : cl;
        if (have > 0)
            memcpy(body, raw.c_str() + body_start, have);

        size_t got = have;
        while (got < cl) {
            ssize_t n = t.do_recv(body + got, cl - got);
            if (n <= 0) break;
            got += (size_t)n;
        }
        if ((int64_t)got < content_length) { free(body); return false; }

        body[cl] = '\0';
        body_ptr = body;
        body_len_out = cl;
    } else {
        // Unknown Content-Length: read until connection close
        std::string body_data;
        if (initial_len > 0)
            body_data.append(raw.c_str() + body_start, initial_len);
        char buf[kRecvBufSize];
        while (true) {
            ssize_t n = t.do_recv(buf, sizeof(buf));
            if (n <= 0) break;
            body_data.append(buf, (size_t)n);
            if ((int64_t)body_data.size() > MAX_BODY_SIZE) {
                body_data.resize((size_t)MAX_BODY_SIZE);
                break;
            }
        }
        body_len_out = body_data.size();
        body_ptr = (char *)checked_memdup(body_data.data(), body_data.size());
    }

    return true;
}

static HttpClientResponseHandle *read_http_response(HttpTransport &t) {
    ry_net_apply_default_recv_timeout(t.fd);

    std::string raw = recv_all(t, kMaxHeaderSize);
    if (raw.empty()) return nullptr;

    size_t hdr_end = raw.find("\r\n\r\n");
    if (hdr_end == std::string::npos) return nullptr;

    size_t headers_start;
    int64_t status_code = parse_status_line(raw, headers_start);
    if (status_code < 0) return nullptr;

    auto parsed_headers = parse_raw_headers(raw, headers_start, hdr_end);

    size_t body_start = hdr_end + 4;
    char *body_ptr = nullptr;
    size_t body_len = 0;
    if (!read_response_body(t, raw, body_start, parsed_headers, body_ptr, body_len)) {
        for (auto &h : parsed_headers) { free(h.key); free(h.val); }
        return nullptr;
    }

    void *resp_mem = arc_alloc(sizeof(HttpClientResponseHandle));
    if (!resp_mem) {
        free(body_ptr);
        for (auto &h : parsed_headers) { free(h.key); free(h.val); }
        return nullptr;
    }
    auto *resp = new (resp_mem) HttpClientResponseHandle{};
    resp->status = status_code;
    resp->body_len = (int64_t)body_len;
    resp->body = body_ptr;
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

    // Absolute URL (http:// or https://)
    if (strncmp(location, "http://", 7) == 0 || strncmp(location, "https://", 8) == 0) {
        return checked_strdup(location);
    }

    // Determine base scheme
    const char *authority_start = nullptr;
    std::string scheme;
    if (strncmp(base_url, "https://", 8) == 0) {
        scheme = "https:";
        authority_start = base_url + 8;
    } else if (strncmp(base_url, "http://", 7) == 0) {
        scheme = "http:";
        authority_start = base_url + 7;
    } else {
        return nullptr;
    }

    // Protocol-relative URL — inherit scheme from base
    if (location[0] == '/' && location[1] == '/') {
        std::string resolved = scheme + location;
        return checked_strdup(resolved.c_str());
    }

    const char *origin_end = strpbrk(authority_start, "/?#");

    std::string origin(base_url, origin_end ? (size_t)(origin_end - base_url)
                                            : strlen(base_url));

    const char *path_start = (origin_end && *origin_end == '/') ? origin_end : nullptr;

    // Absolute path
    if (location[0] == '/') {
        std::string resolved = origin + location;
        return checked_strdup(resolved.c_str());
    }

    // Relative path: append to directory of current path
    if (path_start) {
        const char *last_slash = strrchr(path_start, '/');
        std::string base_dir(base_url,
                             (size_t)(last_slash + 1 - base_url));
        std::string resolved = base_dir + location;
        return checked_strdup(resolved.c_str());
    }

    // No path in base — append /location
    std::string resolved = origin + "/" + location;
    return checked_strdup(resolved.c_str());
}

static bool is_sensitive_header(const char *name) {
    return strcasecmp(name, "Authorization") == 0 ||
           strcasecmp(name, "Proxy-Authorization") == 0 ||
           strcasecmp(name, "Cookie") == 0;
}

static bool is_hop_by_hop_header(const char *name) {
    return strcasecmp(name, "Host") == 0 ||
           strcasecmp(name, "Connection") == 0 ||
           strcasecmp(name, "Keep-Alive") == 0 ||
           strcasecmp(name, "Transfer-Encoding") == 0 ||
           strcasecmp(name, "TE") == 0 ||
           strcasecmp(name, "Upgrade") == 0 ||
           strcasecmp(name, "Proxy-Connection") == 0;
}

static bool is_cross_origin(const char *url_a, const char *url_b) {
    ParsedUrl *a = parse_url(url_a);
    ParsedUrl *b = parse_url(url_b);
    if (!a || !b) {
        __ry_http_parsed_url_free(a);
        __ry_http_parsed_url_free(b);
        return true;
    }
    bool cross = (a->port != b->port || strcasecmp(a->host, b->host) != 0 ||
                  a->is_https != b->is_https);
    __ry_http_parsed_url_free(a);
    __ry_http_parsed_url_free(b);
    return cross;
}

static bool is_redirect_status(int status) {
    return status == 301 || status == 302 || status == 303 ||
           status == 307 || status == 308;
}

// RAII guard for freeaddrinfo
struct AddrInfoGuard {
    struct addrinfo *info;
    ~AddrInfoGuard() { if (info) ::freeaddrinfo(info); }
    AddrInfoGuard(const AddrInfoGuard&) = delete;
    AddrInfoGuard& operator=(const AddrInfoGuard&) = delete;
};

// Establish a TCP or TLS connection using pre-resolved addresses.
static bool establish_connection(const ParsedUrl *parsed,
                                  const struct addrinfo *resolved,
                                  HttpTransport &transport) {
    if (parsed->is_https) {
        void *tls = __ry_tls_connect_resolved(parsed->host, resolved);
        if (!tls) return false;
        __ry_tls_take_ownership(tls, &transport.fd, &transport.ssl);
    } else {
        void *stream = ry_net_connect_resolved(resolved);
        if (!stream) return false;
        transport.fd = ry_net_tcp_take_fd(stream);
    }
    return true;
}

// Build an HTTP/1.1 request string with Host header, user headers, and body.
static std::string build_http_request(const char *method, const ParsedUrl *parsed,
                                       void *headers_map, const char *body,
                                       bool strip_sensitive) {
    std::string request;
    request.reserve(256);
    request += method;
    request += ' ';
    request += parsed->path;
    request += " HTTP/1.1\r\n";

    request += "Host: ";
    request += parsed->host;
    int64_t default_port = parsed->is_https ? 443 : 80;
    if (parsed->port != default_port) {
        request += ':';
        request += std::to_string(parsed->port);
    }
    request += "\r\n";

    if (headers_map) {
        auto *map = (MapHeader *)headers_map;
        for (int64_t i = 0; i < map->len; i++) {
            if (has_crlf(map->keys[i]) || has_crlf(map->vals[i])) continue;
            if (strcasecmp(map->keys[i], "Content-Length") == 0) continue;
            if (is_hop_by_hop_header(map->keys[i])) continue;
            if (strip_sensitive && is_sensitive_header(map->keys[i])) continue;
            request += map->keys[i];
            request += ": ";
            request += map->vals[i];
            request += "\r\n";
        }
    }

    size_t body_len = (body && *body) ? strlen(body) : 0;
    request += "Content-Length: ";
    request += std::to_string(body_len);
    request += "\r\n";
    request += "Connection: close\r\n";
    request += "\r\n";

    if (body_len > 0)
        request.append(body, body_len);

    return request;
}

extern "C" void *__ry_http_client_request(const char *method, const char *url,
                                           void *headers_map, const char *body) {
    if (has_crlf(method)) return nullptr;

    char *owned_url = nullptr;
    char *owned_method = nullptr;
    const char *current_url = url;
    const char *current_method = method;
    const char *current_body = body ? body : "";
    int redirect_count = 0;
    bool strip_sensitive = false;
    void *result = nullptr;
    const char *allow_private = std::getenv("RY_ALLOW_PRIVATE_HTTP");
    bool ssrf_check = !allow_private || strcmp(allow_private, "1") != 0;

    for (;;) {
        ParsedUrl *parsed = parse_url(current_url);
        if (!parsed) break;

        if (has_crlf(parsed->host) || has_crlf(parsed->path)) {
            __ry_http_parsed_url_free(parsed);
            break;
        }

        // Resolve DNS once and use the result for both SSRF check and connection
        struct addrinfo *resolved = nullptr;
        if (ry_net_resolve(parsed->host, parsed->port, &resolved) != 0) {
            __ry_http_parsed_url_free(parsed);
            break;
        }
        AddrInfoGuard guard{resolved};

        if (ssrf_check && ry_net_is_private_addrinfo(resolved)) {
            __ry_http_parsed_url_free(parsed);
            break;
        }

        HttpTransport transport{-1, nullptr};
        if (!establish_connection(parsed, resolved, transport)) {
            __ry_http_parsed_url_free(parsed);
            break;
        }

        std::string request = build_http_request(current_method, parsed, headers_map,
                                                  current_body, strip_sensitive);

        ssize_t sent = transport.do_send(request.c_str(), request.size());
        if (sent < 0) {
            transport.close_transport();
            __ry_http_parsed_url_free(parsed);
            break;
        }

        HttpClientResponseHandle *resp = read_http_response(transport);

        transport.close_transport();
        __ry_http_parsed_url_free(parsed);

        if (!resp) break;

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
                    owned_method = checked_strdup("GET");
                    current_method = owned_method;
                    current_body = "";
                }

                if (!strip_sensitive && is_cross_origin(current_url, new_url))
                    strip_sensitive = true;

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

extern "C" void *__ry_http_client_body_bytes(void *r) {
    if (!r) return makeByteList(nullptr, 0);
    auto *resp = (HttpClientResponseHandle *)r;
    return makeByteList((const uint8_t *)resp->body, resp->body_len);
}

extern "C" const char *__ry_http_client_header(void *r, const char *key) {
    if (!r) return nullptr;
    auto *resp = (HttpClientResponseHandle *)r;
    return find_in_kv_pairs_ci(resp->header_keys, resp->header_values, resp->header_count, key);
}

// Free internal fields of an HttpClientResponseHandle but NOT the handle memory itself.
// Used by ARC weak-reference cleanup paths.
extern "C" void __ry_http_client_response_cleanup(void *r) {
    if (!r) return;
    auto *resp = (HttpClientResponseHandle *)r;
    free_kv_pairs(resp->header_keys, resp->header_values, resp->header_count);
    free(resp->body);
}

extern "C" void __ry_http_client_response_free(void *r) {
    if (!r) return;
    __ry_http_client_response_cleanup(r);
    arc_free(r);
}
