#include "ry/runtime_http_internal.hpp"
#include "ry/runtime_http.hpp"
#include "ry/runtime_net_types.hpp"

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

    std::string host(authority, p);
    if (host.empty()) return nullptr;

    int64_t port = is_https ? 443 : 80;
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

    auto *result = (ParsedUrl *)checked_malloc(sizeof(ParsedUrl));
    result->host = checked_strdup(host.c_str());
    result->port = port;
    result->is_https = is_https;
    result->path = checked_strdup(path.c_str());
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

static HttpClientResponseHandle *read_http_response(HttpTransport &t) {
    __ry_apply_default_recv_timeout(t.fd);

    // Read headers
    std::string raw = recv_all(t, kMaxHeaderSize);
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
        body_data = read_chunked_body(t, raw, body_start, ok);
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
                char *extra = (char *)checked_malloc(remaining);
                size_t got = 0;
                while (got < remaining) {
                    ssize_t n = t.do_recv(extra + got, remaining - got);
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
        }
    }

    // Build response handle
    auto *resp = (HttpClientResponseHandle *)checked_malloc(sizeof(HttpClientResponseHandle));
    resp->status = (int64_t)status_code;
    resp->body_len = (int64_t)body_data.size();
    resp->body = checked_memdup(body_data.data(), body_data.size());

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
    const char *allow_private = std::getenv("RY_ALLOW_PRIVATE_HTTP");
    bool ssrf_check = !allow_private || strcmp(allow_private, "1") != 0;

    for (;;) {
        ParsedUrl *parsed = parse_url(current_url);
        if (!parsed) break;

        if (has_crlf(parsed->host) || has_crlf(parsed->path)) {
            __ry_http_parsed_url_free(parsed);
            break;
        }

        if (ssrf_check && __ry_is_private_host(parsed->host, parsed->port)) {
            __ry_http_parsed_url_free(parsed);
            break;
        }

        // Establish connection (plain TCP or TLS)
        HttpTransport transport{-1, nullptr};
        if (parsed->is_https) {
            void *tls = __ry_tls_connect(parsed->host, parsed->port);
            if (!tls) {
                __ry_http_parsed_url_free(parsed);
                break;
            }
            __ry_tls_take_ownership(tls, &transport.fd, &transport.ssl);
        } else {
            void *stream = __ry_connect(parsed->host, parsed->port);
            if (!stream) {
                __ry_http_parsed_url_free(parsed);
                break;
            }
            transport.fd = __ry_tcp_take_fd(stream);
        }

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
        int64_t default_port = parsed->is_https ? 443 : 80;
        if (parsed->port != default_port) {
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
                    owned_method = checked_strdup("GET");
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
