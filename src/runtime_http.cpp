#include "ry/runtime_http.hpp"
#include "ry/runtime_net.hpp"

#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <strings.h>
#include <string>
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

    // Parse path
    size_t sp2 = request_line.find(' ', sp1 + 1);
    std::string path;
    if (sp2 != std::string::npos)
        path = request_line.substr(sp1 + 1, sp2 - sp1 - 1);
    else
        path = request_line.substr(sp1 + 1);
    req->path = strdup(path.c_str());

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

extern "C" void __ry_http_send_response(void *stream, void *response) {
    auto *handle = (TcpStreamHandle *)stream;
    auto *resp = (HttpResponseHandle *)response;

    // Map status code to reason phrase
    const char *reason = "OK";
    switch (resp->status) {
        case 200: reason = "OK"; break;
        case 201: reason = "Created"; break;
        case 204: reason = "No Content"; break;
        case 301: reason = "Moved Permanently"; break;
        case 302: reason = "Found"; break;
        case 304: reason = "Not Modified"; break;
        case 400: reason = "Bad Request"; break;
        case 401: reason = "Unauthorized"; break;
        case 403: reason = "Forbidden"; break;
        case 404: reason = "Not Found"; break;
        case 405: reason = "Method Not Allowed"; break;
        case 500: reason = "Internal Server Error"; break;
        default: reason = "Unknown"; break;
    }

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
