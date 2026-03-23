#pragma once

#include <cstdint>

extern "C" {

void       *__ry_http_read_request(void *stream);
const char *__ry_http_method(void *req);
const char *__ry_http_path(void *req);
const char *__ry_http_header(void *req, const char *key);
const char *__ry_http_body(void *req);
void       *__ry_http_response_create(int64_t status, void *headers_map, const char *body);
void        __ry_http_send_response(void *stream, void *response);
const char *__ry_http_query(void *req, const char *key);
void       *__ry_http_query_all(void *req);
const char *__ry_http_cookie(void *req, const char *name);
void       *__ry_http_cookies(void *req);
void        __ry_http_request_free(void *req);
void        __ry_http_response_free(void *resp);

// Parse and validate a Content-Length header value.
// Returns >= 0 on success, -1 if null (no header), -2 if invalid, -3 if exceeds max.
int64_t     __ry_http_parse_content_length(const char *value);

// Map an HTTP status code to its RFC 9110 reason phrase.
// Returns "Unknown" for unrecognized codes.
const char *__ry_http_reason_phrase(int64_t status);

}
