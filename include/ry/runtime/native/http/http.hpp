#pragma once

#include <cstdint>


namespace ry {

extern "C" {

void       *__ry_http_read_request(void *stream);
const char *__ry_http_method(void *req);
const char *__ry_http_path(void *req);
const char *__ry_http_header(void *req, const char *key);
const char *__ry_http_body(void *req);
void       *__ry_http_body_bytes(void *req);
void       *__ry_http_response(int64_t status, void *headers_map, const char *body);
void        __ry_http_send_response(void *stream, void *response, int64_t keep_alive);
int64_t     __ry_http_should_keep_alive(void *req);
const char *__ry_http_query(void *req, const char *key);
void       *__ry_http_query_all(void *req);
const char *__ry_http_cookie(void *req, const char *name);
void       *__ry_http_cookies(void *req);
const char *__ry_http_form_field(void *req, const char *name);
void       *__ry_http_form_file(void *req, const char *name);
void       *__ry_http_form_fields(void *req);
void        __ry_http_request_free(void *req);
void        __ry_http_response_free(void *resp);

// Parse and validate a Content-Length header value.
// Returns >= 0 on success, -1 if null (no header), -2 if invalid, -3 if exceeds max.
int64_t     __ry_http_parse_content_length(const char *value);

// Map an HTTP status code to its RFC 9110 reason phrase.
// Returns "Unknown" for unrecognized codes.
const char *__ry_http_reason_phrase(int64_t status);

// HTTP client functions
void       *__ry_http_parse_url(const char *url);
void        __ry_http_parsed_url_free(void *parsed);
void       *__ry_http_client_request(const char *method, const char *url,
                                      void *headers_map, const char *body);
void       *__ry_http_get(const char *url);
void       *__ry_http_post(const char *url, const char *body, void *headers_map);
int64_t     __ry_http_client_status(void *resp);
const char *__ry_http_client_body(void *resp);
void       *__ry_http_client_body_bytes(void *resp);
const char *__ry_http_client_header(void *resp, const char *key);
void        __ry_http_client_response_free(void *resp);

}

} // namespace ry
