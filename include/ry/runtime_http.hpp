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
void        __ry_http_request_free(void *req);
void        __ry_http_response_free(void *resp);

}
