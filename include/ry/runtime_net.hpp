#pragma once

#include <cstdint>

extern "C" {

void *__ry_bind(const char *host, int64_t port);
void  __ry_listen(void *listener, int64_t backlog);
void *__ry_accept(void *listener);
void *__ry_connect(const char *host, int64_t port);
int64_t __ry_tcp_send(void *stream, void *byte_list);
void   *__ry_tcp_recv(void *stream, int64_t max_bytes);
void    __ry_tcp_close(void *handle);

}
