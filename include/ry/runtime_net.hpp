#pragma once

#include <cstddef>
#include <cstdint>
#include <sys/types.h>

// Send all bytes, retrying on EINTR and partial writes.
// Returns total bytes sent, or -1 on error.
ssize_t __ry_send_all(int fd, const void *buf, size_t len);

extern "C" {

void *__ry_bind(const char *host, int64_t port);
int64_t __ry_listen(void *listener, int64_t backlog);
void *__ry_accept(void *listener);
void *__ry_connect(const char *host, int64_t port);
int64_t __ry_tcp_send(void *stream, void *byte_list);
void   *__ry_tcp_recv(void *stream, int64_t max_bytes);
void    __ry_tcp_close(void *handle);
void    __ry_tcp_listener_close(void *listener);
void    __ry_tcp_listener_shutdown(void *listener);
int64_t __ry_listener_port(void *listener);

}
