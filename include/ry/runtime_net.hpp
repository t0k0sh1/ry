#pragma once

#include <cstddef>
#include <cstdint>
#include <sys/types.h>
#include <netdb.h>

// Send all bytes, retrying on EINTR and partial writes.
// Returns total bytes sent, or -1 on error.
ssize_t __ry_send_all(int fd, const void *buf, size_t len);

// Convert milliseconds to timeval and set socket option.
// Shared by TCP and TLS timeout implementations.
void __ry_set_socket_timeval(int fd, int option, int64_t ms);

// Apply 30s default recv timeout if none has been set.
void __ry_apply_default_recv_timeout(int fd);

// Extract fd from a TcpStreamHandle and free the handle.
// Caller takes ownership of the fd.
int __ry_tcp_take_fd(void *stream);

// SSRF protection: check if a hostname resolves to a private/loopback IP
bool __ry_is_private_host(const char *host, int64_t port);

// SSRF protection: check if any address in a pre-resolved addrinfo list is private
bool __ry_is_private_addrinfo(const struct addrinfo *info);

// Resolve hostname to addrinfo list. Caller must freeaddrinfo(*out).
// Returns 0 on success, -1 on failure.
int __ry_resolve(const char *host, int64_t port, struct addrinfo **out);

extern "C" {

void *__ry_bind(const char *host, int64_t port);
int64_t __ry_listen(void *listener, int64_t backlog);
void *__ry_accept(void *listener);
void *__ry_connect(const char *host, int64_t port);
void *__ry_connect_resolved(const struct addrinfo *info);
int64_t __ry_tcp_send(void *stream, void *byte_list);
void   *__ry_tcp_receive(void *stream, int64_t max_bytes);
void    __ry_tcp_close(void *handle);
void    __ry_tcp_listener_close(void *listener);
void    __ry_tcp_listener_shutdown(void *listener);
int64_t __ry_listener_port(void *listener);
void    __ry_tcp_set_timeout(void *stream, int64_t ms);
void    __ry_tcp_set_recv_timeout(void *stream, int64_t ms);
void    __ry_tcp_set_send_timeout(void *stream, int64_t ms);

}
