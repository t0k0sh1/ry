#pragma once

#include <cstdint>
#include <cstddef>
#include <sys/types.h>

// Forward declarations for C library types (must be at global scope)
using SSL = struct ssl_st;
struct addrinfo;

namespace ry {

// SSL_write loop with partial write and WANT_READ/WANT_WRITE handling.
ssize_t __ry_tls_send_all(SSL *ssl, const void *buf, size_t len);

// Extract fd and SSL* from a TlsStreamHandle, then free the handle shell.
// Caller takes ownership of both the fd and SSL* and must close them.
void __ry_tls_take_ownership(void *tls_stream, int *out_fd, SSL **out_ssl);

extern "C" {

void   *__ry_net_tls_connect(const char *host, int64_t port);
void   *__ry_tls_connect_resolved(const char *host, const struct addrinfo *info);
int64_t __ry_tls_send(void *tls_stream, void *byte_list);
void   *__ry_tls_receive(void *tls_stream, int64_t max_bytes);
void    __ry_tls_close(void *tls_stream);
void    __ry_tls_set_timeout(void *tls_stream, int64_t ms);
void    __ry_tls_set_recv_timeout(void *tls_stream, int64_t ms);
void    __ry_tls_set_send_timeout(void *tls_stream, int64_t ms);

}

} // namespace ry
