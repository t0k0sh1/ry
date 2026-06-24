#include "ry/runtime/core/alloc.hpp"
#include "ry/runtime/core/error.hpp"
#include "ry/runtime/native/http/tls.hpp"
#include "ry/runtime/core/arc.hpp"
#include "ry/runtime/native/net_utils.hpp"
#include "ry/runtime/native/io.hpp"

#include <openssl/ssl.h>
#include <openssl/err.h>
#include <openssl/x509v3.h>

#include <cerrno>
#include <climits>
#include <csignal>
#include <cstdlib>
#include <cstring>
#include <mutex>
#include <new>
#include <unistd.h>
#include <sys/socket.h>
#include <sys/time.h>


namespace ry {

DEFINE_LAST_ERROR(tls)

struct TlsStreamHandle {
    int fd;
    SSL *ssl;
};

// ============================================================
// Global SSL_CTX (lazy-initialized, singleton)
// ============================================================

static SSL_CTX *g_tls_ctx = nullptr;
static std::once_flag g_tls_init;

static SSL_CTX *get_global_tls_ctx() {
    std::call_once(g_tls_init, []() {
        // OpenSSL's SSL_connect / SSL_write call raw write(2) without
        // MSG_NOSIGNAL, so a peer that closes the connection mid-handshake
        // (e.g. plain TCP listener receiving a ClientHello) kills the
        // process with SIGPIPE on Linux. macOS uses SO_NOSIGPIPE at the
        // socket layer, but ignoring the signal is harmless there.
        // Preserve any custom SIGPIPE disposition installed by an embedding
        // host so library users aren't forced into our policy when they
        // already have their own.
        auto prev = std::signal(SIGPIPE, SIG_IGN);
        if (prev != SIG_DFL && prev != SIG_IGN && prev != SIG_ERR) {
            std::signal(SIGPIPE, prev);
        }
        g_tls_ctx = SSL_CTX_new(TLS_client_method());
        if (!g_tls_ctx) return;
        SSL_CTX_set_default_verify_paths(g_tls_ctx);
        SSL_CTX_set_verify(g_tls_ctx, SSL_VERIFY_PEER, nullptr);
        SSL_CTX_set_min_proto_version(g_tls_ctx, TLS1_2_VERSION);
        std::atexit([]() {
            if (g_tls_ctx) { SSL_CTX_free(g_tls_ctx); g_tls_ctx = nullptr; }
        });
    });
    return g_tls_ctx;
}

// ============================================================
// SSL_write loop (handles partial writes)
// ============================================================

ssize_t __ry_tls_send_all(SSL *ssl, const void *buf, size_t len) {
    auto *data = static_cast<const char *>(buf);
    size_t remaining = len;
    while (remaining > 0) {
        int chunk = (remaining > (size_t)INT_MAX) ? INT_MAX : (int)remaining;
        int n = SSL_write(ssl, data, chunk);
        if (n <= 0) {
            int err = SSL_get_error(ssl, n);
            if (err == SSL_ERROR_WANT_WRITE || err == SSL_ERROR_WANT_READ)
                continue;
            return -1;
        }
        data += n;
        remaining -= (size_t)n;
    }
    return (ssize_t)len;
}

// ============================================================
// tls_connect
// ============================================================

static void *tls_handshake(const char *host, int fd) {
    SSL_CTX *ctx = get_global_tls_ctx();
    if (!ctx) {
        setLastError("tlsConnect: SSL context initialization failed");
        ::close(fd);
        return nullptr;
    }

    SSL *ssl = SSL_new(ctx);
    if (!ssl) {
        char ebuf[256];
        ERR_error_string_n(ERR_get_error(), ebuf, sizeof(ebuf));
        setLastError("tlsConnect: SSL_new failed: %s", ebuf);
        ::close(fd);
        return nullptr;
    }

    SSL_set_fd(ssl, fd);
    // Set SNI hostname
    SSL_set_tlsext_host_name(ssl, host);
    // Enable hostname verification
    SSL_set1_host(ssl, host);

    // Perform TLS handshake
    int ret = SSL_connect(ssl);
    if (ret != 1) {
        char ebuf[256];
        ERR_error_string_n(ERR_get_error(), ebuf, sizeof(ebuf));
        setLastError("tlsConnect: TLS handshake failed: %s", ebuf);
        SSL_free(ssl);
        ::close(fd);
        return nullptr;
    }

    // Certificate verification is enforced by SSL_VERIFY_PEER + SSL_set1_host
    long verify_result = SSL_get_verify_result(ssl);
    if (verify_result != X509_V_OK) {
        setLastError("tlsConnect: certificate verification failed: %s",
                     X509_verify_cert_error_string(verify_result));
        SSL_shutdown(ssl);
        SSL_free(ssl);
        ::close(fd);
        return nullptr;
    }

    void *mem = arc_alloc(sizeof(TlsStreamHandle));
    if (!mem) {
        setLastError("tlsConnect: memory allocation failed");
        SSL_shutdown(ssl);
        SSL_free(ssl);
        ::close(fd);
        return nullptr;
    }
    auto *handle = new (mem) TlsStreamHandle;
    handle->fd = fd;
    handle->ssl = ssl;
    return handle;
}

extern "C" void *__ry_tls_connect_resolved(const char *host, const ::addrinfo *info) {
    void *tcp = ry_net_connect_resolved(info);
    if (!tcp) {
        setLastError("tlsConnect: cannot connect to %s: %s", host, strerror(errno));
        return nullptr;
    }
    int fd = ry_net_tcp_take_fd(tcp);
    return tls_handshake(host, fd);
}

extern "C" void *__ry_net_tls_connect(const char *host, int64_t port) {
    void *tcp = ry_net_connect(host, port);
    if (!tcp) {
        setLastError("tlsConnect: cannot connect to %s:%lld: %s",
                     host, (long long)port, strerror(errno));
        return nullptr;
    }
    int fd = ry_net_tcp_take_fd(tcp);
    return tls_handshake(host, fd);
}

// ============================================================
// tls_send
// ============================================================

extern "C" int64_t __ry_tls_send(void *tls_stream, void *byte_list) {
    auto *handle = (TlsStreamHandle *)tls_stream;
    auto *header = (IOListHeader *)byte_list;
    ssize_t sent = __ry_tls_send_all(handle->ssl, header->data, (size_t)header->len);
    return (int64_t)sent;
}

// ============================================================
// tls_receive
// ============================================================

extern "C" void *__ry_tls_receive(void *tls_stream, int64_t max_bytes) {
    auto *h = (TlsStreamHandle *)tls_stream;
    ry_net_apply_default_recv_timeout(h->fd);
    if (max_bytes <= 0) {
        return makeEmptyIOList();
    }
    auto *handle = (TlsStreamHandle *)tls_stream;

    auto *header = (IOListHeader *)arc_alloc(sizeof(IOListHeader));
    header->data = (int8_t *)checked_malloc((size_t)max_bytes);

    int n = SSL_read(handle->ssl, header->data, (int)max_bytes);
    if (n < 0) {
        free(header->data);
        arc_free(header);
        return nullptr;
    }
    if (n == 0) {
        free(header->data);
        header->data = nullptr;
        header->len = 0;
        header->cap = 0;
        return header;
    }
    header->len = (int64_t)n;
    header->cap = max_bytes;
    return header;
}

// ============================================================
// tls_close
// ============================================================

extern "C" void __ry_tls_close(void *tls_stream) {
    if (!tls_stream) return;
    auto *handle = (TlsStreamHandle *)tls_stream;
    SSL_shutdown(handle->ssl);
    SSL_free(handle->ssl);
    ::close(handle->fd);
    arc_free(tls_stream);
}

extern "C" void __ry_tls_cleanup(void *tls_stream) {
    if (!tls_stream) return;
    auto *handle = (TlsStreamHandle *)tls_stream;
    if (handle->ssl) {
        SSL_shutdown(handle->ssl);
        SSL_free(handle->ssl);
        handle->ssl = nullptr;
    }
    if (handle->fd >= 0) {
        ::close(handle->fd);
        handle->fd = -1;
    }
}

// ============================================================
// Ownership transfer (for HttpTransport integration)
// ============================================================

void __ry_tls_take_ownership(void *tls_stream, int *out_fd, SSL **out_ssl) {
    auto *handle = (TlsStreamHandle *)tls_stream;
    *out_fd = handle->fd;
    *out_ssl = handle->ssl;
    arc_free(tls_stream);
}

// ============================================================
// Timeout configuration
// ============================================================

extern "C" void __ry_tls_set_timeout(void *tls_stream, int64_t ms) {
    if (!tls_stream) return;
    auto *handle = (TlsStreamHandle *)tls_stream;
    ry_net_set_socket_timeval(handle->fd, SO_RCVTIMEO, ms);
    ry_net_set_socket_timeval(handle->fd, SO_SNDTIMEO, ms);
}

extern "C" void __ry_tls_set_recv_timeout(void *tls_stream, int64_t ms) {
    if (!tls_stream) return;
    auto *handle = (TlsStreamHandle *)tls_stream;
    ry_net_set_socket_timeval(handle->fd, SO_RCVTIMEO, ms);
}

extern "C" void __ry_tls_set_send_timeout(void *tls_stream, int64_t ms) {
    if (!tls_stream) return;
    auto *handle = (TlsStreamHandle *)tls_stream;
    ry_net_set_socket_timeval(handle->fd, SO_SNDTIMEO, ms);
}

} // namespace ry
