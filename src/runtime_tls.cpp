#include "ry/runtime_tls.hpp"
#include "ry/runtime_arc.hpp"
#include "ry/runtime_net.hpp"
#include "ry/runtime_io.hpp"

#include <openssl/ssl.h>
#include <openssl/err.h>
#include <openssl/x509v3.h>

#include <climits>
#include <cstdlib>
#include <cstring>
#include <mutex>
#include <new>
#include <unistd.h>
#include <sys/socket.h>
#include <sys/time.h>

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

extern "C" void *__ry_tls_connect(const char *host, int64_t port) {
    // Establish plain TCP connection first
    void *tcp = __ry_connect(host, port);
    if (!tcp) return nullptr;

    int fd = __ry_tcp_take_fd(tcp);

    SSL_CTX *ctx = get_global_tls_ctx();
    if (!ctx) {
        ::close(fd);
        return nullptr;
    }

    SSL *ssl = SSL_new(ctx);
    if (!ssl) {
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
        SSL_free(ssl);
        ::close(fd);

        return nullptr;
    }

    // Certificate verification is enforced by SSL_VERIFY_PEER + SSL_set1_host
    long verify_result = SSL_get_verify_result(ssl);
    if (verify_result != X509_V_OK) {
        SSL_shutdown(ssl);
        SSL_free(ssl);
        ::close(fd);

        return nullptr;
    }

    void *mem = arc_alloc(sizeof(TlsStreamHandle));
    if (!mem) {
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
// tls_recv
// ============================================================

extern "C" void *__ry_tls_receive(void *tls_stream, int64_t max_bytes) {
    auto *h = (TlsStreamHandle *)tls_stream;
    __ry_apply_default_recv_timeout(h->fd);
    if (max_bytes <= 0) {
        auto *header = (IOListHeader *)malloc(sizeof(IOListHeader));
        header->len = 0;
        header->cap = 0;
        header->data = nullptr;
        return header;
    }
    auto *handle = (TlsStreamHandle *)tls_stream;

    auto *header = (IOListHeader *)malloc(sizeof(IOListHeader));
    header->data = (int8_t *)malloc((size_t)max_bytes);

    int n = SSL_read(handle->ssl, header->data, (int)max_bytes);
    if (n < 0) {
        free(header->data);
        free(header);
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
    __ry_set_socket_timeval(handle->fd, SO_RCVTIMEO, ms);
    __ry_set_socket_timeval(handle->fd, SO_SNDTIMEO, ms);
}

extern "C" void __ry_tls_set_recv_timeout(void *tls_stream, int64_t ms) {
    if (!tls_stream) return;
    auto *handle = (TlsStreamHandle *)tls_stream;
    __ry_set_socket_timeval(handle->fd, SO_RCVTIMEO, ms);
}

extern "C" void __ry_tls_set_send_timeout(void *tls_stream, int64_t ms) {
    if (!tls_stream) return;
    auto *handle = (TlsStreamHandle *)tls_stream;
    __ry_set_socket_timeval(handle->fd, SO_SNDTIMEO, ms);
}
