#include "ry/runtime_alloc.hpp"
#include "ry/runtime_net.hpp"
#include "ry/runtime_net_types.hpp"
#include "ry/runtime_net_utils.hpp"
#include "ry/runtime_io.hpp"
#include "ry/runtime_arc.hpp"

#include <cstdio>
#include <cstdlib>
#include <cstring>

#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <netdb.h>
#include <unistd.h>
#include <fcntl.h>
#include <poll.h>
#include <errno.h>
#include <sys/time.h>

#include <atomic>
#include <new>


namespace ry {

struct TcpListenerHandle {
    int fd;
    std::atomic<bool> shutdown{false};
};


extern "C" void *__ry_bind(const char *host, int64_t port) {
    if (port < 0 || port > 65535)
        return nullptr;

    ::addrinfo hints{}, *result = nullptr;
    hints.ai_family = AF_INET;
    hints.ai_socktype = SOCK_STREAM;
    hints.ai_flags = AI_PASSIVE;

    char port_str[16];
    snprintf(port_str, sizeof(port_str), "%lld", (long long)port);

    if (::getaddrinfo(host, port_str, &hints, &result) != 0)
        return nullptr;

    int fd = ::socket(result->ai_family, result->ai_socktype, result->ai_protocol);
    if (fd < 0) {
        ::freeaddrinfo(result);
        return nullptr;
    }

    int opt = 1;
    ::setsockopt(fd, SOL_SOCKET, SO_REUSEADDR, &opt, sizeof(opt));
#ifdef SO_NOSIGPIPE
    ::setsockopt(fd, SOL_SOCKET, SO_NOSIGPIPE, &opt, sizeof(opt));
#endif

    if (::bind(fd, result->ai_addr, result->ai_addrlen) < 0) {
        ::close(fd);
        ::freeaddrinfo(result);
        return nullptr;
    }

    ::freeaddrinfo(result);

    void *mem = arc_alloc(sizeof(TcpListenerHandle));
    if (!mem) {
        ::close(fd);
        return nullptr;
    }
    auto *handle = new (mem) TcpListenerHandle;
    handle->fd = fd;
    return handle;
}

extern "C" int64_t __ry_listen(void *listener, int64_t backlog) {
    auto *handle = (TcpListenerHandle *)listener;
    if (::listen(handle->fd, (int)backlog) < 0) {
        return -1;
    }
    return 0;
}

extern "C" void *__ry_accept(void *listener) {
    auto *handle = (TcpListenerHandle *)listener;
    if (handle->shutdown.load(std::memory_order_relaxed)) {
        errno = ECANCELED;
        return nullptr;
    }

    // Use poll() for cross-platform timeout (SO_RCVTIMEO doesn't work
    // for accept() on macOS).
    struct pollfd pfd = {handle->fd, POLLIN, 0};
    int poll_ret = ::poll(&pfd, 1, 1000);  // 1-second timeout
    if (poll_ret == 0) {
        errno = ETIMEDOUT;
        return nullptr;
    }
    if (poll_ret < 0)
        return nullptr;
    if (pfd.revents & (POLLERR | POLLNVAL | POLLHUP))
        return nullptr;

    // Shutdown may have been requested while poll() was blocking.
    if (handle->shutdown.load(std::memory_order_relaxed)) {
        errno = ECANCELED;
        return nullptr;
    }

    struct sockaddr_in client_addr{};
    socklen_t addr_len = sizeof(client_addr);
    int client_fd = ::accept(handle->fd, reinterpret_cast<struct sockaddr *>(&client_addr), &addr_len);
    if (client_fd < 0)
        return nullptr;
#ifdef SO_NOSIGPIPE
    int nosig = 1;
    ::setsockopt(client_fd, SOL_SOCKET, SO_NOSIGPIPE, &nosig, sizeof(nosig));
#endif

    void *smem = arc_alloc(sizeof(TcpStreamHandle));
    if (!smem) {
        ::close(client_fd);
        return nullptr;
    }
    auto *stream = new (smem) TcpStreamHandle;
    stream->fd = client_fd;
    return stream;
}

// Utility functions are implemented in runtime_net_utils.hpp (static inline).
// The extern "C" wrappers below export them for JIT / Ry code to call.

int __ry_resolve(const char *host, int64_t port, ::addrinfo **out) {
    return ry_net_resolve(host, port, out);
}

bool __ry_is_private_addrinfo(const ::addrinfo *info) {
    return ry_net_is_private_addrinfo(info);
}

bool __ry_is_private_addr(const struct sockaddr *sa) {
    if (!sa) return false;
    return ry_net_is_private_addr(sa);
}

bool __ry_is_private_host(const char *host, int64_t port) {
    ::addrinfo *result = nullptr;
    if (ry_net_resolve(host, port, &result) != 0)
        return false;
    bool priv = ry_net_is_private_addrinfo(result);
    ::freeaddrinfo(result);
    return priv;
}

extern "C" void *__ry_connect_resolved(const ::addrinfo *info) {
    return ry_net_connect_resolved(info);
}

extern "C" void *__ry_connect(const char *host, int64_t port) {
    return ry_net_connect(host, port);
}

ssize_t __ry_send_all(int fd, const void *buf, size_t len) {
    return ry_net_send_all(fd, buf, len);
}

extern "C" int64_t __ry_tcp_send(void *stream, void *byte_list) {
    auto *handle = (TcpStreamHandle *)stream;
    auto *header = (IOListHeader *)byte_list;
    ssize_t sent = ry_net_send_all(handle->fd, header->data, (size_t)header->len);
    return (int64_t)sent;
}

static IOListHeader *makeEmptyIOList() {
    auto *header = (IOListHeader *)arc_alloc(sizeof(IOListHeader));
    header->len = 0;
    header->cap = 0;
    header->data = nullptr;
    return header;
}

extern "C" void *__ry_tcp_receive(void *stream, int64_t max_bytes) {
    if (max_bytes <= 0) {
        return makeEmptyIOList();
    }
    auto *handle = (TcpStreamHandle *)stream;
    ry_net_apply_default_recv_timeout(handle->fd);
    auto *header = (IOListHeader *)arc_alloc(sizeof(IOListHeader));
    header->data = (int8_t *)checked_malloc((size_t)max_bytes);
    ssize_t n = ::recv(handle->fd, header->data, (size_t)max_bytes, 0);
    if (n < 0) {
        // Error: free everything and return nullptr
        free(header->data);
        arc_free(header);
        return nullptr;
    }
    if (n == 0) {
        // Connection closed: return empty list (non-null)
        free(header->data);
        header->data = nullptr;
        header->len = 0;
        header->cap = 0;
        return header;
    }
    if (n < (ssize_t)max_bytes) {
        header->data = (int8_t *)checked_realloc(header->data, (size_t)n);
    }
    header->len = (int64_t)n;
    header->cap = (int64_t)n;
    return header;
}

extern "C" void __ry_tcp_close(void *handle) {
    if (!handle) return;
    auto *stream = (TcpStreamHandle *)handle;
    ::close(stream->fd);
    arc_free(handle);
}

extern "C" void __ry_tcp_listener_close(void *listener) {
    if (!listener) return;
    auto *handle = (TcpListenerHandle *)listener;
    ::close(handle->fd);
    arc_free(listener);
}

extern "C" void __ry_tcp_listener_shutdown(void *listener) {
    if (!listener) return;
    auto *handle = (TcpListenerHandle *)listener;
    handle->shutdown.store(true, std::memory_order_relaxed);
    ::shutdown(handle->fd, SHUT_RDWR);
}

// ============================================================
// Timeout configuration — delegate to shared utils
// ============================================================

void __ry_set_socket_timeval(int fd, int option, int64_t ms) {
    ry_net_set_socket_timeval(fd, option, ms);
}

int __ry_tcp_take_fd(void *stream) {
    return ry_net_tcp_take_fd(stream);
}

void __ry_apply_default_recv_timeout(int fd) {
    ry_net_apply_default_recv_timeout(fd);
}

extern "C" void __ry_tcp_set_timeout(void *stream, int64_t ms) {
    if (!stream) return;
    auto *handle = (TcpStreamHandle *)stream;
    ry_net_set_socket_timeval(handle->fd, SO_RCVTIMEO, ms);
    ry_net_set_socket_timeval(handle->fd, SO_SNDTIMEO, ms);
}

extern "C" void __ry_tcp_set_recv_timeout(void *stream, int64_t ms) {
    if (!stream) return;
    auto *handle = (TcpStreamHandle *)stream;
    ry_net_set_socket_timeval(handle->fd, SO_RCVTIMEO, ms);
}

extern "C" void __ry_tcp_set_send_timeout(void *stream, int64_t ms) {
    if (!stream) return;
    auto *handle = (TcpStreamHandle *)stream;
    ry_net_set_socket_timeval(handle->fd, SO_SNDTIMEO, ms);
}

extern "C" void __ry_tcp_cleanup(void *handle) {
    if (!handle) return;
    auto *h = static_cast<TcpStreamHandle *>(handle);
    if (h->fd >= 0) {
        shutdown(h->fd, SHUT_RDWR);
        ::close(h->fd);
        h->fd = -1;
    }
}

extern "C" void __ry_tcp_listener_cleanup(void *listener) {
    if (!listener) return;
    auto *h = static_cast<TcpListenerHandle *>(listener);
    if (h->fd >= 0) {
        ::close(h->fd);
        h->fd = -1;
    }
}

extern "C" int64_t __ry_listener_port(void *listener) {
    if (!listener) return -1;
    auto *handle = (TcpListenerHandle *)listener;
    struct sockaddr_in addr{};
    socklen_t len = sizeof(addr);
    if (::getsockname(handle->fd, reinterpret_cast<struct sockaddr *>(&addr), &len) < 0)
        return -1;
    return (int64_t)ntohs(addr.sin_port);
}

} // namespace ry
