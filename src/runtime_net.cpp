#include "ry/runtime_net.hpp"
#include "ry/runtime_net_types.hpp"
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

struct TcpListenerHandle {
    int fd;
    std::atomic<bool> shutdown{false};
};


extern "C" void *__ry_bind(const char *host, int64_t port) {
    if (port < 0 || port > 65535)
        return nullptr;

    struct addrinfo hints{}, *result = nullptr;
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
    int client_fd = ::accept(handle->fd, (struct sockaddr *)&client_addr, &addr_len);
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

static bool is_private_addr(const struct sockaddr *sa) {
    if (sa->sa_family == AF_INET) {
        auto *sin = (const struct sockaddr_in *)sa;
        uint32_t ip = ntohl(sin->sin_addr.s_addr);
        if ((ip >> 24) == 127) return true;   // 127.0.0.0/8
        if ((ip >> 24) == 10) return true;    // 10.0.0.0/8
        if ((ip >> 20) == 0xAC1) return true; // 172.16.0.0/12
        if ((ip >> 16) == 0xC0A8) return true; // 192.168.0.0/16
        if ((ip >> 16) == 0xA9FE) return true; // 169.254.0.0/16
        if ((ip >> 24) == 0) return true;     // 0.0.0.0/8
        return false;
    }
    if (sa->sa_family == AF_INET6) {
        auto *sin6 = (const struct sockaddr_in6 *)sa;
        if (IN6_IS_ADDR_LOOPBACK(&sin6->sin6_addr)) return true;
        if (IN6_IS_ADDR_LINKLOCAL(&sin6->sin6_addr)) return true;
        uint8_t first = sin6->sin6_addr.s6_addr[0];
        if ((first & 0xFE) == 0xFC) return true; // fc00::/7 (ULA)
        return false;
    }
    return false;
}

bool __ry_is_private_host(const char *host, int64_t port) {
    struct addrinfo hints{}, *result = nullptr;
    hints.ai_family = AF_UNSPEC;
    hints.ai_socktype = SOCK_STREAM;

    char port_str[16];
    snprintf(port_str, sizeof(port_str), "%lld", (long long)port);

    if (::getaddrinfo(host, port_str, &hints, &result) != 0)
        return false;

    for (struct addrinfo *rp = result; rp; rp = rp->ai_next) {
        if (is_private_addr(rp->ai_addr)) {
            ::freeaddrinfo(result);
            return true;
        }
    }
    ::freeaddrinfo(result);
    return false;
}

extern "C" void *__ry_connect(const char *host, int64_t port) {
    struct addrinfo hints{}, *result = nullptr;
    hints.ai_family = AF_INET;
    hints.ai_socktype = SOCK_STREAM;

    char port_str[16];
    snprintf(port_str, sizeof(port_str), "%lld", (long long)port);

    if (::getaddrinfo(host, port_str, &hints, &result) != 0)
        return nullptr;

    int fd = ::socket(result->ai_family, result->ai_socktype, result->ai_protocol);
    if (fd < 0) {
        ::freeaddrinfo(result);
        return nullptr;
    }
#ifdef SO_NOSIGPIPE
    {
        int nosig = 1;
        ::setsockopt(fd, SOL_SOCKET, SO_NOSIGPIPE, &nosig, sizeof(nosig));
    }
#endif

    // Non-blocking connect with 5-second timeout
    int flags = ::fcntl(fd, F_GETFL, 0);
    if (flags < 0 || ::fcntl(fd, F_SETFL, flags | O_NONBLOCK) < 0) {
        ::close(fd);
        return nullptr;
    }

    int conn_ret = ::connect(fd, result->ai_addr, result->ai_addrlen);
    ::freeaddrinfo(result);

    if (conn_ret < 0 && errno != EINPROGRESS) {
        ::close(fd);
        return nullptr;
    }

    if (conn_ret < 0) {
        struct pollfd pfd = {fd, POLLOUT, 0};
        int poll_ret = ::poll(&pfd, 1, 5000);
        if (poll_ret <= 0) {
            ::close(fd);
            return nullptr;
        }
        int so_error = 0;
        socklen_t len = sizeof(so_error);
        if (::getsockopt(fd, SOL_SOCKET, SO_ERROR, &so_error, &len) < 0 || so_error != 0) {
            ::close(fd);
            return nullptr;
        }
    }

    // Restore blocking mode
    ::fcntl(fd, F_SETFL, flags);

    void *smem = arc_alloc(sizeof(TcpStreamHandle));
    if (!smem) {
        ::close(fd);
        return nullptr;
    }
    auto *stream = new (smem) TcpStreamHandle;
    stream->fd = fd;
    return stream;
}

ssize_t __ry_send_all(int fd, const void *buf, size_t len) {
    auto *data = static_cast<const char *>(buf);
    size_t remaining = len;
    // Use MSG_NOSIGNAL on Linux to suppress SIGPIPE (macOS uses SO_NOSIGPIPE instead)
    int flags = 0;
#ifdef MSG_NOSIGNAL
    flags = MSG_NOSIGNAL;
#endif
    while (remaining > 0) {
        ssize_t n = ::send(fd, data, remaining, flags);
        if (n < 0) {
            if (errno == EINTR) continue;
            return -1;
        }
        if (n == 0) return -1;  // defensive: should not happen with remaining > 0
        data += n;
        remaining -= static_cast<size_t>(n);
    }
    return static_cast<ssize_t>(len);
}

extern "C" int64_t __ry_tcp_send(void *stream, void *byte_list) {
    auto *handle = (TcpStreamHandle *)stream;
    auto *header = (IOListHeader *)byte_list;
    ssize_t sent = __ry_send_all(handle->fd, header->data, (size_t)header->len);
    return (int64_t)sent;
}

static IOListHeader *makeEmptyIOList() {
    auto *header = (IOListHeader *)malloc(sizeof(IOListHeader));
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
    __ry_apply_default_recv_timeout(handle->fd);
    auto *header = (IOListHeader *)malloc(sizeof(IOListHeader));
    if (!header) return nullptr;
    header->data = (int8_t *)malloc((size_t)max_bytes);
    if (!header->data) { free(header); return nullptr; }
    ssize_t n = ::recv(handle->fd, header->data, (size_t)max_bytes, 0);
    if (n < 0) {
        // Error: free everything and return nullptr
        free(header->data);
        free(header);
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
        if (auto *shrunk = (int8_t *)realloc(header->data, (size_t)n))
            header->data = shrunk;
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
// Timeout configuration
// ============================================================

void __ry_set_socket_timeval(int fd, int option, int64_t ms) {
    struct timeval tv;
    if (ms <= 0) {
        tv.tv_sec = 0;
        tv.tv_usec = 0;
    } else {
        tv.tv_sec = ms / 1000;
        tv.tv_usec = (ms % 1000) * 1000;
    }
    ::setsockopt(fd, SOL_SOCKET, option, &tv, sizeof(tv));
}

int __ry_tcp_take_fd(void *stream) {
    auto *handle = (TcpStreamHandle *)stream;
    int fd = handle->fd;
    arc_free(stream);
    return fd;
}

void __ry_apply_default_recv_timeout(int fd) {
    struct timeval current_tv{};
    socklen_t tv_len = sizeof(current_tv);
    if (::getsockopt(fd, SOL_SOCKET, SO_RCVTIMEO, &current_tv, &tv_len) == 0 &&
        current_tv.tv_sec == 0 && current_tv.tv_usec == 0) {
        struct timeval tv = {30, 0};
        ::setsockopt(fd, SOL_SOCKET, SO_RCVTIMEO, &tv, sizeof(tv));
    }
}

extern "C" void __ry_tcp_set_timeout(void *stream, int64_t ms) {
    if (!stream) return;
    auto *handle = (TcpStreamHandle *)stream;
    __ry_set_socket_timeval(handle->fd, SO_RCVTIMEO, ms);
    __ry_set_socket_timeval(handle->fd, SO_SNDTIMEO, ms);
}

extern "C" void __ry_tcp_set_recv_timeout(void *stream, int64_t ms) {
    if (!stream) return;
    auto *handle = (TcpStreamHandle *)stream;
    __ry_set_socket_timeval(handle->fd, SO_RCVTIMEO, ms);
}

extern "C" void __ry_tcp_set_send_timeout(void *stream, int64_t ms) {
    if (!stream) return;
    auto *handle = (TcpStreamHandle *)stream;
    __ry_set_socket_timeval(handle->fd, SO_SNDTIMEO, ms);
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
    if (::getsockname(handle->fd, (struct sockaddr *)&addr, &len) < 0)
        return -1;
    return (int64_t)ntohs(addr.sin_port);
}
