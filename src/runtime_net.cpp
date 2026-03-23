#include "ry/runtime_net.hpp"
#include "ry/runtime_io.hpp"

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

struct TcpStreamHandle {
    int fd;
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

    auto *handle = new (std::nothrow) TcpListenerHandle;
    if (!handle) {
        ::close(fd);
        return nullptr;
    }
    handle->fd = fd;
    return handle;
}

extern "C" void __ry_listen(void *listener, int64_t backlog) {
    auto *handle = (TcpListenerHandle *)listener;
    if (::listen(handle->fd, (int)backlog) < 0) {
        fprintf(stderr, "runtime error: listen() failed\n");
        exit(1);
    }
}

extern "C" void *__ry_accept(void *listener) {
    auto *handle = (TcpListenerHandle *)listener;
    if (handle->shutdown.load(std::memory_order_relaxed))
        return nullptr;

    // Use poll() for cross-platform timeout (SO_RCVTIMEO doesn't work
    // for accept() on macOS).
    struct pollfd pfd = {handle->fd, POLLIN, 0};
    int poll_ret = ::poll(&pfd, 1, 1000);  // 1-second timeout
    if (poll_ret <= 0)
        return nullptr;
    if (pfd.revents & (POLLERR | POLLNVAL | POLLHUP))
        return nullptr;

    struct sockaddr_in client_addr{};
    socklen_t addr_len = sizeof(client_addr);
    int client_fd = ::accept(handle->fd, (struct sockaddr *)&client_addr, &addr_len);
    if (client_fd < 0)
        return nullptr;
#ifdef SO_NOSIGPIPE
    int nosig = 1;
    ::setsockopt(client_fd, SOL_SOCKET, SO_NOSIGPIPE, &nosig, sizeof(nosig));
#endif

    auto *stream = new (std::nothrow) TcpStreamHandle;
    if (!stream) {
        ::close(client_fd);
        return nullptr;
    }
    stream->fd = client_fd;
    return stream;
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

    auto *stream = new (std::nothrow) TcpStreamHandle;
    if (!stream) {
        ::close(fd);
        return nullptr;
    }
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
    if (sent < 0) {
        fprintf(stderr, "runtime error: tcp send() failed\n");
        exit(1);
    }
    return (int64_t)sent;
}

static IOListHeader *makeEmptyIOList() {
    auto *header = (IOListHeader *)malloc(sizeof(IOListHeader));
    header->len = 0;
    header->cap = 0;
    header->data = nullptr;
    return header;
}

extern "C" void *__ry_tcp_recv(void *stream, int64_t max_bytes) {
    if (max_bytes <= 0) {
        return makeEmptyIOList();
    }
    auto *handle = (TcpStreamHandle *)stream;
    struct timeval tv = {30, 0};
    ::setsockopt(handle->fd, SOL_SOCKET, SO_RCVTIMEO, &tv, sizeof(tv));
    auto *header = (IOListHeader *)malloc(sizeof(IOListHeader));
    header->data = (int8_t *)malloc((size_t)max_bytes);
    ssize_t n = ::recv(handle->fd, header->data, (size_t)max_bytes, 0);
    if (n <= 0) {
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

extern "C" void __ry_tcp_close(void *handle) {
    if (!handle) return;
    auto *stream = (TcpStreamHandle *)handle;
    ::close(stream->fd);
    delete stream;
}

extern "C" void __ry_tcp_listener_close(void *listener) {
    if (!listener) return;
    auto *handle = (TcpListenerHandle *)listener;
    ::close(handle->fd);
    delete handle;
}

extern "C" void __ry_tcp_listener_shutdown(void *listener) {
    if (!listener) return;
    auto *handle = (TcpListenerHandle *)listener;
    handle->shutdown.store(true, std::memory_order_relaxed);
    ::shutdown(handle->fd, SHUT_RDWR);
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
