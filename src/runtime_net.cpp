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

struct TcpListenerHandle {
    int fd;
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

    auto *handle = (TcpListenerHandle *)malloc(sizeof(TcpListenerHandle));
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
    struct timeval tv = {1, 0};
    if (::setsockopt(handle->fd, SOL_SOCKET, SO_RCVTIMEO, &tv, sizeof(tv)) < 0)
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

    auto *stream = (TcpStreamHandle *)malloc(sizeof(TcpStreamHandle));
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

    auto *stream = (TcpStreamHandle *)malloc(sizeof(TcpStreamHandle));
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
    // Works for both TcpListenerHandle and TcpStreamHandle since fd is at offset 0
    int fd = ((TcpListenerHandle *)handle)->fd;
    ::close(fd);
    free(handle);
}
