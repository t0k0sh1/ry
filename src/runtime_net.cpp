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

struct TcpListenerHandle {
    int fd;
};

struct TcpStreamHandle {
    int fd;
};

extern "C" void *__ry_bind(const char *host, int64_t port) {
    int fd = ::socket(AF_INET, SOCK_STREAM, 0);
    if (fd < 0)
        return nullptr;

    if (port < 0 || port > 65535) {
        ::close(fd);
        return nullptr;
    }

    int opt = 1;
    ::setsockopt(fd, SOL_SOCKET, SO_REUSEADDR, &opt, sizeof(opt));

    struct sockaddr_in addr{};
    addr.sin_family = AF_INET;
    addr.sin_port = htons(static_cast<uint16_t>(port));

    if (::inet_pton(AF_INET, host, &addr.sin_addr) <= 0) {
        ::close(fd);
        return nullptr;
    }

    if (::bind(fd, (struct sockaddr *)&addr, sizeof(addr)) < 0) {
        ::close(fd);
        return nullptr;
    }

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
    struct sockaddr_in client_addr{};
    socklen_t addr_len = sizeof(client_addr);
    int client_fd = ::accept(handle->fd, (struct sockaddr *)&client_addr, &addr_len);
    if (client_fd < 0)
        return nullptr;

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

    if (::connect(fd, result->ai_addr, result->ai_addrlen) < 0) {
        ::close(fd);
        ::freeaddrinfo(result);
        return nullptr;
    }

    ::freeaddrinfo(result);

    auto *stream = (TcpStreamHandle *)malloc(sizeof(TcpStreamHandle));
    stream->fd = fd;
    return stream;
}

extern "C" int64_t __ry_tcp_send(void *stream, void *byte_list) {
    auto *handle = (TcpStreamHandle *)stream;
    auto *header = (IOListHeader *)byte_list;
    ssize_t sent = ::send(handle->fd, header->data, (size_t)header->len, 0);
    if (sent < 0) {
        fprintf(stderr, "runtime error: tcp send() failed\n");
        exit(1);
    }
    return (int64_t)sent;
}

extern "C" void *__ry_tcp_recv(void *stream, int64_t max_bytes) {
    if (max_bytes <= 0) {
        auto *header = (IOListHeader *)malloc(sizeof(IOListHeader));
        header->len = 0;
        header->cap = 0;
        header->data = nullptr;
        return header;
    }
    auto *handle = (TcpStreamHandle *)stream;
    auto *header = (IOListHeader *)malloc(sizeof(IOListHeader));
    header->data = (int8_t *)malloc((size_t)max_bytes);
    ssize_t n = ::recv(handle->fd, header->data, (size_t)max_bytes, 0);
    if (n < 0) {
        // Connection error: return empty list
        header->len = 0;
        header->cap = max_bytes;
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
