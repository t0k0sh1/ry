// Shared TCP utility functions used by both ry_net and ry_http.
// All functions have static linkage so each native lib gets its own copy,
// ensuring no inter-library link dependencies.
#pragma once

#include "ry/runtime_net_types.hpp"
#include "ry/runtime_arc.hpp"

#include <cstdint>
#include <cstdio>
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
#include <new>


namespace ry {

// ===== DNS resolution =====

static inline int ry_net_resolve(const char *host, int64_t port,
                                 ::addrinfo **out) {
    ::addrinfo hints{};
    hints.ai_family = AF_UNSPEC;
    hints.ai_socktype = SOCK_STREAM;

    char port_str[16];
    snprintf(port_str, sizeof(port_str), "%lld", (long long)port);

    *out = nullptr;
    if (::getaddrinfo(host, port_str, &hints, out) != 0)
        return -1;
    return 0;
}

// ===== SSRF protection =====

static inline bool ry_net_is_private_ipv4_raw(uint32_t ip) {
    if ((ip >> 24) == 127) return true;    // 127.0.0.0/8
    if ((ip >> 24) == 10) return true;     // 10.0.0.0/8
    if ((ip >> 20) == 0xAC1) return true;  // 172.16.0.0/12
    if ((ip >> 16) == 0xC0A8) return true; // 192.168.0.0/16
    if ((ip >> 16) == 0xA9FE) return true; // 169.254.0.0/16
    if ((ip >> 24) == 0) return true;      // 0.0.0.0/8
    if ((ip >> 22) == 0x191) return true;  // 100.64.0.0/10 (CGNAT)
    if ((ip >> 17) == 0x6309) return true; // 198.18.0.0/15 (benchmarking)
    if ((ip >> 28) >= 0xE) return true;    // 224.0.0.0/4+ (multicast + reserved)
    return false;
}

static inline bool ry_net_is_private_addr(const struct sockaddr *sa) {
    if (sa->sa_family == AF_INET) {
        auto *sin = (const struct sockaddr_in *)sa;
        return ry_net_is_private_ipv4_raw(ntohl(sin->sin_addr.s_addr));
    }
    if (sa->sa_family == AF_INET6) {
        auto *sin6 = (const struct sockaddr_in6 *)sa;
        if (IN6_IS_ADDR_V4MAPPED(&sin6->sin6_addr)) {
            uint32_t ip_raw;
            std::memcpy(&ip_raw, &sin6->sin6_addr.s6_addr[12], sizeof(ip_raw));
            return ry_net_is_private_ipv4_raw(ntohl(ip_raw));
        }
        if (IN6_IS_ADDR_LOOPBACK(&sin6->sin6_addr)) return true;
        if (IN6_IS_ADDR_UNSPECIFIED(&sin6->sin6_addr)) return true;
        if (IN6_IS_ADDR_MULTICAST(&sin6->sin6_addr)) return true;
        if (IN6_IS_ADDR_LINKLOCAL(&sin6->sin6_addr)) return true;
        uint8_t first = sin6->sin6_addr.s6_addr[0];
        if ((first & 0xFE) == 0xFC) return true; // fc00::/7 (ULA)
        return false;
    }
    return false;
}

static inline bool ry_net_is_private_addrinfo(const ::addrinfo *info) {
    for (const ::addrinfo *rp = info; rp; rp = rp->ai_next) {
        if (ry_net_is_private_addr(rp->ai_addr))
            return true;
    }
    return false;
}

// ===== Connection =====

static inline void *ry_net_connect_resolved(const ::addrinfo *info) {
    for (const ::addrinfo *rp = info; rp; rp = rp->ai_next) {
        int fd = ::socket(rp->ai_family, rp->ai_socktype, rp->ai_protocol);
        if (fd < 0) continue;
#ifdef SO_NOSIGPIPE
        {
            int nosig = 1;
            ::setsockopt(fd, SOL_SOCKET, SO_NOSIGPIPE, &nosig, sizeof(nosig));
        }
#endif
        // Non-blocking connect with timeout
        int flags = ::fcntl(fd, F_GETFL, 0);
        if (flags < 0 || ::fcntl(fd, F_SETFL, flags | O_NONBLOCK) < 0) {
            ::close(fd);
            continue;
        }

        int conn_ret = ::connect(fd, rp->ai_addr, rp->ai_addrlen);
        if (conn_ret < 0 && errno != EINPROGRESS) {
            ::close(fd);
            continue;
        }
        if (conn_ret < 0) {
            struct pollfd pfd = {fd, POLLOUT, 0};
            int poll_ret = ::poll(&pfd, 1, 1000);
            if (poll_ret <= 0) {
                ::close(fd);
                continue;
            }
            int so_error = 0;
            socklen_t len = sizeof(so_error);
            if (::getsockopt(fd, SOL_SOCKET, SO_ERROR, &so_error, &len) < 0 ||
                so_error != 0) {
                ::close(fd);
                continue;
            }
        }

        // Restore blocking mode
        if (::fcntl(fd, F_SETFL, flags) < 0) {
            ::close(fd);
            continue;
        }

        void *smem = arc_alloc(sizeof(TcpStreamHandle));
        if (!smem) {
            ::close(fd);
            return nullptr;
        }
        auto *stream = new (smem) TcpStreamHandle;
        stream->fd = fd;
        return stream;
    }
    return nullptr;
}

static inline void *ry_net_connect(const char *host, int64_t port) {
    ::addrinfo *result = nullptr;
    if (ry_net_resolve(host, port, &result) != 0)
        return nullptr;
    void *stream = ry_net_connect_resolved(result);
    ::freeaddrinfo(result);
    return stream;
}

// ===== Data transfer =====

static inline ssize_t ry_net_send_all(int fd, const void *buf, size_t len) {
    auto *data = static_cast<const char *>(buf);
    size_t remaining = len;
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
        if (n == 0) return -1;
        data += n;
        remaining -= static_cast<size_t>(n);
    }
    return static_cast<ssize_t>(len); // NOLINT(bugprone-narrowing-conversions)
}

// ===== Timeout configuration =====

static inline void ry_net_set_socket_timeval(int fd, int option, int64_t ms) {
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

static inline void ry_net_apply_default_recv_timeout(int fd) {
    struct timeval current_tv{};
    socklen_t tv_len = sizeof(current_tv);
    if (::getsockopt(fd, SOL_SOCKET, SO_RCVTIMEO, &current_tv, &tv_len) == 0 &&
        current_tv.tv_sec == 0 && current_tv.tv_usec == 0) {
        struct timeval tv = {30, 0};
        ::setsockopt(fd, SOL_SOCKET, SO_RCVTIMEO, &tv, sizeof(tv));
    }
}

// ===== Handle management =====

static inline int ry_net_tcp_take_fd(void *stream) {
    auto *handle = (TcpStreamHandle *)stream;
    int fd = handle->fd;
    arc_free(stream);
    return fd;
}

} // namespace ry
