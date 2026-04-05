### Fixed

- Broadened SSRF private address filter to block carrier-grade NAT (`100.64.0.0/10`), benchmarking (`198.18.0.0/15`), multicast (`224.0.0.0/4`), reserved (`240.0.0.0/4`), IPv6 unspecified (`::`), and IPv6 multicast (`ff00::/8`) (#667)
- Added error handling for `fcntl` failure when restoring blocking mode after non-blocking connect (#667)
