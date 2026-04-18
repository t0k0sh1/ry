### Fixed

- `base64.encode`, `base64.decode`, `base64.encode_url_safe`, `base64.decode_url_safe` no longer silently truncate input at embedded NUL bytes. `encode` / `encode_url_safe` now correctly process the full binary payload (binary-safe). `decode` / `decode_url_safe` now return `Err("invalid base64 character at position N")` for inputs containing NUL (since NUL is not a valid base64 character), instead of silently succeeding on the prefix before the NUL (#1129).
