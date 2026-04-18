### Added

- `base64.encode_bytes(List<u8>) -> str` and `base64.encode_bytes_url_safe(List<u8>) -> str` for encoding raw binary byte lists to base64 without going through `str` (#1130)
- `base64.decode_bytes(str) -> Result<List<u8>, Error>` and `base64.decode_bytes_url_safe(str) -> Result<List<u8>, Error>` for decoding base64 directly to raw bytes, preserving embedded NUL bytes and non-UTF-8 sequences (#1130)
