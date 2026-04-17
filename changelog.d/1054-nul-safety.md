### Changed

- `path.join`, `path.basename`, `path.dirname`, `path.extension` now return `Result<str, Error>` instead of `str`; callers receive a typed error if any argument contains an embedded NUL byte (#1054)
- `filesystem.is_file`, `filesystem.is_dir`, `filesystem.is_symlink` now return `Result<bool, Error>` instead of `bool`; callers receive a typed error if the path contains an embedded NUL byte (#1054)
- `http.listen` handler type is now `function(HttpRequest) -> Result<HttpResponse, Error>`; the listen loop synthesises a 500 response when the handler returns `Err` (#1054)
- `http.header(req, key)`, `http.query(req, key)`, `http.cookie(req, name)`, `http.form_field(req, name)`, `http.form_file(req, name)` now return `Result<Option<…>, Error>` instead of `Option<…>`; callers receive a typed error if the key/name contains an embedded NUL byte (#1054)
- `http.response(status, headers, body)` now returns `Result<HttpResponse, Error>` instead of `HttpResponse`; callers receive a typed error if any header key or value contains an embedded NUL byte (#1054)
- `http.header(resp, key)` (client response accessor) now returns `Result<Option<str>, Error>` instead of `Option<str>` (#1054)

### Fixed

- HTTP client body truncated at first embedded NUL byte: `Content-Length` was computed with `strlen(body)`; now uses `stringByteLen(body)` for binary-safe payloads (#1054)
- HTTP request URL silently truncated at embedded NUL: `http_get`, `http_post`, and `http_request` now reject URLs containing embedded NUL bytes with a typed `Err` (#1054)
- HTTP `http_request` method silently truncated at embedded NUL: now rejected with a typed `Err` (#1054)
- HTTP header build used `std::string::operator+=` on Ry handles, truncating values at the first NUL; replaced with byte-length-correct `append(data, byte_len)` (#1054)
- DNS hostname lookup (`net.bind`, `net.connect`, `net.tls_connect`) silently truncated hosts containing embedded NUL bytes; now rejected with a typed `Err` (#1054)
- `path.join`, `path.basename`, `path.dirname`, `path.extension`, `path.resolve` silently truncated paths at embedded NUL bytes; now rejected with a typed `Err` (#1054)
- `filesystem` functions silently truncated paths at embedded NUL bytes; now rejected with a typed `Err` (#1054)
