#include "ry/runtime_io.hpp"
#include "ry/runtime_arc.hpp"
#include "ry/runtime_http_types.hpp"
#include "ry/runtime_string.hpp"

#include <cstdarg>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <unistd.h>
#include <fcntl.h>


namespace ry {

static const long MAX_READ_SIZE = 256L * 1024 * 1024; // 256 MB

static FILE *fopen_nofollow(const char *path, const char *mode) {
    int flags = O_NOFOLLOW;
    if (strcmp(mode, "r") == 0 || strcmp(mode, "rb") == 0)
        flags |= O_RDONLY;
    else if (strcmp(mode, "w") == 0 || strcmp(mode, "wb") == 0)
        flags |= O_WRONLY | O_CREAT | O_TRUNC;
    else if (strcmp(mode, "a") == 0)
        flags |= O_WRONLY | O_CREAT | O_APPEND;
    int fd = open(path, flags, 0644);
    if (fd < 0) return nullptr;
    FILE *f = fdopen(fd, mode);
    if (!f) { close(fd); return nullptr; }
    return f;
}

// __ry_set_last_error / __ry_get_last_error are defined in runtime_error.cpp
// (part of ry_lib). Native libs resolve them from the process at runtime.

static void setLastError(const char *fmt, ...) {
    char buf[512];
    va_list args;
    va_start(args, fmt);
    vsnprintf(buf, sizeof(buf), fmt, args);
    va_end(args);
    __ry_set_last_error(buf);
}

// IOListHeader and makeByteList are defined in runtime_io.hpp

// ===== Standard input =====

extern "C" const char *__ry_read_line() {
    char *line = nullptr;
    size_t len = 0;
    ssize_t nread = getline(&line, &len, stdin);
    if (nread == -1) {
        free(line);
        return makeString("", 0);
    }
    if (nread > 0 && line[nread - 1] == '\n')
        --nread;
    const char *result = makeString(line, static_cast<size_t>(nread));
    free(line);
    return result;
}

// Returns: 0 = line read, 1 = EOF (no data), -1 = error
// *out_line is set to a Ry string handle on success (0-return only).
// stdin counterpart of __ry_io_file_read_line.
extern "C" int64_t __ry_io_read_line(const char **out_line) {
    *out_line = nullptr;
    char *line = nullptr;
    size_t len = 0;
    ssize_t nread = getline(&line, &len, stdin);
    if (nread == -1) {
        free(line);
        if (feof(stdin)) return 1;
        setLastError("readLine: I/O error reading from stdin");
        return -1;
    }
    if (nread > 0 && line[nread - 1] == '\n') --nread;
    *out_line = makeString(line, (size_t)nread);
    free(line);
    return 0;
}

extern "C" const char *__ry_input_prompt(const char *prompt) {
    size_t promptLen = static_cast<size_t>(stringByteLen(prompt));
    if (promptLen > 0)
        fwrite(prompt, 1, promptLen, stdout);
    fflush(stdout);
    return __ry_read_line();
}

extern "C" const char *__ry_read_all() {
    size_t cap = 4096;
    size_t len = 0;
    char *buf = (char *)checked_malloc(cap);

    for (;;) {
        if (len + 1 >= cap) {
            if (cap > SIZE_MAX / 2) { free(buf); oom_abort(cap); }
            cap *= 2;
            buf = (char *)checked_realloc(buf, cap);
        }
        size_t to_read = cap - len - 1;
        size_t n = fread(buf + len, 1, to_read, stdin);
        if (n == 0) {
            break;
        }
        len += n;
    }
    const char *result = makeString(buf, len);
    free(buf);
    return result;
}

// ===== File I/O =====

extern "C" const char *__ry_read_text(const char *path) {
    if (hasEmbeddedNul(path)) {
        setLastError("read_text: argument contains an embedded NUL byte");
        return nullptr;
    }
    FILE *f = fopen_nofollow(path, "r");
    if (!f) {
        setLastError("cannot open file '%s' for reading", path);
        return nullptr;
    }
    if (fseek(f, 0, SEEK_END) != 0) {
        fclose(f);
        setLastError("cannot seek file '%s'", path);
        return nullptr;
    }
    long size = ftell(f);
    if (size < 0) {
        fclose(f);
        setLastError("cannot determine size of file '%s'", path);
        return nullptr;
    }
    if (size > MAX_READ_SIZE) {
        fclose(f);
        setLastError("file '%s' is too large (%ld bytes, max %ld)", path, size, MAX_READ_SIZE);
        return nullptr;
    }
    fseek(f, 0, SEEK_SET);

    char *buf = (char *)checked_malloc((size_t)size + 1);
    size_t nread = fread(buf, 1, (size_t)size, f);
    fclose(f);
    const char *result = makeString(buf, nread);
    free(buf);
    return result;
}

extern "C" int64_t __ry_write_text(const char *path, const char *content) {
    if (hasEmbeddedNul(path)) {
        setLastError("write_text: argument contains an embedded NUL byte");
        return 1;
    }
    FILE *f = fopen_nofollow(path, "w");
    if (!f) {
        setLastError("cannot open file '%s' for writing", path);
        return 1;
    }
    int64_t byteLen = stringByteLen(content);
    size_t written = fwrite(content, 1, static_cast<size_t>(byteLen), f);
    int closeRc = fclose(f);
    if (static_cast<int64_t>(written) != byteLen || closeRc != 0) {
        setLastError("failed to write to file '%s'", path);
        return 1;
    }
    return 0;
}

extern "C" int64_t __ry_append_text(const char *path, const char *content) {
    if (hasEmbeddedNul(path)) {
        setLastError("append_text: argument contains an embedded NUL byte");
        return 1;
    }
    FILE *f = fopen_nofollow(path, "a");
    if (!f) {
        setLastError("cannot open file '%s' for appending", path);
        return 1;
    }
    int64_t byteLen = stringByteLen(content);
    size_t written = fwrite(content, 1, static_cast<size_t>(byteLen), f);
    int closeRc = fclose(f);
    if (static_cast<int64_t>(written) != byteLen || closeRc != 0) {
        setLastError("failed to append to file '%s'", path);
        return 1;
    }
    return 0;
}

extern "C" int64_t __ry_file_exists(const char *path) {
    if (hasEmbeddedNul(path)) return 0;
    return access(path, F_OK) == 0 ? 1 : 0;
}

extern "C" int64_t __ry_delete_file(const char *path) {
    if (hasEmbeddedNul(path)) {
        setLastError("delete_file: argument contains an embedded NUL byte");
        return 1;
    }
    if (remove(path) != 0) {
        setLastError("cannot delete file '%s'", path);
        return 1;
    }
    return 0;
}

extern "C" void *__ry_read_bytes(const char *path) {
    if (hasEmbeddedNul(path)) {
        setLastError("read_bytes: argument contains an embedded NUL byte");
        return nullptr;
    }
    FILE *f = fopen_nofollow(path, "rb");
    if (!f) {
        setLastError("cannot open file '%s' for reading", path);
        return nullptr;
    }
    if (fseek(f, 0, SEEK_END) != 0) {
        fclose(f);
        setLastError("cannot seek file '%s'", path);
        return nullptr;
    }
    long size = ftell(f);
    if (size < 0) {
        fclose(f);
        setLastError("cannot determine size of file '%s'", path);
        return nullptr;
    }
    if (size > MAX_READ_SIZE) {
        fclose(f);
        setLastError("file '%s' is too large (%ld bytes, max %ld)", path, size, MAX_READ_SIZE);
        return nullptr;
    }
    fseek(f, 0, SEEK_SET);

    auto *header = (IOListHeader *)arc_alloc(sizeof(IOListHeader));
    header->data = (int8_t *)checked_malloc(static_cast<size_t>(size));
    size_t nread = fread(header->data, 1, static_cast<size_t>(size), f);
    header->len = (int64_t)nread;
    header->cap = (int64_t)nread;
    fclose(f);
    return header;
}

extern "C" int64_t __ry_write_bytes(const char *path, void *list) {
    if (hasEmbeddedNul(path)) {
        setLastError("write_bytes: argument contains an embedded NUL byte");
        return 1;
    }
    auto *header = (IOListHeader *)list;
    FILE *f = fopen_nofollow(path, "wb");
    if (!f) {
        setLastError("cannot open file '%s' for writing", path);
        return 1;
    }
    size_t written = fwrite(header->data, 1, static_cast<size_t>(header->len), f);
    fclose(f);
    if ((int64_t)written != header->len) {
        setLastError("failed to write all bytes to '%s'", path);
        return 1;
    }
    return 0;
}

// ===== Byte conversions =====

extern "C" void *__ry_str_to_bytes(const char *s) {
    return makeByteList((const uint8_t *)s, stringByteLen(s));
}

extern "C" const char *__ry_bytes_to_str(void *list) {
    auto *header = (IOListHeader *)list;
    return makeString(reinterpret_cast<const char *>(header->data),
                      static_cast<size_t>(header->len));
}

// ===== File handle API =====

struct IoFileHandle {
    FILE *fp;
};

extern "C" void *__ry_io_file_open(const char *path, const char *mode) {
    if (!path) {
        setLastError("open: path is null");
        return nullptr;
    }
    if (!mode) {
        setLastError("open: mode is null");
        return nullptr;
    }
    if (hasEmbeddedNul(path)) {
        setLastError("open: path contains an embedded NUL byte");
        return nullptr;
    }
    if (hasEmbeddedNul(mode)) {
        setLastError("open: mode contains an embedded NUL byte");
        return nullptr;
    }
    if (strcmp(mode, "r") != 0 && strcmp(mode, "w") != 0 && strcmp(mode, "a") != 0 &&
        strcmp(mode, "rb") != 0 && strcmp(mode, "wb") != 0) {
        setLastError("open: invalid mode '%s' (must be \"r\", \"w\", \"a\", \"rb\", or \"wb\")", mode);
        return nullptr;
    }
    FILE *fp = fopen_nofollow(path, mode);
    if (!fp) {
        setLastError("open: cannot open '%s' in mode '%s'", path, mode);
        return nullptr;
    }
    auto *h = static_cast<IoFileHandle *>(arc_alloc(sizeof(IoFileHandle)));
    h->fp = fp;
    return h;
}

extern "C" const char *__ry_io_file_read_all(void *handle) {
    auto *h = static_cast<IoFileHandle *>(handle);
    if (!h || !h->fp) {
        setLastError("readAll: file handle is not open");
        return nullptr;
    }
    // Seek-based fast path for regular files: read from current cursor to EOF
    long start = ftell(h->fp);
    if (start >= 0 && fseek(h->fp, 0, SEEK_END) == 0) {
        long end = ftell(h->fp);
        long size = end - start;
        if (size > MAX_READ_SIZE) {
            setLastError("readAll: file too large (%ld bytes, max %ld)", size, MAX_READ_SIZE);
            return nullptr;
        }
        if (size >= 0) {
            fseek(h->fp, start, SEEK_SET);
            char *buf = (char *)checked_malloc((size_t)size + 1);
            size_t nread = fread(buf, 1, (size_t)size, h->fp);
            const char *result = makeString(buf, nread);
            free(buf);
            return result;
        }
    }
    // Chunk-based fallback for non-seekable handles
    size_t cap = 4096, len = 0;
    char *buf = (char *)checked_malloc(cap);
    for (;;) {
        if (len + 1 >= cap) {
            if (cap > SIZE_MAX / 2) { free(buf); oom_abort(cap); }
            cap *= 2;
            buf = (char *)checked_realloc(buf, cap);
        }
        size_t n = fread(buf + len, 1, cap - len - 1, h->fp);
        if (n == 0) break;
        len += n;
        if (len > (size_t)MAX_READ_SIZE) {
            free(buf);
            setLastError("readAll: file too large (max %ld bytes)", MAX_READ_SIZE);
            return nullptr;
        }
    }
    const char *result = makeString(buf, len);
    free(buf);
    return result;
}

// Returns: 0 = line read, 1 = EOF (no data), -1 = error
// *out_line is set to a Ry string handle on success (0-return only)
extern "C" int64_t __ry_io_file_read_line(void *handle, const char **out_line) {
    *out_line = nullptr;
    auto *h = static_cast<IoFileHandle *>(handle);
    if (!h || !h->fp) {
        setLastError("readLine: file handle is not open");
        return -1;
    }
    char *line = nullptr;
    size_t len = 0;
    ssize_t nread = getline(&line, &len, h->fp);
    if (nread == -1) {
        free(line);
        if (feof(h->fp)) return 1;
        setLastError("readLine: I/O error reading file");
        return -1;
    }
    if (nread > 0 && line[nread - 1] == '\n') --nread;
    *out_line = makeString(line, (size_t)nread);
    free(line);
    return 0;
}

extern "C" int64_t __ry_io_file_write_text(void *handle, const char *s) {
    auto *h = static_cast<IoFileHandle *>(handle);
    if (!h || !h->fp) {
        setLastError("writeText: file handle is not open");
        return -1;
    }
    int64_t byteLen = stringByteLen(s);
    size_t written = fwrite(s, 1, (size_t)byteLen, h->fp);
    if ((int64_t)written != byteLen) {
        setLastError("writeText: failed to write to file");
        return -1;
    }
    return 0;
}

extern "C" void __ry_io_file_close(void *handle) {
    auto *h = static_cast<IoFileHandle *>(handle);
    if (h && h->fp) {
        fclose(h->fp);
        h->fp = nullptr;
    }
}

extern "C" void __ry_io_file_cleanup(void *handle) {
    auto *h = static_cast<IoFileHandle *>(handle);
    if (h && h->fp) {
        fclose(h->fp);
        h->fp = nullptr;
    }
    // ARC machinery calls arc_free after this
}

} // namespace ry
