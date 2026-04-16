#include "ry/runtime_io.hpp"
#include "ry/runtime_http_types.hpp"

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
        return checked_strdup("");
    }
    if (nread > 0 && line[nread - 1] == '\n')
        line[nread - 1] = '\0';
    return line;
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
    buf[len] = '\0';
    return buf;
}

// ===== File I/O =====

extern "C" const char *__ry_read_text(const char *path) {
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
    buf[nread] = '\0';
    fclose(f);
    return buf;
}

extern "C" int64_t __ry_write_text(const char *path, const char *content) {
    FILE *f = fopen_nofollow(path, "w");
    if (!f) {
        setLastError("cannot open file '%s' for writing", path);
        return 1;
    }
    if (fputs(content, f) == EOF || fclose(f) != 0) {
        setLastError("failed to write to file '%s'", path);
        return 1;
    }
    return 0;
}

extern "C" int64_t __ry_append_text(const char *path, const char *content) {
    FILE *f = fopen_nofollow(path, "a");
    if (!f) {
        setLastError("cannot open file '%s' for appending", path);
        return 1;
    }
    if (fputs(content, f) == EOF || fclose(f) != 0) {
        setLastError("failed to append to file '%s'", path);
        return 1;
    }
    return 0;
}

extern "C" int64_t __ry_file_exists(const char *path) {
    return access(path, F_OK) == 0 ? 1 : 0;
}

extern "C" int64_t __ry_delete_file(const char *path) {
    if (remove(path) != 0) {
        setLastError("cannot delete file '%s'", path);
        return 1;
    }
    return 0;
}

extern "C" void *__ry_read_bytes(const char *path) {
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
    size_t len = strlen(s);
    return makeByteList((const uint8_t *)s, (int64_t)len);
}

extern "C" const char *__ry_bytes_to_str(void *list) {
    auto *header = (IOListHeader *)list;
    for (int64_t i = 0; i < header->len; ++i) {
        if (header->data[i] == 0) {
            setLastError("bytes_to_str() input contains NUL byte at index %lld", (long long)i);
            return nullptr;
        }
    }
    char *buf = (char *)checked_malloc(static_cast<size_t>(header->len) + 1);
    memcpy(buf, header->data, static_cast<size_t>(header->len));
    buf[header->len] = '\0';
    return buf;
}

} // namespace ry
