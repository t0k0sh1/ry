#include "ry/runtime_io.hpp"

#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <unistd.h>

// ListHeader layout: {i64 len, i64 cap, ptr data}
// Matches the generic list ABI used by codegen (same as runtime_regex.cpp)
// but with i8* data for byte lists instead of char** for string lists.
struct IOListHeader {
    int64_t len;
    int64_t cap;
    int8_t *data;
};

static IOListHeader *makeByteList(const uint8_t *bytes, int64_t len) {
    auto *header = (IOListHeader *)malloc(sizeof(IOListHeader));
    header->len = len;
    header->cap = len;
    header->data = (int8_t *)malloc(len);
    memcpy(header->data, bytes, len);
    return header;
}

// ===== Standard input =====

extern "C" const char *__ry_read_line() {
    size_t cap = 256;
    size_t len = 0;
    char *buf = (char *)malloc(cap);

    int ch;
    while ((ch = fgetc(stdin)) != EOF && ch != '\n') {
        if (len + 1 >= cap) {
            cap *= 2;
            buf = (char *)realloc(buf, cap);
        }
        buf[len++] = (char)ch;
    }
    buf[len] = '\0';
    return buf;
}

extern "C" const char *__ry_read_all() {
    size_t cap = 4096;
    size_t len = 0;
    char *buf = (char *)malloc(cap);

    for (;;) {
        if (len + 1 >= cap) {
            cap *= 2;
            buf = (char *)realloc(buf, cap);
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
    FILE *f = fopen(path, "r");
    if (!f) {
        fprintf(stderr, "runtime error: cannot open file '%s' for reading\n", path);
        exit(1);
    }
    if (fseek(f, 0, SEEK_END) != 0) {
        fclose(f);
        fprintf(stderr, "runtime error: cannot seek file '%s'\n", path);
        exit(1);
    }
    long size = ftell(f);
    if (size < 0) {
        fclose(f);
        fprintf(stderr, "runtime error: cannot determine size of file '%s'\n", path);
        exit(1);
    }
    fseek(f, 0, SEEK_SET);

    char *buf = (char *)malloc(size + 1);
    size_t nread = fread(buf, 1, size, f);
    buf[nread] = '\0';
    fclose(f);
    return buf;
}

extern "C" void __ry_write_text(const char *path, const char *content) {
    FILE *f = fopen(path, "w");
    if (!f) {
        fprintf(stderr, "runtime error: cannot open file '%s' for writing\n", path);
        exit(1);
    }
    fputs(content, f);
    fclose(f);
}

extern "C" void __ry_append_text(const char *path, const char *content) {
    FILE *f = fopen(path, "a");
    if (!f) {
        fprintf(stderr, "runtime error: cannot open file '%s' for appending\n", path);
        exit(1);
    }
    fputs(content, f);
    fclose(f);
}

extern "C" int64_t __ry_file_exists(const char *path) {
    return access(path, F_OK) == 0 ? 1 : 0;
}

extern "C" void __ry_delete_file(const char *path) {
    if (remove(path) != 0) {
        fprintf(stderr, "runtime error: cannot delete file '%s'\n", path);
        exit(1);
    }
}

extern "C" void *__ry_read_bytes(const char *path) {
    FILE *f = fopen(path, "rb");
    if (!f) {
        fprintf(stderr, "runtime error: cannot open file '%s' for reading\n", path);
        exit(1);
    }
    if (fseek(f, 0, SEEK_END) != 0) {
        fclose(f);
        fprintf(stderr, "runtime error: cannot seek file '%s'\n", path);
        exit(1);
    }
    long size = ftell(f);
    if (size < 0) {
        fclose(f);
        fprintf(stderr, "runtime error: cannot determine size of file '%s'\n", path);
        exit(1);
    }
    fseek(f, 0, SEEK_SET);

    auto *header = (IOListHeader *)malloc(sizeof(IOListHeader));
    header->data = (int8_t *)malloc(size);
    size_t nread = fread(header->data, 1, size, f);
    header->len = (int64_t)nread;
    header->cap = (int64_t)nread;
    fclose(f);
    return header;
}

extern "C" void __ry_write_bytes(const char *path, void *list) {
    auto *header = (IOListHeader *)list;
    FILE *f = fopen(path, "wb");
    if (!f) {
        fprintf(stderr, "runtime error: cannot open file '%s' for writing\n", path);
        exit(1);
    }
    size_t written = fwrite(header->data, 1, header->len, f);
    fclose(f);
    if ((int64_t)written != header->len) {
        fprintf(stderr, "runtime error: failed to write all bytes to '%s'\n", path);
        exit(1);
    }
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
            fprintf(stderr, "runtime error: bytes_to_str() input contains NUL byte at index %lld\n",
                    (long long)i);
            exit(1);
        }
    }
    char *buf = (char *)malloc(header->len + 1);
    memcpy(buf, header->data, header->len);
    buf[header->len] = '\0';
    return buf;
}
