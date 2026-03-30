#pragma once

#include <cstdint>
#include <cstdlib>
#include <cstring>
#include <string>
#include <vector>

// ListHeader layout: {i64 len, i64 cap, ptr data}
// Matches the generic list ABI used by codegen.
// Shared across runtime modules that return string lists.
struct ListHeader {
    int64_t len;
    int64_t cap;
    char **data;
};

// Duplicate a string of known length into a heap-allocated null-terminated
// buffer.
inline char *dupString(const char *s, size_t n) {
    char *buf = (char *)malloc(n + 1);
    if (!buf) return nullptr;
    memcpy(buf, s, n);
    buf[n] = '\0';
    return buf;
}

// Build a heap-allocated ListHeader from a vector of strings.
// Each string is individually heap-allocated via dupString.
inline ListHeader *makeStringList(const std::vector<std::string> &items) {
    auto *header = (ListHeader *)malloc(sizeof(ListHeader));
    if (!header) return nullptr;
    header->len = (int64_t)items.size();
    header->cap = (int64_t)items.size();
    header->data =
        (char **)malloc(sizeof(char *) * (items.empty() ? 1 : items.size()));
    if (!header->data) {
        free(header);
        return nullptr;
    }
    for (size_t i = 0; i < items.size(); ++i) {
        header->data[i] = dupString(items[i].c_str(), items[i].size());
        if (!header->data[i]) {
            for (size_t j = 0; j < i; ++j) free(header->data[j]);
            free(header->data);
            free(header);
            return nullptr;
        }
    }
    return header;
}
