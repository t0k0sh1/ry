#pragma once

#include <cstdint>
#include <cstdlib>
#include <cstring>
#include <string>
#include <vector>

#include "ry/runtime_alloc.hpp"
#include "ry/runtime_arc.hpp"


namespace ry {

// ListHeader layout: {i64 len, i64 cap, ptr data}
// Matches the generic list ABI used by codegen.
// Shared across runtime modules that return string lists.
struct ListHeader {
    int64_t len;
    int64_t cap;
    char **data;
};

// Duplicate a string of known length into a plain heap buffer.
// Individual list element strings are owned by the caller and are never
// individually freed by the list destructor (which only frees the
// data-pointer array).  Plain checked_malloc is sufficient here — the ARC
// machinery never retains/releases individual string elements extracted from
// a list.
inline char *dupString(const char *s, size_t n) {
    char *buf = (char *)checked_malloc(n + 1);
    memcpy(buf, s, n);
    buf[n] = '\0';
    return buf;
}

// Build an ARC-managed ListHeader from a vector of strings.
// The header itself has a proper ARC prefix (strong_count = 1) so that Ry's
// retain/release machinery can manage the list's lifetime correctly.
// Individual string elements use plain heap allocation (see dupString above);
// the list ARC destructor frees only the data-pointer array, not elements.
inline ListHeader *makeStringList(const std::vector<std::string> &items) {
    auto *header = (ListHeader *)arc_alloc(sizeof(ListHeader));
    header->len = (int64_t)items.size();
    header->cap = (int64_t)items.size();
    header->data = (char **)checked_array_malloc(
        items.empty() ? 1 : items.size(), sizeof(char *));
    for (size_t i = 0; i < items.size(); ++i) {
        header->data[i] = dupString(items[i].c_str(), items[i].size());
    }
    return header;
}

// MatchData layout: {char* full, ListHeader* groups}
// Matches the LLVM StructType for the Ry `Match` record: {ptr, ptr}.
// data is cast to char** per the generic list ABI; actual element stride is
// sizeof(MatchData) and is resolved at codegen time via TypeMeta::ListElem.
struct MatchData {
    char *full;
    void *groups; // ListHeader* for the captured groups (List<str>)
};

// Build an ARC-managed ListHeader from a vector of MatchData values.
// Same ARC-prefix contract as makeStringList.
inline ListHeader *makeMatchList(const std::vector<MatchData> &items) {
    auto *header = (ListHeader *)arc_alloc(sizeof(ListHeader));
    header->len = (int64_t)items.size();
    header->cap = (int64_t)items.size();
    auto *data = (MatchData *)checked_array_malloc(
        items.empty() ? 1 : items.size(), sizeof(MatchData));
    for (size_t i = 0; i < items.size(); ++i)
        data[i] = items[i];
    header->data = (char **)data;
    return header;
}

} // namespace ry
