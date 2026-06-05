#pragma once

#include <cstdint>
#include <cstdlib>
#include <cstring>
#include <string>
#include <vector>

#include "ry/runtime/core/alloc.hpp"
#include "ry/runtime/core/arc.hpp"
#include "ry/runtime/core/string.hpp"


namespace ry {

// ListHeader layout: {i64 len, i64 cap, ptr data}
// Matches the generic list layout used by codegen.
// Shared across runtime modules that return string lists.
struct ListHeader {
    int64_t len;
    int64_t cap;
    char **data;
};

// Duplicate a string of known length into a StringHeader-managed buffer.
// The handle (data pointer past the StringHeader) is returned.
// Individual list element strings are not freed by the list destructor (which
// only frees the data-pointer array); they will be managed by a follow-up
// PR that enables full ARC management for str.  After the migration to
// StringHeader (PR #1022), freeStringSlot() must be used instead of free()
// to release these elements if/when that becomes necessary.
inline char *dupString(const char *s, size_t n) {
    return makeString(s, n);
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
// data is cast to char** per the generic list layout; actual element stride is
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
