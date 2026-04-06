#pragma once

#include <cstdint>


namespace ry {

extern "C" {

// Parse / stringify
void       *__ry_json_parse(const char *text);
const char *__ry_json_stringify(void *value);
const char *__ry_json_stringify_pretty(void *value, int64_t indent);

// Type query
const char *__ry_json_type(void *value);

// Object access (returns null on error, sets last_error)
void *__ry_json_get(void *value, const char *key);

// Array access (returns null on error, sets last_error)
void *__ry_json_at(void *value, int64_t index);

// Value extraction
const char *__ry_json_str(void *value);                   // null on type mismatch
int64_t     __ry_json_int(void *value, int64_t *out);     // 0=ok, non-zero=error
int64_t     __ry_json_float(void *value, double *out);    // 0=ok, non-zero=error
int64_t     __ry_json_bool(void *value, int64_t *out);    // 0=ok, non-zero=error

// Collection info
int64_t __ry_json_len(void *value);
void   *__ry_json_keys(void *value);   // returns ListHeader (List<str>)

// Memory
void __ry_json_free(void *value);

}

} // namespace ry
