#include "ry/runtime_http_types.hpp"

#include <unordered_set>

// ===== Multipart form-data parsing =====

// Extract a parameter value from a header value string, working directly on C strings.
// e.g., extract_param("form-data; name=\"field1\"", "name") -> "field1"
// Handles both quoted and unquoted values.
static std::string extract_param(const char *header_value, const char *param_name) {
    if (!header_value || !param_name) return "";
    size_t pn_len = strlen(param_name);
    const char *p = header_value;
    while ((p = strchr(p, ';')) != nullptr) {
        p++;
        while (*p == ' ' || *p == '\t') p++;
        const char *eq = strchr(p, '=');
        if (!eq) break;
        // Trim trailing whitespace from key
        const char *kend = eq;
        while (kend > p && (kend[-1] == ' ' || kend[-1] == '\t')) kend--;
        size_t klen = (size_t)(kend - p);
        if (klen == pn_len && strncasecmp(p, param_name, pn_len) == 0) {
            const char *v = eq + 1;
            while (*v == ' ' || *v == '\t') v++;
            if (*v == '"') {
                v++;
                const char *vend = strchr(v, '"');
                if (!vend) vend = v + strlen(v);
                return std::string(v, (size_t)(vend - v));
            } else {
                const char *vend = strchr(v, ';');
                if (!vend) vend = v + strlen(v);
                while (vend > v && (vend[-1] == ' ' || vend[-1] == '\t')) vend--;
                return std::string(v, (size_t)(vend - v));
            }
        }
        p = eq + 1;
    }
    return "";
}

static void free_header_pairs(std::vector<HeaderPair> &headers) {
    for (auto &h : headers) { free(h.key); free(h.val); }
}

// Process a single multipart part: extract Content-Disposition, categorize as
// field or file, and append to the appropriate vector with deduplication.
static void process_multipart_part(const char *body, size_t data_start, size_t data_end,
                                     const std::vector<HeaderPair> &part_headers,
                                     std::vector<FormFieldEntry> &fields,
                                     std::vector<FormFileEntry> &files,
                                     std::unordered_set<std::string> &seen_field_names,
                                     std::unordered_set<std::string> &seen_file_names) {
    const char *disposition = nullptr;
    const char *part_content_type = nullptr;
    for (auto &h : part_headers) {
        if (strcasecmp(h.key, "Content-Disposition") == 0)
            disposition = h.val;
        else if (strcasecmp(h.key, "Content-Type") == 0)
            part_content_type = h.val;
    }
    if (!disposition) return;

    std::string name = extract_param(disposition, "name");
    if (name.empty()) return;

    std::string filename = extract_param(disposition, "filename");
    if (!filename.empty()) {
        if (seen_file_names.insert(name).second) {
            files.push_back({
                checked_strdup(name.c_str()),
                checked_strdup(filename.c_str()),
                checked_strdup(part_content_type ? part_content_type : "application/octet-stream"),
                checked_memdup(body + data_start, data_end - data_start),
                (int64_t)(data_end - data_start)
            });
        }
    } else {
        if (seen_field_names.insert(name).second) {
            fields.push_back({
                checked_strdup(name.c_str()),
                checked_strndup(body + data_start, data_end - data_start)
            });
        }
    }
}

static void parse_multipart_form_data(HttpRequestHandle *req) {
    if (req->form_parsed) return;
    req->form_parsed = true;
    req->form_field_count = 0;
    req->form_fields = nullptr;
    req->form_file_count = 0;
    req->form_files = nullptr;

    if (!req->body || !req->body[0]) return;

    const char *content_type = find_in_kv_pairs_ci(
        req->header_keys, req->header_values, req->header_count, "Content-Type");
    if (!content_type) return;
    if (strncasecmp(content_type, "multipart/form-data", 19) != 0) return;
    char next = content_type[19];
    if (next != '\0' && next != ';' && next != ' ' && next != '\t') return;

    std::string boundary = extract_param(content_type, "boundary");
    if (boundary.empty()) return;

    std::string delimiter = "--" + boundary;
    std::string crlf_delimiter = "\r\n" + delimiter;
    size_t body_len = (size_t)req->body_len;

    std::vector<FormFieldEntry> fields;
    std::vector<FormFileEntry> files;
    std::unordered_set<std::string> seen_field_names, seen_file_names;

    const char *delim_ptr = (const char *)memmem(req->body, body_len,
                                                  delimiter.c_str(), delimiter.size());
    if (!delim_ptr) return;
    size_t pos = (size_t)(delim_ptr - req->body) + delimiter.size();

    while (pos < body_len) {
        if (pos + 1 < body_len && req->body[pos] == '\r' && req->body[pos + 1] == '\n')
            pos += 2;
        else
            break;

        const char *hdr_end_ptr = (const char *)memmem(req->body + pos, body_len - pos,
                                                        "\r\n\r\n", 4);
        if (!hdr_end_ptr) break;
        size_t headers_end = (size_t)(hdr_end_ptr - req->body);

        std::string header_region(req->body + pos, headers_end - pos);
        auto part_headers = parse_raw_headers(header_region, 0, header_region.size());

        size_t data_start = headers_end + 4;

        const char *next_ptr = (const char *)memmem(req->body + data_start, body_len - data_start,
                                                     crlf_delimiter.c_str(), crlf_delimiter.size());
        if (!next_ptr) { free_header_pairs(part_headers); break; }
        size_t next_delim = (size_t)(next_ptr - req->body) + 2;
        size_t data_end = (size_t)(next_ptr - req->body);

        process_multipart_part(req->body, data_start, data_end, part_headers,
                                fields, files, seen_field_names, seen_file_names);

        free_header_pairs(part_headers);

        pos = next_delim + delimiter.size();
        if (pos + 1 < body_len && req->body[pos] == '-' && req->body[pos + 1] == '-')
            break;
    }

    req->form_field_count = (int64_t)fields.size();
    if (req->form_field_count > 0) {
        req->form_fields = (FormFieldEntry *)checked_malloc(sizeof(FormFieldEntry) * fields.size());
        memcpy(req->form_fields, fields.data(), sizeof(FormFieldEntry) * fields.size());
    }

    req->form_file_count = (int64_t)files.size();
    if (req->form_file_count > 0) {
        req->form_files = (FormFileEntry *)checked_malloc(sizeof(FormFileEntry) * files.size());
        memcpy(req->form_files, files.data(), sizeof(FormFileEntry) * files.size());
    }
}

extern "C" const char *__ry_http_form_field(void *r, const char *name) {
    auto *req = (HttpRequestHandle *)r;
    parse_multipart_form_data(req);
    for (int64_t i = 0; i < req->form_field_count; i++) {
        if (strcmp(req->form_fields[i].key, name) == 0)
            return req->form_fields[i].value;
    }
    return nullptr;
}

extern "C" void *__ry_http_form_file(void *r, const char *name) {
    auto *req = (HttpRequestHandle *)r;
    parse_multipart_form_data(req);
    for (int64_t i = 0; i < req->form_file_count; i++) {
        if (strcmp(req->form_files[i].name, name) == 0) {
            // Build a Map<str, str> with keys: "filename", "content_type", "data"
            char **keys = (char **)checked_malloc(sizeof(char *) * 3);
            char **vals = (char **)checked_malloc(sizeof(char *) * 3);
            keys[0] = checked_strdup("filename");
            vals[0] = checked_strdup(req->form_files[i].filename);
            keys[1] = checked_strdup("content_type");
            vals[1] = checked_strdup(req->form_files[i].content_type);
            keys[2] = checked_strdup("data");
            vals[2] = checked_memdup(req->form_files[i].data, (size_t)req->form_files[i].data_len);
            return build_str_map(keys, vals, 3);
        }
    }
    // Return nullptr when file not found (Option<Map> None)
    return nullptr;
}

extern "C" void *__ry_http_form_fields(void *r) {
    auto *req = (HttpRequestHandle *)r;
    parse_multipart_form_data(req);
    char **dup_keys = nullptr;
    char **dup_vals = nullptr;
    if (req->form_field_count > 0) {
        dup_keys = (char **)checked_malloc(sizeof(char *) * (size_t)req->form_field_count);
        dup_vals = (char **)checked_malloc(sizeof(char *) * (size_t)req->form_field_count);
        for (int64_t i = 0; i < req->form_field_count; i++) {
            dup_keys[i] = checked_strdup(req->form_fields[i].key);
            dup_vals[i] = checked_strdup(req->form_fields[i].value);
        }
    }
    return build_str_map(dup_keys, dup_vals, req->form_field_count);
}
