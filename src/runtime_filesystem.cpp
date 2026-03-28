#include <cstdarg>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <cerrno>
#include <climits>
#include <string>
#include <vector>

#include <dirent.h>
#include <fcntl.h>
#include <glob.h>
#include <ftw.h>
#include <sys/stat.h>
#include <unistd.h>

#ifdef __APPLE__
#include <copyfile.h>
#endif

namespace {

static thread_local char last_error_buf[512] = {0};

static void setLastError(const char *fmt, ...) {
    va_list ap;
    va_start(ap, fmt);
    vsnprintf(last_error_buf, sizeof(last_error_buf), fmt, ap);
    va_end(ap);
}

struct ListHeader {
    int64_t len;
    int64_t cap;
    char **data;
};

static char *dupString(const char *s, size_t n) {
    char *buf = (char *)malloc(n + 1);
    if (!buf) return nullptr;
    memcpy(buf, s, n);
    buf[n] = '\0';
    return buf;
}

static ListHeader *makeStringList(const std::vector<std::string> &items) {
    auto *header = (ListHeader *)malloc(sizeof(ListHeader));
    if (!header) return nullptr;
    header->len = (int64_t)items.size();
    header->cap = (int64_t)items.size();
    header->data = (char **)malloc(sizeof(char *) * (items.empty() ? 1 : items.size()));
    if (!header->data) { free(header); return nullptr; }
    for (size_t i = 0; i < items.size(); ++i) {
        header->data[i] = dupString(items[i].c_str(), items[i].size());
    }
    return header;
}

// Iterative directory walk using lstat to avoid following symlinks into cycles.
// Returns 0 on success, -1 if any opendir failed (errno preserved).
static int walkIterative(const std::string &root, std::vector<std::string> &out) {
    int firstError = 0;
    std::vector<std::string> stack;
    stack.push_back(root);
    while (!stack.empty()) {
        std::string dir = std::move(stack.back());
        stack.pop_back();
        DIR *dp = opendir(dir.c_str());
        if (!dp) {
            if (firstError == 0) firstError = errno;
            continue;
        }
        struct dirent *ent;
        while ((ent = readdir(dp)) != nullptr) {
            if (strcmp(ent->d_name, ".") == 0 || strcmp(ent->d_name, "..") == 0)
                continue;
            std::string full = dir + "/" + ent->d_name;
            out.push_back(full);
            // Use d_type when available to avoid extra syscall; fall back to lstat.
            // lstat (not stat) prevents following symlinks into cycles.
            bool isDir = false;
#ifdef _DIRENT_HAVE_D_TYPE
            if (ent->d_type == DT_DIR) {
                isDir = true;
            } else if (ent->d_type == DT_UNKNOWN) {
#endif
                struct stat st;
                if (lstat(full.c_str(), &st) == 0 && S_ISDIR(st.st_mode))
                    isDir = true;
#ifdef _DIRENT_HAVE_D_TYPE
            }
#endif
            if (isDir)
                stack.push_back(std::move(full));
        }
        closedir(dp);
    }
    if (firstError != 0) { errno = firstError; return -1; }
    return 0;
}

static int mkdirAll(const char *path, mode_t mode) {
    std::string p(path);
    int saved_errno = 0;
    for (size_t i = 1; i < p.size(); ++i) {
        if (p[i] == '/') {
            p[i] = '\0';
            if (mkdir(p.c_str(), mode) != 0 && errno != EEXIST) {
                saved_errno = errno;
                errno = saved_errno;
                return -1;
            }
            p[i] = '/';
        }
    }
    if (mkdir(p.c_str(), mode) != 0 && errno != EEXIST) return -1;
    return 0;
}

static int removeCallback(const char *fpath, const struct stat * /*sb*/,
                           int /*typeflag*/, struct FTW * /*ftwbuf*/) {
    return ::remove(fpath);
}

} // anonymous namespace

extern "C" {

const char *__ry_filesystem_get_last_error() {
    return strdup(last_error_buf);
}

void *__ry_filesystem_list_dir(const char *path) {
    DIR *dp = opendir(path);
    if (!dp) {
        setLastError("list_dir: cannot open directory '%s': %s", path, strerror(errno));
        return nullptr;
    }
    std::vector<std::string> entries;
    struct dirent *ent;
    while ((ent = readdir(dp)) != nullptr) {
        if (strcmp(ent->d_name, ".") == 0 || strcmp(ent->d_name, "..") == 0)
            continue;
        entries.emplace_back(ent->d_name);
    }
    closedir(dp);
    void *result = makeStringList(entries);
    if (!result) setLastError("list_dir: memory allocation failed");
    return result;
}

void *__ry_filesystem_walk(const char *path) {
    struct stat st;
    if (lstat(path, &st) != 0) {
        setLastError("walk: cannot access '%s': %s", path, strerror(errno));
        return nullptr;
    }
    if (!S_ISDIR(st.st_mode)) {
        setLastError("walk: '%s' is not a directory", path);
        return nullptr;
    }
    std::vector<std::string> files;
    if (walkIterative(path, files) != 0 && files.empty()) {
        setLastError("walk: error traversing '%s': %s", path, strerror(errno));
        return nullptr;
    }
    void *result = makeStringList(files);
    if (!result) setLastError("walk: memory allocation failed");
    return result;
}

void *__ry_filesystem_glob_files(const char *pattern) {
    glob_t gl;
    int ret = glob(pattern, GLOB_NOSORT | GLOB_TILDE, nullptr, &gl);
    if (ret == GLOB_NOMATCH) {
        // No matches is not an error — return empty list
        globfree(&gl);
        std::vector<std::string> empty;
        return makeStringList(empty);
    }
    if (ret != 0) {
        setLastError("glob_files: glob failed for pattern '%s'", pattern);
        globfree(&gl);
        return nullptr;
    }
    std::vector<std::string> matches;
    for (size_t i = 0; i < gl.gl_pathc; ++i) {
        matches.emplace_back(gl.gl_pathv[i]);
    }
    globfree(&gl);
    void *result = makeStringList(matches);
    if (!result) setLastError("glob_files: memory allocation failed");
    return result;
}

int64_t __ry_filesystem_copy(const char *src, const char *dst) {
#ifdef __APPLE__
    if (copyfile(src, dst, nullptr, COPYFILE_ALL) != 0) {
        setLastError("copy: failed to copy '%s' to '%s': %s", src, dst, strerror(errno));
        return 1;
    }
    return 0;
#else
    struct stat src_st;
    if (stat(src, &src_st) != 0) {
        setLastError("copy: cannot stat source '%s': %s", src, strerror(errno));
        return 1;
    }
    FILE *in = fopen(src, "rb");
    if (!in) {
        setLastError("copy: cannot open source '%s': %s", src, strerror(errno));
        return 1;
    }
    FILE *out = fopen(dst, "wb");
    if (!out) {
        setLastError("copy: cannot open destination '%s': %s", dst, strerror(errno));
        fclose(in);
        return 1;
    }
    char buf[65536];
    size_t n;
    while ((n = fread(buf, 1, sizeof(buf), in)) > 0) {
        if (fwrite(buf, 1, n, out) != n) {
            setLastError("copy: write error to '%s': %s", dst, strerror(errno));
            fclose(in);
            fclose(out);
            return 1;
        }
    }
    if (ferror(in)) {
        setLastError("copy: read error from '%s': %s", src, strerror(errno));
        fclose(in);
        fclose(out);
        return 1;
    }
    fclose(in);
    fclose(out);
    // Preserve source file permissions
    ::chmod(dst, src_st.st_mode & 07777);
    return 0;
#endif
}

int64_t __ry_filesystem_move(const char *src, const char *dst) {
    if (rename(src, dst) == 0) return 0;
    if (errno == EXDEV) {
        // Cross-device: fall back to copy + remove
        if (__ry_filesystem_copy(src, dst) != 0) return 1;
        if (::remove(src) != 0) {
            setLastError("move: copied but failed to remove source '%s': %s", src, strerror(errno));
            return 1;
        }
        return 0;
    }
    setLastError("move: failed to move '%s' to '%s': %s", src, dst, strerror(errno));
    return 1;
}

int64_t __ry_filesystem_remove(const char *path) {
    if (::remove(path) == 0) return 0;
    setLastError("remove: failed to remove '%s': %s", path, strerror(errno));
    return 1;
}

int64_t __ry_filesystem_remove_all(const char *path) {
    struct stat st;
    if (lstat(path, &st) != 0) {
        setLastError("remove_all: cannot access '%s': %s", path, strerror(errno));
        return 1;
    }
    if (!S_ISDIR(st.st_mode)) {
        if (::remove(path) != 0) {
            setLastError("remove_all: failed to remove '%s': %s", path, strerror(errno));
            return 1;
        }
        return 0;
    }
    if (nftw(path, removeCallback, 64, FTW_DEPTH | FTW_PHYS) != 0) {
        setLastError("remove_all: failed to remove directory tree '%s': %s", path, strerror(errno));
        return 1;
    }
    return 0;
}

int64_t __ry_filesystem_make_dir(const char *path) {
    if (mkdir(path, 0755) == 0) return 0;
    setLastError("make_dir: failed to create '%s': %s", path, strerror(errno));
    return 1;
}

int64_t __ry_filesystem_make_dir_all(const char *path) {
    if (mkdirAll(path, 0755) == 0) return 0;
    setLastError("make_dir_all: failed to create '%s': %s", path, strerror(errno));
    return 1;
}

int64_t __ry_filesystem_file_size(const char *path, int64_t *out_size) {
    struct stat st;
    if (stat(path, &st) != 0) {
        setLastError("file_size: cannot stat '%s': %s", path, strerror(errno));
        return 1;
    }
    *out_size = (int64_t)st.st_size;
    return 0;
}

int64_t __ry_filesystem_is_file(const char *path) {
    struct stat st;
    if (stat(path, &st) != 0) return 0;
    return S_ISREG(st.st_mode) ? 1 : 0;
}

int64_t __ry_filesystem_is_dir(const char *path) {
    struct stat st;
    if (stat(path, &st) != 0) return 0;
    return S_ISDIR(st.st_mode) ? 1 : 0;
}

int64_t __ry_filesystem_is_symlink(const char *path) {
    struct stat st;
    if (lstat(path, &st) != 0) return 0;
    return S_ISLNK(st.st_mode) ? 1 : 0;
}

int64_t __ry_filesystem_chmod(const char *path, int64_t mode) {
    if (::chmod(path, (mode_t)mode) == 0) return 0;
    setLastError("chmod: failed to chmod '%s': %s", path, strerror(errno));
    return 1;
}

int64_t __ry_filesystem_symlink(const char *target, const char *link_path) {
    if (::symlink(target, link_path) == 0) return 0;
    setLastError("symlink: failed to create symlink '%s' -> '%s': %s",
                 link_path, target, strerror(errno));
    return 1;
}

const char *__ry_filesystem_read_link(const char *path) {
    char buf[PATH_MAX];
    ssize_t len = readlink(path, buf, sizeof(buf) - 1);
    if (len < 0) {
        setLastError("read_link: failed to read link '%s': %s", path, strerror(errno));
        return nullptr;
    }
    buf[len] = '\0';
    return strdup(buf);
}

} // extern "C"
