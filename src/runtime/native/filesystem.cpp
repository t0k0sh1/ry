#include "ry/runtime/core/alloc.hpp"
#include "ry/runtime/core/list.hpp"
#include "ry/runtime/core/string.hpp"
#include "ry/runtime/core/error.hpp"
#include "ry/runtime/core/string.hpp"

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

namespace ry {

DEFINE_LAST_ERROR(filesystem)

namespace {

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

void *__ry_filesystem_listDir(const char *path) {
    if (hasEmbeddedNul(path)) {
        setLastError("listDir: argument contains an embedded NUL byte");
        return nullptr;
    }
    DIR *dp = opendir(path);
    if (!dp) {
        setLastError("listDir: cannot open directory '%s': %s", path, strerror(errno));
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
    if (!result) setLastError("listDir: memory allocation failed");
    return result;
}

void *__ry_filesystem_walk(const char *path) {
    if (hasEmbeddedNul(path)) {
        setLastError("walk: argument contains an embedded NUL byte");
        return nullptr;
    }
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

void *__ry_filesystem_glob(const char *pattern) {
    if (hasEmbeddedNul(pattern)) {
        setLastError("glob: argument contains an embedded NUL byte");
        return nullptr;
    }
    glob_t gl;
    int ret = ::glob(pattern, GLOB_NOSORT | GLOB_TILDE, nullptr, &gl);
    if (ret == GLOB_NOMATCH) {
        // No matches is not an error — return empty list
        globfree(&gl);
        std::vector<std::string> empty;
        return makeStringList(empty);
    }
    if (ret != 0) {
        setLastError("glob: glob failed for pattern '%s'", pattern);
        globfree(&gl);
        return nullptr;
    }
    std::vector<std::string> matches;
    matches.reserve(gl.gl_pathc);
    for (size_t i = 0; i < gl.gl_pathc; ++i) {
        matches.emplace_back(gl.gl_pathv[i]);
    }
    globfree(&gl);
    void *result = makeStringList(matches);
    if (!result) setLastError("glob: memory allocation failed");
    return result;
}

int64_t __ry_filesystem_copy(const char *src, const char *dst) {
    if (hasEmbeddedNul(src)) {
        setLastError("copy: argument contains an embedded NUL byte");
        return 1;
    }
    if (hasEmbeddedNul(dst)) {
        setLastError("copy: argument contains an embedded NUL byte");
        return 1;
    }
#ifdef __APPLE__
    // stat() follows symlinks to match the Linux path's open()+fstat() semantics
    // (which omits O_NOFOLLOW). copyfile() also follows symlinks by default, so
    // both calls observe the same target.
    //
    // TOCTOU caveat: there is a small window between stat() and copyfile()
    // where the source could be replaced. The Linux path closes this gap via
    // an fd-based fstat(); reproducing that on macOS would require switching
    // to fcopyfile() and is out of scope for this guard.
    struct stat src_st;
    if (stat(src, &src_st) != 0) {
        setLastError("copy: cannot stat source '%s': %s", src, strerror(errno));
        return 1;
    }
    if (!S_ISREG(src_st.st_mode)) {
        setLastError("copy: source '%s' is not a regular file", src);
        return 1;
    }
    if (copyfile(src, dst, nullptr, COPYFILE_ALL) != 0) {
        setLastError("copy: failed to copy '%s' to '%s': %s", src, dst, strerror(errno));
        return 1;
    }
    return 0;
#else
    // Open source first, then fstat on the fd to avoid TOCTOU race.
    // O_NOFOLLOW is intentionally omitted so that symlinks are dereferenced
    // (copying the target content), matching the behavior of cp(1).
    int src_fd = open(src, O_RDONLY);
    if (src_fd < 0) {
        setLastError("copy: cannot open source '%s': %s", src, strerror(errno));
        return 1;
    }
    struct stat src_st;
    if (fstat(src_fd, &src_st) != 0) {
        setLastError("copy: cannot stat source '%s': %s", src, strerror(errno));
        close(src_fd);
        return 1;
    }
    if (!S_ISREG(src_st.st_mode)) {
        setLastError("copy: source '%s' is not a regular file", src);
        close(src_fd);
        return 1;
    }
    // Open without O_TRUNC first, then check for same-inode to prevent
    // zeroing the source when src and dst refer to the same file.
    int dst_fd = open(dst, O_WRONLY | O_CREAT, src_st.st_mode & 07777);
    if (dst_fd < 0) {
        setLastError("copy: cannot open destination '%s': %s", dst, strerror(errno));
        close(src_fd);
        return 1;
    }
    struct stat dst_st;
    if (fstat(dst_fd, &dst_st) != 0) {
        setLastError("copy: cannot stat destination '%s': %s", dst, strerror(errno));
        close(src_fd);
        close(dst_fd);
        return 1;
    }
    if (src_st.st_dev == dst_st.st_dev && src_st.st_ino == dst_st.st_ino) {
        setLastError("copy: source and destination are the same file '%s'", src);
        close(src_fd);
        close(dst_fd);
        return 1;
    }
    if (ftruncate(dst_fd, 0) != 0) {
        setLastError("copy: cannot truncate destination '%s': %s", dst, strerror(errno));
        close(src_fd);
        close(dst_fd);
        return 1;
    }
    char buf[65536];
    ssize_t n;
    for (;;) {
        n = read(src_fd, buf, sizeof(buf));
        if (n == 0) break;
        if (n < 0) {
            if (errno == EINTR) continue;
            setLastError("copy: read error from '%s': %s", src, strerror(errno));
            close(src_fd);
            close(dst_fd);
            return 1;
        }
        const char *p = buf;
        ssize_t remaining = n;
        while (remaining > 0) {
            ssize_t written = write(dst_fd, p, static_cast<size_t>(remaining));
            if (written <= 0) {
                if (written < 0 && errno == EINTR) continue;
                setLastError("copy: write error to '%s': %s", dst,
                             written == 0 ? "write returned 0" : strerror(errno));
                close(src_fd);
                close(dst_fd);
                return 1;
            }
            p += written;
            remaining -= written;
        }
    }
    // Preserve source file permissions via fd (no TOCTOU).
    // Failure is non-fatal (best-effort permission preservation).
    if (fchmod(dst_fd, src_st.st_mode & 07777) != 0) {
        // Silently ignore — permissions may not be preserved on some filesystems
    }
    close(src_fd);
    // Check close(dst_fd) to detect deferred write errors (e.g. NFS flush).
    if (close(dst_fd) != 0) {
        setLastError("copy: failed to close destination '%s': %s", dst, strerror(errno));
        return 1;
    }
    return 0;
#endif
}

int64_t __ry_filesystem_move(const char *src, const char *dst) {
    if (hasEmbeddedNul(src)) {
        setLastError("move: argument contains an embedded NUL byte");
        return 1;
    }
    if (hasEmbeddedNul(dst)) {
        setLastError("move: argument contains an embedded NUL byte");
        return 1;
    }
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
    if (hasEmbeddedNul(path)) {
        setLastError("remove: argument contains an embedded NUL byte");
        return 1;
    }
    if (::remove(path) == 0) return 0;
    setLastError("remove: failed to remove '%s': %s", path, strerror(errno));
    return 1;
}

int64_t __ry_filesystem_removeAll(const char *path) {
    if (hasEmbeddedNul(path)) {
        setLastError("removeAll: argument contains an embedded NUL byte");
        return 1;
    }
    // Try unlink first (handles files and symlinks without TOCTOU).
    // If it fails because the path is a directory, fall through to nftw.
    if (unlink(path) == 0) return 0;
    int unlinkErr = errno;
    if (unlinkErr == EISDIR) {
        // Definitely a directory — remove recursively
    } else if (unlinkErr == EPERM) {
        // EPERM can mean directory (Linux) or permission denied (sticky bit).
        // Disambiguate with lstat.
        struct stat st;
        if (lstat(path, &st) != 0 || !S_ISDIR(st.st_mode)) {
            setLastError("removeAll: failed to remove '%s': %s", path, strerror(unlinkErr));
            return 1;
        }
    } else {
        setLastError("removeAll: failed to remove '%s': %s", path, strerror(unlinkErr));
        return 1;
    }
    // Path is a directory — remove recursively
    if (nftw(path, removeCallback, 64, FTW_DEPTH | FTW_PHYS) != 0) {
        setLastError("removeAll: failed to remove directory tree '%s': %s", path, strerror(errno));
        return 1;
    }
    return 0;
}

int64_t __ry_filesystem_mkdir(const char *path) {
    if (hasEmbeddedNul(path)) {
        setLastError("mkdir: argument contains an embedded NUL byte");
        return 1;
    }
    if (::mkdir(path, 0755) == 0) return 0;
    setLastError("mkdir: failed to create '%s': %s", path, strerror(errno));
    return 1;
}

int64_t __ry_filesystem_mkdirAll(const char *path) {
    if (hasEmbeddedNul(path)) {
        setLastError("mkdirAll: argument contains an embedded NUL byte");
        return 1;
    }
    if (mkdirAll(path, 0755) == 0) return 0;
    setLastError("mkdirAll: failed to create '%s': %s", path, strerror(errno));
    return 1;
}

int64_t __ry_filesystem_fileSize(const char *path, int64_t *out_size) {
    if (hasEmbeddedNul(path)) {
        setLastError("fileSize: argument contains an embedded NUL byte");
        return 1;
    }
    struct stat st;
    if (stat(path, &st) != 0) {
        setLastError("fileSize: cannot stat '%s': %s", path, strerror(errno));
        return 1;
    }
    *out_size = (int64_t)st.st_size;
    return 0;
}

int64_t __ry_filesystem_isFile(const char *path, int64_t *out) {
    if (hasEmbeddedNul(path)) {
        setLastError("isFile: argument contains an embedded NUL byte");
        return 1;
    }
    struct stat st;
    *out = (stat(path, &st) == 0 && S_ISREG(st.st_mode)) ? 1 : 0;
    return 0;
}

int64_t __ry_filesystem_isDir(const char *path, int64_t *out) {
    if (hasEmbeddedNul(path)) {
        setLastError("isDir: argument contains an embedded NUL byte");
        return 1;
    }
    struct stat st;
    *out = (stat(path, &st) == 0 && S_ISDIR(st.st_mode)) ? 1 : 0;
    return 0;
}

int64_t __ry_filesystem_isSymlink(const char *path, int64_t *out) {
    if (hasEmbeddedNul(path)) {
        setLastError("isSymlink: argument contains an embedded NUL byte");
        return 1;
    }
    struct stat st;
    *out = (lstat(path, &st) == 0 && S_ISLNK(st.st_mode)) ? 1 : 0;
    return 0;
}

int64_t __ry_filesystem_chmod(const char *path, int64_t mode) {
    if (hasEmbeddedNul(path)) {
        setLastError("chmod: argument contains an embedded NUL byte");
        return 1;
    }
    if (::chmod(path, (mode_t)mode) == 0) return 0;
    setLastError("chmod: failed to chmod '%s': %s", path, strerror(errno));
    return 1;
}

int64_t __ry_filesystem_symlink(const char *target, const char *link_path) {
    if (hasEmbeddedNul(target)) {
        setLastError("symlink: argument contains an embedded NUL byte");
        return 1;
    }
    if (hasEmbeddedNul(link_path)) {
        setLastError("symlink: argument contains an embedded NUL byte");
        return 1;
    }
    if (::symlink(target, link_path) == 0) return 0;
    setLastError("symlink: failed to create symlink '%s' -> '%s': %s",
                 link_path, target, strerror(errno));
    return 1;
}

const char *__ry_filesystem_readLink(const char *path) {
    if (hasEmbeddedNul(path)) {
        setLastError("readLink: argument contains an embedded NUL byte");
        return nullptr;
    }
    char buf[PATH_MAX];
    ssize_t len = readlink(path, buf, sizeof(buf) - 1);
    if (len < 0) {
        setLastError("readLink: failed to read link '%s': %s", path, strerror(errno));
        return nullptr;
    }
    buf[len] = '\0';
    return makeString(buf, static_cast<size_t>(len));
}

} // extern "C"

} // namespace ry
