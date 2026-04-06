#include "ry/runtime_list.hpp"
#include "ry/runtime_error.hpp"

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
            ssize_t written = write(dst_fd, p, remaining);
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
            setLastError("remove_all: failed to remove '%s': %s", path, strerror(unlinkErr));
            return 1;
        }
    } else {
        setLastError("remove_all: failed to remove '%s': %s", path, strerror(unlinkErr));
        return 1;
    }
    // Path is a directory — remove recursively
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

} // namespace ry
