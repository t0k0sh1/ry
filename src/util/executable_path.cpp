#include "ry/util/executable_path.hpp"

#include <cstdlib>
#include <unistd.h>

#ifdef __APPLE__
#include <mach-o/dyld.h>
#endif

namespace ry {

std::string get_executable_path() {
#ifdef __APPLE__
    char path[4096];
    uint32_t size = sizeof(path);
    if (_NSGetExecutablePath(path, &size) != 0) {
        return "";
    }
    char *real = realpath(path, nullptr);
    if (!real) return std::string(path);
    std::string result(real);
    free(real);
    return result;
#else
    char path[4096];
    ssize_t len = readlink("/proc/self/exe", path, sizeof(path) - 1);
    if (len == -1) return "";
    path[len] = '\0';
    return std::string(path);
#endif
}

} // namespace ry
