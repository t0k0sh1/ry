#include "ry/runtime/core/string.hpp"
#include "ry/runtime/core/arc.hpp"
#include "ry/ry_layout.hpp"

#include <cstddef>
#include <cstdint>
#include <cstring>

using namespace ry;

extern "C" {
void *__ry_io_file_open(const char *path, const char *mode);
void __ry_io_file_cleanup(void *handle);
void __ry_arc_free_counted(void *header_ptr);
}

extern "C" int LLVMFuzzerTestOneInput(const uint8_t *data, size_t size) {
    char *path = makeString(reinterpret_cast<const char *>(data), size);
    // Try all three valid modes plus an invalid one; modes must be Ry string
    // handles because __ry_io_file_open calls hasEmbeddedNul() on them.
    static char *modes[4];
    static bool modes_init = false;
    if (!modes_init) {
        modes[0] = makeString("r", 1);
        modes[1] = makeString("w", 1);
        modes[2] = makeString("a", 1);
        modes[3] = makeString("z", 1);
        modes_init = true;
    }
    for (const char *mode : modes) {
        void *h = __ry_io_file_open(path, mode);
        if (h) {
            __ry_io_file_cleanup(h);
            __ry_arc_free_counted(static_cast<char *>(h) - ARC_HEADER_SIZE);
        }
    }
    freeStringSlot(path);
    return 0;
}
