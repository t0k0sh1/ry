#include "ry/runtime_json.hpp"
#include "ry/runtime_string.hpp"

#include <cstddef>
#include <cstdint>

using namespace ry;

extern "C" int LLVMFuzzerTestOneInput(const uint8_t *data, size_t size) {
    // __ry_json_parse expects a Ry string handle (StringHeader-prefixed allocation).
    // makeString allocates such a handle from a raw byte buffer.
    char *input = makeString(reinterpret_cast<const char *>(data), size);
    void *h = __ry_json_parse(input);
    freeStringSlot(input);
    if (h != nullptr) {
        __ry_json_free(h);
    }
    return 0;
}
