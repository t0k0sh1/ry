#include "ry/runtime/core/any.hpp"
#include "ry/runtime/native/json5.hpp"
#include "ry/runtime/core/string.hpp"

#include <cstddef>
#include <cstdint>

using namespace ry;

extern "C" int LLVMFuzzerTestOneInput(const uint8_t *data, size_t size) {
    char *input = makeString(reinterpret_cast<const char *>(data), size);
    RyAny out{};
    int64_t status = __ry_json5_parse_to_any(input, &out);
    freeStringSlot(input);
    if (status == 0) {
        __ry_json5_release_any(&out);
    }
    return 0;
}
