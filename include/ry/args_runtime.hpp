#pragma once


namespace ry {

extern "C" {
    void __ry_args_init(int argc, const char* const* argv);
    int __ry_args_count();
    const char* __ry_args_get(int index);
}

} // namespace ry
