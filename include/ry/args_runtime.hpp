#pragma once

extern "C" {
    void __ry_args_init(int argc, char** argv);
    int __ry_args_count();
    const char* __ry_args_get(int index);
}
