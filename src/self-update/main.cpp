#include "ry/cli/self_update.hpp"

int main(int argc, char *argv[]) {
    if (argc <= 1) {
        return cmd_ry_self_update(0, nullptr);
    }
    return cmd_ry_self_update(argc - 1, argv + 1);
}
