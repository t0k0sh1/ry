# Ry

## build

```
# Run CMake to configure the project
cmake -B build \
  -DLLVM_DIR=/usr/local/llvm/lib/cmake/llvm \
  -DMLIR_DIR=/usr/local/llvm/lib/cmake/mlir
# Build the project
cmake --build build
```
