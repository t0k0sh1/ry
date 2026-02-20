cmake_minimum_required(VERSION 3.20)
project(ry)

set(CMAKE_CXX_STANDARD 17)

find_package(LLVM REQUIRED CONFIG)

add_executable(ry src/main.cpp)

include_directories(${LLVM_INCLUDE_DIRS})
add_definitions(${LLVM_DEFINITIONS})

llvm_map_components_to_libnames(LLVM_LIBS
  core
  support
  passes
)

target_link_libraries(ry PRIVATE ${LLVM_LIBS})
