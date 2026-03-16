cmake_minimum_required(VERSION 3.20)
project(ry)

if(NOT RY_VERSION)
    file(READ "${CMAKE_CURRENT_SOURCE_DIR}/VERSION" RY_VERSION)
    string(STRIP "${RY_VERSION}" RY_VERSION)
endif()
add_compile_definitions(RY_VERSION="${RY_VERSION}")

if(NOT CMAKE_BUILD_TYPE AND NOT CMAKE_CONFIGURATION_TYPES)
    set(CMAKE_BUILD_TYPE Release CACHE STRING "Build type" FORCE)
endif()

set(CMAKE_CXX_STANDARD 17)

# LLVM は -fno-rtti でビルドされているため合わせる
add_compile_options(-fno-rtti)

find_package(LLVM REQUIRED CONFIG)
include_directories(${LLVM_INCLUDE_DIRS})
add_definitions(${LLVM_DEFINITIONS})

if(CMAKE_SYSTEM_PROCESSOR MATCHES "arm64|aarch64|ARM64")
    set(RY_ARCH_COMPONENTS aarch64codegen aarch64asmparser)
else()
    set(RY_ARCH_COMPONENTS x86codegen x86asmparser)
endif()

llvm_map_components_to_libnames(LLVM_LIBS
    core support passes
    orcjit jitlink native nativecodegen
    ${RY_ARCH_COMPONENTS} targetparser
)

# ===== GoogleTest (FetchContent) =====
include(FetchContent)
FetchContent_Declare(
    googletest
    URL https://github.com/google/googletest/archive/refs/tags/v1.17.0.tar.gz
    DOWNLOAD_EXTRACT_TIMESTAMP TRUE
)
set(INSTALL_GTEST OFF CACHE BOOL "" FORCE)
FetchContent_MakeAvailable(googletest)

# ===== 共有ライブラリ =====
add_library(ry_lib STATIC
    src/lexer.cpp
    src/parser.cpp
    src/codegen.cpp
    src/codegen_builtin.cpp
    src/codegen_stmt.cpp
    src/codegen_expr.cpp
    src/codegen_call.cpp
    src/codegen_lambda.cpp
    src/module_loader.cpp
    src/jit.cpp
    src/test_runtime.cpp
    src/runtime_hash.cpp
    src/project_config.cpp
    src/self_update.cpp
)
target_include_directories(ry_lib PUBLIC
    ${CMAKE_CURRENT_SOURCE_DIR}/include
    ${LLVM_INCLUDE_DIRS}
)
target_link_libraries(ry_lib PUBLIC ${LLVM_LIBS})

# ===== メイン実行ファイル =====
add_executable(ry src/main.cpp)
if(APPLE)
    target_link_libraries(ry PRIVATE -Wl,-force_load,$<TARGET_FILE:ry_lib> ry_lib)
else()
    target_link_libraries(ry PRIVATE -Wl,--whole-archive ry_lib -Wl,--no-whole-archive)
    # Linux: JIT の DynamicLibrarySearchGenerator がプロセスシンボルを参照できるようにする
    target_link_options(ry PRIVATE -rdynamic)
endif()

# ===== テスト =====
enable_testing()

add_executable(ry_tests
    tests/test_lexer.cpp
    tests/test_parser.cpp
    tests/test_codegen.cpp
    tests/test_codegen_stmt.cpp
    tests/test_codegen_collection.cpp
    tests/test_codegen_advanced.cpp
    tests/test_codegen_contract.cpp
    tests/test_project_config.cpp
    tests/test_self_update.cpp
)
if(APPLE)
    target_link_libraries(ry_tests PRIVATE
        -Wl,-force_load,$<TARGET_FILE:ry_lib>
        ry_lib
        GTest::gtest_main
    )
else()
    target_link_libraries(ry_tests PRIVATE
        -Wl,--whole-archive ry_lib -Wl,--no-whole-archive
        GTest::gtest_main
    )
    # Linux: JIT の DynamicLibrarySearchGenerator がプロセスシンボルを参照できるようにする
    target_link_options(ry_tests PRIVATE -rdynamic)
endif()

include(GoogleTest)
gtest_discover_tests(ry_tests)
