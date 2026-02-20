cmake_minimum_required(VERSION 3.20)
project(ry)

set(CMAKE_CXX_STANDARD 17)

find_package(LLVM REQUIRED CONFIG)
include_directories(${LLVM_INCLUDE_DIRS})
add_definitions(${LLVM_DEFINITIONS})

llvm_map_components_to_libnames(LLVM_LIBS
    core support passes
    orcjit jitlink native nativecodegen
    x86codegen x86asmparser targetparser
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
)
target_include_directories(ry_lib PUBLIC
    ${CMAKE_CURRENT_SOURCE_DIR}/include
    ${LLVM_INCLUDE_DIRS}
)
target_link_libraries(ry_lib PUBLIC ${LLVM_LIBS})

# ===== メイン実行ファイル =====
add_executable(ry src/main.cpp)
target_link_libraries(ry PRIVATE ry_lib)

# ===== テスト =====
enable_testing()

add_executable(ry_tests
    tests/test_lexer.cpp
    tests/test_parser.cpp
    tests/test_codegen.cpp
)
target_link_libraries(ry_tests PRIVATE
    ry_lib
    GTest::gtest_main
)

include(GoogleTest)
gtest_discover_tests(ry_tests)
