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

find_program(CCACHE_PROGRAM ccache)
if(CCACHE_PROGRAM)
    set(CMAKE_C_COMPILER_LAUNCHER "${CCACHE_PROGRAM}")
    set(CMAKE_CXX_COMPILER_LAUNCHER "${CCACHE_PROGRAM}")
endif()
find_package(Threads REQUIRED)
find_package(OpenSSL 3.0 REQUIRED)

# LLVM は -fno-rtti でビルドされているため合わせる
add_compile_options(-fno-rtti)

option(ENABLE_ASAN "Enable AddressSanitizer" OFF)
option(ENABLE_UBSAN "Enable UndefinedBehaviorSanitizer" OFF)
option(ENABLE_TSAN "Enable ThreadSanitizer" OFF)

if(ENABLE_TSAN AND (ENABLE_ASAN OR ENABLE_UBSAN))
    message(FATAL_ERROR "ENABLE_TSAN cannot be combined with ENABLE_ASAN or ENABLE_UBSAN")
endif()

if(ENABLE_ASAN)
    add_compile_options(-fsanitize=address -fno-omit-frame-pointer)
    add_link_options(-fsanitize=address)
endif()

if(ENABLE_UBSAN)
    # vptr: プロジェクトは -fno-rtti でビルドされているため vptr チェックは機能しない
    # function: LLVM が C 風の関数ポインタキャストを多用するため false positive だらけになる
    add_compile_options(
        -fsanitize=undefined
        -fno-sanitize=vptr,function
        -fno-sanitize-recover=undefined
        -fno-omit-frame-pointer
    )
    add_link_options(-fsanitize=undefined)
endif()

if(ENABLE_TSAN)
    add_compile_options(-fsanitize=thread -fno-omit-frame-pointer)
    add_link_options(-fsanitize=thread)
endif()

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
    src/trace.cpp
    src/source_manager.cpp
    src/diagnostic.cpp
    src/directive_meta.cpp
    src/lexer.cpp
    src/parser.cpp
    src/parser_expr.cpp
    src/type_node.cpp
    src/parser_decl.cpp
    src/codegen.cpp
    src/codegen_type.cpp
    src/codegen_builtin.cpp
    src/codegen_match.cpp
    src/codegen_test.cpp
    src/codegen_stmt.cpp
    src/codegen_stmt_loop.cpp
    src/codegen_fn.cpp
    src/codegen_expr.cpp
    src/codegen_expr_literal.cpp
    src/codegen_expr_cast.cpp
    src/stdlib_registry.cpp
    src/codegen_call.cpp
    src/codegen_call_dispatch.cpp
    src/codegen_call_string.cpp
    src/codegen_call_collection.cpp
    src/codegen_call_set_ops.cpp
    src/codegen_call_higher_order.cpp
    src/codegen_call_io.cpp
    src/codegen_call_iterator.cpp
    src/codegen_call_result.cpp
    src/codegen_lambda.cpp
    src/sema_return.cpp
    src/module_loader.cpp
    src/jit.cpp
    src/test_runtime.cpp
    src/runtime_hash.cpp
    src/runtime_regex.cpp
    src/runtime_regex_parser.cpp
    src/runtime_utf8.cpp
    src/runtime_sort.cpp
    src/project_config.cpp
    src/self_update.cpp
    src/args_runtime.cpp
    src/paths.cpp
    src/runtime_parallel.cpp
    src/runtime_print.cpp
    src/codegen_call_json.cpp
    src/formatter.cpp
    src/formatter_stmt.cpp
    src/file_watcher.cpp
    src/coverage_runtime.cpp
    src/dotenv.cpp
    src/codegen_call_native.cpp
    src/codegen_call_thread.cpp
    src/codegen_arith.cpp
    src/codegen_any.cpp
    src/runtime_any.cpp
    src/codegen_arc.cpp
    src/codegen_arc_cow.cpp
    src/codegen_arc_gc.cpp
    src/codegen_tostring.cpp
    src/codegen_fn_generic.cpp
    src/codegen_metadata.cpp
    src/codegen_stmt_misc.cpp
    src/codegen_call_user.cpp
    src/runtime_error.cpp
    src/cli.cpp
    src/jit_runner.cpp
    src/test_runner.cpp
)
target_precompile_headers(ry_lib PRIVATE
    <algorithm>
    <cstdint>
    <cstdio>
    <cstdlib>
    <cstring>
    <filesystem>
    <fstream>
    <functional>
    <iostream>
    <map>
    <memory>
    <mutex>
    <optional>
    <sstream>
    <stdexcept>
    <string>
    <string_view>
    <unordered_map>
    <unordered_set>
    <variant>
    <vector>
    <llvm/IR/IRBuilder.h>
    <llvm/IR/LLVMContext.h>
    <llvm/IR/Module.h>
    <llvm/IR/Verifier.h>
    <llvm/Support/raw_ostream.h>
)
target_include_directories(ry_lib PUBLIC
    ${CMAKE_CURRENT_SOURCE_DIR}/include
    ${LLVM_INCLUDE_DIRS}
)
target_link_libraries(ry_lib PUBLIC ${LLVM_LIBS} Threads::Threads OpenSSL::SSL OpenSSL::Crypto)

# ===== Native shared libraries for @native("libname") =====
# Stdlib runtime implementations live exclusively in shared libraries.
# The JIT loads them dynamically when @native("libname") functions are called.
# ry_lib contains only compiler infrastructure (codegen, parser, etc.) and
# language-builtin runtimes (print, regex, hash, sort, utf8, any, parallel).

function(add_ry_native_lib PKG_NAME)
    set(SOURCES ${ARGN})
    add_library(ry_${PKG_NAME} SHARED ${SOURCES})
    target_include_directories(ry_${PKG_NAME} PRIVATE
        ${CMAKE_CURRENT_SOURCE_DIR}/include)
    set_target_properties(ry_${PKG_NAME} PROPERTIES
        OUTPUT_NAME "ry_${PKG_NAME}"
        LIBRARY_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/lib"
        MACOSX_RPATH TRUE
        BUILD_RPATH "${CMAKE_BINARY_DIR}/lib"
        INSTALL_RPATH "$<IF:$<PLATFORM_ID:Darwin>,@loader_path,\$ORIGIN>")
    # Native libs resolve infrastructure symbols (__ry_set_last_error etc.)
    # from the main process at runtime, not via link-time dependency.
    if(APPLE)
        target_link_options(ry_${PKG_NAME} PRIVATE -undefined dynamic_lookup)
    endif()
endfunction()

# Self-contained packages (no cross-runtime dependencies)
add_ry_native_lib(base64     src/runtime_base64.cpp)
add_ry_native_lib(path       src/runtime_path.cpp)
add_ry_native_lib(convert    src/runtime_convert.cpp)
add_ry_native_lib(filesystem src/runtime_filesystem.cpp)
add_ry_native_lib(gc         src/runtime_gc.cpp)

# All native libs are independent — no inter-library link dependencies.
# __ry_set_last_error / __ry_get_last_error live in ry_lib (the main executable)
# and are resolved from the process at runtime via -undefined dynamic_lookup
# (macOS) or -rdynamic (Linux).
add_ry_native_lib(io         src/runtime_io.cpp)
add_ry_native_lib(json       src/runtime_json.cpp)
add_ry_native_lib(net        src/runtime_net.cpp)

add_ry_native_lib(thread     src/runtime_thread.cpp)
if(APPLE)
    target_link_libraries(ry_thread PRIVATE Threads::Threads)
else()
    target_link_libraries(ry_thread PRIVATE Threads::Threads atomic)
endif()

# runtime_hash.cpp is compiled here (not in a separate shared lib) because ry_http
# needs hash symbols at link time for HTTP header map construction.
# Shared TCP utilities are compiled directly into ry_http via runtime_net_utils.hpp.
add_ry_native_lib(http       src/runtime_http.cpp src/runtime_http_client.cpp
                             src/runtime_http_multipart.cpp src/runtime_tls.cpp
                             src/runtime_hash.cpp)
target_link_libraries(ry_http PRIVATE OpenSSL::SSL OpenSSL::Crypto Threads::Threads)

set(RY_NATIVE_LIBS ry_base64 ry_path ry_convert ry_filesystem ry_gc ry_io ry_json ry_net ry_thread ry_http)

# ===== メイン実行ファイル =====
# Native shared libraries are NOT linked here — the JIT loads them on-demand
# via find_native_library() + DynamicLibrarySearchGenerator::Load() (see main.cpp).
add_executable(ry src/main.cpp)
add_dependencies(ry ${RY_NATIVE_LIBS})
if(APPLE)
    target_link_libraries(ry PRIVATE -Wl,-force_load,$<TARGET_FILE:ry_lib> ry_lib)
else()
    # --no-as-needed for atomic: JIT-generated code may reference __atomic_load
    # (from LLVM atomicrmw lowering), so libatomic must stay linked even though
    # ry_lib has no direct references to it.
    target_link_libraries(ry PRIVATE -Wl,--whole-archive ry_lib -Wl,--no-whole-archive -Wl,--no-as-needed atomic -Wl,--as-needed)
    # -rdynamic: export process symbols so JIT DynamicLibrarySearchGenerator and
    # dlopen'd native libs can find __ry_set_last_error etc. from the process
    target_link_options(ry PRIVATE -rdynamic)
endif()

# ===== テスト =====
enable_testing()

add_executable(ry_tests
    tests/test_lexer.cpp
    tests/test_parser.cpp
    tests/test_codegen.cpp
    tests/test_codegen_stmt.cpp
    tests/test_spec_concurrency.cpp
    tests/test_codegen_collection.cpp
    tests/test_codegen_advanced.cpp
    tests/test_codegen_contract.cpp
    tests/test_codegen_directive.cpp
    tests/test_project_config.cpp
    tests/test_self_update.cpp
    tests/test_diagnostic.cpp
    tests/test_paths.cpp
    tests/test_builtin_stdlib_registry.cpp
    tests/test_regex_runtime.cpp
    tests/test_codegen_regex.cpp
    tests/test_codegen_mock.cpp
    tests/test_codegen_fail.cpp
    tests/test_sema_return.cpp
    tests/test_codegen_iterator.cpp
    tests/test_codegen_parameterized.cpp
    tests/test_runtime_utf8.cpp
    tests/test_runtime_http.cpp
    tests/test_codegen_io.cpp
    tests/test_codegen_net.cpp
    tests/test_codegen_http.cpp
    tests/test_formatter.cpp
    tests/test_file_watcher.cpp
    tests/test_coverage_runtime.cpp
    tests/test_dotenv.cpp
    tests/test_runtime_json.cpp
    tests/test_runtime_any.cpp
    tests/test_runtime_print.cpp
    tests/test_stdin.cpp
    tests/test_entry_point.cpp
    tests/test_help.cpp
    tests/test_trace.cpp
    tests/test_codegen_arc.cpp
    tests/test_runtime_gc.cpp
    tests/test_codegen_type_safety.cpp
)
if(APPLE)
    target_link_libraries(ry_tests PRIVATE
        -Wl,-force_load,$<TARGET_FILE:ry_lib>
        ry_lib
        ${RY_NATIVE_LIBS}
        GTest::gtest_main
        GTest::gmock
    )
else()
    target_link_libraries(ry_tests PRIVATE
        -Wl,--whole-archive ry_lib -Wl,--no-whole-archive
        -Wl,--no-as-needed ${RY_NATIVE_LIBS} atomic -Wl,--as-needed
        GTest::gtest_main
        GTest::gmock
    )
    # Linux: JIT の DynamicLibrarySearchGenerator がプロセスシンボルを参照できるようにする
    target_link_options(ry_tests PRIVATE -rdynamic)
endif()

target_compile_definitions(ry_tests PRIVATE RY_BINARY_PATH="$<TARGET_FILE:ry>")
target_precompile_headers(ry_tests REUSE_FROM ry_lib)

include(GoogleTest)
if(ENABLE_ASAN OR ENABLE_UBSAN OR ENABLE_TSAN)
    # サニタイザー有効時は gtest_discover_tests を使わない:
    #   - ASan: ビルド時のバイナリ実行で LLVM の container-overflow false positive が発生
    #   - UBSan/TSan: 同様に discovery 時の実行が予期せぬ失敗を引き起こしうる
    add_test(NAME ry_tests COMMAND ry_tests)
    if(ENABLE_ASAN)
        set_tests_properties(ry_tests PROPERTIES
            ENVIRONMENT "ASAN_OPTIONS=detect_container_overflow=0")
    endif()
else()
    gtest_discover_tests(ry_tests)
endif()
