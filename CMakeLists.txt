cmake_minimum_required(VERSION 3.25)
project(ry)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

if(NOT RY_VERSION)
    set(RY_VERSION "0.0.0")
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
option(ENABLE_FUZZER "Enable libFuzzer coverage-guided fuzzing (Clang-only; use 'fuzz' preset)" OFF)

if(ENABLE_TSAN AND (ENABLE_ASAN OR ENABLE_UBSAN OR ENABLE_FUZZER))
    message(FATAL_ERROR "ENABLE_TSAN cannot be combined with ENABLE_ASAN, ENABLE_UBSAN, or ENABLE_FUZZER")
endif()

if(ENABLE_FUZZER AND NOT CMAKE_CXX_COMPILER_ID STREQUAL "Clang")
    message(FATAL_ERROR "ENABLE_FUZZER requires Clang (detected '${CMAKE_CXX_COMPILER_ID}'). Use 'cmake --preset fuzz' which sets CMAKE_CXX_COMPILER=/usr/local/llvm/bin/clang++.")
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

if(ENABLE_FUZZER)
    # -fsanitize=fuzzer-no-link instruments all TUs for edge coverage without
    # injecting libFuzzer's main(). The actual -fsanitize=fuzzer (which adds main())
    # is applied only to fuzz executable link steps in add_ry_fuzz_target below.
    add_compile_options(-fsanitize=fuzzer-no-link)
endif()

# Strict warning flags for internal targets (#895)
# Applied per-target so FetchContent targets (GoogleTest) are not affected.
set(RY_WARNING_FLAGS -Wall -Wextra -Wpedantic -Wconversion -Wshadow)

find_package(LLVM REQUIRED CONFIG)
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
# In CI containers (docker/ci.Dockerfile and docker/ci-glibc-old.Dockerfile)
# the gtest source tarball is pre-baked at $RY_VENDORED_GTEST_TARBALL so
# CMake never needs network access. Outside the container we fall back to
# fetching from github.com.
include(FetchContent)
if(DEFINED ENV{RY_VENDORED_GTEST_TARBALL} AND EXISTS "$ENV{RY_VENDORED_GTEST_TARBALL}")
    FetchContent_Declare(
        googletest
        URL "file://$ENV{RY_VENDORED_GTEST_TARBALL}"
        DOWNLOAD_EXTRACT_TIMESTAMP TRUE
        SYSTEM
    )
else()
    FetchContent_Declare(
        googletest
        URL https://github.com/google/googletest/archive/refs/tags/v1.17.0.tar.gz
        DOWNLOAD_EXTRACT_TIMESTAMP TRUE
        SYSTEM
    )
endif()
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
    src/codegen_call_base64.cpp
    src/codegen_call_iterator.cpp
    src/codegen_call_result.cpp
    src/codegen_call_option.cpp
    src/codegen_lambda.cpp
    src/sema_return.cpp
    src/module_loader.cpp
    src/jit.cpp
    src/test_runtime.cpp
    src/runtime_string.cpp
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
    src/runtime_any_typed_coll.cpp
    src/codegen_arc.cpp
    src/codegen_arc_cow.cpp
    src/codegen_call_runtime_internal.cpp
    src/runtime_arc_counter.cpp
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
target_include_directories(ry_lib PUBLIC ${CMAKE_CURRENT_SOURCE_DIR}/include)
target_include_directories(ry_lib SYSTEM PUBLIC ${LLVM_INCLUDE_DIRS})
target_link_libraries(ry_lib PUBLIC ${LLVM_LIBS} Threads::Threads OpenSSL::SSL OpenSSL::Crypto)
target_compile_options(ry_lib PRIVATE ${RY_WARNING_FLAGS})

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
    target_compile_options(ry_${PKG_NAME} PRIVATE ${RY_WARNING_FLAGS})
    if(APPLE)
        target_link_options(ry_${PKG_NAME} PRIVATE -undefined dynamic_lookup)
    endif()
endfunction()

# add_ry_fuzz_target(<name> <source> [<source> ...])
# Builds a libFuzzer harness that whole-archive-links ry_lib so that all symbols
# (including __ry_set_last_error etc.) are available to sources compiled directly
# into the harness (e.g. runtime_json.cpp). Requires ENABLE_FUZZER=ON (Clang-only).
function(add_ry_fuzz_target TARGET_NAME)
    set(SOURCES ${ARGN})
    add_executable(${TARGET_NAME} ${SOURCES})
    if(APPLE)
        target_link_libraries(${TARGET_NAME} PRIVATE
            -Wl,-force_load,$<TARGET_FILE:ry_lib>
            ry_lib)
    else()
        target_link_libraries(${TARGET_NAME} PRIVATE
            -Wl,--whole-archive ry_lib -Wl,--no-whole-archive
            -Wl,--no-as-needed atomic -Wl,--as-needed)
        target_link_options(${TARGET_NAME} PRIVATE -rdynamic)
    endif()
    # -fsanitize=fuzzer links libFuzzer's main() into this executable only
    target_link_options(${TARGET_NAME} PRIVATE -fsanitize=fuzzer)
    target_compile_options(${TARGET_NAME} PRIVATE ${RY_WARNING_FLAGS})
endfunction()

# Self-contained packages (no cross-runtime dependencies)
add_ry_native_lib(base64     src/runtime_base64.cpp)
add_ry_native_lib(path       src/runtime_path.cpp)
add_ry_native_lib(convert    src/runtime_convert.cpp)
add_ry_native_lib(filesystem src/runtime_filesystem.cpp)
add_ry_native_lib(gc         src/runtime_gc.cpp)
add_ry_native_lib(testing    src/runtime_testing.cpp)

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
                             src/runtime_http_multipart.cpp src/runtime_http_error.cpp
                             src/runtime_tls.cpp src/runtime_hash.cpp)
target_link_libraries(ry_http PRIVATE OpenSSL::SSL OpenSSL::Crypto Threads::Threads)

set(RY_NATIVE_LIBS ry_base64 ry_path ry_convert ry_filesystem ry_gc ry_testing ry_io ry_json ry_net ry_thread ry_http)

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
target_compile_options(ry PRIVATE ${RY_WARNING_FLAGS})

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
    tests/test_runtime_json_any_panic.cpp
    tests/test_runtime_any.cpp
    tests/test_runtime_print.cpp
    tests/test_runtime_string.cpp
    tests/test_stdin.cpp
    tests/test_input_builtin.cpp
    tests/test_read_line_builtin.cpp
    tests/test_entry_point.cpp
    tests/test_help.cpp
    tests/test_trace.cpp
    tests/test_codegen_arc.cpp
    tests/test_runtime_gc.cpp
    tests/test_runtime_rwlock_stress.cpp
    tests/test_runtime_arc_contention_stress.cpp
    tests/test_runtime_lock_stress.cpp
    tests/test_codegen_type_safety.cpp
    tests/test_emit_llvm_ir.cpp
    tests/test_deprecated_warnings.cpp
    tests/test_spec_timeout.cpp
    tests/test_runtime_io_file.cpp
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
target_compile_definitions(ry_tests PRIVATE RY_SOURCE_ROOT="${CMAKE_CURRENT_SOURCE_DIR}")
target_precompile_headers(ry_tests REUSE_FROM ry_lib)
target_compile_options(ry_tests PRIVATE ${RY_WARNING_FLAGS})

include(GoogleTest)
if(ENABLE_ASAN OR ENABLE_UBSAN OR ENABLE_TSAN OR ENABLE_FUZZER)
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

# ===== FileCheck IR ゴールデンテスト =====
find_program(FILECHECK_EXECUTABLE FileCheck
    HINTS
        ${LLVM_TOOLS_BINARY_DIR}
        /opt/homebrew/opt/llvm@${LLVM_VERSION_MAJOR}/bin
        /opt/homebrew/opt/llvm/bin
        /usr/local/llvm/bin
        /usr/lib/llvm-${LLVM_VERSION_MAJOR}/bin
    DOC "LLVM FileCheck for IR golden tests")

if(FILECHECK_EXECUTABLE)
    message(STATUS "FileCheck found: ${FILECHECK_EXECUTABLE}")
    file(GLOB FILECHECK_TEST_FILES "${CMAKE_CURRENT_SOURCE_DIR}/tests/filecheck/*.ry")
    foreach(f IN LISTS FILECHECK_TEST_FILES)
        get_filename_component(name "${f}" NAME_WE)
        add_test(NAME "filecheck_${name}"
            COMMAND bash -c
                "set -o pipefail; \"$<TARGET_FILE:ry>\" --emit-llvm-ir \"${f}\" | \"${FILECHECK_EXECUTABLE}\" \"${f}\"")
        set_tests_properties("filecheck_${name}" PROPERTIES LABELS "filecheck")
    endforeach()
else()
    message(STATUS "FileCheck not found; skipping IR golden tests \
(macOS: brew install llvm@${LLVM_VERSION_MAJOR}, Linux: apt-get install llvm-${LLVM_VERSION_MAJOR}-tools)")
endif()

# ===== libFuzzer ハーネス =====
# Built only when ENABLE_FUZZER=ON (use 'cmake --preset fuzz').
# Each harness whole-archive-links ry_lib so coverage instrumentation covers the
# entire compiler pipeline, not just the harness translation unit.
if(ENABLE_FUZZER)
    add_ry_fuzz_target(fuzz_parser tests/fuzz/fuzz_parser.cpp)
    # fuzz_json compiles runtime_json.cpp directly (it lives in ry_json SHARED lib,
    # not ry_lib) so the JSON parser symbols are available without dlopen.
    add_ry_fuzz_target(fuzz_json tests/fuzz/fuzz_json.cpp src/runtime_json.cpp)
    add_ry_fuzz_target(fuzz_utf8 tests/fuzz/fuzz_utf8.cpp)
    # fuzz_io_open compiles runtime_io.cpp directly (it lives in ry_io SHARED lib,
    # not ry_lib) so the File handle symbols are available without dlopen.
    add_ry_fuzz_target(fuzz_io_open tests/fuzz/fuzz_io_open.cpp src/runtime_io.cpp)
endif()
