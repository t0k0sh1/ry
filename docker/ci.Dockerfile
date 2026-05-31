# syntax=docker/dockerfile:1.7
#
# ry CI base image (regular).
#
# Built by .github/workflows/build-ci-image.yml and pushed to
# ghcr.io/<owner>/ry-ci:llvm-21-rev<N> (immutable) and :latest.
#
# Dependencies are sourced from github.com — either by downloading
# pre-built tarballs from a project's GitHub Releases page (cmake,
# ninja, ccache) or by source-building from a GitHub Releases source
# tarball (LLVM, OpenSSL, cppcheck, gtest). The Rust toolchain is the
# sole exception: it is installed from static.rust-lang.org/dist (the
# official Rust binary distribution channel, used for issue #1950's
# Rust reimplementation of the LLVM IR emission shared library). No
# `apt` or `apt-get` invocations anywhere — that is the central
# immunity property the image provides against Ubuntu apt mirror
# outages (issue #1505).
#
# Multi-arch: linux/amd64 + linux/arm64. Per-arch builds run on native
# GitHub-hosted runners (ubuntu-24.04 + ubuntu-24.04-arm) and the
# resulting per-arch images are combined into a manifest list.
#
# Base: `gcc:14-trixie` (Debian 13). Pre-installed and used here:
# gcc-14, g++-14, glibc 2.41, git, wget, curl, libc-dev, make, ld,
# tar, xz, gzip, unzip, python3, autoconf.

#
# Stage 1: cmake-bootstrap — download CMake prebuilt binary tarball.
#
FROM gcc:14-trixie AS cmake-bootstrap

ARG CMAKE_VERSION=3.30.5
ARG TARGETARCH

WORKDIR /tmp
RUN set -eux; \
    case "${TARGETARCH}" in \
        amd64)  CARCH=x86_64 ;; \
        arm64)  CARCH=aarch64 ;; \
        *) echo "unsupported TARGETARCH: ${TARGETARCH}" >&2; exit 1 ;; \
    esac; \
    wget -q "https://github.com/Kitware/CMake/releases/download/v${CMAKE_VERSION}/cmake-${CMAKE_VERSION}-linux-${CARCH}.tar.gz"; \
    tar -xzf "cmake-${CMAKE_VERSION}-linux-${CARCH}.tar.gz"; \
    mv "cmake-${CMAKE_VERSION}-linux-${CARCH}" /opt/cmake; \
    rm "cmake-${CMAKE_VERSION}-linux-${CARCH}.tar.gz"

#
# Stage 2: ninja-bootstrap — download Ninja prebuilt zip.
#
FROM gcc:14-trixie AS ninja-bootstrap

ARG NINJA_VERSION=1.12.1
ARG TARGETARCH

WORKDIR /tmp
RUN set -eux; \
    case "${TARGETARCH}" in \
        amd64)  NASSET=ninja-linux.zip ;; \
        arm64)  NASSET=ninja-linux-aarch64.zip ;; \
        *) echo "unsupported TARGETARCH: ${TARGETARCH}" >&2; exit 1 ;; \
    esac; \
    wget -q "https://github.com/ninja-build/ninja/releases/download/v${NINJA_VERSION}/${NASSET}"; \
    mkdir -p /opt/ninja/bin; \
    unzip -j "${NASSET}" -d /opt/ninja/bin; \
    chmod +x /opt/ninja/bin/ninja; \
    rm "${NASSET}"

#
# Stage 3: ccache-bootstrap — source-build ccache from github tarball.
#
# ccache only ships a prebuilt linux-x86_64 binary; aarch64 must be
# built from source. To keep both arches uniform we source-build on
# both. ZSTD_FROM_INTERNET=ON lets ccache's CMake fetch the zstd
# source itself (still from github.com), avoiding a dedicated
# zstd-bootstrap stage. REDIS_STORAGE_BACKEND/DOCUMENTATION/TESTING
# are off because ry CI does not need them.
#
FROM gcc:14-trixie AS ccache-bootstrap

ARG CCACHE_VERSION=4.10.2

COPY --from=cmake-bootstrap /opt/cmake /opt/cmake
COPY --from=ninja-bootstrap /opt/ninja /opt/ninja
ENV PATH=/opt/cmake/bin:/opt/ninja/bin:${PATH}

WORKDIR /tmp
RUN set -eux; \
    wget -q "https://github.com/ccache/ccache/releases/download/v${CCACHE_VERSION}/ccache-${CCACHE_VERSION}.tar.gz"; \
    tar -xzf "ccache-${CCACHE_VERSION}.tar.gz"; \
    cmake -S "ccache-${CCACHE_VERSION}" -B build -G Ninja \
        -DCMAKE_BUILD_TYPE=Release \
        -DZSTD_FROM_INTERNET=ON \
        -DREDIS_STORAGE_BACKEND=OFF \
        -DENABLE_TESTING=OFF \
        -DENABLE_DOCUMENTATION=OFF; \
    cmake --build build --target ccache -j"$(nproc)"; \
    install -m 0755 build/ccache /opt/ccache; \
    rm -rf "ccache-${CCACHE_VERSION}" build "ccache-${CCACHE_VERSION}.tar.gz"

#
# Stage 4: openssl-builder — source-build OpenSSL 3 from github.com.
#
FROM gcc:14-trixie AS openssl-builder

ARG OPENSSL_VERSION=3.3.2

WORKDIR /tmp
RUN set -eux; \
    wget -q "https://github.com/openssl/openssl/releases/download/openssl-${OPENSSL_VERSION}/openssl-${OPENSSL_VERSION}.tar.gz"; \
    tar -xzf "openssl-${OPENSSL_VERSION}.tar.gz"; \
    cd "openssl-${OPENSSL_VERSION}"; \
    ./config --prefix=/opt/openssl --openssldir=/opt/openssl/ssl \
             --libdir=lib no-tests no-docs shared; \
    make -j"$(nproc)"; \
    make install_sw install_ssldirs; \
    cd /tmp; \
    rm -rf "openssl-${OPENSSL_VERSION}" "openssl-${OPENSSL_VERSION}.tar.gz"

#
# Stage 5: llvm-builder — source-build LLVM 21 + clang + clang-tools-extra.
#
# This is the heaviest stage; on a native amd64 runner it takes 30-60 min,
# on arm64 60-90 min. Image rebuilds are infrequent (only on Dockerfile
# changes), so this is the right tradeoff vs depending on apt.llvm.org.
#
FROM gcc:14-trixie AS llvm-builder

ARG LLVM_VERSION=21.1.8

COPY --from=cmake-bootstrap /opt/cmake /opt/cmake
COPY --from=ninja-bootstrap /opt/ninja /opt/ninja
ENV PATH=/opt/cmake/bin:/opt/ninja/bin:${PATH}

WORKDIR /tmp
RUN set -eux; \
    wget -q "https://github.com/llvm/llvm-project/releases/download/llvmorg-${LLVM_VERSION}/llvm-project-${LLVM_VERSION}.src.tar.xz"; \
    tar -xJf "llvm-project-${LLVM_VERSION}.src.tar.xz"; \
    cd "llvm-project-${LLVM_VERSION}.src"; \
    cmake -S llvm -B build -G Ninja \
        -DCMAKE_BUILD_TYPE=Release \
        -DCMAKE_INSTALL_PREFIX=/usr/local/llvm \
        -DLLVM_ENABLE_PROJECTS="clang;clang-tools-extra" \
        -DLLVM_ENABLE_RUNTIMES="compiler-rt" \
        -DLLVM_TARGETS_TO_BUILD="X86;AArch64" \
        -DLLVM_ENABLE_RTTI=ON \
        -DLLVM_BUILD_LLVM_DYLIB=ON \
        -DLLVM_LINK_LLVM_DYLIB=ON \
        -DLLVM_INCLUDE_EXAMPLES=OFF \
        -DLLVM_INCLUDE_TESTS=OFF \
        -DLLVM_INCLUDE_DOCS=OFF \
        -DLLVM_INCLUDE_BENCHMARKS=OFF \
        -DCLANG_INCLUDE_TESTS=OFF \
        -DCLANG_INCLUDE_DOCS=OFF \
        -DCOMPILER_RT_BUILD_LIBFUZZER=ON \
        -DCOMPILER_RT_BUILD_SANITIZERS=ON \
        -DCOMPILER_RT_BUILD_BUILTINS=ON \
        -DCOMPILER_RT_BUILD_PROFILE=OFF \
        -DCOMPILER_RT_BUILD_XRAY=OFF \
        -DCOMPILER_RT_BUILD_MEMPROF=OFF \
        -DCOMPILER_RT_BUILD_ORC=OFF; \
    cmake --build build --parallel; \
    cmake --install build; \
    cd /tmp; \
    rm -rf "llvm-project-${LLVM_VERSION}.src" "llvm-project-${LLVM_VERSION}.src.tar.xz"

#
# Stage 6: rust-bootstrap — install Rust standalone toolchain.
#
# Rust is distributed from static.rust-lang.org, the official binary
# distribution channel. This is the only non-github.com source in the
# image; rust-lang.org is as stable a primary distribution as github
# and provides the canonical tarball + cryptographic install scripts.
# We do NOT use rustup (toolchain manager) — only the standalone Rust
# tarball — to keep the image immutable and offline-friendly.
#
# Required for crates/ry_llvm_emit (issue #1950, Rust reimplementation
# of the LLVM IR emission shared library behind the same C ABI).
# LLVM_SYS_211_PREFIX is wired in the final image to the existing
# /usr/local/llvm install so llvm-sys = "210" finds LLVM 21 at build.
#
FROM gcc:14-trixie AS rust-bootstrap

ARG RUST_VERSION=1.83.0
ARG TARGETARCH

WORKDIR /tmp
RUN set -eux; \
    case "${TARGETARCH}" in \
        amd64)  RARCH=x86_64-unknown-linux-gnu ;; \
        arm64)  RARCH=aarch64-unknown-linux-gnu ;; \
        *) echo "unsupported TARGETARCH: ${TARGETARCH}" >&2; exit 1 ;; \
    esac; \
    wget -q "https://static.rust-lang.org/dist/rust-${RUST_VERSION}-${RARCH}.tar.xz"; \
    tar -xJf "rust-${RUST_VERSION}-${RARCH}.tar.xz"; \
    "rust-${RUST_VERSION}-${RARCH}/install.sh" \
        --prefix=/opt/rust \
        --components="rustc,cargo,rust-std-${RARCH}"; \
    rm -rf "rust-${RUST_VERSION}-${RARCH}" "rust-${RUST_VERSION}-${RARCH}.tar.xz"

#
# Stage 7: cppcheck-builder — source-build cppcheck.
#
FROM gcc:14-trixie AS cppcheck-builder

ARG CPPCHECK_VERSION=2.16.0

COPY --from=cmake-bootstrap /opt/cmake /opt/cmake
COPY --from=ninja-bootstrap /opt/ninja /opt/ninja
ENV PATH=/opt/cmake/bin:/opt/ninja/bin:${PATH}

WORKDIR /tmp
RUN set -eux; \
    wget -q "https://github.com/danmar/cppcheck/archive/refs/tags/${CPPCHECK_VERSION}.tar.gz" \
        -O "cppcheck-${CPPCHECK_VERSION}.tar.gz"; \
    tar -xzf "cppcheck-${CPPCHECK_VERSION}.tar.gz"; \
    cd "cppcheck-${CPPCHECK_VERSION}"; \
    cmake -B build -G Ninja \
        -DCMAKE_BUILD_TYPE=Release \
        -DCMAKE_INSTALL_PREFIX=/opt/cppcheck \
        -DUSE_MATCHCOMPILER=ON; \
    cmake --build build --parallel; \
    cmake --install build; \
    cd /tmp; \
    rm -rf "cppcheck-${CPPCHECK_VERSION}" "cppcheck-${CPPCHECK_VERSION}.tar.gz"

#
# Final image
#
FROM gcc:14-trixie

ARG LLVM_VERSION=21.1.8
ARG GTEST_VERSION=1.17.0

LABEL org.opencontainers.image.source=https://github.com/t0k0sh1/ry
LABEL org.opencontainers.image.description="ry CI image — gcc:14-trixie + LLVM ${LLVM_VERSION} (source-built) + OpenSSL 3 + cmake + ninja + ccache + cppcheck + Rust (static.rust-lang.org). Built entirely from github.com + rust-lang.org sources; no apt anywhere."
LABEL org.opencontainers.image.licenses=Apache-2.0

# Copy artefacts from builder stages.
COPY --from=cmake-bootstrap /opt/cmake /opt/cmake
COPY --from=ninja-bootstrap /opt/ninja /opt/ninja
COPY --from=ccache-bootstrap /opt/ccache /usr/local/bin/ccache
COPY --from=openssl-builder /opt/openssl /opt/openssl
COPY --from=llvm-builder /usr/local/llvm /usr/local/llvm
COPY --from=cppcheck-builder /opt/cppcheck /opt/cppcheck
COPY --from=rust-bootstrap /opt/rust /opt/rust

# Vendor the GoogleTest source tarball at /opt/gtest. CMakeLists.txt
# is updated in the same PR to consume this via FetchContent_Declare
# with file:// when RY_VENDORED_GTEST_TARBALL is set.
WORKDIR /opt/gtest
RUN set -eux; \
    wget -q "https://github.com/google/googletest/archive/refs/tags/v${GTEST_VERSION}.tar.gz" \
        -O "/opt/gtest/googletest-v${GTEST_VERSION}.tar.gz"

ENV PATH=/opt/cmake/bin:/opt/ninja/bin:/opt/cppcheck/bin:/usr/local/llvm/bin:/opt/rust/bin:${PATH}
ENV LD_LIBRARY_PATH=/opt/openssl/lib64:/opt/openssl/lib:/usr/local/llvm/lib:/opt/rust/lib
ENV OPENSSL_ROOT_DIR=/opt/openssl
ENV LLVM_DIR=/usr/local/llvm/lib/cmake/llvm
ENV CC=/usr/local/llvm/bin/clang
ENV CXX=/usr/local/llvm/bin/clang++
ENV RY_VENDORED_GTEST_TARBALL=/opt/gtest/googletest-v${GTEST_VERSION}.tar.gz
ENV CARGO_HOME=/opt/cargo
ENV LLVM_SYS_211_PREFIX=/usr/local/llvm

WORKDIR /workspace
