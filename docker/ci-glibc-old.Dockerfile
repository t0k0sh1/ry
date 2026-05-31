# syntax=docker/dockerfile:1.7
#
# ry CI base image (older-glibc variant for release binaries).
#
# Built by .github/workflows/build-ci-image.yml and pushed to
# ghcr.io/<owner>/ry-ci-glibc-old:llvm-21-rev<N> (immutable) and :latest.
#
# Same as docker/ci.Dockerfile but based on `gcc:14-bookworm` (Debian
# 12, glibc 2.36) instead of `gcc:14-trixie` (Debian 13, glibc 2.41).
# Used by .github/workflows/release.yml so that the resulting release
# binary links against an older glibc and runs on a wider range of
# Linux distributions out in the wild.
#
# Multi-arch: linux/amd64 + linux/arm64. No `apt` or `apt-get`
# invocations anywhere — see ci.Dockerfile header for the reasoning.
# Rust is the sole non-github.com source (static.rust-lang.org), added
# for #1950 (Rust LLVM IR emission shared library).

FROM gcc:14-bookworm AS cmake-bootstrap

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

FROM gcc:14-bookworm AS ninja-bootstrap

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

# ccache only ships a prebuilt linux-x86_64 binary; aarch64 must be
# source-built. To keep both arches uniform we source-build on both.
# See docker/ci.Dockerfile for the rationale on the cmake flags.
FROM gcc:14-bookworm AS ccache-bootstrap

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

FROM gcc:14-bookworm AS openssl-builder

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

FROM gcc:14-bookworm AS llvm-builder

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
# Rust toolchain (standalone tarball from static.rust-lang.org). See
# docker/ci.Dockerfile rust-bootstrap stage for the rationale and the
# distribution-source justification (#1950).
#
FROM gcc:14-bookworm AS rust-bootstrap

ARG RUST_VERSION=1.83.0
ARG TARGETARCH

# Integrity verification via the published .sha256 companion file; see the
# rust-bootstrap stage in docker/ci.Dockerfile for the rationale.
WORKDIR /tmp
RUN set -eux; \
    case "${TARGETARCH}" in \
        amd64)  RARCH=x86_64-unknown-linux-gnu ;; \
        arm64)  RARCH=aarch64-unknown-linux-gnu ;; \
        *) echo "unsupported TARGETARCH: ${TARGETARCH}" >&2; exit 1 ;; \
    esac; \
    TARBALL="rust-${RUST_VERSION}-${RARCH}.tar.xz"; \
    wget -q "https://static.rust-lang.org/dist/${TARBALL}"; \
    wget -q "https://static.rust-lang.org/dist/${TARBALL}.sha256"; \
    sha256sum -c "${TARBALL}.sha256"; \
    tar -xJf "${TARBALL}"; \
    "rust-${RUST_VERSION}-${RARCH}/install.sh" \
        --prefix=/opt/rust \
        --components="rustc,cargo,rust-std-${RARCH}"; \
    rm -rf "rust-${RUST_VERSION}-${RARCH}" "${TARBALL}" "${TARBALL}.sha256"

FROM gcc:14-bookworm

ARG LLVM_VERSION=21.1.8
ARG GTEST_VERSION=1.17.0

LABEL org.opencontainers.image.source=https://github.com/t0k0sh1/ry
LABEL org.opencontainers.image.description="ry release CI image — gcc:14-bookworm (glibc 2.36) + LLVM ${LLVM_VERSION} (source-built) + OpenSSL 3 + cmake + ninja + ccache + Rust (static.rust-lang.org). Built entirely from github.com + rust-lang.org sources; no apt anywhere."
LABEL org.opencontainers.image.licenses=Apache-2.0

COPY --from=cmake-bootstrap /opt/cmake /opt/cmake
COPY --from=ninja-bootstrap /opt/ninja /opt/ninja
COPY --from=ccache-bootstrap /opt/ccache /usr/local/bin/ccache
COPY --from=openssl-builder /opt/openssl /opt/openssl
COPY --from=llvm-builder /usr/local/llvm /usr/local/llvm
COPY --from=rust-bootstrap /opt/rust /opt/rust

WORKDIR /opt/gtest
RUN set -eux; \
    wget -q "https://github.com/google/googletest/archive/refs/tags/v${GTEST_VERSION}.tar.gz" \
        -O "/opt/gtest/googletest-v${GTEST_VERSION}.tar.gz"

ENV PATH=/opt/cmake/bin:/opt/ninja/bin:/usr/local/llvm/bin:/opt/rust/bin:${PATH}
ENV LD_LIBRARY_PATH=/opt/openssl/lib64:/opt/openssl/lib:/usr/local/llvm/lib:/opt/rust/lib
ENV OPENSSL_ROOT_DIR=/opt/openssl
ENV LLVM_DIR=/usr/local/llvm/lib/cmake/llvm
ENV CC=/usr/local/llvm/bin/clang
ENV CXX=/usr/local/llvm/bin/clang++
ENV RY_VENDORED_GTEST_TARBALL=/opt/gtest/googletest-v${GTEST_VERSION}.tar.gz
ENV CARGO_HOME=/opt/cargo
ENV LLVM_SYS_211_PREFIX=/usr/local/llvm

WORKDIR /workspace
