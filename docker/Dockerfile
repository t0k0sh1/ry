# syntax=docker/dockerfile:1.7
#
# ry Linux dev environment.
#
# Inherits from ghcr.io/<owner>/ry-ci:llvm-21 (built by
# .github/workflows/build-ci-image.yml). The CI image already provides:
#   - clang/clang++ 21 (source-built, /usr/local/llvm)
#   - cmake (/opt/cmake), ninja (/opt/ninja), ccache (/usr/local/bin/ccache)
#   - OpenSSL 3 (/opt/openssl), cppcheck (/opt/cppcheck)
#   - vendored gtest source tarball at $RY_VENDORED_GTEST_TARBALL
#   - CC/CXX/PATH/LD_LIBRARY_PATH/OPENSSL_ROOT_DIR/LLVM_DIR pre-set
#
# This Dockerfile only adds the dev-specific bits: a non-root user
# matching the host UID for shared volume safety, a writable ccache
# dir under that user, and the entrypoint script.

ARG CI_IMAGE_OWNER=t0k0sh1
ARG CI_IMAGE_TAG=llvm-21
FROM ghcr.io/${CI_IMAGE_OWNER}/ry-ci:${CI_IMAGE_TAG}

# Create the ubuntu user matching the historical local-dev image so
# `docker/run.sh` continues to work without changes. UID 1000 is the
# default uid on Apple Silicon hosts; if the host UID differs the
# user can override with `docker run --user $(id -u):$(id -g)`.
ARG DEV_USER=ubuntu
ARG DEV_UID=1000
ARG DEV_GID=1000
RUN set -eux; \
    if ! getent group ${DEV_GID} >/dev/null; then \
        groupadd --gid ${DEV_GID} ${DEV_USER}; \
    fi; \
    if ! id -u ${DEV_USER} >/dev/null 2>&1; then \
        useradd --uid ${DEV_UID} --gid ${DEV_GID} --create-home --shell /bin/bash ${DEV_USER}; \
    fi; \
    mkdir -p /home/${DEV_USER}/.cache/ccache /home/${DEV_USER}/.cargo; \
    chown -R ${DEV_USER}:${DEV_USER} /home/${DEV_USER}

ENV HOME=/home/${DEV_USER}
ENV CCACHE_DIR=/home/${DEV_USER}/.cache/ccache
# Override the base image's CARGO_HOME=/opt/cargo (root-owned, and absent in this
# dev image) with a ${DEV_USER}-writable path. Since the #1993 cutover, every
# preset builds the emit Rust cdylib via corrosion, so the non-root dev
# user needs a writable cargo registry/cache. run.sh mounts a named volume here
# for cross-run persistence.
ENV CARGO_HOME=/home/${DEV_USER}/.cargo

WORKDIR /workspace
# WORKDIR creates /workspace as root:root 0755 before USER drops to ubuntu.
# Issue #1876 switched run.sh to per-entry bind mounts, so /workspace is no
# longer overlaid by a project-root mount — without this chown,
# test_spec_timeout creates /workspace/build/test_spec_timeout_* under asan /
# fuzz presets (where /workspace/build is not mounted) and EACCES.
RUN chown ${DEV_UID}:${DEV_GID} /workspace

COPY entrypoint.sh /usr/local/bin/entrypoint.sh
RUN chmod +x /usr/local/bin/entrypoint.sh

USER ${DEV_USER}
ENTRYPOINT ["/usr/local/bin/entrypoint.sh"]
