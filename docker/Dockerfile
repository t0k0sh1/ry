FROM ubuntu:24.04

ENV DEBIAN_FRONTEND=noninteractive

# The ubuntu:24.04 archive signing key may be expired in environments where the system clock is
# set to a future date. Replace Signed-By with Trusted:yes to bypass GPG verification.
# This is intentional for a dev-only image; do not use in production.
# zlib1g-dev: required by LLVMSupport link interface (ZLIB::ZLIB) when using apt.llvm.org packages
# libzstd-dev: required by LLVMSupport link interface (zstd::libzstd_shared)
RUN sed -i 's|^Signed-By:.*|Trusted: yes|' /etc/apt/sources.list.d/ubuntu.sources \
  && apt-get update \
  && apt-get install -y \
    cmake \
    ninja-build \
    ccache \
    libssl-dev \
    libgtest-dev \
    libgmock-dev \
    zlib1g-dev \
    libzstd-dev \
    wget \
    gnupg \
    ca-certificates \
    software-properties-common \
    git \
  && rm -rf /var/lib/apt/lists/*

# Install LLVM 21 via apt.llvm.org (mirrors CI apt fallback path in setup-llvm action)
RUN wget -qO- https://apt.llvm.org/llvm.sh | bash -s -- 21 \
  && apt-get install -y \
    libllvm21 \
    llvm-21-dev \
    clang-21 \
  && rm -rf /var/lib/apt/lists/* \
  && ln -sfn /usr/lib/llvm-21 /usr/local/llvm

ENV CC=/usr/local/llvm/bin/clang \
    CXX=/usr/local/llvm/bin/clang++ \
    PATH=/usr/local/llvm/bin:$PATH

WORKDIR /workspace

COPY entrypoint.sh /usr/local/bin/entrypoint.sh
RUN chmod +x /usr/local/bin/entrypoint.sh

ENTRYPOINT ["/usr/local/bin/entrypoint.sh"]
