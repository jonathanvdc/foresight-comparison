FROM ubuntu:latest

ARG DEBIAN_FRONTEND=noninteractive

# Install base tools, Python, Rust, and Haskell Stack
RUN apt-get update && apt-get install -y --no-install-recommends \
    build-essential \
    curl \
    git \
    ca-certificates \
    python3 \
    python3-pip \
    pkg-config \
    libssl-dev \
    zlib1g-dev \
    libgmp-dev \
    libtinfo6 \
    haskell-stack \
 && rm -rf /var/lib/apt/lists/*

RUN python3 -m pip install --no-cache-dir psutil

ENV LANG=C.UTF-8
ENV LC_ALL=C.UTF-8

# Install OpenJDK and sbt (Scala build tool)
USER root
RUN apt-get update && apt-get install -y --no-install-recommends \
    gnupg \
    apt-transport-https \
    ca-certificates \
    curl \
 && mkdir -p /etc/apt/keyrings \
 && curl -fsSL "https://keyserver.ubuntu.com/pks/lookup?op=get&search=0x99E82A75642AC823" \
    | gpg --dearmor -o /etc/apt/keyrings/sbt-archive-keyring.gpg \
 && echo "deb [signed-by=/etc/apt/keyrings/sbt-archive-keyring.gpg] https://repo.scala-sbt.org/scalasbt/debian all main" > /etc/apt/sources.list.d/sbt.list \
 && echo "deb [signed-by=/etc/apt/keyrings/sbt-archive-keyring.gpg] https://scala.jfrog.io/artifactory/debian all main" > /etc/apt/sources.list.d/scala.list \
 && apt-get update && apt-get install -y --no-install-recommends \
    openjdk-21-jdk \
    sbt \
 && rm -rf /var/lib/apt/lists/*

# (Optional) JAVA_HOME is not strictly required for sbt, but some tools expect it.
# We set it dynamically at shell init time to support multiple architectures.
RUN echo 'export JAVA_HOME="$(dirname $(dirname $(readlink -f $(which javac))))"' > /etc/profile.d/java_home.sh \
 && chmod +x /etc/profile.d/java_home.sh

# Create a non-root user
ARG USERNAME=builder
ARG UID=1000
RUN set -eux; \
    if id -u "${USERNAME}" >/dev/null 2>&1; then \
      echo "User ${USERNAME} already exists"; \
    else \
      if getent passwd "${UID}" >/dev/null; then \
        echo "UID ${UID} in use; creating ${USERNAME} with next available UID"; \
        useradd -m -U -s /bin/bash "${USERNAME}"; \
      else \
        useradd -m -u "${UID}" -U -s /bin/bash "${USERNAME}"; \
      fi; \
    fi

USER ${USERNAME}
WORKDIR /workspace

# Install Rust via rustup (ensures latest stable with edition2024 support)
RUN curl -sSf https://sh.rustup.rs | sh -s -- -y --profile minimal \
 && ~/.cargo/bin/rustup update stable \
 && ~/.cargo/bin/rustup default stable \
 && ~/.cargo/bin/rustc --version \
 && ~/.cargo/bin/cargo --version

# Clone and install egglog and egglog-experimental
RUN git clone https://github.com/egraphs-good/egglog-experimental.git /tmp/egglog-experimental \
 && cd /tmp/egglog-experimental \
 # && git checkout 24e3e83016301937f285b2f2ff7889007c5c09b4 \
 && cd / \
 && git clone https://github.com/egraphs-good/egglog.git /tmp/egglog \
 && cd /tmp/egglog \
 # && git checkout b066a521e4710bd74034bfa71a435c26f8ac821f \
 && cd /tmp/egglog-experimental && ~/.cargo/bin/cargo install --path=. \
 && cd /tmp/egglog && ~/.cargo/bin/cargo install --path=. \
 && rm -rf /tmp/egglog /tmp/egglog-experimental

# Install GHC via ghcup and use it with Stack (to avoid stack downloading/compiling GHC)
ENV GHCUP_INSTALL_BASE_PREFIX="/home/${USERNAME}"
RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | BOOTSTRAP_HASKELL_NONINTERACTIVE=1 BOOTSTRAP_HASKELL_MINIMAL=1 sh \
 && /home/${USERNAME}/.ghcup/bin/ghcup --version \
 # Install a GHC for hegg-bench
 && /home/${USERNAME}/.ghcup/bin/ghcup install ghc 9.6.6 \
 && /home/${USERNAME}/.ghcup/bin/ghcup set ghc 9.6.6
# Put ghcup and GHC on PATH
ENV PATH="/home/${USERNAME}/.ghcup/bin:/home/${USERNAME}/.cabal/bin:${PATH}"

# Put cargo on PATH
ENV PATH="/home/${USERNAME}/.cargo/bin:${PATH}"

# Set Stack’s working directory
ENV STACK_ROOT="/home/${USERNAME}/.stack"

ENV BENCH_SECONDS=60
# Space-separated list of thread counts to pass to run_benchmarks.py (e.g., "1 2 4 8")
ENV FORESIGHT_THREAD_COUNTS="1"

# Copy the repo into the container (optional; can also mount instead)
COPY --chown=${USERNAME}:${USERNAME} . /workspace

# (Optional) Pre-build benchmark projects
RUN cd slotted && cargo build --release
RUN cd egg && cargo build --release
RUN cd hegg-bench && stack --system-ghc --no-install-ghc build

# (Optional) Warm sbt caches for Foresight benchmarks if the project exists
RUN if [ -d "/workspace/foresight" ]; then bash -lc 'cd /workspace/foresight && sbt -v about || true'; fi

# Default command: run benchmarks with --seconds from env variable, redirecting all output to stderr
CMD ["/bin/bash", "-lc", "python3 -u run_benchmarks.py --seconds \"${BENCH_SECONDS}\" --foresight-thread-counts ${FORESIGHT_THREAD_COUNTS} 1>&2 && cat results.csv"]