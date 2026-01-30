# Build stage: Rust toolchain installation
FROM ubuntu:24.04 as builder

# Install build dependencies and development tools
RUN apt-get update && apt-get install -y --no-install-recommends \
    curl \
    build-essential \
    git \
    pkg-config \
    libssl-dev \
    ca-certificates \
    && rm -rf /var/lib/apt/lists/*

# Install Rust toolchain using rustup
RUN curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- \
    -y \
    --default-toolchain stable \
    --profile minimal \
    && rm -rf /root/.cargo/registry/cache/* \
    && rm -rf /root/.cargo/registry/index/*

# Set Rust environment variables
ENV PATH="/root/.cargo/bin:${PATH}"

# Install ggen from crates.io (locked version for reproducibility)
RUN cargo install ggen --locked --quiet

# Production stage: Minimal runtime image
FROM ubuntu:24.04

# Install minimal runtime dependencies (no build tools)
RUN apt-get update && apt-get install -y --no-install-recommends \
    curl \
    git \
    ca-certificates \
    && rm -rf /var/lib/apt/lists/*

# Create non-root user for security (UID 1000)
RUN groupadd -r ggen && useradd -r -g ggen -u 1000 ggen

# Create workspace directory with proper permissions
RUN mkdir -p /workspace && chown -R ggen:ggen /workspace

# Copy Rust artifacts from builder stage
COPY --from=builder /root/.cargo/bin/ggen /usr/local/bin/ggen
COPY --from=builder /root/.cargo/bin/cargo /usr/local/bin/cargo
COPY --from=builder /root/.rustup /root/.rustup

# Set environment variables for MCP and agent execution
ENV GGEN_HOME=/workspace \
    MCP_PROXY_URL="" \
    TIMEOUT_DEFAULT=120 \
    PATH="/root/.cargo/bin:${PATH}" \
    RUST_LOG=warn \
    CARGO_HOME=/root/.cargo

# Set working directory
WORKDIR /workspace

# Switch to non-root user
USER ggen

# Health check: verify ggen is functional
HEALTHCHECK --interval=30s --timeout=10s --start-period=5s --retries=3 \
    CMD ggen --version || exit 1

# Default command: bash shell for interactive use
ENTRYPOINT ["/bin/bash"]
CMD ["-c", "echo 'ggen agent environment ready'; /bin/bash"]
