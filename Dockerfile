# Multi-stage build for minimal production image
FROM rust:bookworm AS builder

# Install build dependencies
RUN apt-get update && apt-get install -y \
    build-essential \
    pkg-config \
    libssl-dev \
    libclang-dev \
    clang \
    && rm -rf /var/lib/apt/lists/*

# Set working directory
WORKDIR /build

# Copy workspace files
COPY Cargo.toml Cargo.lock ./
COPY src ./src
COPY crates ./crates
COPY playground ./playground
COPY benches ./benches

# Build release binary from workspace (ggen-cli package)
RUN cargo build --release --package ggen-cli-lib --bin ggen

# Production image - minimal
FROM debian:bookworm-slim

# Install runtime dependencies only
RUN apt-get update && apt-get install -y \
    ca-certificates \
    libssl3 \
    && rm -rf /var/lib/apt/lists/*

# Copy binary from builder
COPY --from=builder /build/target/release/ggen /usr/local/bin/ggen

# Set working directory for user projects
WORKDIR /workspace

# Verify installation
RUN ggen --version

# Default command
CMD ["ggen", "--help"]
