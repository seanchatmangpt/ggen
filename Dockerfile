# Multi-stage build for minimal production image
FROM rust:slim AS builder

# Install build dependencies
RUN apt-get update && apt-get install -y \
    build-essential \
    pkg-config \
    libssl-dev \
    && rm -rf /var/lib/apt/lists/*

# Set working directory
WORKDIR /build

# Copy workspace files
COPY Cargo.toml Cargo.lock ./
COPY crates ./crates

# Build release binary
RUN cargo build --release --bin ggen --manifest-path crates/ggen-cli/Cargo.toml

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
