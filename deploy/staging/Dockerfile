# Multi-stage Rust build for ggen v6.0.0 (production-grade staging)
# Stage 1: Builder
FROM rust:1.80-alpine3.19 AS builder

RUN apk add --no-cache \
    musl-dev \
    openssl-dev \
    ca-certificates \
    git \
    bash

WORKDIR /build

# Copy workspace files
COPY Cargo.toml Cargo.lock* ./
COPY crates ./crates

# Build release binary with optimizations
RUN --mount=type=cache,target=/usr/local/cargo/registry \
    --mount=type=cache,target=/build/target \
    cargo build --release \
    --package ggen-cli \
    2>&1 | tee /tmp/build.log && \
    cp /build/target/release/ggen /usr/local/bin/ggen && \
    chmod +x /usr/local/bin/ggen

# Stage 2: Runtime (minimal, production-ready)
FROM alpine:3.19

RUN apk add --no-cache \
    ca-certificates \
    curl \
    jq \
    bash \
    libc6-compat \
    libssl3 \
    openssl

WORKDIR /app

# Copy only the compiled binary from builder
COPY --from=builder /usr/local/bin/ggen /usr/local/bin/ggen

# Create non-root user for security
RUN addgroup -g 1000 ggen && \
    adduser -u 1000 -G ggen -s /sbin/nologin -h /app ggen

RUN mkdir -p /app/specs /app/generated && \
    chown -R ggen:ggen /app

USER ggen

# Health check endpoint (uses ggen validate command via curl)
HEALTHCHECK --interval=30s --timeout=5s --start-period=10s --retries=3 \
    CMD curl -f http://localhost:8080/health || exit 1

# Expose port for metrics/health (ggen-api)
EXPOSE 8080

# Default command: run health dashboard
CMD ["ggen", "health-dashboard", "--port", "8080", "--bind", "0.0.0.0"]
