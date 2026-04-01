# Weaver Registry Live-Check Dockerfile
#
# Builds official weaver SDK CLI for semantic convention validation.
# Uses `registry live-check` subcommand to validate OTLP spans against semconv.
#
# CLI: weaver registry live-check -r /semconv/model --otlp-grpc-port 4316
#
# See: docs/weaver-registry-integration-plan.md
# See: https://github.com/credolabs/weaver-rdk

# Stage 1: Builder (compiles weaver SDK from source)
FROM rust:1.81-slim-bookworm AS builder

# Install build dependencies
RUN apt-get update && apt-get install -y \
    pkg-config \
    libssl-dev \
    ca-certificates \
    curl \
    && rm -rf /var/lib/apt/lists/*

# Set working directory
WORKDIR /build

# Download and build official weaver SDK v0.22.0
RUN curl -L https://github.com/credolabs/weaver-rdk/archive/refs/tags/v0.22.0.tar.gz | tar xz && \
    mv weaver-rdk-0.22.0 weaver-rdk

WORKDIR /build/weaver-rdk

# Build weaver CLI with all features
RUN cargo build --release --bin weaver

# Stage 2: Runtime (minimal image for production)
FROM debian:bookworm-slim

# Install runtime dependencies (curl for health checks)
RUN apt-get update && apt-get install -y \
    ca-certificates \
    libssl3 \
    curl \
    && rm -rf /var/lib/apt/lists/*

# Create non-root user for security
RUN useradd -r -s /bin/false weaver

# Copy weaver CLI binary from builder
COPY --from=builder /build/weaver-rdk/target/release/weaver /usr/local/bin/weaver

# Create directories for semconv registry and reports
RUN mkdir -p /semconv/model /semconv/policies /reports && \
    chown -R weaver:weaver /semconv /reports

# Switch to non-root user
USER weaver

# Expose OTLP receiver port (4316) and admin port (4320)
EXPOSE 4316 4320

# Health check (admin port liveness)
HEALTHCHECK --interval=15s --timeout=5s --start-period=30s --retries=5 \
    CMD curl -sf http://localhost:4320/ || exit 1

# Set environment variables
ENV RUST_LOG=info

# Run weaver registry live-check (validates incoming OTLP spans)
ENTRYPOINT ["/usr/local/bin/weaver"]
CMD ["registry", "live-check", \
     "-r", "/semconv/model", \
     "--skip-policies", \
     "--input-source", "otlp", \
     "--otlp-grpc-port", "4316", \
     "--admin-port", "4320", \
     "--report-path", "/reports", \
     "--inactivity-timeout", "86400"]
