# Build stage
FROM rust:latest as builder

WORKDIR /build

# Copy workspace
COPY . .

# Build the example
RUN cargo build --release --example tps-reference-system

# Runtime stage
FROM debian:bookworm-slim

RUN apt-get update && apt-get install -y \
    curl \
    ca-certificates \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /app

# Copy binary from builder
COPY --from=builder /build/target/release/tps-system /app/tps-system

# Health check
HEALTHCHECK --interval=30s --timeout=10s --start-period=20s --retries=3 \
    CMD curl -f http://localhost:8080/health || exit 1

# Expose ports
EXPOSE 8080 9090

# Run
CMD ["/app/tps-system"]
