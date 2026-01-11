# Multi-stage build for production
FROM rust:1.75 as builder

WORKDIR /usr/src/app

# Copy manifests
COPY Cargo.toml ./

# Build dependencies (cached layer)
RUN mkdir src && \
    echo "fn main() {}" > src/main.rs && \
    cargo build --release && \
    rm -rf src

# Copy source code
COPY . .

# Build application
RUN cargo build --release

# Runtime stage
FROM debian:bookworm-slim

RUN apt-get update && \
    apt-get install -y ca-certificates libssl3 libpq5 && \
    rm -rf /var/lib/apt/lists/*

WORKDIR /app

# Copy binary from builder
COPY --from=builder /usr/src/app/target/release/async-web-service /app/
COPY --from=builder /usr/src/app/migrations /app/migrations

EXPOSE 8080

CMD ["./async-web-service"]
