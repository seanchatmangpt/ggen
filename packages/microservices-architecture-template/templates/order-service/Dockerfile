# Order Service Dockerfile - Rust/Axum

# Stage 1: Build
FROM rust:1.75-alpine as builder

RUN apk add --no-cache musl-dev openssl-dev

WORKDIR /app

# Copy manifests
COPY Cargo.toml Cargo.lock ./

# Build dependencies (cached layer)
RUN mkdir src && \
    echo "fn main() {}" > src/main.rs && \
    cargo build --release && \
    rm -rf src

# Copy source code
COPY src ./src

# Build application
RUN cargo build --release

# Stage 2: Runtime
FROM alpine:3.19

RUN apk add --no-cache ca-certificates curl

WORKDIR /app

# Copy binary from builder
COPY --from=builder /app/target/release/order-service .

# Create non-root user
RUN addgroup -g 1000 appuser && \
    adduser -D -u 1000 -G appuser appuser && \
    chown -R appuser:appuser /app

USER appuser

EXPOSE 8003

HEALTHCHECK --interval=30s --timeout=5s --start-period=20s --retries=3 \
  CMD curl -f http://localhost:8003/health/live || exit 1

CMD ["./order-service"]
