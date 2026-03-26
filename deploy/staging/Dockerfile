# Multi-stage Docker build for ggen v6.0.0 (production-grade staging)
# Builds Linux ARM64 binary inside Docker
# Build context: project root (run from project root: docker build -f deploy/staging/Dockerfile .)

FROM --platform=linux/arm64 rust:1.91-slim AS builder

RUN apt-get update && apt-get install -y \
    pkg-config \
    libssl-dev \
    ca-certificates \
    git \
    bash \
    perl \
    build-essential \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /build

# Copy workspace files
COPY Cargo.toml Cargo.lock* ./
COPY crates ./crates
COPY benches ./benches

# Build release binary (single job to reduce memory usage)
RUN CARGO_BUILD_JOBS=1 cargo build --release \
    --package ggen-cli-lib \
    2>&1 | tee /tmp/build.log && \
    cp /build/target/release/ggen /usr/local/bin/ggen && \
    chmod +x /usr/local/bin/ggen && \
    file /usr/local/bin/ggen

# Runtime stage
FROM --platform=linux/arm64 debian:bookworm-slim

RUN apt-get update && apt-get install -y \
    ca-certificates \
    curl \
    jq \
    bash \
    libssl3 \
    openssl \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /app

COPY --from=builder /usr/local/bin/ggen /usr/local/bin/ggen

RUN groupadd -g 1000 ggen && \
    useradd -u 1000 -g ggen -s /sbin/nologin -d /app ggen && \
    mkdir -p /app/specs /app/generated && \
    chown -R ggen:ggen /app

USER ggen

HEALTHCHECK --interval=30s --timeout=5s --start-period=10s --retries=3 \
    CMD curl -f http://localhost:8080/health || exit 1

EXPOSE 8080

CMD ["ggen", "health-dashboard", "--port", "8080", "--bind", "0.0.0.0"]
