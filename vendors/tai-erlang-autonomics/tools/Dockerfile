# TAIEA Cloud Run Deployment Container
#
# Multi-stage build:
# 1. Builder: Compiles Erlang release
# 2. Runtime: Minimal base image with runtime dependencies
#
# Usage:
#   docker build -f tools/Dockerfile -t taiea:1.0.0 .
#   docker run -p 8080:8080 -e TAIEA_ENV=prod taiea:1.0.0

##############################################################################
# Stage 1: Builder
##############################################################################

FROM debian:bookworm-slim as builder

# Install Erlang build dependencies
RUN apt-get update && apt-get install -y --no-install-recommends \
    build-essential \
    ca-certificates \
    curl \
    git \
    libssl-dev \
    wget \
    && rm -rf /var/lib/apt/lists/*

# Install Erlang/OTP 26
RUN apt-get update && apt-get install -y --no-install-recommends \
    erlang-base \
    erlang-dev \
    erlang-common-test \
    erlang-crypto \
    erlang-inets \
    erlang-mnesia \
    erlang-public-key \
    erlang-ssl \
    erlang-syntax-tools \
    erlang-tools \
    && rm -rf /var/lib/apt/lists/*

# Install Rebar3
RUN curl -fsSL https://github.com/erlang/rebar3/releases/download/3.23.1/rebar3 \
    -o /usr/local/bin/rebar3 && \
    chmod +x /usr/local/bin/rebar3

WORKDIR /build

# Copy source code
COPY . .

# Build release
RUN rebar3 release

##############################################################################
# Stage 2: Runtime
##############################################################################

FROM debian:bookworm-slim

# Install runtime dependencies
RUN apt-get update && apt-get install -y --no-install-recommends \
    ca-certificates \
    curl \
    libssl3 \
    && rm -rf /var/lib/apt/lists/*

# Install Erlang runtime only
RUN apt-get update && apt-get install -y --no-install-recommends \
    erlang-base \
    erlang-crypto \
    erlang-inets \
    erlang-mnesia \
    erlang-public-key \
    erlang-ssl \
    && rm -rf /var/lib/apt/lists/*

# Create non-root user
RUN useradd -m -u 1000 taiea

# Copy release from builder
COPY --from=builder --chown=taiea:taiea \
    /build/_build/default/rel/taiea \
    /app

WORKDIR /app

# Set ownership
RUN chown -R taiea:taiea /app

# Switch to non-root user
USER taiea

# Health check
HEALTHCHECK --interval=30s --timeout=10s --start-period=5s --retries=3 \
    CMD curl -f http://localhost:${PORT:-8080}/health || exit 1

# Environment variables
ENV PORT=8080 \
    TAIEA_ENV=prod \
    RUNNER_LOG_FORMAT=json \
    ERLANG_COOKIE=taiea-secret

# Expose port
EXPOSE 8080

# Start application
ENTRYPOINT ["/app/bin/taiea"]
CMD ["foreground"]
