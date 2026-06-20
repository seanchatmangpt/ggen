# Build stage
FROM rust:1.75-slim AS builder
WORKDIR /build
# Cache dependencies layer
COPY Cargo.toml Cargo.lock ./
COPY src/Cargo.toml src/
COPY crates/core/Cargo.toml crates/core/
COPY crates/domain/Cargo.toml crates/domain/
COPY crates/service/Cargo.toml crates/service/
COPY crates/config/Cargo.toml crates/config/
COPY crates/sqlite/Cargo.toml crates/sqlite/
COPY crates/mcp-server/Cargo.toml crates/mcp-server/
# Build with actual source
COPY . .
RUN cargo build --release --bin cargo-project

# Runtime stage — minimal distroless image
FROM debian:bookworm-slim AS runtime
RUN apt-get update && apt-get install -y --no-install-recommends libssl3 ca-certificates && rm -rf /var/lib/apt/lists/*
COPY --from=builder /build/target/release/cargo-project /usr/local/bin/cargo-project
ENV BIND_ADDR=0.0.0.0:8080
EXPOSE 8080
ENTRYPOINT ["/usr/local/bin/cargo-project"]
CMD ["serve"]
