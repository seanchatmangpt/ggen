# Quick Start Guide: Testcontainers Integration

## Prerequisites

1. **Docker**: Install Docker Desktop or Docker Engine
2. **Rust**: Install Rust toolchain (`rustup`)
3. **Make**: Standard build tool

## Running Tests

### 1. Rust Integration Tests (with testcontainers crate)

```bash
cd rust/knhk-integration-tests
cargo test
```

These tests automatically:
- Start Docker containers (Kafka, PostgreSQL)
- Run tests against real services
- Clean up containers after tests

### 2. Docker Compose Integration Tests

```bash
# From project root
make test-integration-docker
```

Or manually:

```bash
cd tests/integration
docker-compose up -d  # or 'docker compose up -d'
sleep 10  # Wait for services
make -C ../.. test-kafka-integration test-lockchain-integration test-etl-integration
docker-compose down  # or 'docker compose down'
```

### 3. Individual Test Targets

```bash
# Build integration test binaries
make test-kafka-integration test-lockchain-integration test-etl-integration

# Run manually (requires Docker containers running)
./tests/integration/test_kafka_integration
./tests/integration/test_lockchain_integration
./tests/integration/test_etl_integration
```

## Troubleshooting

### Docker not found
```bash
# Install Docker Desktop (macOS/Windows) or Docker Engine (Linux)
# Verify installation:
docker --version
docker-compose --version  # or 'docker compose version'
```

### Port conflicts
If ports 9092, 5432, 4317, 6379 are already in use:
- Stop conflicting services
- Or modify `docker-compose.yml` to use different ports

### Container startup issues
```bash
# Check container logs
cd tests/integration
docker-compose logs

# Restart containers
docker-compose restart
```

### Rust compilation errors
```bash
# Clean and rebuild
cd rust/knhk-integration-tests
cargo clean
cargo build

# Check dependencies
cargo tree
```

## Test Structure

- **Rust tests**: Use `testcontainers` crate for automatic container management
- **C tests**: Use Docker Compose for manual orchestration
- **Both**: Test against real services (no mocks)

## Next Steps

1. Extend tests with real data ingestion
2. Add more container types (Redis, etc.)
3. Implement actual connector logic in tests
4. Add performance benchmarks

