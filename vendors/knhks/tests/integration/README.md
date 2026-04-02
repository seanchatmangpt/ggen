# Integration Tests with Testcontainers

This directory contains integration tests using Testcontainers for real containerized services.

## Structure

- **Rust Tests** (`../rust/knhk-integration-tests/`): Rust integration tests using the `testcontainers` crate
- **Docker Compose** (`docker-compose.yml`): Orchestrates test containers
- **C Integration Tests**: C tests that verify connectivity to containerized services

## Prerequisites

- Docker and Docker Compose installed
- For Rust tests: Rust toolchain with `cargo`

## Running Tests

### Rust Integration Tests (with testcontainers crate)

```bash
cd rust/knhk-integration-tests
cargo test
```

### Docker Compose Integration Tests

```bash
# From project root
make test-integration-docker

# Or manually
cd tests/integration
docker-compose up -d
./docker_test.sh
docker-compose down
```

## Test Containers

- **Kafka**: Confluent Kafka for connector testing
- **PostgreSQL**: Database for lockchain storage
- **OTEL Collector**: OpenTelemetry collector for observability
- **Redis**: Cache layer (optional)

## Test Flow

1. **Start containers**: Docker Compose starts all required services
2. **Health checks**: Verify services are ready
3. **Run tests**: Execute integration tests against real services
4. **Cleanup**: Stop and remove containers

## Extending Tests

### Adding a new Rust test:

```rust
#[tokio::test]
async fn test_new_feature() -> Result<()> {
    let docker = Cli::default();
    let container = docker.run(YourImage::default());
    // Test logic
    Ok(())
}
```

### Adding a new C test:

1. Create `test_*.c` in `tests/integration/`
2. Add build rule to `Makefile`
3. Add test execution to `docker_test.sh`

## Notes

- Containers are ephemeral - data is not persisted between test runs
- Tests run in isolated Docker network
- All tests use real containerized services (no mocks)

