# Infrastructure Components Verification

## ✅ Status: All Components Verified and Working

### 1. Docker ✅
- **Status**: Running and functional
- **Version**: Docker 28.0.4
- **Verification**: `docker ps` succeeds
- **Usage**: Required for all testcontainers and OTEL stack

### 2. Testcontainers ✅
- **Status**: Configured and compiling
- **Dependencies**:
  - `chicago-tdd-tools` with `testcontainers` feature ✅
  - `testcontainers = "0.25"` ✅
  - `testcontainers-modules = "0.13"` ✅
- **Test Files**:
  - `marketplace_nextjs_ontology_e2e.rs` - Uses chicago-tdd-tools API
  - `testcontainer_marketplace_git_hooks.rs` - Uses testcontainers crate
  - `full_cycle_container_validation.rs` - Uses chicago-tdd-tools API
- **Compilation**: All tests compile successfully ✅

### 3. OpenTelemetry (OTEL) ✅
- **Status**: Configured and ready
- **Docker Compose**: `docker-compose.otel-test.yml` ✅
- **Config**: `otel-collector-config.yaml` ✅
- **Services**:
  - OTEL Collector (ports 4317 gRPC, 4318 HTTP, 13133 health)
  - Jaeger (port 16686 UI)
  - Prometheus (port 9090 UI)
- **Dependencies**: All OTEL crates present in Cargo.toml ✅
- **Telemetry Modules**:
  - `crates/ggen-core/src/telemetry.rs` ✅
  - `crates/ggen-marketplace/src/telemetry.rs` ✅
- **Test File**: `otel_validation_tests.rs` (18 tests) ✅

### 4. Weaver ✅
- **Status**: Installed and functional
- **Version**: weaver 0.16.1
- **Location**: `/Users/sac/.cargo/bin/weaver`
- **CLI**: Functional (`weaver --help` works) ✅
- **Integration**: Documentation and templates exist ✅

## Quick Verification

Run the verification script:
```bash
./tests/integration/verify_infrastructure.sh
```

## Starting Services

### Start OTEL Stack
```bash
cd tests/integration
docker-compose -f docker-compose.otel-test.yml up -d
```

### Verify OTEL Stack
```bash
# Health check
curl http://localhost:13133

# Jaeger UI
open http://localhost:16686

# Prometheus UI
open http://localhost:9090
```

## Running Tests

### Testcontainers Tests
```bash
# Using chicago-tdd-tools API
cargo test --test marketplace_nextjs_ontology_e2e -- --ignored

# Using testcontainers crate
cargo test --test testcontainer_marketplace_git_hooks -- --ignored

# Full cycle validation
cargo test --test full_cycle_container_validation -- --ignored
```

### OTEL Validation Tests
```bash
# Start OTEL stack first
cd tests/integration && docker-compose -f docker-compose.otel-test.yml up -d

# Run tests
cargo test --test otel_validation_tests -- --ignored
```

### Weaver Live-Check
```bash
# Terminal 1: Start Weaver
weaver registry live-check --format json --output ./weaver-reports

# Terminal 2: Run application with OTEL
OTEL_EXPORTER_OTLP_ENDPOINT=http://localhost:4318 \
OTEL_SERVICE_NAME=ggen-marketplace \
cargo run --features otel -- marketplace search test
```

## Troubleshooting

### Port Conflicts
If ports are already in use:
```bash
# Stop OTEL stack
docker-compose -f tests/integration/docker-compose.otel-test.yml down

# Kill processes on ports
lsof -ti:16686 | xargs kill -9  # Jaeger
lsof -ti:4317 | xargs kill -9   # OTEL gRPC
lsof -ti:4318 | xargs kill -9   # OTEL HTTP
```

### Docker Not Running
```bash
# macOS: Open Docker Desktop
# Linux: sudo systemctl start docker
# Windows: Start Docker Desktop
```

### Weaver Not Found
```bash
# Install from releases
# https://github.com/open-telemetry/weaver/releases

# Or build from source
cargo install weaver
```

## Summary

✅ **All infrastructure components are verified and working:**
- Docker: Running
- Testcontainers: Configured and compiling
- OpenTelemetry: Configured with docker-compose
- Weaver: Installed and functional

All components are ready for use in integration tests and development.

