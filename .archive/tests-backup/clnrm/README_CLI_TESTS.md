# CLI Command Tests with clnrm

This directory contains hermetic integration tests using [clnrm](https://github.com/seanchatmangpt/clnrm) to verify that ggen CLI commands actually work end-to-end.

## Problem Statement

Traditional Rust tests pass but CLI commands don't work. This is a critical gap - tests verify code compiles and functions exist, but don't verify the actual user experience.

## Solution: clnrm Hermetic Testing

clnrm provides:
- **Hermetic Isolation**: Each test runs in isolated Docker containers
- **File System Verification**: Tests verify files are actually created
- **Deterministic Execution**: Fixed seeds and timestamps prevent flakiness
- **7-Layer Validation**: Prevents false positives ("fake greens")

## Test File: `cli_commands.clnrm.toml`

This test file verifies critical CLI commands that create files:

1. **Build ggen** - Ensures binary compiles
2. **Version check** - Verifies CLI is functional
3. **CI Workflow** - Tests `ggen ci workflow` creates `.github/workflows/*.yml`
4. **Workflow Init** - Tests `ggen workflow init` creates `.workflows/*.json`
5. **Paper New** - Tests `ggen paper new` creates paper RDF files

## Running Tests

### Prerequisites

1. **Build clnrm**:
```bash
cd /tmp/clnrm
cargo build --release --features otel
```

2. **Ensure Docker is running**:
```bash
docker ps
```

### Run Tests

```bash
# Using cargo make (recommended)
cargo make test-clnrm

# Direct clnrm command
clnrm run tests/clnrm/cli_commands.clnrm.toml --workspace /Users/sac/ggen
```

### Validate Test File

```bash
clnrm validate tests/clnrm/cli_commands.clnrm.toml
```

## Test Format (clnrm v2.0.0)

```toml
[test]
name = "test_name"
timeout = "120s"

[containers.app]
image = "rust:1.90-slim"
env = { VAR = "value" }

[[steps]]
name = "step_name"
container = "app"
exec = ["sh", "-c", "command"]
assert.exit_code = 0
assert.stdout_contains = "expected"
```

## Key Differences from Rust Tests

| Feature | Rust Tests | clnrm Tests |
|---------|-----------|-------------|
| **File Creation** | ❌ Can't verify | ✅ Verifies files exist |
| **Isolation** | ⚠️ Shared state | ✅ Isolated containers |
| **Determinism** | ⚠️ Flaky timing | ✅ Fixed seeds |
| **CLI Testing** | ⚠️ Mocked | ✅ Real CLI execution |

## Integration with CI/CD

Add to `.github/workflows/ci.yml`:

```yaml
- name: Run clnrm CLI tests
  run: cargo make test-clnrm
```

## Troubleshooting

### Volume Mount Issues

If tests fail with volume mount errors, ensure:
- Docker has access to workspace directory
- Paths are absolute (not relative)
- Permissions are correct

### Timeout Issues

Increase timeout in test file:
```toml
[test]
timeout = "300s"  # Increase from 120s
```

### Container Build Failures

Ensure Rust image is available:
```bash
docker pull rust:1.90-slim
```

## Next Steps

1. Add more CLI commands to test suite
2. Add performance benchmarks
3. Integrate with CI/CD pipeline
4. Add OpenTelemetry validation

