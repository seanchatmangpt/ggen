# CLI Testing with clnrm - Complete Guide

## Problem Statement

Traditional Rust tests pass but CLI commands don't work. This is a critical gap:
- ✅ Tests verify code compiles
- ✅ Tests verify functions exist
- ❌ Tests don't verify CLI commands actually work
- ❌ Tests don't verify files are actually created

## Solution: clnrm Hermetic Testing

[clnrm](https://github.com/seanchatmangpt/clnrm) provides hermetic integration testing with Docker containers to verify CLI commands work end-to-end.

### Key Benefits

1. **Hermetic Isolation**: Each test runs in isolated Docker containers
2. **File System Verification**: Tests verify files are actually created
3. **Deterministic Execution**: Fixed seeds prevent flakiness
4. **7-Layer Validation**: Prevents false positives ("fake greens")

## Quick Start

### Prerequisites

1. **Docker must be running**:
```bash
docker ps
```

2. **Build clnrm v2.0.0** (if not already built):
```bash
cd /tmp/clnrm
cargo build --release --features otel
```

### Run Tests

```bash
# Using cargo make (recommended)
cargo make test-clnrm

# Direct clnrm command
/tmp/clnrm/target/release/clnrm run tests/clnrm/cli_commands.clnrm.toml
```

### Validate Test File

```bash
/tmp/clnrm/target/release/clnrm validate tests/clnrm/cli_commands.clnrm.toml
```

## Test File Format (clnrm v2.0.0)

```toml
[test]
name = "test_name"
timeout = "120s"

[containers.app]
image = "rust:1.90-slim"
env = { VAR = "value" }
volumes = [{ host = "/path/to/host", container = "/workspace", readonly = true }]
workdir = "/workspace"

[[steps]]
name = "step_name"
container = "app"
exec = ["sh", "-c", "command"]
assert.exit_code = 0
assert.stdout_contains = "expected"
```

## Current Test Coverage

The `cli_commands.clnrm.toml` test file verifies:

1. **Build ggen** - Ensures binary compiles
2. **Version check** - Verifies CLI is functional
3. **CI Workflow** - Tests `ggen ci workflow` creates `.github/workflows/*.yml`
4. **Workflow Init** - Tests `ggen workflow init` creates `.workflows/*.json`
5. **Paper New** - Tests `ggen paper new` creates paper RDF files

## Adding New Tests

To add a new CLI command test:

```toml
[[steps]]
name = "test_new_command"
container = "ggen"
exec = ["sh", "-c", "/workspace/target/release/ggen new-command --arg value"]
assert.exit_code = 0
assert.stdout_contains = "expected_output"

[[steps]]
name = "verify_new_command_creates_file"
container = "ggen"
exec = ["sh", "-c", "test -f /tmp/expected-file && echo 'SUCCESS' || echo 'FAILED'"]
assert.exit_code = 0
assert.stdout_contains = "SUCCESS"
```

## Troubleshooting

### Volume Mount Issues

If tests fail with "No such file or directory":
- Ensure Docker has access to workspace directory
- Use absolute paths for volume mounts
- Check permissions: `ls -la /Users/sac/ggen`

### Build Failures

If build step fails:
- Check Rust image is available: `docker pull rust:1.90-slim`
- Verify workspace is mounted: `docker run --rm -v /Users/sac/ggen:/workspace rust:1.90-slim ls /workspace`

### Timeout Issues

Increase timeout in test file:
```toml
[test]
timeout = "300s"  # Increase from 120s
```

### Container Not Starting

Check Docker is running:
```bash
docker ps
docker info
```

## Integration with CI/CD

Add to `.github/workflows/ci.yml`:

```yaml
- name: Run clnrm CLI tests
  run: |
    cd /tmp && git clone https://github.com/seanchatmangpt/clnrm.git
    cd clnrm && cargo build --release --features otel
    cd ${{ github.workspace }}
    /tmp/clnrm/target/release/clnrm run tests/clnrm/cli_commands.clnrm.toml
```

## Comparison with Rust Tests

| Feature | Rust Tests | clnrm Tests |
|---------|-----------|-------------|
| **File Creation** | ❌ Can't verify | ✅ Verifies files exist |
| **Isolation** | ⚠️ Shared state | ✅ Isolated containers |
| **Determinism** | ⚠️ Flaky timing | ✅ Fixed seeds |
| **CLI Testing** | ⚠️ Mocked | ✅ Real CLI execution |
| **False Positives** | ⚠️ Common | ✅ 7-layer validation |

## Next Steps

1. ✅ Basic CLI command tests
2. ⏳ Add more commands (template generate, graph query, etc.)
3. ⏳ Add performance benchmarks
4. ⏳ Add OpenTelemetry validation
5. ⏳ Integrate with CI/CD pipeline

## References

- [clnrm README](https://github.com/seanchatmangpt/clnrm/blob/master/README.md)
- [clnrm v2.0.0 Config Reference](../../../clnrm/docs/V2_0_0_CONFIG_REFERENCE.md)
- [clnrm Migration Guide](../../../clnrm/docs/V2_0_0_MIGRATION_GUIDE.md)

