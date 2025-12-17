# Quickstart: Running E2E Tests

**Feature**: 011-e2e-testcontainers | **Date**: 2025-12-16

Run end-to-end tests to verify `ggen sync` works correctly across Linux and macOS.

## Prerequisites

- **Docker Desktop** (for Linux container tests)
- **Rust 1.75+** (`rustup update stable`)
- **cargo-make** (`cargo install cargo-make`)

## Quick Start (5 minutes)

### 1. Run all E2E tests locally

```bash
# Run E2E tests (skips Docker tests if Docker unavailable)
cargo make test-e2e
```

### 2. Run platform-specific tests

```bash
# Linux container tests (requires Docker)
cargo make test-e2e-linux

# macOS native tests (current platform)
cargo make test-e2e-macos
```

### 3. Run a specific fixture

```bash
# Test thesis-gen example only
cargo test --package ggen-e2e -- thesis_gen --nocapture
```

## Test Categories

| Command | Description | Requires Docker |
|---------|-------------|-----------------|
| `cargo make test-e2e` | All E2E tests | Auto-detect |
| `cargo make test-e2e-linux` | Linux container tests | Yes |
| `cargo make test-e2e-macos` | macOS native tests | No |
| `cargo make test-e2e-homebrew` | Homebrew install test | No |
| `cargo make test-e2e-cross` | Cross-platform comparison | Yes |

## Environment Variables

| Variable | Default | Description |
|----------|---------|-------------|
| `UPDATE_GOLDEN` | unset | Set to update golden files |
| `GGEN_E2E_TIMEOUT` | 300 | Test timeout in seconds |
| `TESTCONTAINERS_COMMAND` | drop | Set to `keep` to debug containers |
| `GGEN_LOG` | info | Log level for ggen sync |

## Updating Golden Files

When `ggen sync` output changes intentionally:

```bash
# Update all golden files
UPDATE_GOLDEN=1 cargo make test-e2e

# Update specific fixture
UPDATE_GOLDEN=1 cargo test --package ggen-e2e -- thesis_gen

# Review changes
git diff tests/e2e/golden/
```

## Debugging Failed Tests

### 1. View container logs

```bash
# Keep container running after test
TESTCONTAINERS_COMMAND=keep cargo make test-e2e-linux

# Find container ID
docker ps -a | grep ggen

# View logs
docker logs <container_id>

# Cleanup manually
docker rm -f <container_id>
```

### 2. Run with verbose output

```bash
RUST_BACKTRACE=1 GGEN_LOG=debug cargo test --package ggen-e2e -- --nocapture
```

### 3. Inspect golden file diff

Failed tests print a unified diff. To see full context:

```bash
diff -u tests/e2e/golden/thesis-gen/output/thesis.tex target/e2e/thesis-gen/output/thesis.tex
```

## CI Integration

E2E tests run automatically on:
- **Pull Requests**: Linux + macOS matrix
- **Push to master**: Full test suite
- **Release**: Cross-platform comparison + Homebrew verification

### Skipping E2E tests

Add `[skip e2e]` to commit message to skip E2E tests in CI (use sparingly).

## Fixture Structure

Each test fixture is a complete ggen project:

```
tests/e2e/fixtures/
├── minimal/                    # Fast smoke test
│   ├── ggen.toml
│   ├── ontology/schema.ttl
│   └── templates/main.tera
└── thesis-gen-sample/          # Subset of thesis-gen
    ├── ggen.toml
    ├── ontology/
    └── templates/

tests/e2e/golden/
├── minimal/output/             # Expected output
└── thesis-gen/output/
```

## Adding a New Fixture

1. Create fixture directory:
   ```bash
   mkdir -p tests/e2e/fixtures/my-fixture/{ontology,templates}
   ```

2. Add ggen project files:
   ```bash
   cp templates/ggen.toml.example tests/e2e/fixtures/my-fixture/ggen.toml
   # Edit ggen.toml, add ontology, templates
   ```

3. Generate initial golden files:
   ```bash
   cd tests/e2e/fixtures/my-fixture
   ggen sync
   cp -r output ../../golden/my-fixture/
   ```

4. Add fixture to test suite:
   ```rust
   // tests/e2e/e2e_examples.rs
   #[test]
   fn test_my_fixture() {
       run_fixture("my-fixture").unwrap();
   }
   ```

5. Update contracts:
   ```yaml
   # specs/011-e2e-testcontainers/contracts/test-fixtures.yaml
   fixtures:
     - name: my-fixture
       ...
   ```

## Troubleshooting

### Docker not found

```
Error: Docker daemon not running
```

**Fix**: Start Docker Desktop or install Docker.

### Timeout errors

```
Error: Test timed out after 300s
```

**Fix**: Increase timeout:
```bash
GGEN_E2E_TIMEOUT=600 cargo make test-e2e
```

### Golden file mismatch

```
Error: Golden file mismatch: output/thesis.tex
```

**Fix**: Review diff, then update if change is intentional:
```bash
UPDATE_GOLDEN=1 cargo make test-e2e
git diff tests/e2e/golden/
git add tests/e2e/golden/
```

### Cross-platform differences

```
Error: Platform difference detected between linux-x86_64 and darwin-arm64
```

**Root causes**:
1. **Line endings**: Ensure `.gitattributes` enforces LF
2. **Timestamps**: ggen sync should not embed timestamps
3. **Path separators**: Use `/` consistently in output

**Fix**: Check diff for platform-specific content and fix in templates.

## Performance Tips

1. **Use minimal fixture** for quick feedback:
   ```bash
   cargo test --package ggen-e2e -- minimal --nocapture
   ```

2. **Run tests in parallel** (safe due to container isolation):
   ```bash
   cargo test --package ggen-e2e -- --test-threads=4
   ```

3. **Cache Docker images**:
   ```bash
   docker pull ubuntu:22.04
   ```

## Next Steps

- Read the [data model](./data-model.md) for entity details
- Review [research](./research.md) for implementation patterns
- See [spec.md](./spec.md) for full requirements
