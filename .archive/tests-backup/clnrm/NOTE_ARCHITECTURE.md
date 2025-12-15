# Architecture Note: clnrm Testing

## Issue

The clnrm tests require building ggen inside a Linux Docker container, but:
- Host system: macOS ARM64 (arm64)
- Docker container: Linux ARM64 (aarch64)
- Binaries are not cross-compatible

## Current Status

The test framework is set up correctly, but the build step fails because:
1. Building from scratch in Docker is slow (5-10 minutes)
2. Requires all dependencies to be available
3. May hit compilation errors that need to be fixed

## Solutions

### Option 1: Pre-build Linux Binary (Recommended for CI/CD)

Build a Linux binary first, then mount it:

```bash
# Build Linux binary using cross-compilation or CI
cargo build --release --target aarch64-unknown-linux-gnu

# Mount the Linux binary in tests
volumes = [
    { host = "/Users/sac/ggen/target/aarch64-unknown-linux-gnu/release", container = "/target", readonly = true }
]
```

### Option 2: Skip Build, Test Locally

For local development, skip the build step and test with pre-built binary:

```toml
# Comment out build step
# [[steps]]
# name = "build_ggen"
# ...
```

### Option 3: Use GitHub Actions

Run clnrm tests in GitHub Actions where Linux binaries are available:

```yaml
- name: Build ggen
  run: cargo build --release -p ggen-cli-lib --bin ggen

- name: Run clnrm tests
  run: clnrm run tests/clnrm/cli_commands.clnrm.toml
```

## Current Test File

The test file `cli_commands.clnrm.toml` is correctly configured but will fail on macOS due to architecture mismatch. It will work correctly in Linux CI environments.

