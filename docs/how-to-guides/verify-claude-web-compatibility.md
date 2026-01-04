# Verify Claude Code Web Compatibility (PR 108)

This guide explains how to verify that ggen works correctly in the Claude Code on the web environment, which uses a Debian-based universal image.

## Overview

PR 108 ensures ggen is compatible with Claude Code on the web's Debian bookworm environment. This verification uses:

- **Testcontainers**: Test ggen in Debian bookworm containers
- **Act**: Test GitHub Actions workflows locally
- **Poka-Yoke**: Error prevention and validation safeguards

## Prerequisites

- Docker installed and running
- Rust toolchain installed
- `act` installed (optional, for workflow testing)

## Quick Start

Run the complete verification:

```bash
cargo make verify-pr108
```

This runs all verification checkpoints:
1. ✅ Docker is running
2. ✅ Testcontainers can create containers
3. ✅ Debian bookworm image is available
4. ✅ Rust toolchain is available in container
5. ✅ ggen can be installed
6. ✅ ggen executes correctly
7. ✅ Containers are cleaned up
8. ✅ Act can run workflows (if installed)
9. ✅ Workflows complete successfully

## Individual Verification Steps

### 1. Test Debian Container

Test ggen in a Debian bookworm container:

```bash
cargo make test-debian-container
```

This verifies:
- Container startup
- Rust toolchain installation
- ggen installation
- ggen execution
- Container cleanup

### 2. Run Poka-Yoke Certification

Run the poka-yoke certification test suite:

```bash
cargo make certify-poka-yoke
```

This validates:
- Pre-flight checks (Docker, act, testcontainers)
- Timeout guards
- Error detector fail-fast behavior
- Cleanup verification

### 3. Test Workflows with Act

Test GitHub Actions workflows locally:

```bash
# List available workflows
cargo make act-list

# Test a specific workflow
cargo make act WORKFLOW=deploy-docs.yml JOB=deploy-docs
```

## Manual Verification

### Using the Verification Script

Run the comprehensive verification script:

```bash
./scripts/verify-pr108.sh
```

This script:
1. Performs pre-flight checks (poka-yoke)
2. Tests Debian containers
3. Tests workflows with act
4. Runs poka-yoke certification
5. Verifies cleanup

### Running Tests Directly

#### Debian Container Tests

```bash
# Run all Debian container tests
cargo test --package ggen-e2e --lib tests::debian_claude_web_test -- --ignored

# Run specific test
cargo test --package ggen-e2e --lib tests::debian_claude_web_test::test_debian_bookworm_container_startup -- --ignored
```

#### Poka-Yoke Certification Tests

```bash
# Run all poka-yoke tests
cargo test --test poka_yoke_certification_test

# Run with ignored tests (requires Docker)
cargo test --test poka_yoke_certification_test -- --ignored
```

## Verification Checkpoints

### Checkpoint 1: Docker is Running

```bash
docker ps
```

Should show running containers or empty list (not an error).

### Checkpoint 2: Testcontainers Can Create Containers

This is verified automatically by the Debian container tests.

### Checkpoint 3: Debian Bookworm Image Available

```bash
docker image inspect debian:bookworm-slim
```

If image doesn't exist, it will be pulled automatically on first use.

### Checkpoint 4: Rust Toolchain in Container

Verified by `test_rust_toolchain_in_debian` test.

### Checkpoint 5: ggen Installation

Verified by `test_ggen_installation_in_debian` test.

### Checkpoint 6: ggen Execution

Verified by `test_ggen_execution_in_debian` test.

### Checkpoint 7: Container Cleanup

Verified by `test_container_cleanup` test and cleanup verifier.

### Checkpoint 8: Act Workflow Testing

```bash
act --dryrun -W .github/workflows/deploy-docs.yml
```

### Checkpoint 9: Workflow Completion

Verified by running workflows with act.

## Poka-Yoke Safeguards

The verification includes comprehensive poka-yoke (error prevention) safeguards:

### Pre-Flight Checks

- Docker availability check
- Act availability check (optional)
- Testcontainers availability check
- Timeout enforcement

### Error Detection

- Fail-fast on errors
- Maximum error threshold
- Comprehensive error messages

### Cleanup Verification

- Container count tracking
- Cleanup verification after tests
- Resource leak detection

### Timeout Guards

- All operations have timeouts
- Timeout enforcement
- Timeout reporting

## Troubleshooting

### Docker Not Running

**Error**: `Cannot connect to the Docker daemon`

**Solution**:
```bash
# Start Docker Desktop (macOS/Windows) or Docker daemon (Linux)
docker ps  # Verify it's running
```

### Testcontainers Failures

**Error**: Container startup failed

**Solution**:
- Verify Docker is running
- Check Docker has enough resources (memory, CPU)
- Try pulling the image manually: `docker pull debian:bookworm-slim`

### Act Not Installed

**Warning**: `act is not installed (optional for workflow testing)`

**Solution**:
```bash
# macOS
brew install act

# Linux
curl https://raw.githubusercontent.com/nektos/act/master/install.sh | sudo bash
```

### Container Cleanup Issues

**Warning**: Container count increased significantly

**Solution**:
- Check for stuck containers: `docker ps -a`
- Clean up manually: `docker container prune -f`
- Verify testcontainers cleanup is working

## CI Integration

The verification can be integrated into CI:

```yaml
# .github/workflows/pr108-verification.yml
name: PR 108 Verification

on:
  pull_request:
    paths:
      - 'crates/ggen-e2e/**'
      - 'scripts/verify-pr108.sh'
      - '.github/workflows/**'

jobs:
  verify:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - uses: dtolnay/rust-toolchain@stable
      - name: Install cargo-make
        run: cargo install cargo-make --locked
      - name: Verify PR 108
        run: cargo make verify-pr108
```

## Success Criteria

✅ All Debian container tests pass  
✅ ggen installs and executes correctly in Debian bookworm  
✅ Act workflows run successfully locally  
✅ All poka-yoke safeguards are in place  
✅ Certification test suite passes  
✅ Documentation is complete  
✅ CI integration works  

## Related Resources

- [Claude Code on the web documentation](https://code.claude.com/docs/en/claude-code-on-the-web)
- [Testcontainers Rust documentation](https://rust.testcontainers.org/)
- [Act documentation](https://github.com/nektos/act)
- [Poka-Yoke Design Guide](../explanations/poka-yoke-design.md)


