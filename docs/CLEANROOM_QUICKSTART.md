# Cleanroom Test Quick-Start Guide

**Get started with cleanroom production validation in 5 minutes**

## What Are Cleanroom Tests?

Cleanroom tests validate that ggen works correctly in **completely isolated, production-like environments**. They use Docker containers with resource constraints, network isolation, and security boundaries to ensure no hidden dependencies on your local machine.

### Why Cleanroom Tests Matter

- **Complete Isolation**: Tests run in fresh containers with no host dependencies
- **Production Validation**: Verifies behavior under real resource constraints
- **Security Testing**: Validates network isolation and input sanitization
- **Deterministic**: Same inputs always produce identical results
- **User-Facing Bug Prevention**: Catches issues users will encounter in production

## Quick Start

### Prerequisites

1. **Docker** must be running:
```bash
# Check Docker is running
docker info

# If not running:
# macOS: Start Docker Desktop
# Linux: sudo systemctl start docker
```

2. **Rust toolchain** installed:
```bash
rustc --version  # Should show 1.75 or later
```

### Running Cleanroom Tests

#### Option 1: Using Cargo Make (Recommended)
```bash
# Run all cleanroom tests
cargo make test-cleanroom

# Run comprehensive production readiness validation (includes cleanroom)
cargo make production-readiness
```

#### Option 2: Using Cargo Directly
```bash
# Run cleanroom tests
cargo test --package ggen-cli-lib --test integration testcontainers_cleanroom

# Run with logging
RUST_LOG=info cargo test --package ggen-cli-lib --test integration testcontainers_cleanroom
```

#### Option 3: Using the Validation Script
```bash
# Full validation (includes cleanroom tests)
./scripts/production-readiness-validation.sh --full

# Quick validation (skips cleanroom tests)
./scripts/production-readiness-validation.sh --quick
```

## Test Files

The cleanroom test infrastructure consists of:

```
cli/tests/
├── cleanroom_production.rs                    # Main cleanroom test harness (700+ lines)
├── marketplace_cleanroom_e2e.rs               # Marketplace end-to-end tests
├── cleanroom_marketplace_production_test.rs   # Marketplace production tests
└── integration/
    └── testcontainers_cleanroom.rs            # Testcontainers integration tests
```

### Test Coverage

✅ **Marketplace Tests** (`cleanroom_production.rs` lines 47-156)
- Search validation
- Package installation
- Package listing
- JSON output
- Error handling

✅ **Lifecycle Tests** (`cleanroom_production.rs` lines 158-289)
- List phases
- Show phase details
- Run single phase
- Pipeline execution
- Readiness checks

✅ **Template Tests** (`cleanroom_production.rs` lines 291-342)
- List templates
- Show template details
- Template rendering

✅ **Integration Workflows** (`cleanroom_production.rs` lines 344-413)
- Complete marketplace → lifecycle → template workflows
- Multi-step validation
- State persistence

✅ **Error Handling** (`cleanroom_production.rs` lines 415-495)
- No panics on invalid input
- Graceful error messages
- Invalid command handling

✅ **Security Tests** (`cleanroom_production.rs` lines 497-578)
- Path traversal prevention
- Command injection prevention
- Input sanitization

✅ **Performance Tests** (`cleanroom_production.rs` lines 580-629)
- Commands complete in <5 seconds
- Resource constraint validation

## Current Status: Known Issue 🚨

### Testcontainers Version Conflict

**Error:**
```
error: failed to select a version for `bollard-stubs`
versions that meet the requirements `=1.45.0-rc.26.0.1` are: 1.45.0-rc.26.0.1
all possible versions conflict with previously selected packages.
previously selected package `bollard-stubs v1.42.0-rc.3`
```

**Root Cause:**
- `ggen-core` uses `testcontainers` v0.22 (requires `bollard-stubs =1.45.0-rc.26.0.1`)
- `ggen-ai` uses `testcontainers` v0.15 (requires `bollard-stubs =1.42.0-rc.3`)
- These versions are incompatible

**Fix Options:**

#### Option A: Upgrade ggen-ai to testcontainers v0.22 (Recommended)
```bash
# Edit ggen-ai/Cargo.toml
# Change:
testcontainers = "0.15"
# To:
testcontainers = "0.22"

# Then update lockfile
cargo update testcontainers
```

#### Option B: Downgrade ggen-core to testcontainers v0.15
```bash
# Edit ggen-core/Cargo.toml
# Change:
testcontainers = "0.22"
# To:
testcontainers = "0.15"

# Then update lockfile
cargo update testcontainers
```

#### Option C: Remove testcontainers from one package
```bash
# If ggen-ai doesn't need testcontainers in production:
# Comment out testcontainers dependency in ggen-ai/Cargo.toml
# [dev-dependencies]
# testcontainers = "0.15"  # Comment this line

# Then rebuild
cargo clean
cargo build
```

**Recommended Action:** Use **Option A** - upgrade ggen-ai to match ggen-core's version.

### After Fixing the Conflict

Once the testcontainers version conflict is resolved:

```bash
# 1. Verify fix
cargo build

# 2. Run cleanroom tests
cargo make test-cleanroom

# 3. Verify all tests pass
cargo make production-readiness
```

## Test Architecture

### CleanroomEnv Fixture

The `CleanroomEnv` fixture provides complete isolation for each test:

```rust
struct CleanroomEnv {
    temp_dir: TempDir,           // Fresh temporary directory
    project_root: PathBuf,        // Isolated project root
}

impl CleanroomEnv {
    fn new() -> Result<Self, Box<dyn std::error::Error>> {
        let temp_dir = TempDir::new()?;
        let project_root = temp_dir.path().to_path_buf();
        Ok(Self { temp_dir, project_root })
    }

    fn ggen_cmd(&self) -> Command {
        let mut cmd = Command::cargo_bin("ggen")
            .expect("Failed to find ggen binary");
        cmd.current_dir(&self.project_root);
        cmd
    }
}
```

### Test Pattern

Every cleanroom test follows this pattern:

```rust
#[test]
fn cleanroom_test_example() {
    // 1. Create fresh environment
    let env = CleanroomEnv::new()
        .expect("Failed to create cleanroom env");

    // 2. Initialize if needed
    env.init_marketplace()
        .expect("Failed to initialize marketplace");

    // 3. Run command
    env.ggen_cmd()
        .arg("market").arg("search").arg("rust")
        .assert()
        .success()
        .stdout(predicate::str::contains("Searching marketplace"));

    // 4. Cleanup is automatic (TempDir Drop)
}
```

## Test Categories Explained

### 1. Marketplace Tests
- **What**: Validate package management functionality
- **Why**: Ensure users can discover, install, and manage packages
- **Example**: Search for packages, install from registry, list installed packages

### 2. Lifecycle Tests
- **What**: Validate project lifecycle management
- **Why**: Ensure users can manage project workflows
- **Example**: Initialize project, run build phase, execute pipelines

### 3. Template Tests
- **What**: Validate template generation functionality
- **Why**: Ensure users can generate code from templates
- **Example**: List templates, show template details, render templates

### 4. Integration Tests
- **What**: Validate complete workflows combining multiple systems
- **Why**: Ensure systems work together correctly
- **Example**: Search marketplace → install package → generate code → build project

### 5. Error Handling Tests
- **What**: Validate graceful error handling without panics
- **Why**: Production code must never crash with `.expect()` or `.unwrap()`
- **Example**: Invalid commands, malformed input, missing files

### 6. Security Tests
- **What**: Validate input sanitization and security boundaries
- **Why**: Prevent path traversal, command injection, and other attacks
- **Example**: Malicious file paths, command injection attempts, unsafe input

### 7. Performance Tests
- **What**: Validate commands complete in reasonable time
- **Why**: Ensure system is responsive under resource constraints
- **Example**: Commands complete in <5 seconds with limited resources

## Troubleshooting

### Docker Not Running
```bash
# Error: Docker daemon is not running
# Solution: Start Docker daemon

# macOS
# Start Docker Desktop application

# Linux
sudo systemctl start docker

# Verify
docker info
```

### Docker Images Missing
```bash
# Error: Cannot pull image
# Solution: Ensure Docker can reach registry

# Check Docker login
docker login

# Pull required images manually
docker pull rust:1.75
docker pull postgres:15-alpine
docker pull redis:7-alpine
```

### Test Timeout
```bash
# Error: Test exceeded timeout
# Solution: Increase timeout or check container health

# Increase test timeout
RUST_TEST_TIMEOUT=600 cargo test --package ggen-cli-lib --test integration

# Check container logs
docker ps -a
docker logs <container_id>
```

### Permission Denied
```bash
# Error: Permission denied accessing /var/run/docker.sock
# Solution: Add user to docker group

# Linux
sudo usermod -aG docker $USER
newgrp docker

# Verify
docker ps
```

### Cleanup Failed Containers
```bash
# If containers are left running after tests
docker ps -a | grep testcontainers
docker rm -f $(docker ps -aq)
docker network prune -f
```

## Best Practices

### 1. Always Use Fresh Environment
```rust
// ✅ GOOD: Each test creates fresh environment
#[test]
fn test_marketplace() {
    let env = CleanroomEnv::new().unwrap();
    // ... test code
}

// ❌ BAD: Sharing environment between tests
static ENV: LazyLock<CleanroomEnv> = // DON'T DO THIS
```

### 2. Initialize Only What You Need
```rust
// ✅ GOOD: Initialize only required components
let env = CleanroomEnv::new().unwrap();
env.init_marketplace().unwrap();  // Only if testing marketplace

// ❌ BAD: Initialize everything for every test
env.init_marketplace().unwrap();
env.init_project().unwrap();
env.init_templates().unwrap();  // Unnecessary overhead
```

### 3. Use Predicates for Assertions
```rust
// ✅ GOOD: Use predicates for flexible matching
cmd.assert()
    .success()
    .stdout(predicate::str::contains("Success"));

// ❌ BAD: Exact string matching
cmd.assert()
    .success()
    .stdout("Exactly this string"); // Too brittle
```

### 4. Test Both Success and Failure
```rust
// ✅ GOOD: Test error handling
cmd.arg("invalid-command")
    .assert()
    .failure()
    .stderr(predicate::str::contains("Unknown command"));

// ❌ BAD: Only test success paths
cmd.arg("valid-command").assert().success();
```

## Next Steps

1. **Fix Testcontainers Conflict** - Align versions across workspace
2. **Run Tests** - Verify all cleanroom tests pass
3. **Add New Tests** - Extend coverage for new features
4. **CI Integration** - Add cleanroom tests to CI pipeline
5. **Monitor Performance** - Track test execution times

## Additional Resources

- [Comprehensive Documentation](CLEANROOM_PRODUCTION_TESTS.md)
- [Enhancement Summary](../CLEANROOM_ENHANCEMENT_SUMMARY.md)
- [Production Readiness Guide](../docs/PRODUCTION_READINESS_8020.md)
- [Test Code](../cli/tests/cleanroom_production.rs)

## Success Criteria

✅ Cleanroom tests are ready for production when:

- [ ] All tests pass consistently
- [ ] Tests complete in <10 minutes
- [ ] No panics or crashes
- [ ] Docker resources are cleaned up
- [ ] CI pipeline integration complete
- [ ] Documentation is up to date

**Current Status:** 🚧 Tests implemented, blocked by testcontainers version conflict

**Next Action:** Fix testcontainers version conflict, then run `cargo make test-cleanroom`
