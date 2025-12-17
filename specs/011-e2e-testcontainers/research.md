# Research: End-to-End Testing with Testcontainers

**Feature**: 011-e2e-testcontainers | **Date**: 2025-12-16
**Status**: Phase 0 Complete

## Executive Summary

This research covers testcontainers-rs patterns for CLI testing, GitHub Actions cross-platform matrix strategies, golden file testing practices, and Homebrew installation verification. Key findings inform the implementation plan for cross-platform E2E testing of `ggen sync`.

## 1. Testcontainers-rs Patterns

### 1.1 Core Architecture

[Testcontainers for Rust](https://rust.testcontainers.org/) provides container lifecycle management via RAII semantics:

- **Automatic Cleanup**: Containers are removed when they go out of scope (via `Drop` trait)
- **Parallel Testing**: Random port mapping enables concurrent test execution
- **Sync/Async Support**: `SyncRunner` (with `blocking` feature) and `AsyncRunner`

### 1.2 Container Lifecycle

```rust
// Async pattern (recommended for ggen's tokio-based architecture)
use testcontainers::runners::AsyncRunner;
use testcontainers::GenericImage;

let container = GenericImage::new("ubuntu", "22.04")
    .with_wait_for(WaitFor::message_on_stdout("ready"))
    .start()
    .await
    .expect("container started");

// Container automatically cleaned up when dropped
```

### 1.3 Custom Images for CLI Testing

For testing `ggen sync`, we need a custom image with:
- Rust toolchain (for cargo install)
- OR pre-built ggen binary mounted
- Sample ontology files

**Recommended approach**: Use `GenericImage` with Ubuntu LTS base, mount test fixtures as volumes.

### 1.4 Cleanup Strategies

| Strategy | Use Case | Configuration |
|----------|----------|---------------|
| Auto-drop (default) | Normal testing | N/A |
| Keep containers | Debugging | `TESTCONTAINERS_COMMAND=keep` |
| Manual cleanup | CI recovery | `docker rm -f $(docker ps -aq)` |

### 1.5 Key Dependencies

From ggen's existing `Cargo.toml`:
```toml
testcontainers = "0.25"
testcontainers-modules = "0.13"
chicago-tdd-tools = { version = "1.4.0", features = ["testcontainers"] }
```

## 2. GitHub Actions Cross-Platform Matrix

### 2.1 Current ggen CI Structure

ggen already has cross-platform builds in `ci.yml`:
```yaml
build-matrix:
  strategy:
    fail-fast: false
    matrix:
      os: [ubuntu-latest, macos-latest]
      rust: [stable, beta, nightly]
```

### 2.2 E2E Matrix Strategy

For E2E testing, a dedicated matrix with Docker support:

```yaml
e2e-test:
  strategy:
    fail-fast: false
    matrix:
      include:
        # Linux with Docker (testcontainers)
        - os: ubuntu-latest
          platform: linux-x86_64
          docker: true
        # macOS Intel (native)
        - os: macos-13
          platform: darwin-x86_64
          docker: false
        # macOS ARM (native)
        - os: macos-latest
          platform: darwin-arm64
          docker: false
```

### 2.3 Docker Availability

| Runner | Docker | Notes |
|--------|--------|-------|
| ubuntu-latest | ✅ Pre-installed | Full testcontainers support |
| macos-latest | ❌ Not available | Native testing only |
| macos-13 | ❌ Not available | Native testing only |

**Decision**: Use testcontainers for Linux, native execution for macOS.

### 2.4 Caching Strategy

```yaml
- name: Cache cargo
  uses: actions/cache@v4
  with:
    path: |
      ~/.cargo/bin/
      ~/.cargo/registry/index/
      ~/.cargo/registry/cache/
      ~/.cargo/git/db/
      target/
    key: ${{ runner.os }}-cargo-e2e-${{ hashFiles('**/Cargo.lock') }}
```

## 3. Golden File Testing

### 3.1 Pattern Overview

Golden file testing compares generated output against known-good "golden" files:

1. **Generate**: Run `ggen sync` on test project
2. **Compare**: Diff output against golden files
3. **Update**: `--update-golden` flag to refresh expected output

### 3.2 Implementation Patterns

```rust
use std::fs;
use std::path::Path;

pub fn assert_golden(actual: &str, golden_path: &Path) -> Result<(), GoldenError> {
    if std::env::var("UPDATE_GOLDEN").is_ok() {
        fs::write(golden_path, actual)?;
        return Ok(());
    }

    let expected = fs::read_to_string(golden_path)?;
    if actual != expected {
        return Err(GoldenError::Mismatch {
            expected: expected.clone(),
            actual: actual.to_string(),
            diff: create_unified_diff(&expected, actual),
        });
    }
    Ok(())
}
```

### 3.3 Directory Structure

```
tests/e2e/golden/
├── thesis-gen/
│   ├── output/
│   │   ├── thesis.tex           # Expected main file
│   │   ├── chapters/
│   │   │   └── chapter-1.tex
│   │   └── references.bib
│   └── manifest.json            # Checksums for all files
└── minimal-project/
    └── output/
        └── generated.rs
```

### 3.4 Cross-Platform Considerations

**Critical**: Line endings can differ across platforms. Normalize before comparison:
- Convert `\r\n` to `\n` before comparison
- Use binary mode for checksums
- Store golden files with LF line endings (`.gitattributes`)

## 4. Homebrew Installation Testing

### 4.1 Current Homebrew Setup

ggen has existing Homebrew infrastructure:
- Tap: `seanchatmangpt/homebrew-tap`
- Formula: `ggen.rb`
- Automated updates via `homebrew-release.yml`

### 4.2 Testing Approaches

| Approach | Pros | Cons |
|----------|------|------|
| Real `brew install` | Most realistic | Slow, requires tap access |
| Local formula | Faster | May miss tap issues |
| Binary verification | Very fast | Doesn't test brew flow |

**Recommended**: Test `brew install seanchatmangpt/tap/ggen` on release workflow only.

### 4.3 Verification Script

```bash
#!/bin/bash
# e2e-homebrew-test.sh

# Uninstall if present
brew uninstall ggen 2>/dev/null || true

# Install from tap
brew install seanchatmangpt/tap/ggen

# Verify installation
ggen --version

# Run smoke test
cd /tmp
ggen init test-project
cd test-project
ggen sync

# Verify output
[ -f "generated/output.rs" ] && echo "✅ Homebrew installation verified"
```

## 5. Container Image Selection

### 5.1 Options Analysis

| Image | Size | Boot Time | Rust Support |
|-------|------|-----------|--------------|
| ubuntu:22.04 | 77MB | ~2s | apt install |
| rust:1.75 | 1.4GB | ~5s | Pre-installed |
| alpine:3.19 | 7MB | <1s | apk install (slow) |

### 5.2 Decision

**Use `ubuntu:22.04`** because:
1. Matches CI environment (`ubuntu-latest`)
2. Reasonable size/boot tradeoff
3. Full glibc support (no musl issues)
4. Can install Rust OR mount pre-built binary

### 5.3 Custom Dockerfile (Optional)

For faster tests, pre-build a ggen test image:

```dockerfile
FROM ubuntu:22.04

RUN apt-get update && apt-get install -y \
    curl \
    build-essential \
    && rm -rf /var/lib/apt/lists/*

# Install Rust
RUN curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y
ENV PATH="/root/.cargo/bin:${PATH}"

# Pre-compile ggen dependencies (cache layer)
WORKDIR /ggen
COPY Cargo.toml Cargo.lock ./
COPY crates ./crates
RUN cargo build --release

# Entry point
ENTRYPOINT ["/ggen/target/release/ggen"]
```

## 6. Test Scenarios

Based on spec requirements (FR-001 through FR-014):

| ID | Scenario | Platform | Method |
|----|----------|----------|--------|
| E2E-001 | ggen sync on Linux | linux-x86_64 | testcontainer |
| E2E-002 | ggen sync on macOS Intel | darwin-x86_64 | native |
| E2E-003 | ggen sync on macOS ARM | darwin-arm64 | native |
| E2E-004 | Cross-platform output comparison | all | CI job |
| E2E-005 | Homebrew install verification | darwin-* | CI job |
| E2E-006 | thesis-gen example validation | all | golden files |
| E2E-007 | Container cleanup verification | linux-x86_64 | testcontainer |
| E2E-008 | Parallel test execution | linux-x86_64 | testcontainer |

## 7. Risk Analysis

| Risk | Impact | Mitigation |
|------|--------|------------|
| Docker not available on macOS runners | Medium | Native execution fallback |
| Flaky container startup | High | Retry with backoff, timeout config |
| Golden file drift | Medium | `UPDATE_GOLDEN` env var, CI bot PRs |
| Homebrew tap access failures | Low | Skip on fork PRs, retry logic |
| Platform-specific line endings | Medium | Normalize in comparison |

## 8. Recommendations

1. **Crate Structure**: Create `ggen-e2e` crate with clear module boundaries
2. **Test Isolation**: Use `#[ignore]` for Docker-dependent tests
3. **CI Integration**: Separate workflows for Linux (testcontainers) and macOS (native)
4. **Golden Files**: Store in `tests/e2e/golden/` with `.gitattributes` for LF
5. **Homebrew Testing**: Only on release workflow, not PR CI
6. **Timeouts**: 5 minutes per test, 10 minutes total E2E suite

## Sources

- [testcontainers-rs GitHub](https://github.com/testcontainers/testcontainers-rs)
- [Testcontainers for Rust Quickstart](https://rust.testcontainers.org/quickstart/testcontainers/)
- [testcontainers-modules docs](https://docs.rs/testcontainers-modules/latest/testcontainers_modules/)
- [GitHub Actions Matrix Builds](https://github.com/orgs/community/discussions/132605)
- [Cross-Platform GitHub Actions](https://www.edwardthomson.com/blog/github_actions_3_crossplatform_builds.html)
- [Matrix Strategy Guide](https://runs-on.com/github-actions/the-matrix-strategy/)
