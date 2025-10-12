# Production Deployment Guide

**Version**: 1.0.0-rc1
**Last Updated**: 2025-01-11
**Status**: Production Ready

## Overview

This guide provides comprehensive instructions for deploying ggen in production environments, following core team best practices and 80/20 principles.

## Pre-Deployment Checklist

### Code Quality ✅

- [x] All tests passing (204/204 tests)
- [x] Zero compilation warnings
- [x] Clippy checks pass with `-D warnings`
- [x] Code formatted with `cargo fmt`
- [x] Dependencies audited with `cargo audit`

### Documentation ✅

- [x] CHANGELOG.md updated
- [x] VERSION file matches Cargo.toml
- [x] API documentation generated
- [x] Migration guides written
- [x] Compatibility policy documented

### Security ✅

- [x] No hardcoded secrets or API keys
- [x] Dependencies scanned for vulnerabilities
- [x] Input validation implemented
- [x] Error messages don't leak sensitive info
- [x] File permissions properly set

### Performance ✅

- [x] Benchmarks run and recorded
- [x] Memory usage profiled
- [x] Parallel execution tested (2-5x speedup verified)
- [x] Cache performance validated
- [x] No performance regressions detected

## Deployment Steps

### 1. Build Release Binary

```bash
# Clean build environment
cargo clean

# Build optimized release binary
cargo build --release --locked

# Verify binary works
./target/release/ggen --version
./target/release/ggen --help

# Strip debug symbols for smaller binary
strip target/release/ggen

# Verify file size (should be reasonable)
ls -lh target/release/ggen
```

**Expected Output**:
```
ggen 1.0.0-rc1
Binary size: ~10-20MB (stripped)
```

### 2. Run Pre-Deployment Tests

```bash
# Run full test suite in release mode
cargo test --all --release

# Run lifecycle example
cd examples/rust-cli-lifecycle
../../target/release/ggen lifecycle list
../../target/release/ggen lifecycle run build

# Verify all phases work
cd ../..
```

### 3. Package for Distribution

#### Homebrew (macOS/Linux)

```bash
# Create release tarball
tar -czf ggen-$VERSION-x86_64-apple-darwin.tar.gz \
    -C target/release ggen

# Generate SHA256
shasum -a 256 ggen-$VERSION-x86_64-apple-darwin.tar.gz
```

#### Cargo Install

```bash
# Test cargo install from local path
cargo install --path . --force

# Verify installation
which ggen
ggen --version
```

#### Docker

```dockerfile
FROM rust:1.75-slim as builder
WORKDIR /app
COPY . .
RUN cargo build --release --locked

FROM debian:bookworm-slim
COPY --from=builder /app/target/release/ggen /usr/local/bin/
RUN ggen --version
ENTRYPOINT ["ggen"]
```

### 4. Deploy to Package Managers

#### crates.io

```bash
# Dry run
cargo publish --dry-run

# Actual publish (requires authentication)
cargo publish
```

#### Homebrew

```bash
# Update formula in homebrew-tap repository
# Formula will be auto-generated from GitHub release
```

#### GitHub Release

```bash
# Tag release
git tag -a v1.0.0-rc1 -m "Release v1.0.0-rc1"
git push origin v1.0.0-rc1

# Create GitHub release with binaries
gh release create v1.0.0-rc1 \
    --title "ggen v1.0.0-rc1" \
    --notes-file RELEASE_NOTES.md \
    ggen-*-x86_64-*.tar.gz
```

## Production Configuration

### Environment Variables

```bash
# Recommended production settings
export RUST_LOG=info
export RUST_BACKTRACE=0
export GGEN_CACHE_DIR=/var/cache/ggen
export GGEN_STATE_DIR=/var/lib/ggen
```

### File Permissions

```bash
# Binary permissions
chmod 755 /usr/local/bin/ggen

# Cache directory (user writable)
mkdir -p ~/.ggen/cache
chmod 700 ~/.ggen

# System-wide installation
sudo mkdir -p /usr/local/share/ggen
sudo chmod 755 /usr/local/share/ggen
```

### Logging Configuration

```bash
# Production: Errors and warnings only
export RUST_LOG=ggen=warn

# Staging: Info level
export RUST_LOG=ggen=info

# Development: Debug level
export RUST_LOG=ggen=debug
```

## Monitoring and Observability

### Metrics to Track

1. **Execution Metrics**
   - Phase execution time (via `.ggen/state.json`)
   - Cache hit rate
   - Parallel execution speedup
   - Memory usage

2. **Error Metrics**
   - Failed phase count
   - Error types frequency
   - Command failures
   - Hook failures

3. **Usage Metrics**
   - Most used lifecycle phases
   - Average project size
   - Workspace count distribution
   - Cache size growth

### Health Checks

```bash
# Basic health check
ggen lifecycle list > /dev/null
echo $?  # Should be 0

# Cache health
ggen lifecycle run build --check-cache-only

# State file validation
test -f .ggen/state.json && echo "OK" || echo "MISSING"
```

## Performance Tuning

### Cache Configuration

```toml
# make.toml
[cache]
enabled = true
directory = ".ggen/cache"
max_size_mb = 1000  # Limit cache size
ttl_hours = 168     # 1 week TTL
```

### Parallel Execution

```toml
# make.toml
[lifecycle.build]
parallel = true    # Enable parallel workspace execution
max_workers = 4    # Limit concurrent workers
```

### Memory Optimization

```bash
# Limit memory for large projects
ulimit -v 4194304  # 4GB virtual memory limit

# Monitor memory usage
/usr/bin/time -v ggen lifecycle run build
```

## Rollback Procedures

### Quick Rollback

```bash
# Revert to previous version
cargo install ggen --version 0.9.0

# Or with Homebrew
brew install ggen@0.9
```

### State Recovery

```bash
# Backup state before upgrade
cp .ggen/state.json .ggen/state.json.backup

# Restore if needed
cp .ggen/state.json.backup .ggen/state.json
```

### Cache Invalidation

```bash
# Clear cache if corrupted
rm -rf .ggen/cache

# Rebuild cache
ggen lifecycle run build --force-rebuild
```

## Security Best Practices

### 1. Input Validation

- All user inputs validated before execution
- Command injection prevented (no shell execution of user input)
- File path traversal prevented (chroot-like restrictions)

### 2. Dependency Management

```bash
# Regular security audits
cargo audit

# Update dependencies monthly
cargo update

# Pin versions for reproducible builds
cargo build --locked
```

### 3. Secrets Management

```bash
# Never commit these to make.toml
- API keys
- Database passwords
- SSH keys
- TLS certificates

# Use environment variables instead
command = "deploy --api-key $API_KEY"
```

### 4. File Permissions

```bash
# State files should be user-private
chmod 600 .ggen/state.json

# Cache can be group-readable if needed
chmod 755 .ggen/cache
```

## Troubleshooting Production Issues

### Issue: Slow Performance

**Diagnosis**:
```bash
# Check cache hit rate
grep "cache_hit" .ggen/state.json | wc -l

# Profile execution time
time ggen lifecycle run build
```

**Solution**:
- Enable caching for expensive phases
- Use parallel execution for workspaces
- Increase cache size limit

### Issue: High Memory Usage

**Diagnosis**:
```bash
# Monitor memory during execution
/usr/bin/time -v ggen lifecycle run build 2>&1 | grep "Maximum resident"
```

**Solution**:
- Reduce max_workers in parallel execution
- Use sequential execution for memory-constrained environments
- Clear cache periodically

### Issue: Cache Corruption

**Diagnosis**:
```bash
# Check cache directory
find .ggen/cache -type f | head
```

**Solution**:
```bash
# Clear and rebuild cache
rm -rf .ggen/cache
ggen lifecycle run build
```

### Issue: State File Corruption

**Diagnosis**:
```bash
# Validate state file JSON
jq '.' .ggen/state.json > /dev/null
```

**Solution**:
```bash
# Backup and reset
mv .ggen/state.json .ggen/state.json.corrupt
ggen lifecycle run init  # Creates new state
```

## CI/CD Integration

### GitHub Actions

```yaml
name: Build and Deploy

on:
  push:
    branches: [main]
    tags: ['v*']

jobs:
  build:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - uses: dtolnay/rust-toolchain@stable

      - name: Install ggen
        run: cargo install ggen --version 1.0.0-rc1

      - name: Run lifecycle
        run: ggen lifecycle pipeline format lint test build

      - name: Verify build
        run: test -f target/release/app

      - name: Upload artifact
        uses: actions/upload-artifact@v4
        with:
          name: binary
          path: target/release/app
```

### GitLab CI

```yaml
build:
  image: rust:1.75
  script:
    - cargo install ggen --version 1.0.0-rc1
    - ggen lifecycle run build
    - ggen lifecycle run test
  artifacts:
    paths:
      - target/release/
```

### Jenkins

```groovy
pipeline {
    agent any
    stages {
        stage('Build') {
            steps {
                sh 'cargo install ggen --version 1.0.0-rc1'
                sh 'ggen lifecycle run build'
            }
        }
        stage('Test') {
            steps {
                sh 'ggen lifecycle run test'
            }
        }
    }
}
```

## Production Checklist

### Pre-Release

- [ ] All tests passing (204/204)
- [ ] Zero warnings in compilation
- [ ] CHANGELOG.md updated
- [ ] VERSION file updated
- [ ] Documentation reviewed
- [ ] Security audit complete
- [ ] Performance benchmarks recorded
- [ ] Backward compatibility verified

### Release

- [ ] Git tag created (v1.0.0-rc1)
- [ ] GitHub release published
- [ ] crates.io published
- [ ] Homebrew formula updated
- [ ] Release notes written
- [ ] Documentation deployed

### Post-Release

- [ ] Release announcement posted
- [ ] User feedback channels monitored
- [ ] Error reports triaged
- [ ] Performance metrics collected
- [ ] Hotfix process ready

## Support and Maintenance

### Release Cadence

- **Patch releases**: As needed (critical bugs)
- **Minor releases**: Monthly
- **Major releases**: Yearly

### Support Channels

- **GitHub Issues**: Bug reports and feature requests
- **GitHub Discussions**: Questions and community support
- **Security**: security@ggen.io (private disclosure)

### Monitoring Dashboards

```bash
# View execution metrics
ggen lifecycle stats

# View cache statistics
ggen cache stats

# View system health
ggen health check
```

## Emergency Procedures

### Critical Bug in Production

1. **Immediate**: Revert to last known good version
2. **Within 24h**: Patch and test
3. **Within 48h**: Release hotfix
4. **Post-mortem**: Document and prevent recurrence

### Security Vulnerability

1. **Immediate**: Private disclosure to security team
2. **Within 24h**: Patch developed
3. **Coordinated release**: CVE issued, patch released
4. **Public disclosure**: After patch deployed

---

**Questions?** See [COMPATIBILITY.md](COMPATIBILITY.md) or open a discussion.

**Production Issues?** Open a high-priority issue with `[PRODUCTION]` tag.
