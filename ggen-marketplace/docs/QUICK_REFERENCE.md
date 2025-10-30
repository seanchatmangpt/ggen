# Quick Reference - CI/CD & Features

## 🚀 Quick Commands

### Local Testing
```bash
# Run full verification
./scripts/verify-build.sh

# Test specific feature
cargo test --features p2p
cargo test --features graphql
cargo test --features crypto

# Test all features
cargo test --all-features

# Format and lint
cargo fmt
cargo clippy --all-features -- -D warnings
```

### Building
```bash
# Default (minimal)
cargo build

# With features
cargo build --features p2p
cargo build --features graphql-server
cargo build --features crypto
cargo build --all-features
```

## 🎯 Feature Flags

| Feature | Dependencies | Purpose |
|---------|-------------|---------|
| `default` | None | Minimal setup |
| `p2p` | libp2p | Peer-to-peer networking |
| `graphql` | async-graphql | GraphQL API |
| `graphql-server` | graphql + axum + tower | Full GraphQL server |
| `crypto` | ed25519-dalek + rand | Cryptographic signing |
| `all` | Everything | Complete functionality |

## 📋 CI/CD Triggers

### Automatic (on every push)
- Unit tests (all feature combinations)
- Linting (format + clippy)
- Security audit
- Code coverage

### Conditional (commit message triggers)
```bash
# Trigger P2P tests
git commit -m "feat: improve routing [p2p]"

# Trigger GraphQL tests
git commit -m "feat: add subscriptions [graphql]"

# Trigger both
git commit -m "feat: major update [p2p] [graphql]"
```

### Branch-Specific
- `main` branch: Runs all tests including smoke tests
- Feature branches: Runs standard test matrix

## 📁 Key Files

| File | Purpose |
|------|---------|
| `Cargo.toml` | Dependencies and feature flags |
| `.github/workflows/ci.yml` | CI/CD pipeline definition |
| `scripts/verify-build.sh` | Local build verification |
| `docs/RELEASE_CHECKLIST.md` | Pre-release validation |
| `docs/CI_CD_SETUP.md` | Complete CI/CD guide |
| `docs/FEATURE_DEVELOPMENT.md` | Feature development guide |

## 🔍 Troubleshooting

### Build Failures
```bash
# Clean and rebuild
cargo clean
cargo build --all-features

# Check specific feature
cargo check --features p2p
```

### Test Failures
```bash
# Run single test
cargo test --test test_name -- --nocapture

# Run with logging
RUST_LOG=debug cargo test
```

### Format Issues
```bash
# Auto-fix formatting
cargo fmt

# Check without fixing
cargo fmt -- --check
```

### Clippy Warnings
```bash
# Auto-fix where possible
cargo clippy --all-features --fix

# Show all warnings
cargo clippy --all-features -- -W clippy::all
```

## 📊 CI/CD Pipeline Overview

```
Push/PR → GitHub Actions
    │
    ├─→ Test Matrix (18 combinations)
    │   ├─ stable × [default, p2p, graphql, crypto, all]
    │   └─ beta × [default, p2p, graphql, crypto, all]
    │
    ├─→ Linting
    │   ├─ cargo fmt --check
    │   ├─ cargo clippy (default)
    │   └─ cargo clippy --all-features
    │
    ├─→ Integration Tests (all features)
    │
    ├─→ Smoke Tests (conditional)
    │   ├─ P2P (if [p2p] in message or main)
    │   └─ GraphQL (if [graphql] in message or main)
    │
    ├─→ Security Audit (cargo audit)
    │
    ├─→ Code Coverage (tarpaulin → Codecov)
    │
    ├─→ Build Verification (verify-build.sh)
    │
    └─→ All Checks Pass ✅
```

## 🎯 Release Workflow

### Pre-Release
```bash
# 1. Run verification
./scripts/verify-build.sh

# 2. Update version
# Edit Cargo.toml: version = "X.Y.Z"

# 3. Update changelog
# Edit CHANGELOG.md

# 4. Commit
git commit -am "chore: bump version to X.Y.Z"

# 5. Tag
git tag -a vX.Y.Z -m "Release vX.Y.Z"

# 6. Push
git push && git push --tags
```

### Publishing
```bash
# Dry run
cargo publish --dry-run

# Publish to crates.io
cargo publish
```

## 🔒 Security Best Practices

### ✅ DO
```rust
// Proper error handling
let result = operation()
    .map_err(|e| anyhow::anyhow!("Context: {}", e))?;

// Feature-gated code
#[cfg(feature = "p2p")]
use libp2p::*;
```

### ❌ DON'T
```rust
// Avoid in production
let result = operation().expect("This crashes");
let value = option.unwrap();

// Missing feature gate
use libp2p::*;  // Won't compile without p2p feature!
```

## 📈 Performance Metrics

### Build Times (with cache)
- Single feature: ~2 min
- Full pipeline: ~8 min
- Clean build: ~25 min

### Cache Effectiveness
- Registry: ~95% hit rate
- Build artifacts: ~85% hit rate
- Total time saved: ~60%

## 🔗 Useful Links

- [Cargo Features](https://doc.rust-lang.org/cargo/reference/features.html)
- [GitHub Actions](https://docs.github.com/en/actions)
- [Rust CI Best Practices](https://github.com/actions-rs)

---

**Updated:** 2025-10-13 | **Version:** 1.0.0
