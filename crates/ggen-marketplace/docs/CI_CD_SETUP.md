# CI/CD Pipeline Setup Guide

## Overview

The Ggen Marketplace uses a comprehensive GitHub Actions CI/CD pipeline to ensure code quality, test coverage, and production readiness across all feature combinations.

## Pipeline Architecture

### Build Matrix Strategy

The pipeline tests multiple feature combinations:
- **Default features** (minimal dependencies)
- **P2P feature** (distributed networking)
- **GraphQL feature** (API interface)
- **Crypto feature** (cryptographic signing)
- **All features** (complete functionality)

### Rust Version Matrix

Tests run on:
- `stable` - Current stable Rust release
- `beta` - Upcoming Rust release (early detection of breaking changes)

## Pipeline Jobs

### 1. Test Suite (`test`)

**Purpose:** Validate functionality across all feature combinations

**Matrix:**
```yaml
rust: [stable, beta]
features: ['', 'p2p', 'graphql', 'crypto', 'all']
```

**Steps:**
1. Checkout code
2. Install Rust toolchain
3. Cache dependencies
4. Build with specified features
5. Run tests

**Caching Strategy:**
- Cargo registry cached by `Cargo.lock` hash
- Cargo git index cached
- Build artifacts cached

### 2. Linting (`lint`)

**Purpose:** Enforce code quality standards

**Checks:**
- **Formatting:** `cargo fmt -- --check`
- **Clippy (default):** `cargo clippy -- -D warnings`
- **Clippy (all features):** `cargo clippy --all-features -- -D warnings`

**Note:** Clippy warnings are treated as errors (`-D warnings`)

### 3. Integration Tests (`integration-tests`)

**Purpose:** Verify cross-feature interactions

**Configuration:**
- Runs after unit tests pass
- Single-threaded execution (`--test-threads=1`)
- Tests all features together

**Environment:**
```bash
RUST_TEST_THREADS=1
```

### 4. P2P Smoke Test (`p2p-smoke-test`)

**Purpose:** Validate P2P networking functionality

**Trigger Conditions:**
- Commit message contains `[p2p]`
- Push to `main` branch

**Features:**
- 5-minute timeout
- Single-threaded test execution
- Isolated P2P tests only

**Example commit trigger:**
```bash
git commit -m "feat: add DHT routing [p2p]"
```

### 5. GraphQL Smoke Test (`graphql-smoke-test`)

**Purpose:** Validate GraphQL API functionality

**Trigger Conditions:**
- Commit message contains `[graphql]`
- Push to `main` branch

**Tests:**
- Schema validation
- Query/mutation functionality
- GraphQL-specific features

**Example commit trigger:**
```bash
git commit -m "feat: add marketplace queries [graphql]"
```

### 6. Security Audit (`security-audit`)

**Purpose:** Detect security vulnerabilities in dependencies

**Tool:** `cargo-audit`

**Runs:** On every push

**Action on failure:** Pipeline fails, security issues must be resolved

### 7. Code Coverage (`coverage`)

**Purpose:** Track test coverage metrics

**Tool:** `cargo-tarpaulin`

**Configuration:**
- All features enabled
- 5-minute timeout
- Generates XML report for Codecov

**Integration:**
- Uploads to Codecov automatically
- Non-blocking (fails are warnings only)

**Minimum threshold:** 80% coverage recommended

### 8. Build Verification (`build-verification`)

**Purpose:** Comprehensive build validation

**Script:** `./scripts/verify-build.sh`

**Checks:**
- ✓ Default features build
- ✓ P2P feature build
- ✓ GraphQL feature build
- ✓ Crypto feature build
- ✓ All features build
- ✓ Default tests pass
- ✓ All feature tests pass
- ✓ Code formatting
- ✓ Clippy checks
- ✓ Documentation builds

### 9. All Checks Pass (`all-checks-pass`)

**Purpose:** Final gate before merge

**Dependencies:** All previous jobs must succeed

**Use case:** Branch protection rule requirement

## Feature Flags

### Available Features

```toml
[features]
default = []
p2p = ["libp2p"]
graphql = ["async-graphql", "async-graphql-axum"]
graphql-server = ["graphql", "axum", "tower", "tower-http"]
crypto = ["ed25519-dalek", "rand"]
all = ["p2p", "graphql-server", "crypto"]
```

### Building with Features

```bash
# Default (minimal)
cargo build

# With P2P
cargo build --features p2p

# With GraphQL
cargo build --features graphql

# With crypto
cargo build --features crypto

# Everything
cargo build --all-features
```

### Testing with Features

```bash
# Test specific feature
cargo test --features p2p

# Test all features
cargo test --all-features

# Test without default features
cargo test --no-default-features --features "p2p,crypto"
```

## Local Development Workflow

### Before Committing

Run the build verification script:

```bash
chmod +x scripts/verify-build.sh
./scripts/verify-build.sh
```

### Quick Checks

```bash
# Format code
cargo fmt

# Run clippy
cargo clippy --all-features -- -D warnings

# Run tests
cargo test --all-features
```

### Pre-Push Checklist

- [ ] Code formatted: `cargo fmt`
- [ ] No clippy warnings: `cargo clippy --all-features`
- [ ] Tests pass: `cargo test --all-features`
- [ ] Documentation builds: `cargo doc --no-deps`
- [ ] Build verification passes: `./scripts/verify-build.sh`

## Triggering Specific Tests

### Force P2P Testing

Include `[p2p]` in commit message:
```bash
git commit -m "feat: improve peer discovery [p2p]"
```

### Force GraphQL Testing

Include `[graphql]` in commit message:
```bash
git commit -m "feat: add subscription support [graphql]"
```

### Force All Tests

Push to `main` branch or include both tags:
```bash
git commit -m "feat: major release [p2p] [graphql]"
```

## Branch Protection Rules

### Recommended Settings

For `main` branch:
- ✓ Require status checks to pass before merging
- ✓ Require branches to be up to date before merging
- ✓ Required checks:
  - `test (stable, all)`
  - `lint`
  - `integration-tests`
  - `build-verification`
  - `all-checks-pass`

### Pull Request Requirements

- At least one approval
- All CI checks must pass
- Merge commits preferred (avoid rebasing)

**Why Merge Commits Are Preferred:**

Merge commits are the recommended approach for integrating pull requests because they:

- **Preserve Complete History**: Merge commits maintain the full development timeline, showing exactly when features were developed and integrated. This historical accuracy is crucial for debugging, auditing, and understanding project evolution.

- **Maintain Accurate Timestamps**: Each commit retains its original timestamp and author information, providing an accurate record of when work was actually done. Rebasing rewrites these timestamps, making it difficult to understand the true development timeline.

- **Track Relationships**: Merge commits clearly show the relationship between feature branches and the main branch, making it easy to identify which commits belong to which feature and when they were integrated.

- **Enable Proper Conflict Resolution Tracking**: When conflicts occur, merge commits preserve the resolution process. This allows teams to understand how conflicts were resolved and learn from past decisions.

- **Support Collaborative Workflows**: Merge operations work seamlessly with multiple developers without requiring force-pushes. This eliminates the disruption that rebasing causes when team members have already pulled a branch.

- **Enable Better Debugging**: Stable commit references (SHAs) allow CI/CD pipelines, issue trackers, deployment systems, and debugging tools to reliably reference specific commits. Rebasing changes commit hashes, breaking these critical integrations.

**Why Rebasing Is an Anti-Pattern (Never Use It):**

Rebasing is an anti-pattern that should never be used because it:

- **Rewrites History**: Rewrites history, making it impossible to see the actual sequence of development. This destroys the historical accuracy that is essential for debugging, auditing, and understanding project evolution.

- **Requires Force-Push**: Requires force-pushes that disrupt all other developers working on the same branch. This creates a destructive workflow that breaks collaboration and forces team members to re-sync their work.

- **Breaks CI/CD**: Changes commit SHAs, breaking CI/CD pipelines, issue trackers, deployment systems, and all external references. This creates a cascade of failures across the entire development infrastructure.

- **Loses Context**: Loses merge context and conflict resolution history. Merge commits preserve the complete development timeline and show how conflicts were resolved, which is essential for learning and maintaining code quality.

- **Creates Unnecessary Conflicts**: Creates unnecessary conflicts when multiple developers collaborate on the same branch. Each developer must rebase their work on top of others' rebased commits, creating a conflict spiral that merging avoids entirely.

- **Makes Debugging Impossible**: Makes debugging harder by changing commit references that tools depend on. Stable commit SHAs allow debugging tools, CI/CD systems, and deployment pipelines to reliably reference specific commits. Rebasing breaks these critical integrations.

**Always use merge operations - never use rebase. Use `git pull` (which merges by default) instead of `git pull --rebase`.**

## Caching Strategy

### What Gets Cached

1. **Cargo Registry** (`~/.cargo/registry`)
   - Key: `${{ runner.os }}-cargo-registry-${{ hashFiles('**/Cargo.lock') }}`

2. **Cargo Git Index** (`~/.cargo/git`)
   - Key: `${{ runner.os }}-cargo-index-${{ hashFiles('**/Cargo.lock') }}`

3. **Build Artifacts** (`target/`)
   - Key: `${{ runner.os }}-cargo-build-target-${{ hashFiles('**/Cargo.lock') }}`

### Cache Invalidation

Caches automatically invalidate when:
- `Cargo.lock` changes
- 7 days have passed (GitHub Actions default)

## Performance Optimization

### Build Time Improvements

1. **Dependency caching** reduces build time by ~60%
2. **Incremental compilation** enabled by default
3. **Parallel test execution** when possible
4. **Matrix parallelization** runs feature combinations concurrently

### Expected Build Times

| Job | Time (cached) | Time (clean) |
|-----|---------------|--------------|
| Test (single feature) | ~2 min | ~8 min |
| Lint | ~1 min | ~6 min |
| Integration tests | ~3 min | ~10 min |
| Build verification | ~5 min | ~15 min |
| **Total pipeline** | ~8 min | ~25 min |

## Troubleshooting

### Common Issues

#### 1. Clippy Warnings

**Error:** `cargo clippy -- -D warnings` fails

**Solution:**
```bash
cargo clippy --all-features --fix
git commit -am "fix: resolve clippy warnings"
```

#### 2. Format Check Fails

**Error:** `cargo fmt -- --check` fails

**Solution:**
```bash
cargo fmt
git commit -am "style: format code"
```

#### 3. P2P Tests Timeout

**Error:** P2P smoke tests exceed 5-minute timeout

**Solution:**
- Check for infinite loops in P2P code
- Reduce test scope
- Increase timeout in workflow file

#### 4. Feature Compilation Fails

**Error:** Feature combination doesn't compile

**Solution:**
```bash
# Test locally first
cargo build --features "p2p,graphql"
cargo test --features "p2p,graphql"
```

#### 5. Cache Issues

**Error:** Stale or corrupted cache

**Solution:**
- Go to GitHub Actions > Caches
- Delete problematic cache
- Re-run workflow

## Security Considerations

### Secrets Management

- Never commit secrets to code
- Use GitHub Secrets for sensitive data
- Audit dependencies regularly with `cargo audit`

### Dependency Security

Pipeline automatically:
- Runs `cargo audit` on every push
- Fails build if critical vulnerabilities found
- Reports advisory database version

### Supply Chain Security

- Lock file committed (`Cargo.lock`)
- Reproducible builds guaranteed
- Dependency verification via checksums

## Monitoring and Alerts

### GitHub Actions Notifications

Configure notifications in GitHub Settings:
- Email on workflow failure
- Slack/Discord integration available
- Status badges for README

### Codecov Integration

Track coverage trends:
- Automatic coverage reports
- PR comments with coverage changes
- Configurable thresholds

### Status Badge

Add to README.md:
```markdown
![CI](https://github.com/your-org/ggen-marketplace/workflows/CI/badge.svg)
```

## Release Pipeline

### Versioning

Follow semantic versioning (SemVer):
- `MAJOR.MINOR.PATCH`
- Breaking changes increment MAJOR
- New features increment MINOR
- Bug fixes increment PATCH

### Release Process

1. Update version in `Cargo.toml`
2. Run `./scripts/verify-build.sh`
3. Update `CHANGELOG.md`
4. Create release tag: `git tag -a v1.0.0`
5. Push tag: `git push origin v1.0.0`
6. Publish to crates.io: `cargo publish`

### Automated Release (Future)

Consider setting up:
- Automated changelog generation
- GitHub Releases creation
- Crates.io publishing via CI

## Continuous Improvement

### Metrics to Track

- Build time trends
- Test coverage percentage
- Clippy warning frequency
- Security audit findings
- Cache hit rates

### Regular Reviews

- Monthly pipeline performance review
- Quarterly security audit
- Yearly dependency update cycle

## Resources

- [GitHub Actions Documentation](https://docs.github.com/en/actions)
- [Cargo Features Guide](https://doc.rust-lang.org/cargo/reference/features.html)
- [Rust CI/CD Best Practices](https://github.com/actions-rs)

---

**Last Updated:** 2025-10-13
**Pipeline Version:** 1.0.0
**Maintainer:** CI/CD Engineering Team
