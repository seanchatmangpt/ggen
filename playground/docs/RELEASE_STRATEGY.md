# HTF Playground Release Strategy

## Executive Summary

This document defines the comprehensive release strategy for the HTF (Hyper-Thesis Framework) playground, which serves as a reference implementation for clap-noun-verb. The strategy leverages the mature ggen infrastructure while establishing playground-specific processes.

**Current State:**
- Published crate: `clap-noun-verb` 5.1.0 on crates.io
- Playground version: `htf-cli` 0.1.0 (workspace member)
- Parent workspace: ggen 3.3.0

---

## 1. Versioning Strategy

### 1.1 Semantic Versioning (SemVer)

All versions follow [Semantic Versioning 2.0.0](https://semver.org/):

```
MAJOR.MINOR.PATCH[-PRERELEASE][+BUILD]

Examples:
- 1.0.0       - Initial stable release
- 1.1.0       - New features (backward compatible)
- 1.1.1       - Bug fixes
- 2.0.0       - Breaking changes
- 2.0.0-alpha.1 - Pre-release alpha
- 2.0.0-beta.1  - Pre-release beta
- 2.0.0-rc.1    - Release candidate
```

### 1.2 Version Components

| Component | Incremented When | Examples |
|-----------|-----------------|----------|
| **MAJOR** | Breaking changes to public API | Command structure changes, removed features |
| **MINOR** | New features (backward compatible) | New commands, new output formats |
| **PATCH** | Bug fixes, security patches | Error handling fixes, documentation |

### 1.3 Pre-release Identifiers

| Identifier | Purpose | Stability |
|------------|---------|-----------|
| `alpha.N` | Early testing, API may change | Unstable |
| `beta.N` | Feature complete, bug fixing | Semi-stable |
| `rc.N` | Release candidate, final testing | Stable |

### 1.4 Version Synchronization Strategy

```
Playground (htf-cli)    clap-noun-verb    ggen Workspace
-----------------       --------------    --------------
Independent             Independent       Independent
1.x.x                   5.x.x             3.x.x

Note: Playground follows clap-noun-verb major version compatibility:
- htf-cli 1.x requires clap-noun-verb ^5.0 || ^3.7
- Major version bump in clap-noun-verb MAY require playground major bump
```

### 1.5 Git Tag Format

```bash
# Format: v{VERSION}
v1.0.0              # Stable release
v1.1.0-alpha.1      # Pre-release
htf-v1.0.0          # Playground-specific tag (if needed)

# Lightweight tags for dev builds (not published)
dev-20251121-abc1234
```

---

## 2. Release Automation (CI/CD)

### 2.1 GitHub Actions Workflow Structure

```yaml
# .github/workflows/playground-release.yml
name: Playground Release

on:
  push:
    tags:
      - "htf-v*"
  workflow_dispatch:
    inputs:
      version:
        description: 'Version to release (e.g., 1.0.0)'
        required: true
      prerelease:
        description: 'Pre-release identifier (alpha.1, beta.1, rc.1)'
        required: false

env:
  CARGO_TERM_COLOR: always
  RUST_BACKTRACE: 1

concurrency:
  group: ${{ github.workflow }}-${{ github.ref }}
  cancel-in-progress: false

jobs:
  # Gate 1: Quality validation (reuse existing quality-gates.yml)
  quality-gates:
    uses: ./.github/workflows/quality-gates.yml

  # Gate 2: Security audit (reuse existing)
  security-audit:
    uses: ./.github/workflows/security-audit.yml

  # Gate 3: Playground-specific tests
  playground-tests:
    name: Playground Test Suite
    runs-on: ubuntu-latest
    timeout-minutes: 15
    steps:
      - uses: actions/checkout@v4
      - uses: dtolnay/rust-toolchain@stable
      - name: Run playground tests
        run: |
          cd playground
          cargo test --all-features
          cargo doc --no-deps

  # Gate 4: Build release artifacts
  build-release:
    name: Build Release (${{ matrix.target }})
    needs: [quality-gates, security-audit, playground-tests]
    runs-on: ${{ matrix.os }}
    strategy:
      fail-fast: false
      matrix:
        include:
          - target: x86_64-apple-darwin
            os: macos-latest
          - target: aarch64-apple-darwin
            os: macos-latest
          - target: x86_64-unknown-linux-gnu
            os: ubuntu-latest
          - target: aarch64-unknown-linux-gnu
            os: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - uses: dtolnay/rust-toolchain@stable
        with:
          targets: ${{ matrix.target }}
      - name: Build playground release
        run: |
          cd playground
          cargo build --release --target ${{ matrix.target }}
      - name: Create archive
        run: |
          mkdir -p release
          cp playground/target/${{ matrix.target }}/release/htf release/
          tar -czf htf-${{ matrix.target }}.tar.gz -C release htf
          sha256sum htf-${{ matrix.target }}.tar.gz > htf-${{ matrix.target }}.tar.gz.sha256

  # Gate 5: Create GitHub Release
  create-release:
    name: Create GitHub Release
    needs: [build-release]
    runs-on: ubuntu-latest
    steps:
      - uses: softprops/action-gh-release@v2
        with:
          files: |
            htf-*.tar.gz
            htf-*.tar.gz.sha256
          generate_release_notes: true
          prerelease: ${{ contains(github.ref, 'alpha') || contains(github.ref, 'beta') || contains(github.ref, 'rc') }}

  # Gate 6: Publish to crates.io (optional, if playground published separately)
  publish-crate:
    name: Publish to crates.io
    needs: [create-release]
    runs-on: ubuntu-latest
    if: "!contains(github.ref, 'alpha') && !contains(github.ref, 'beta')"
    steps:
      - uses: actions/checkout@v4
      - uses: dtolnay/rust-toolchain@stable
      - name: Publish htf-cli
        run: |
          cd playground
          cargo publish --token ${{ secrets.CARGO_REGISTRY_TOKEN }}
        env:
          CARGO_REGISTRY_TOKEN: ${{ secrets.CARGO_REGISTRY_TOKEN }}
```

### 2.2 Changelog Automation

**Tool: git-cliff**

```toml
# cliff.toml (playground-specific)
[changelog]
header = """
# Changelog
All notable changes to HTF Playground will be documented in this file.
"""
body = """
{% for group, commits in commits | group_by(attribute="group") %}
### {{ group | upper_first }}
{% for commit in commits %}
- {{ commit.message | upper_first }} ([{{ commit.id | truncate(length=7, end="") }}](https://github.com/seanchatmangpt/ggen/commit/{{ commit.id }}))
{% endfor %}
{% endfor %}
"""
footer = """
---
Generated by [git-cliff](https://github.com/orhun/git-cliff)
"""
trim = true

[git]
conventional_commits = true
filter_unconventional = true
commit_parsers = [
    { message = "^feat", group = "Features" },
    { message = "^fix", group = "Bug Fixes" },
    { message = "^doc", group = "Documentation" },
    { message = "^perf", group = "Performance" },
    { message = "^refactor", group = "Refactoring" },
    { message = "^style", group = "Style" },
    { message = "^test", group = "Testing" },
    { message = "^chore", group = "Miscellaneous" },
    { message = "^ci", group = "CI/CD" },
]
filter_commits = false
tag_pattern = "htf-v[0-9]*"
```

### 2.3 Release Automation Script

```bash
#!/bin/bash
# scripts/release-playground.sh
set -euo pipefail

VERSION=${1:-}
PRERELEASE=${2:-}

if [ -z "$VERSION" ]; then
    echo "Usage: $0 <version> [prerelease]"
    echo "Example: $0 1.0.0"
    echo "Example: $0 1.0.0 alpha.1"
    exit 1
fi

FULL_VERSION="$VERSION"
if [ -n "$PRERELEASE" ]; then
    FULL_VERSION="$VERSION-$PRERELEASE"
fi

echo "Releasing HTF Playground v$FULL_VERSION"
echo "=================================="

# 1. Update version in Cargo.toml
echo "Updating Cargo.toml..."
cd playground
sed -i.bak "s/^version = \".*\"/version = \"$FULL_VERSION\"/" Cargo.toml
rm Cargo.toml.bak

# 2. Update Cargo.lock
echo "Updating Cargo.lock..."
cargo update -p htf-cli

# 3. Run quality gates locally
echo "Running quality gates..."
cargo fmt --check
cargo clippy --all-features -- -D warnings
cargo test --all-features

# 4. Generate changelog
echo "Generating changelog..."
cd ..
git cliff --tag "htf-v$FULL_VERSION" --output playground/CHANGELOG.md

# 5. Commit and tag
echo "Creating commit and tag..."
git add playground/Cargo.toml playground/CHANGELOG.md Cargo.lock
git commit -m "chore(playground): release v$FULL_VERSION"
git tag -a "htf-v$FULL_VERSION" -m "HTF Playground v$FULL_VERSION"

echo ""
echo "Release prepared! To publish:"
echo "  git push origin master"
echo "  git push origin htf-v$FULL_VERSION"
```

---

## 3. Distribution Strategy

### 3.1 Distribution Channels

| Channel | Target Audience | Update Frequency | Priority |
|---------|----------------|------------------|----------|
| **crates.io** | Rust developers | Every release | Primary |
| **GitHub Releases** | All users | Every release | Primary |
| **Homebrew** | macOS users | Stable releases | Secondary |
| **Binary releases** | CI/CD pipelines | Every release | Primary |

### 3.2 crates.io Publishing

**Publishing Checklist:**

```markdown
- [ ] Version bumped in Cargo.toml
- [ ] Cargo.toml metadata complete (description, license, repository)
- [ ] README.md is current
- [ ] All dependencies use crates.io versions (no git/path deps)
- [ ] `cargo publish --dry-run` succeeds
- [ ] Documentation builds: `cargo doc --no-deps`
```

**Cargo.toml Requirements:**

```toml
[package]
name = "htf-cli"
version = "1.0.0"
edition = "2021"
authors = ["Sean Chatman <sean@chatmangpt.com>"]
description = "Hyper-Thesis Framework: RDF-backed thesis planning with scheduling, profiling, and checking"
license = "MIT"
repository = "https://github.com/seanchatmangpt/ggen"
homepage = "https://github.com/seanchatmangpt/ggen/tree/master/playground"
documentation = "https://docs.rs/htf-cli"
readme = "README.md"
keywords = ["thesis", "planning", "rdf", "cli", "scheduling"]
categories = ["command-line-utilities", "development-tools"]
exclude = [
    "docs/",
    "examples/",
    "*.log",
]
```

### 3.3 GitHub Releases

**Release Assets:**

```
htf-v1.0.0/
  htf-x86_64-apple-darwin.tar.gz          # macOS Intel
  htf-x86_64-apple-darwin.tar.gz.sha256
  htf-aarch64-apple-darwin.tar.gz         # macOS Apple Silicon
  htf-aarch64-apple-darwin.tar.gz.sha256
  htf-x86_64-unknown-linux-gnu.tar.gz     # Linux x64
  htf-x86_64-unknown-linux-gnu.tar.gz.sha256
  htf-aarch64-unknown-linux-gnu.tar.gz    # Linux ARM64
  htf-aarch64-unknown-linux-gnu.tar.gz.sha256
  CHANGELOG.md                             # Release changelog
  LICENSE                                  # License file
```

### 3.4 Homebrew Distribution

**Formula Template:**

```ruby
# Formula/htf.rb
class Htf < Formula
  desc "Hyper-Thesis Framework: RDF-backed thesis planning CLI"
  homepage "https://github.com/seanchatmangpt/ggen/tree/master/playground"
  license "MIT"
  version "1.0.0"

  on_macos do
    if Hardware::CPU.arm?
      url "https://github.com/seanchatmangpt/ggen/releases/download/htf-v1.0.0/htf-aarch64-apple-darwin.tar.gz"
      sha256 "PLACEHOLDER_SHA256"
    else
      url "https://github.com/seanchatmangpt/ggen/releases/download/htf-v1.0.0/htf-x86_64-apple-darwin.tar.gz"
      sha256 "PLACEHOLDER_SHA256"
    end
  end

  on_linux do
    if Hardware::CPU.arm?
      url "https://github.com/seanchatmangpt/ggen/releases/download/htf-v1.0.0/htf-aarch64-unknown-linux-gnu.tar.gz"
      sha256 "PLACEHOLDER_SHA256"
    else
      url "https://github.com/seanchatmangpt/ggen/releases/download/htf-v1.0.0/htf-x86_64-unknown-linux-gnu.tar.gz"
      sha256 "PLACEHOLDER_SHA256"
    end
  end

  def install
    bin.install "htf"
  end

  test do
    assert_match version.to_s, shell_output("#{bin}/htf --version")
  end
end
```

---

## 4. Release Process

### 4.1 Release Types

| Type | Cadence | Approval | Testing |
|------|---------|----------|---------|
| **Major (X.0.0)** | As needed | Team review + stakeholders | Full regression |
| **Minor (x.X.0)** | Monthly | Team review | Integration + E2E |
| **Patch (x.x.X)** | Weekly | Single approver | Unit + affected areas |
| **Hotfix** | Immediate | Emergency approval | Minimal targeted |

### 4.2 Release Checklist

```markdown
## Pre-Release Checklist

### Code Quality
- [ ] All tests pass (`cargo test --workspace`)
- [ ] Clippy clean (`cargo clippy -- -D warnings`)
- [ ] Format clean (`cargo fmt --check`)
- [ ] No panic points in production code
- [ ] Documentation compiles (`cargo doc --no-deps`)

### Version Management
- [ ] Version bumped in Cargo.toml
- [ ] CHANGELOG.md updated
- [ ] README.md updated if needed
- [ ] Breaking changes documented

### Security
- [ ] `cargo audit` passes (no critical vulnerabilities)
- [ ] `cargo deny check` passes
- [ ] Dependencies reviewed for updates

### Testing
- [ ] Unit tests pass
- [ ] Integration tests pass
- [ ] E2E tests pass
- [ ] Performance benchmarks within SLOs
- [ ] Manual smoke test completed

### Release
- [ ] Git tag created and pushed
- [ ] GitHub Release created
- [ ] crates.io published
- [ ] Homebrew formula updated (if applicable)
- [ ] Release announcement drafted
```

### 4.3 Release Workflow Diagram

```
                                    START
                                      |
                                      v
                            +------------------+
                            | Feature Complete |
                            +--------+---------+
                                     |
                                     v
                            +------------------+
                            | Quality Gates    |
                            | (CI/CD Pipeline) |
                            +--------+---------+
                                     |
                        +------------+------------+
                        |                         |
                        v                         v
                   +--------+              +-------------+
                   | PASS   |              | FAIL        |
                   +----+---+              +------+------+
                        |                         |
                        v                         v
              +------------------+         +-------------+
              | Version Bump     |         | Fix Issues  |
              | Changelog Update |         | Re-submit   |
              +--------+---------+         +------+------+
                       |                         |
                       v                         |
              +------------------+               |
              | Create PR        |<--------------+
              +--------+---------+
                       |
                       v
              +------------------+
              | Review & Approve |
              +--------+---------+
                       |
                       v
              +------------------+
              | Merge to Master  |
              +--------+---------+
                       |
                       v
              +------------------+
              | Create Git Tag   |
              +--------+---------+
                       |
                       v
              +------------------+
              | Build Artifacts  |
              | (Multi-platform) |
              +--------+---------+
                       |
           +-----------+-----------+
           |           |           |
           v           v           v
      +--------+  +--------+  +--------+
      | GitHub |  | crates |  | Brew   |
      | Release|  | .io    |  | Formula|
      +--------+  +--------+  +--------+
                       |
                       v
              +------------------+
              | Post-Release     |
              | Monitoring       |
              +--------+---------+
                       |
                       v
                     END
```

### 4.4 Staging Process

**Staging Environments:**

```
Development -> Staging -> Production

Development:
  - Branch: feature/* or dev/*
  - Build: Debug mode
  - Testing: Unit tests only
  - Duration: As needed

Staging:
  - Branch: release/*
  - Build: Release mode
  - Testing: Full test suite + E2E
  - Duration: 1-3 days
  - Artifacts: Pre-release to GitHub (alpha/beta)

Production:
  - Branch: master (via merge)
  - Build: Release mode + LTO
  - Testing: Smoke tests
  - Artifacts: Full distribution
```

### 4.5 Rollback Strategy

**Rollback Triggers:**
- Critical runtime errors in production
- Security vulnerabilities discovered post-release
- Significant performance regression
- Data corruption risk

**Rollback Procedure:**

```bash
#!/bin/bash
# scripts/rollback-playground.sh
set -euo pipefail

ROLLBACK_VERSION=${1:-}

if [ -z "$ROLLBACK_VERSION" ]; then
    echo "Usage: $0 <version-to-rollback-to>"
    echo "Example: $0 1.0.0"
    exit 1
fi

echo "Rolling back to v$ROLLBACK_VERSION"
echo "================================="

# 1. Verify the version exists
if ! git rev-parse "htf-v$ROLLBACK_VERSION" >/dev/null 2>&1; then
    echo "Error: Tag htf-v$ROLLBACK_VERSION not found"
    exit 1
fi

# 2. Create rollback tag
CURRENT_DATE=$(date +%Y%m%d%H%M%S)
ROLLBACK_TAG="htf-v$ROLLBACK_VERSION-rollback-$CURRENT_DATE"

# 3. Yank problematic version from crates.io (if applicable)
# cargo yank --version $PROBLEMATIC_VERSION htf-cli

# 4. Create GitHub issue documenting rollback
echo "Creating rollback documentation..."

# 5. Notify team
echo ""
echo "ROLLBACK COMPLETE"
echo "================="
echo "Rolled back to: htf-v$ROLLBACK_VERSION"
echo "Action items:"
echo "  1. Communicate to users via announcement"
echo "  2. Create post-mortem document"
echo "  3. Fix issues in new version"
```

**Rollback Communication Template:**

```markdown
## Service Disruption Notice

**Date:** [DATE]
**Affected Version:** htf-cli [VERSION]
**Status:** Rolled back to [PREVIOUS_VERSION]

### Summary
A critical issue was discovered in htf-cli [VERSION] that [BRIEF DESCRIPTION].

### Impact
[DESCRIBE USER IMPACT]

### Resolution
We have rolled back to the previous stable version [PREVIOUS_VERSION].

### Timeline
- [TIME] - Issue discovered
- [TIME] - Investigation started
- [TIME] - Rollback decision made
- [TIME] - Rollback completed

### Next Steps
- Post-mortem scheduled for [DATE]
- Fixed version ETA: [DATE]

### Workaround
[IF APPLICABLE]

For questions, please open an issue at [REPO_URL].
```

---

## 5. Deployment Environments

### 5.1 Environment Matrix

| Environment | Purpose | Branch | Artifacts | Audience |
|-------------|---------|--------|-----------|----------|
| **Development** | Active development | feature/*, dev/* | Debug builds | Developers |
| **CI** | Continuous integration | All branches | Test builds | Automated |
| **Staging** | Pre-release validation | release/* | Pre-release | QA, Beta testers |
| **Production** | General availability | master + tags | Release builds | All users |

### 5.2 Environment Configuration

**Development:**
```toml
[profile.dev]
opt-level = 0
debug = true
incremental = true
```

**Staging:**
```toml
[profile.release]
opt-level = 2
debug = true  # Keep debug info for troubleshooting
lto = "thin"
```

**Production:**
```toml
[profile.release]
opt-level = 3
debug = false
lto = true
strip = true
codegen-units = 1
```

### 5.3 Feature Flags

```rust
// Compile-time feature flags
#[cfg(feature = "dev")]
fn enable_dev_features() {
    // Additional logging, debug commands
}

#[cfg(feature = "telemetry")]
fn enable_telemetry() {
    // OpenTelemetry integration
}
```

---

## 6. Monitoring and Observability

### 6.1 Post-Release Monitoring

**Metrics to Track:**

| Metric | Source | Alert Threshold |
|--------|--------|-----------------|
| Download count | crates.io, GitHub | Baseline -50% |
| Error reports | GitHub Issues | >5 new in 24h |
| Build failures | CI logs | Any critical |
| Performance | Benchmarks | >10% regression |

### 6.2 Health Check Endpoints

For CLI tools, health is measured by:

```bash
# Basic health check
htf --version  # Should return version quickly

# Feature health check
htf schedule --help  # Should return help text

# Data health check
htf check --dry-run  # Should validate without modifying
```

### 6.3 OpenTelemetry Integration

```rust
// Optional telemetry for debugging production issues
#[cfg(feature = "telemetry")]
use opentelemetry::trace::Tracer;

#[instrument]
pub async fn execute_command(cmd: &Command) -> Result<()> {
    info!(command = %cmd.name(), "Executing command");
    // Command execution
}
```

### 6.4 Monitoring Dashboard

```
+------------------------------------------------------------------+
|                    HTF Release Monitoring                         |
+------------------------------------------------------------------+
|                                                                  |
|  Downloads (24h)        Issues (7d)          Build Status        |
|  +----------------+     +----------------+   +----------------+   |
|  |  crates.io:    |     | Open:     5    |   | master: PASS   |   |
|  |    152 (+12%)  |     | Closed:  12    |   | release: PASS  |   |
|  |  GitHub:       |     | Critical: 0    |   | staging: PASS  |   |
|  |    89 (+8%)    |     +----------------+   +----------------+   |
|  +----------------+                                              |
|                                                                  |
|  Version Distribution    Performance (ms)   Security            |
|  +----------------+     +----------------+   +----------------+   |
|  | 1.0.0:  45%    |     | p50:   12      |   | Audit: PASS    |   |
|  | 0.9.0:  30%    |     | p95:   45      |   | CVEs:  0       |   |
|  | 0.8.x:  25%    |     | p99:   120     |   | Updates: 2     |   |
|  +----------------+     +----------------+   +----------------+   |
|                                                                  |
+------------------------------------------------------------------+
```

---

## 7. Hotfix and Patch Process

### 7.1 Hotfix Classification

| Severity | Response Time | Approval | Example |
|----------|--------------|----------|---------|
| **Critical (P0)** | <4 hours | Emergency | Security vulnerability, data loss |
| **High (P1)** | <24 hours | Fast-track | Major feature broken |
| **Medium (P2)** | <72 hours | Normal | Minor feature broken |
| **Low (P3)** | Next release | Normal | Cosmetic issues |

### 7.2 Hotfix Workflow

```
Issue Reported
      |
      v
+----------------+
| Triage & Classify |
+--------+-------+
         |
   +-----+-----+
   |           |
   v           v
P0/P1       P2/P3
   |           |
   v           v
Hotfix     Normal
Branch     Release
   |           |
   v           |
+----------+   |
| Fix &    |   |
| Test     |   |
+----+-----+   |
     |         |
     v         |
+----------+   |
| Review   |   |
| (Fast)   |   |
+----+-----+   |
     |         |
     v         |
+----------+   |
| Merge to |   |
| Master   |   |
+----+-----+   |
     |         |
     v         |
+----------+   |
| Tag &    |   |
| Release  |   |
+----+-----+   |
     |         |
     v         v
+------------------+
| Backport to      |
| Active Branches  |
+------------------+
```

### 7.3 Hotfix Script

```bash
#!/bin/bash
# scripts/hotfix-playground.sh
set -euo pipefail

ISSUE_NUMBER=${1:-}
SEVERITY=${2:-P1}

if [ -z "$ISSUE_NUMBER" ]; then
    echo "Usage: $0 <issue-number> [severity]"
    echo "Example: $0 123 P0"
    exit 1
fi

# 1. Create hotfix branch from latest tag
LATEST_TAG=$(git describe --tags --abbrev=0)
BRANCH_NAME="hotfix/$ISSUE_NUMBER"

git checkout -b "$BRANCH_NAME" "$LATEST_TAG"

echo "Hotfix branch created: $BRANCH_NAME"
echo ""
echo "Next steps:"
echo "  1. Fix the issue"
echo "  2. Run: cargo test"
echo "  3. Commit with: fix(playground): [description] (fixes #$ISSUE_NUMBER)"
echo "  4. Push and create PR"
echo "  5. After merge, run: ./scripts/release-playground.sh X.X.X"
```

---

## 8. Dependency Management

### 8.1 Dependency Update Strategy

| Category | Update Frequency | Approval | Testing |
|----------|-----------------|----------|---------|
| **Security fixes** | Immediate | Fast-track | Targeted |
| **Major versions** | Quarterly review | Team | Full regression |
| **Minor versions** | Monthly | Single | Integration |
| **Patch versions** | Weekly (automated) | Automated | Unit |

### 8.2 Dependabot Configuration

```yaml
# .github/dependabot.yml (playground section)
version: 2
updates:
  - package-ecosystem: "cargo"
    directory: "/playground"
    schedule:
      interval: "weekly"
      day: "monday"
    open-pull-requests-limit: 5
    groups:
      rust-minor:
        patterns:
          - "*"
        update-types:
          - "minor"
          - "patch"
    ignore:
      # Ignore major version updates (manual review)
      - dependency-name: "clap"
        update-types: ["version-update:semver-major"]
      - dependency-name: "clap-noun-verb"
        update-types: ["version-update:semver-major"]
```

### 8.3 Dependency Audit

```bash
#!/bin/bash
# scripts/audit-dependencies.sh
set -euo pipefail

echo "HTF Playground Dependency Audit"
echo "================================"

cd playground

# 1. Security vulnerabilities
echo ""
echo "Security Audit:"
cargo audit

# 2. License compliance
echo ""
echo "License Check:"
cargo deny check licenses

# 3. Outdated dependencies
echo ""
echo "Outdated Dependencies:"
cargo outdated

# 4. Unused dependencies
echo ""
echo "Unused Dependencies:"
cargo +nightly udeps --all-targets 2>/dev/null || echo "Install cargo-udeps for unused dep detection"

# 5. Duplicate dependencies
echo ""
echo "Duplicate Dependencies:"
cargo tree -d

echo ""
echo "Audit complete!"
```

### 8.4 Major Dependency Update Process

```markdown
## Major Dependency Update Checklist

### Preparation
- [ ] Review changelog for breaking changes
- [ ] Check compatibility with other dependencies
- [ ] Review migration guide (if available)
- [ ] Estimate effort for migration

### Implementation
- [ ] Create feature branch: `deps/update-[name]-[version]`
- [ ] Update Cargo.toml
- [ ] Fix compilation errors
- [ ] Update code for API changes
- [ ] Update tests

### Validation
- [ ] All tests pass
- [ ] No new clippy warnings
- [ ] Performance benchmarks within tolerance
- [ ] Manual testing of affected features

### Documentation
- [ ] Update CHANGELOG.md
- [ ] Document migration notes (if user-facing)
- [ ] Update README if needed
```

---

## 9. Documentation Publishing

### 9.1 Documentation Types

| Type | Location | Update Trigger |
|------|----------|----------------|
| **API Docs** | docs.rs | Every crates.io publish |
| **User Guide** | playground/README.md | Every release |
| **Architecture** | playground/docs/ARCHITECTURE.md | Major releases |
| **Changelog** | playground/CHANGELOG.md | Every release |

### 9.2 docs.rs Configuration

```toml
# Cargo.toml
[package.metadata.docs.rs]
all-features = true
rustdoc-args = ["--cfg", "docsrs"]
```

### 9.3 Release Notes Template

```markdown
# HTF CLI v[VERSION]

## Highlights

- **[FEATURE]**: [Brief description]
- **[FEATURE]**: [Brief description]

## What's Changed

### Features
- [Feature description] (#[PR_NUMBER])

### Bug Fixes
- [Fix description] (#[PR_NUMBER])

### Performance
- [Improvement description] (#[PR_NUMBER])

### Documentation
- [Doc update description] (#[PR_NUMBER])

## Breaking Changes

[If any, describe migration steps]

## Dependency Updates

| Dependency | Previous | New |
|------------|----------|-----|
| clap-noun-verb | 3.7 | 5.1 |

## Installation

```bash
# From crates.io
cargo install htf-cli

# From Homebrew
brew install seanchatmangpt/tap/htf

# From source
git clone https://github.com/seanchatmangpt/ggen
cd ggen/playground
cargo install --path .
```

## Full Changelog

[Link to CHANGELOG.md]

## Contributors

@[username1], @[username2]
```

---

## 10. Communication and Announcements

### 10.1 Communication Channels

| Channel | Audience | Content | Timing |
|---------|----------|---------|--------|
| **GitHub Releases** | Users, Developers | Full release notes | Every release |
| **README Badge** | Visitors | Current version | Automatic |
| **CHANGELOG** | Developers | Detailed changes | Every release |
| **Twitter/X** | Community | Highlights | Major releases |
| **Discord/Slack** | Community | Discussion | Major releases |

### 10.2 Release Announcement Template

```markdown
## HTF CLI [VERSION] Released!

We're excited to announce the release of HTF CLI [VERSION]!

### Highlights

- [Highlight 1]
- [Highlight 2]
- [Highlight 3]

### Install/Upgrade

```bash
cargo install htf-cli
```

### Links

- Release notes: [URL]
- Documentation: [URL]
- Issues: [URL]

Thanks to all contributors!

#RustLang #CLI #OpenSource
```

### 10.3 Version Badge

Add to README.md:

```markdown
[![Crates.io](https://img.shields.io/crates/v/htf-cli.svg)](https://crates.io/crates/htf-cli)
[![Documentation](https://docs.rs/htf-cli/badge.svg)](https://docs.rs/htf-cli)
[![License](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)
```

---

## 11. Support and Lifecycle Policy

### 11.1 Version Support Matrix

| Version | Status | Support End | Notes |
|---------|--------|-------------|-------|
| 1.x (current) | Active | TBD | Full support |
| 0.x | EOL | Release of 1.0 | Security only until EOL |

### 11.2 Support Levels

| Level | Description | Scope |
|-------|-------------|-------|
| **Active** | Full support | Features, bugs, security, docs |
| **Maintenance** | Limited support | Security fixes only |
| **EOL** | No support | No updates |

### 11.3 LTS Policy (Future)

For projects requiring long-term stability:

- LTS releases every 12 months
- LTS support for 24 months
- Security patches backported to LTS
- Breaking changes only in major versions

---

## 12. Automation Tools Summary

### 12.1 Required Tools

| Tool | Purpose | Installation |
|------|---------|--------------|
| **cargo-release** | Automate version bumps | `cargo install cargo-release` |
| **git-cliff** | Generate changelogs | `cargo install git-cliff` |
| **cargo-audit** | Security scanning | `cargo install cargo-audit` |
| **cargo-deny** | License/dependency checks | `cargo install cargo-deny` |
| **cargo-outdated** | Dependency freshness | `cargo install cargo-outdated` |

### 12.2 GitHub Actions Used

| Workflow | Purpose |
|----------|---------|
| `quality-gates.yml` | Code quality enforcement |
| `security-audit.yml` | Security vulnerability scanning |
| `release.yml` | Build and publish releases |
| `homebrew-release.yml` | Update Homebrew formula |

### 12.3 Local Development Commands

```bash
# Pre-release checks
cargo make pre-commit     # Format, lint, test
cargo make audit          # Security check
cargo make slo-check      # Performance validation

# Release preparation
./scripts/release-playground.sh 1.0.0
./scripts/release-playground.sh 1.0.0 alpha.1

# Post-release
./scripts/rollback-playground.sh 0.9.0  # If needed
```

---

## Appendix A: Quick Reference

### Release Command Cheat Sheet

```bash
# Check readiness
cargo test --all-features
cargo clippy -- -D warnings
cargo audit
cargo deny check

# Create release
./scripts/release-playground.sh 1.0.0

# Push release
git push origin master
git push origin htf-v1.0.0

# Publish to crates.io
cd playground && cargo publish

# Rollback if needed
./scripts/rollback-playground.sh 0.9.0
```

### Version Increment Guide

```
Current: 1.2.3

Bug fix only:           1.2.4
New feature:            1.3.0
Breaking change:        2.0.0
Pre-release alpha:      1.3.0-alpha.1
Pre-release beta:       1.3.0-beta.1
Release candidate:      1.3.0-rc.1
```

---

*Document Version: 1.0.0*
*Last Updated: 2025-11-21*
*Author: DevOps/Release Specialist*
