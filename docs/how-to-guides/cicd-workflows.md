<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [CI/CD Workflows Guide](#cicd-workflows-guide)
  - [Overview](#overview)
  - [Workflows](#workflows)
    - [Core Development Workflows](#core-development-workflows)
      - [CI (Continuous Integration)](#ci-continuous-integration)
      - [Build](#build)
      - [Test](#test)
      - [Lint](#lint)
    - [Release & Distribution Workflows](#release--distribution-workflows)
      - [Release](#release)
      - [Homebrew Release](#homebrew-release)
      - [Publish Registry](#publish-registry)
    - [Marketplace & Documentation Workflows](#marketplace--documentation-workflows)
      - [Marketplace](#marketplace)
      - [Marketplace Test](#marketplace-test)
      - [Marketplace Docs](#marketplace-docs)
      - [Deploy Docs](#deploy-docs)
      - [Docker](#docker)
    - [Testing & Quality Workflows](#testing--quality-workflows)
      - [London TDD Tests](#london-tdd-tests)
      - [Security Audit](#security-audit)
    - [Deprecated/Maintenance Workflows](#deprecatedmaintenance-workflows)
      - [P2P Marketplace CI (DEPRECATED)](#p2p-marketplace-ci-deprecated)
      - [P2P Release (DEPRECATED)](#p2p-release-deprecated)
  - [Using Workflows](#using-workflows)
    - [Triggering Workflows Manually](#triggering-workflows-manually)
    - [Monitoring Workflow Status](#monitoring-workflow-status)
    - [Creating Releases](#creating-releases)
    - [Workflow Concurrency](#workflow-concurrency)
  - [Environment Variables](#environment-variables)
  - [Secrets & Credentials](#secrets--credentials)
  - [Troubleshooting](#troubleshooting)
    - [CI Workflow Failing](#ci-workflow-failing)
    - [Release Workflow Failing](#release-workflow-failing)
    - [Marketplace Deployment Not Working](#marketplace-deployment-not-working)
    - [Docker Build Failing](#docker-build-failing)
  - [Best Practices](#best-practices)
    - [For Developers](#for-developers)
    - [For Release Management](#for-release-management)
    - [For CI Maintenance](#for-ci-maintenance)
  - [Related Documentation](#related-documentation)
  - [Quick Links](#quick-links)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# CI/CD Workflows Guide

This guide explains ggen's GitHub Actions workflows and how they automate build, test, and deployment processes.

## Overview

ggen uses GitHub Actions to provide:
- **Continuous Integration**: Automated testing and linting on every push and PR
- **Continuous Deployment**: Automatic releases to Homebrew and crates.io
- **Marketplace Deployment**: Automatic deployment to GitHub Pages
- **Security**: Automated security audits and vulnerability scanning
- **Documentation**: Automated documentation building and deployment

## Workflows

### Core Development Workflows

#### CI (Continuous Integration)
**File**: `.github/workflows/ci.yml`
**Triggers**: Push to `master`, Pull Requests
**Purpose**: Comprehensive testing and validation

**Checks**:
- File organization (no Rust files in root)
- Clippy warnings (linting)
- Test suite execution
- Documentation tests
- Build verification

**Run time**: ~10-15 minutes

**Status**: Check GitHub Actions tab

#### Build
**File**: `.github/workflows/build.yml`
**Triggers**: Push to `master`
**Purpose**: Compile release binaries

**Creates**:
- Release binary for Linux
- Architecture-specific builds

#### Test
**File**: `.github/workflows/test.yml`
**Triggers**: Push to `master`, Pull Requests
**Purpose**: Run all test suites

**Runs**:
- Unit tests
- Integration tests
- E2E tests (Chicago TDD framework)
- Property-based tests

#### Lint
**File**: `.github/workflows/lint.yml`
**Triggers**: Push to `main`, Pull Requests
**Purpose**: Code quality checks

**Includes**:
- Clippy warnings
- Format check (rustfmt)
- Dependency audit (deny.rs)
- Security checks

### Release & Distribution Workflows

#### Release
**File**: `.github/workflows/release.yml`
**Triggers**: Tag creation (GitHub Release published)
**Purpose**: Create GitHub releases and update version

**Steps**:
1. Extract version from tag
2. Validate Cargo.toml matches tag
3. Create GitHub release
4. Upload artifacts
5. Update changelog

**Version format**: `v2.6.0` (tag) → `2.6.0` (version)

#### Homebrew Release
**File**: `.github/workflows/homebrew-release.yml`
**Triggers**: Release published
**Purpose**: Update Homebrew formula

**Steps**:
1. Extract version from release tag
2. Validate version in Cargo.toml
3. Generate SHA256 hash of tarball
4. Update formula in `seanchatmangpt/tap`
5. Create PR in Homebrew tap repository

**Result**: `brew install ggen` installs latest version

**Timing**: 2-5 minutes after release published

#### Publish Registry
**File**: `.github/workflows/publish-registry.yml`
**Triggers**: Push to `main`
**Purpose**: Update crates.io registry

**Steps**:
1. Extract version from Cargo.toml
2. Check if version exists on crates.io
3. Publish to crates.io if new version
4. Create release notes

**Result**: New version available via `cargo install ggen`

### Marketplace & Documentation Workflows

#### Marketplace
**File**: `.github/workflows/marketplace.yml`
**Triggers**: Changes to `marketplace/**`, Manual trigger
**Purpose**: Deploy marketplace to GitHub Pages

**Deploys**:
- Marketplace homepage
- Package registry (packages.toml)
- Package documentation
- Search index

**Deployed to**: `https://seanchatmangpt.github.io/ggen/marketplace/`

**Deploy time**: 2-3 minutes

#### Marketplace Test
**File**: `.github/workflows/marketplace-test.yml`
**Triggers**: Changes to `marketplace/**`
**Purpose**: Validate marketplace packages

**Checks**:
- Registry format validation
- Package metadata validation
- Signature verification
- Package integrity

#### Marketplace Docs
**File**: `.github/workflows/marketplace-docs.yml`
**Triggers**: Changes to marketplace docs
**Purpose**: Build and deploy marketplace documentation

#### Deploy Docs
**File**: `.github/workflows/deploy-docs.yml`
**Triggers**: Changes to `docs/**`, `README.md`
**Purpose**: Build and deploy documentation site

**Uses**: mdBook (configured in `docs/book.toml`)
**Deployed to**: GitHub Pages documentation site
**Deploy time**: 2-3 minutes

#### Docker
**File**: `.github/workflows/docker.yml`
**Triggers**: Push to `main`, Release published
**Purpose**: Build Docker images

**Builds**:
- Development image
- Production image
- Latest tag
- Version tag

**Registries**: Docker Hub, GitHub Container Registry

### Testing & Quality Workflows

#### London TDD Tests
**File**: `.github/workflows/london-tdd-tests.yml`
**Triggers**: Pull requests
**Purpose**: Run London School TDD tests

**Framework**: Chicago TDD
**Test type**: Mock-driven development tests
**Coverage requirement**: >85% on critical paths

#### Security Audit
**File**: `.github/workflows/security-audit.yml`
**Triggers**: Daily schedule, Manual trigger
**Purpose**: Security vulnerability scanning

**Scans**:
- Dependency vulnerabilities (Cargo audit)
- License compliance
- Code security issues
- FMEA analysis

### Deprecated/Maintenance Workflows

> ⚠️ **Note**: The following workflows reference P2P marketplace functionality that was removed in v2.6.0 and are deprecated:

#### P2P Marketplace CI (DEPRECATED)
**File**: `.github/workflows/p2p-marketplace-ci.yml` ⚠️ **OBSOLETE - REMOVED**
**Status**: Deprecated (P2P removed in v2.6.0)
**Action**: Consider for removal

#### P2P Release (DEPRECATED)
**File**: `.github/workflows/p2p-release.yml` ⚠️ **OBSOLETE - REMOVED**
**Status**: Deprecated (P2P removed in v2.6.0)
**Action**: Consider for removal

## Using Workflows

### Triggering Workflows Manually

Some workflows support manual triggering:

```bash
# View workflows in GitHub
https://github.com/seanchatmangpt/ggen/actions

# Manually trigger marketplace deployment
1. Go to Actions tab
2. Select "Deploy Marketplace to GitHub Pages"
3. Click "Run workflow"
```

### Monitoring Workflow Status

```bash
# Check all workflows
https://github.com/seanchatmangpt/ggen/actions

# View specific workflow
https://github.com/seanchatmangpt/ggen/actions/workflows/ci.yml

# Check workflow runs
- Green checkmark = Success
- Red X = Failed
- Yellow circle = In progress
```

### Creating Releases

```bash
# 1. Create a git tag (triggers Release workflow)
git tag v2.7.0
git push origin v2.7.0

# 2. Create GitHub release (if not auto-created)
# Go to https://github.com/seanchatmangpt/ggen/releases
# Create release from tag

# 3. Workflows automatically:
# - Create GitHub release
# - Update Homebrew formula
# - Publish to crates.io
# - Deploy marketplace
# - Update documentation
```

### Workflow Concurrency

Workflows use concurrency groups to prevent simultaneous runs of the same workflow:

```yaml
concurrency:
  group: ${{ github.workflow }}-${{ github.ref }}
  cancel-in-progress: true
```

This means:
- Only one CI run per branch
- New pushes cancel previous runs
- Prevents wasted resources

## Environment Variables

Workflows use these environment variables:

```bash
CARGO_TERM_COLOR: always      # Colored cargo output
RUST_BACKTRACE: 1             # Better error traces
```

## Secrets & Credentials

Workflows use these secrets (configured in GitHub):

- `CRATES_IO_TOKEN` - For publishing to crates.io
- `HOMEBREW_GITHUB_TOKEN` - For updating Homebrew tap
- Docker registry credentials (for Docker builds)

> ⚠️ **Note**: Credentials are only available to approved workflows and protected branches.

## Troubleshooting

### CI Workflow Failing

**Check**:
1. View workflow logs in Actions tab
2. Common issues:
   - Clippy warnings (fix warnings or update clippy config)
   - Test failures (run tests locally: `cargo test`)
   - File organization violations
   - Formatting issues (run `cargo fmt`)

**Fix**:
```bash
# Run CI checks locally
cargo fmt
cargo clippy
cargo test
cargo build
```

### Release Workflow Failing

**Check**:
1. Version in tag matches Cargo.toml
2. Tag format is `vX.Y.Z`
3. Release is published (not draft)

**Fix**:
```bash
# Verify version
cat Cargo.toml | grep "^version"

# Verify tag
git tag -l | grep "v2.6.0"
```

### Marketplace Deployment Not Working

**Check**:
1. Changes pushed to `marketplace/**`
2. Workflow shows green checkmark
3. GitHub Pages is enabled (Settings → Pages → Deploy from branch: gh-pages)

**Fix**:
```bash
# Check gh-pages branch
git fetch origin
git branch -r | grep gh-pages

# Verify deployed files
curl https://seanchatmangpt.github.io/ggen/marketplace/

# Trigger manually
# Go to Actions → Marketplace workflow → Run workflow
```

### Docker Build Failing

**Check**:
1. Docker is available on runner
2. Registry credentials are set in secrets
3. Dockerfile is valid

**Fix**:
```bash
# Test locally
docker build -t ggen:latest .
docker run ggen:latest ggen --version
```

## Best Practices

### For Developers

1. **Keep workflows simple** - Complex logic belongs in scripts
2. **Use stable actions** - Pin action versions (e.g., `@v4`)
3. **Test locally first** - Run checks before pushing: `cargo make ci`
4. **Monitor workflow status** - Check Actions tab after pushing
5. **Review logs** - Click workflow run to see detailed logs

### For Release Management

1. **Follow semantic versioning** - `vX.Y.Z` format
2. **Update CHANGELOG** - Document changes before releasing
3. **Create release notes** - Provide user-friendly release info
4. **Wait for workflows** - Let all workflows complete before announcing

### For CI Maintenance

1. **Review deprecated workflows** - Remove unused workflows
2. **Update action versions** - Keep actions up to date
3. **Monitor secrets** - Rotate credentials periodically
4. **Archive old releases** - Clean up old GitHub releases

## Related Documentation

- **[Release Process](../explanations/architecture.md)** - How releases work
- **[CONTRIBUTING.md](../../CONTRIBUTING.md)** - Pull request requirements
- **Cargo Make Tasks**: Run `cargo make help` or see `Makefile.toml` for available tasks
- **[Security](../../SECURITY.md)** - Security practices

## Quick Links

- **Actions Dashboard**: https://github.com/seanchatmangpt/ggen/actions
- **Releases**: https://github.com/seanchatmangpt/ggen/releases
- **Marketplace**: https://seanchatmangpt.github.io/ggen/marketplace/
- **Homebrew Tap**: https://github.com/seanchatmangpt/tap
