# P2P Marketplace Release Automation Checklist

**Version**: 2.4.0
**Date**: 2025-11-02
**Status**: ✅ Ready for Production

---

## Pre-Release Checklist

### 1. Version Updates

- [ ] Update `Cargo.toml` (workspace root)
  ```toml
  [package]
  version = "2.4.0"
  ```

- [ ] Update `ggen-marketplace/Cargo.toml`
  ```toml
  [package]
  version = "2.4.0"
  ```

- [ ] Update `cli/Cargo.toml`
  ```toml
  [dependencies]
  ggen-marketplace = { path = "../ggen-marketplace", version = "2.4.0" }
  ```

- [ ] Verify all workspace members use consistent versions
  ```bash
  grep -r 'version = "' */Cargo.toml | grep -v dependencies
  ```

### 2. Changelog

- [ ] Add new section to `CHANGELOG.md`
  ```markdown
  ## [2.4.0] - 2025-11-02

  ### Added
  - P2P marketplace backend with libp2p integration
  - Comprehensive CI/CD pipeline with 6 feature configurations
  - Automated release pipeline for crates.io + GitHub
  - Security scanning with cargo audit

  ### Changed
  - Enhanced marketplace search with Chicago TDD tests
  - Improved build matrix for feature flag validation

  ### Fixed
  - Build issues with no-default-features configuration
  ```

### 3. Local Validation

- [ ] **Build all feature configurations**
  ```bash
  # Default features
  cargo build --workspace

  # All features (P2P)
  cargo build --workspace --all-features

  # No default features
  cargo build --workspace --no-default-features

  # P2P only
  cd ggen-marketplace && cargo build --features p2p
  ```

- [ ] **Run full test suite**
  ```bash
  cargo test --workspace --all-features
  ```

- [ ] **Run Chicago TDD tests**
  ```bash
  cargo test --test marketplace_search_chicago_tdd
  cargo test --test marketplace_install_e2e
  ```

- [ ] **Clippy validation**
  ```bash
  cargo clippy --workspace --all-features -- -D warnings
  ```

- [ ] **Security audit**
  ```bash
  cargo install cargo-audit
  cargo audit
  ```

- [ ] **Documentation check**
  ```bash
  cargo doc --workspace --all-features --no-deps
  ```

### 4. Git Preparation

- [ ] **Commit version changes**
  ```bash
  git add Cargo.toml */Cargo.toml CHANGELOG.md
  git commit -m "chore: prepare v2.4.0 release"
  git push origin master
  ```

- [ ] **Create and push tag**
  ```bash
  git tag v2.4.0
  git push origin v2.4.0
  ```

---

## Automated Release Process

### Trigger: Tag Push

Once you push the tag (`git push origin v2.4.0`), GitHub Actions automatically:

#### Phase 1: Pre-flight Checks (~5 min)

- ✅ Validate version consistency across Cargo.toml files
- ✅ Check CHANGELOG.md contains release entry
- ✅ Run full test suite with all features
- ✅ Run clippy with all features
- ✅ Run security audit

**If any check fails, release stops here** ⛔

#### Phase 2: Build Release Artifacts (~15 min)

Parallel builds for:
- ✅ Linux x86_64 (`x86_64-unknown-linux-gnu`)
- ✅ macOS x86_64 (`x86_64-apple-darwin`)
- ✅ macOS ARM64 (`aarch64-apple-darwin`)

For each platform:
- Build with `--release --all-features`
- Create `.tar.gz` archive
- Generate SHA256 checksum

#### Phase 3: Publish to crates.io (~5 min)

1. Publish `ggen-marketplace` first
   ```bash
   cd ggen-marketplace
   cargo publish --all-features
   ```

2. Wait 60 seconds for crates.io propagation

3. Publish workspace crates in dependency order:
   ```bash
   cd utils && cargo publish
   cd ggen-core && cargo publish
   cd ggen-ai && cargo publish
   cd cli && cargo publish
   cargo publish  # Root crate
   ```

#### Phase 4: Create GitHub Release (~2 min)

- ✅ Create GitHub release with tag `v2.4.0`
- ✅ Extract changelog section for this version
- ✅ Upload 6 artifacts (3 binaries + 3 checksums)
- ✅ Mark as latest release

---

## Manual Release (Alternative)

### If Automatic Release Fails

1. **Trigger workflow manually**:
   - Go to: https://github.com/seanchatmangpt/ggen/actions/workflows/p2p-release.yml
   - Click "Run workflow"
   - Enter version: `2.4.0`
   - Set dry_run: `false`
   - Click "Run workflow"

2. **Dry run first** (recommended):
   - Set dry_run: `true`
   - Verify all checks pass
   - Then run again with dry_run: `false`

### Manual crates.io Publishing

If automated publishing fails:

```bash
# 1. Get crates.io token
# Go to https://crates.io/settings/tokens

# 2. Login to crates.io
cargo login <your-token>

# 3. Publish marketplace first
cd ggen-marketplace
cargo publish --all-features

# 4. Wait for propagation
sleep 60

# 5. Publish workspace crates
cd ../utils && cargo publish
cd ../ggen-core && cargo publish
cd ../ggen-ai && cargo publish
cd ../cli && cargo publish
cd .. && cargo publish
```

---

## Post-Release Verification

### 1. Verify crates.io Publication

- [ ] Check marketplace is published:
  ```bash
  cargo search ggen-marketplace
  ```

- [ ] Test installation from crates.io:
  ```bash
  cargo install ggen-marketplace --version 2.4.0
  ```

- [ ] Verify crates.io page:
  - https://crates.io/crates/ggen-marketplace

### 2. Verify GitHub Release

- [ ] Check release exists:
  - https://github.com/seanchatmangpt/ggen/releases/tag/v2.4.0

- [ ] Verify artifacts uploaded (6 files):
  - `ggen-2.4.0-x86_64-unknown-linux-gnu.tar.gz`
  - `ggen-2.4.0-x86_64-unknown-linux-gnu.tar.gz.sha256`
  - `ggen-2.4.0-x86_64-apple-darwin.tar.gz`
  - `ggen-2.4.0-x86_64-apple-darwin.tar.gz.sha256`
  - `ggen-2.4.0-aarch64-apple-darwin.tar.gz`
  - `ggen-2.4.0-aarch64-apple-darwin.tar.gz.sha256`

- [ ] Check changelog extracted correctly

- [ ] Verify "Latest release" badge updated

### 3. Test Binary Installation

**Linux**:
```bash
curl -L https://github.com/seanchatmangpt/ggen/releases/download/v2.4.0/ggen-2.4.0-x86_64-unknown-linux-gnu.tar.gz -o ggen.tar.gz
tar xzf ggen.tar.gz
./ggen-2.4.0/ggen --version
```

**macOS (Intel)**:
```bash
curl -L https://github.com/seanchatmangpt/ggen/releases/download/v2.4.0/ggen-2.4.0-x86_64-apple-darwin.tar.gz -o ggen.tar.gz
tar xzf ggen.tar.gz
./ggen-2.4.0/ggen --version
```

**macOS (ARM)**:
```bash
curl -L https://github.com/seanchatmangpt/ggen/releases/download/v2.4.0/ggen-2.4.0-aarch64-apple-darwin.tar.gz -o ggen.tar.gz
tar xzf ggen.tar.gz
./ggen-2.4.0/ggen --version
```

### 4. Verify Checksums

```bash
# Download checksum file
curl -L https://github.com/seanchatmangpt/ggen/releases/download/v2.4.0/ggen-2.4.0-x86_64-apple-darwin.tar.gz.sha256 -o checksum.txt

# Verify
shasum -a 256 -c checksum.txt
```

---

## Rollback Procedure

### If Release Has Critical Issues

#### 1. Yank from crates.io

```bash
cargo login <your-token>
cargo yank --vers 2.4.0 ggen-marketplace
cargo yank --vers 2.4.0 ggen
```

**Note**: Yanking does not delete the version, just marks it as "do not use"

#### 2. Mark GitHub Release as Pre-release

- Go to https://github.com/seanchatmangpt/ggen/releases/tag/v2.4.0
- Edit release
- Check "Set as a pre-release"
- Save changes

#### 3. Create Hotfix

```bash
# Create hotfix branch
git checkout -b hotfix/v2.4.1

# Fix issues
# ...

# Update to 2.4.1
# Update Cargo.toml files
# Update CHANGELOG.md

# Commit and release
git commit -m "fix: critical issue in v2.4.0"
git tag v2.4.1
git push origin hotfix/v2.4.1
git push origin v2.4.1
```

---

## Troubleshooting

### Issue: "crate not found in registry"

**Cause**: crates.io propagation delay

**Solution**: Wait 5-10 minutes and retry

### Issue: "version already exists"

**Cause**: Version already published to crates.io

**Solution**: Increment version number (e.g., 2.4.0 → 2.4.1)

### Issue: "unauthorized"

**Cause**: Invalid or missing CARGO_REGISTRY_TOKEN

**Solution**:
1. Generate new token at https://crates.io/settings/tokens
2. Add to GitHub Secrets as `CARGO_REGISTRY_TOKEN`

### Issue: Build fails on specific platform

**Cause**: Platform-specific dependency or code issue

**Solution**:
1. Fix the issue in code
2. Increment version
3. Re-release

---

## CI/CD Configuration

### GitHub Secrets Required

Add to: https://github.com/seanchatmangpt/ggen/settings/secrets/actions

| Secret Name | Description | Required For |
|------------|-------------|--------------|
| `CARGO_REGISTRY_TOKEN` | crates.io API token | Publishing to crates.io |

### Workflow Files

| File | Purpose | Trigger |
|------|---------|---------|
| `.github/workflows/p2p-marketplace-ci.yml` | CI/CD pipeline | Push, PR |
| `.github/workflows/p2p-release.yml` | Release automation | Tag push, manual |

### Workflow Status

Check at: https://github.com/seanchatmangpt/ggen/actions

---

## Release Schedule

### Semantic Versioning

- **Major (3.0.0)**: Breaking changes
- **Minor (2.5.0)**: New features, backwards compatible
- **Patch (2.4.1)**: Bug fixes, backwards compatible

### Release Frequency

- **Stable releases**: Monthly
- **Hotfixes**: As needed
- **Pre-releases**: Weekly (for testing)

---

## Contact & Support

### Release Manager

- **Primary**: CI/CD Engineer (Claude-Flow)
- **Backup**: Project maintainers

### Documentation

- **CI/CD Overview**: `docs/P2P_CICD_VALIDATION_REPORT.md`
- **Quick Start**: `docs/P2P_CICD_QUICK_SUMMARY.md`
- **This Checklist**: `docs/P2P_CICD_RELEASE_CHECKLIST.md`

### Coordination

```bash
# Pre-task hook
npx claude-flow@alpha hooks pre-task --description "release v2.4.0"

# Notify on completion
npx claude-flow@alpha hooks notify --message "release v2.4.0 complete"

# Post-task hook
npx claude-flow@alpha hooks post-task --task-id "release-2.4.0"
```

---

## Summary

✅ **Pre-Release**: Version updates, changelog, local validation, git tags
✅ **Automated Release**: Pre-flight → Build → Publish → GitHub Release
✅ **Post-Release**: Verify crates.io, GitHub, binary downloads
✅ **Rollback**: Yank from crates.io, mark as pre-release, create hotfix

**Estimated Total Time**: ~30 minutes (mostly automated)

---

**Last Updated**: 2025-11-02
**CI/CD Engineer**: Claude-Flow coordination
**Working Directory**: /Users/sac/ggen
