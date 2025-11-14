# Release Checklist - v2.6.0

**Release Date**: 2025-11-12  
**Version**: 2.6.0 (Minor Release)  
**Status**: Preparation Complete

**FMEA-Based Validation**: This release uses FMEA (Failure Mode and Effects Analysis) to prevent release failures. See [FMEA Analysis](./docs/FMEA_RELEASE_V2.6.0.md) for complete failure mode analysis and automated validation checks.

## Pre-Release Validation

### Version Updates ✅
- [x] Root `Cargo.toml` updated to 2.6.0
- [x] `crates/ggen-utils/Cargo.toml` updated to 2.6.0
- [x] `crates/ggen-cli/Cargo.toml` updated to 2.6.0
- [x] `crates/ggen-core/Cargo.toml` updated to 2.6.0
- [x] `crates/ggen-ai/Cargo.toml` updated to 2.6.0
- [x] `crates/ggen-marketplace/Cargo.toml` updated to 2.6.0
- [x] `crates/ggen-domain/Cargo.toml` remains at 3.1.0 (separate version track)
- [x] Dependency versions updated in root `Cargo.toml`
- [x] `VERSION` file updated to 2.6.0

### Documentation ✅
- [x] `CHANGELOG.md` updated with v2.6.0 release notes
- [x] Release date set to 2025-11-12
- [x] Waste elimination changes documented
- [x] HTTP/GraphQL removal documented
- [x] Waste prevention controls documented

### Code Quality ✅
- [x] All HTTP dependencies removed
- [x] All GraphQL code removed
- [x] Telemetry updated to use workspace OpenTelemetry versions
- [x] Waste prevention controls added to Makefile.toml
- [x] Waste prevention standards added to Rust coding standards

## Release Steps

### 1. Final Verification (Before Tagging)

**FMEA-Based Validation**: All release validations are automated via `cargo make release-validate`. See [FMEA Analysis](./docs/FMEA_RELEASE_V2.6.0.md) for details.

```bash
# Run comprehensive release validation (all FMEA checks)
cargo make release-validate
# Expected: All validation checks pass
# Includes: git state, version consistency, artifacts, build, security, CHANGELOG, breaking changes, docs sync, tests, linting

# Or run individual validations:
cargo make release-validate-git-state      # Verify clean git state
cargo make release-validate-version         # Verify version consistency
cargo make release-validate-artifacts      # Verify release artifacts
cargo make release-validate-build          # Verify release build
cargo make release-validate-security        # Verify security audit
cargo make release-validate-changelog      # Verify CHANGELOG entry
cargo make release-validate-breaking-changes # Check for breaking changes
cargo make release-validate-docs-sync      # Verify documentation sync
```

**Manual Verification** (if needed):
```bash
# Verify git state is clean
git status --porcelain
# Expected: Clean working directory

# Verify all versions match
grep "version = \"2.6.0\"" Cargo.toml crates/*/Cargo.toml | grep -v ggen-domain
# Expected: All show 2.6.0 (except ggen-domain which is 3.1.0)

# Verify compilation
cargo make check
# Expected: All crates compile successfully

# Verify tests pass
cargo make test
# Expected: All tests pass

# Verify linting
cargo make lint
# Expected: No linting errors
```

### 2. Create Release Tag

```bash
# Create annotated tag
git tag -a v2.6.0 -m "Release v2.6.0 - CLI-Only Version

Major Changes:
- Removed HTTP dependencies (CLI-only focus)
- Removed GraphQL module (waste elimination)
- Added waste prevention controls
- Improved code quality (Kaizen improvements)

See CHANGELOG.md for complete details."

# Push tag
git push origin v2.6.0
```

### 3. Create GitHub Release

```bash
# Create GitHub release with notes from CHANGELOG.md
gh release create v2.6.0 \
  --title "ggen v2.6.0 - CLI-Only Version" \
  --notes-file <(sed -n '/## \[2.6.0\]/,/## \[2.5.0\]/p' CHANGELOG.md | sed '$d')
```

### 4. Post-Release Tasks

- [ ] Update README.md badges if needed
- [ ] Announce release on social media/channels
- [ ] Monitor for any issues
- [ ] Update any external documentation

## Release Notes Summary

### Removed (Waste Elimination)
- HTTP client/server dependencies (reqwest, axum, tower, tower-http)
- GraphQL API layer (~1,291 lines)
- CentralizedRegistry HTTP backend (~386 lines)
- Content distribution HTTP server (~373 lines)
- Distributed setup example

### Added
- Waste prevention controls (`check-dead-code`, `check-unused-features`, `validate-examples`)
- Waste prevention standards in Rust coding standards
- Code quality improvements (Kaizen)

### Changed
- Telemetry switched from HTTP to gRPC exporter
- CLI-focused architecture (no HTTP dependencies)

## Verification Commands

```bash
# Quick validation
cargo make check && cargo make test-unit && cargo make lint

# Full validation
cargo make ci

# Release binary verification
cargo make build-release
./target/release/ggen --version
# Expected: ggen 2.6.0
```

## Notes

- ggen-domain remains at version 3.1.0 (separate version track)
- All HTTP/GraphQL code removed for CLI-only focus
- Waste prevention controls prevent future accumulation
- Release focuses on code quality and waste elimination
