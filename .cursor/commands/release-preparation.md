# Release Preparation - Essential Checklist

## Purpose

This command guides agents through release preparation. It verifies code is production-ready, updates version, creates release artifacts, and validates everything before release.

---

## Release Checklist

### Pre-Release: Code Ready?

**Action**: Verify code is production-ready.

**Checklist**:
- [ ] No uncommitted changes: `git status --porcelain` (should be empty)
- [ ] No TODOs/FIXMEs in production code: `grep -r "TODO\|FIXME" src/`
- [ ] No unimplemented!() calls: `grep -r "unimplemented\|todo!" src/`
- [ ] All tests pass: `cargo make test`
- [ ] Linting passes: `cargo make lint`
- [ ] Code compiles: `cargo make check`

**CRITICAL**: Fix any issues before proceeding. Don't release broken code.

---

### Step 1: Update Version

**Action**: Update version in Cargo.toml and workspace crates.

**Process**:
```bash
# Current version
grep "^version" Cargo.toml

# Update version in Cargo.toml (change to new version)
# Also update: crates/*/Cargo.toml

# Verify update
grep "^version" Cargo.toml
```

**Checklist**:
- [ ] Update version in Cargo.toml
- [ ] Update version in workspace crates (crates/*/Cargo.toml)
- [ ] Verify all version updates applied
- [ ] Code still compiles: `cargo make check`

---

### Step 2: Create/Update Artifacts

**Action**: Create release artifacts (CHANGELOG, release notes).

**Artifacts**:

#### CHANGELOG.md
```markdown
## [X.Y.Z] - YYYY-MM-DD

### Added
- New features

### Changed
- Changes to existing functionality

### Fixed
- Bug fixes

### Removed
- Removed features
```

**Process**:
1. Add new version section to CHANGELOG.md
2. List features, changes, fixes, removals
3. Verify format matches existing entries

**Checklist**:
- [ ] CHANGELOG.md has new version section
- [ ] Features documented
- [ ] Changes documented
- [ ] Fixes documented
- [ ] Format matches existing entries

#### Release Notes (Optional)
```markdown
# Release v2.7.0

**Key Highlights**:
- Feature 1
- Feature 2
- Important fix

**Download**: [crates.io](https://crates.io/crates/ggen)

**Changelog**: [Full changelog](./CHANGELOG.md)
```

**Process**:
1. Create release notes summarizing key points
2. Link to detailed changelog
3. Include download/installation info

**Checklist**:
- [ ] Release notes created (or confirm not needed)
- [ ] Key features highlighted
- [ ] Download/installation info included

---

### Step 3: Final Validation

**Action**: Validate everything is ready for release.

**Full Validation**:
```bash
# Clean build
cargo clean
cargo make check

# All tests pass
cargo make test

# Linting clean
cargo make lint

# Format correct
cargo make fmt

# No uncommitted changes
git status --porcelain

# Expected: All pass, no output from git status
```

**Checklist**:
- [ ] Code compiles: `cargo make check`
- [ ] All tests pass: `cargo make test`
- [ ] No warnings: `cargo make lint`
- [ ] Format correct: `cargo make fmt`
- [ ] Git clean: `git status --porcelain`
- [ ] Version updated
- [ ] CHANGELOG updated
- [ ] No TODOs/FIXMEs in production code

---

### Step 4: Create Release Tag

**Action**: Tag release in git.

**Process**:
```bash
# Create tag
git tag -a vX.Y.Z -m "Release vX.Y.Z"

# Example
git tag -a v2.7.0 -m "Release v2.7.0"

# Verify tag created
git tag -l v2.7.0 -n1

# Show what's in tag
git show v2.7.0
```

**Checklist**:
- [ ] Tag created: `git tag -a vX.Y.Z -m "Release vX.Y.Z"`
- [ ] Tag verified: `git tag -l vX.Y.Z -n1`
- [ ] Tag points to correct commit

---

### Step 5: Create Release

**Action**: Create release (publish to crates.io, GitHub, etc).

**For Crates.io**:
```bash
# Verify package
cargo package --allow-dirty

# Publish (requires authentication)
cargo publish

# Verify on crates.io
# Visit: https://crates.io/crates/ggen/vX.Y.Z
```

**Checklist**:
- [ ] Package verified: `cargo package --allow-dirty`
- [ ] Published: `cargo publish`
- [ ] Visible on crates.io after ~1 minute
- [ ] Version correctly published

---

### Step 6: Post-Release

**Action**: Complete post-release tasks.

**Tasks**:
1. **Update docs**: Ensure documentation is current
2. **Announce**: Announce release (if applicable)
3. **Verify installation**: Test installing from crates.io
4. **Tag pushed**: Push tag to remote: `git push origin vX.Y.Z`

**Checklist**:
- [ ] Documentation updated
- [ ] Announcement made (if applicable)
- [ ] Installation tested
- [ ] Tag pushed to remote
- [ ] Release is live and working

---

## Quick Reference: Release Workflow

```bash
# Pre-release checks
cargo make test
cargo make lint
cargo make check
git status

# Update version
# - Edit Cargo.toml version
# - Edit crates/*/Cargo.toml version

# Update CHANGELOG.md
# - Add new version section
# - Document changes

# Final validation
cargo clean && cargo make test
git status  # Should be empty

# Create tag
git tag -a vX.Y.Z -m "Release vX.Y.Z"

# Publish
cargo publish

# Push tag
git push origin vX.Y.Z

# Verify
# - Check crates.io for new version
# - Verify documentation updated
```

---

## Expert Insights

**Why this matters**: Releases are critical. Incomplete releases cause problems for users.

**Key principles**:
- **Quality first** - Don't release broken code
- **Complete process** - Follow all steps, don't skip
- **Verify everything** - Test installation, verify version, confirm visibility
- **Document changes** - Update CHANGELOG and release notes

**Remember**:
- **Pre-release checks are critical** - Catch issues before release
- **Version updates must be complete** - Update all files with version
- **CHANGELOG is documentation** - Users rely on it to understand changes
- **Tag is permanent** - Double-check before creating tag
- **Publish is final** - Can't un-publish to crates.io

