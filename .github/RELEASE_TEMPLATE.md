# Release Template for ggen

Use this template for creating new releases of ggen.

## Pre-Release Checklist

- [ ] Version number finalized
- [ ] CHANGELOG.md updated with all changes
- [ ] RELEASE_NOTES.md created for new features
- [ ] MIGRATION_GUIDE.md created (if breaking changes)
- [ ] All tests passing (`cargo make test`)
- [ ] All linting passing (`cargo make lint`)
- [ ] Performance targets met (`cargo make slo-check`)
- [ ] Security audit completed (`cargo make audit`)
- [ ] Documentation updated
- [ ] Git tag created and signed
- [ ] GitHub Release draft created
- [ ] Changelog reviewed and approved

## Release Steps

### 1. Update Version Numbers

Update in these files:
- [ ] `Cargo.toml` (root)
- [ ] `crates/*/Cargo.toml` (all workspace members)
- [ ] `docs/README.md`
- [ ] Any hardcoded version strings

### 2. Update Documentation

- [ ] Create `docs/releases/vX.Y.Z/RELEASE-NOTES.md`
- [ ] Create `docs/releases/vX.Y.Z/CHANGELOG.md`
- [ ] Create `docs/releases/vX.Y.Z/INSTALLATION.md`
- [ ] Create `docs/releases/vX.Y.Z/MIGRATION-GUIDE.md` (if applicable)
- [ ] Update `docs/README.md` with release section
- [ ] Update main `CHANGELOG.md`

### 3. Git Operations

```bash
# Create release branch
git checkout -b release/vX.Y.Z

# Commit version updates
git add -A
git commit -m "release: Prepare vX.Y.Z release"

# Create annotated git tag
git tag -a vX.Y.Z -m "Release vX.Y.Z: [Short description]"

# Push to origin
git push origin release/vX.Y.Z
git push origin --tags
```

### 4. Create GitHub Release

Using the GitHub CLI:

```bash
gh release create vX.Y.Z \
  --title "ggen vX.Y.Z: [Release Title]" \
  --notes-file RELEASE_NOTES.md \
  --draft=false
```

Or manually:
- Go to GitHub > Releases > Draft new release
- Use tag `vX.Y.Z`
- Copy content from RELEASE_NOTES.md
- Add changelog summary
- Attach artifacts (if applicable)
- Publish

### 5. Publish to Crates.io

```bash
# Verify package can build
cargo build --release

# Publish each workspace member
cd crates/ggen-utils
cargo publish

cd ../ggen-core
cargo publish

# ... publish all packages ...

# Finally publish root package
cd ../..
cargo publish
```

### 6. Verify Release

- [ ] GitHub Release appears
- [ ] crates.io updated
- [ ] Documentation pages updated
- [ ] Version tags created correctly

## Release Content Template

### Release Title

```
ggen vX.Y.Z: [One-line description]
```

Examples:
- ggen v0.2.0: Unified Ontology Framework
- ggen v1.0.0: Production Ready Release
- ggen v0.1.5: Bug Fixes and Performance

### Release Description

```markdown
# ggen vX.Y.Z Release

**Release Date:** [YYYY-MM-DD]

## Key Highlights

- Feature 1
- Feature 2
- Feature 3

## What's New

### Added
- Feature descriptions
- New capabilities
- Performance improvements

### Changed
- Breaking changes (if any)
- API updates
- Behavior modifications

### Fixed
- Bug fixes
- Performance regressions
- Security issues

## Installation

See [INSTALLATION.md](docs/releases/vX.Y.Z/INSTALLATION.md)

## Migration

For users upgrading from previous versions, see [MIGRATION-GUIDE.md](docs/releases/vX.Y.Z/MIGRATION-GUIDE.md)

## Downloads

- Source: [GitHub](https://github.com/seanchatmangpt/ggen/archive/refs/tags/vX.Y.Z.tar.gz)
- crates.io: `cargo install ggen@X.Y.Z`
- Documentation: [Release Docs](docs/releases/vX.Y.Z/)

## Contributors

- [List of contributors]

## Support

- Report issues: [Issues](https://github.com/seanchatmangpt/ggen/issues)
- Discussions: [Discussions](https://github.com/seanchatmangpt/ggen/discussions)
- Documentation: [Docs](https://github.com/seanchatmangpt/ggen/docs)

---

For detailed changes, see [CHANGELOG.md](docs/releases/vX.Y.Z/CHANGELOG.md)
```

## Breaking Changes Template

If release contains breaking changes:

```markdown
## ⚠️ Breaking Changes

### Removed Features

- Feature X: [Reason for removal]
- Deprecated API Y: [Migration path]

### Changed Behavior

- Command Z: [Old behavior → New behavior]

### Migration Required

For detailed upgrade instructions, see [MIGRATION-GUIDE.md](docs/releases/vX.Y.Z/MIGRATION-GUIDE.md)

**Quick Migration:**

```bash
# Run automated migration
ggen migrate --from X.Y.Z --to X+1.Y.Z

# Or manual steps...
```
```

## Performance Metrics Template

Include this in release notes if applicable:

```markdown
## Performance

### Benchmarks

| Operation | Previous | New | Improvement |
|-----------|----------|-----|------------|
| First Build | 18s | 15s | +20% |
| Incremental | 2.5s | 2.0s | +25% |
| Generation | 800ms | 600ms | +33% |

### SLO Compliance

- ✅ First build: ≤15s
- ✅ Incremental: ≤2s
- ✅ RDF processing: ≤5s for 1k+ triples
- ✅ Memory: ≤100MB
- ✅ CLI: ≤3s end-to-end
```

## Security Advisory Template

Use if security issues are fixed:

```markdown
## Security

### CVE Fixes

- [CVE-XXXX-XXXXX]: [Issue description]
  - **Severity**: Medium/High/Critical
  - **Affected Versions**: X.Y.Z - A.B.C
  - **Fix**: [How it was fixed]
  - **Workaround**: [If not updating]

### Recommendations

Users should update to vX.Y.Z immediately.
```

## Post-Release Tasks

- [ ] Close release milestone on GitHub
- [ ] Update website with new version
- [ ] Announce in relevant channels
- [ ] Monitor for issues with new release
- [ ] Respond to bug reports promptly
- [ ] Plan next release cycle

## Version Numbering

Follow [Semantic Versioning](https://semver.org/):

- **MAJOR** (X.0.0): Breaking changes
- **MINOR** (0.Y.0): New features, backwards compatible
- **PATCH** (0.0.Z): Bug fixes, backwards compatible

### Examples

- 0.1.0 → 0.2.0: New features (minor bump)
- 0.2.0 → 0.2.1: Bug fix (patch bump)
- 0.2.0 → 1.0.0: Breaking changes (major bump)

## Documentation Structure

Each release should have:

```
docs/releases/vX.Y.Z/
├── RELEASE-NOTES.md      # User-facing highlights
├── CHANGELOG.md          # Detailed technical changes
├── INSTALLATION.md       # Setup instructions
├── MIGRATION-GUIDE.md    # Upgrade path (if applicable)
└── README.md             # Index of release docs
```

## Communication Template

### GitHub Discussions

```markdown
ggen vX.Y.Z is now available!

## What's New

[Key highlights]

## Installation

[Quick install command]

## Questions?

Ask in this discussion or open an issue.
```

### Social Media

```
ggen vX.Y.Z is here! 🚀

Key features:
- Feature 1
- Feature 2
- Performance improvements

Learn more: [link to release]
```

---

**Notes:**
- Adapt template as needed for your release
- Ensure all checks pass before publishing
- Double-check version numbers
- Verify documentation links work
- Test installation from multiple platforms
