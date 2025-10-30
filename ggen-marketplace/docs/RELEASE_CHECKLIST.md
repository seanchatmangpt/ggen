# Ggen Marketplace Release Checklist

## Pre-Release Verification

### Code Quality
- [ ] All tests pass locally with `cargo test --all-features`
- [ ] No clippy warnings with `cargo clippy --all-features -- -D warnings`
- [ ] Code is properly formatted with `cargo fmt -- --check`
- [ ] Documentation builds without warnings `cargo doc --all-features --no-deps`
- [ ] No security vulnerabilities with `cargo audit`

### Feature Verification
- [ ] Default features build: `cargo build`
- [ ] P2P feature builds: `cargo build --features p2p`
- [ ] GraphQL feature builds: `cargo build --features graphql`
- [ ] Crypto feature builds: `cargo build --features crypto`
- [ ] All features build: `cargo build --all-features`

### Testing
- [ ] Unit tests pass: `cargo test --lib`
- [ ] Integration tests pass: `cargo test --test '*'`
- [ ] Doc tests pass: `cargo test --doc`
- [ ] P2P smoke tests pass: `cargo test --features p2p p2p --lib`
- [ ] GraphQL smoke tests pass: `cargo test --features graphql graphql --lib`
- [ ] All feature tests pass: `cargo test --all-features`

### CI/CD Pipeline
- [ ] GitHub Actions CI passes on all branches
- [ ] All feature matrix builds succeed
- [ ] Integration tests complete successfully
- [ ] P2P smoke tests pass (if applicable)
- [ ] GraphQL smoke tests pass (if applicable)
- [ ] Security audit completes without critical issues
- [ ] Code coverage meets minimum threshold (>80%)

### Documentation
- [ ] CHANGELOG.md updated with release notes
- [ ] README.md reflects current features
- [ ] API documentation is complete and accurate
- [ ] Migration guide created (if breaking changes)
- [ ] Examples are up-to-date and tested

### Version Management
- [ ] Version number updated in `Cargo.toml`
- [ ] Version follows semantic versioning (MAJOR.MINOR.PATCH)
- [ ] Git tag created with version number
- [ ] Release branch created (if applicable)

### Dependencies
- [ ] All dependencies are up-to-date
- [ ] No deprecated dependencies
- [ ] Security audit clean: `cargo audit`
- [ ] License compliance verified

### Performance
- [ ] Benchmarks run and results documented
- [ ] No performance regressions detected
- [ ] Memory usage is acceptable
- [ ] Build times are reasonable

### Security
- [ ] Security audit passes: `cargo audit`
- [ ] No hardcoded secrets or credentials
- [ ] Crypto implementations follow best practices
- [ ] Input validation is comprehensive
- [ ] Error messages don't leak sensitive information

## Release Process

### 1. Version Bump
```bash
# Update version in Cargo.toml
# Update CHANGELOG.md
git add Cargo.toml CHANGELOG.md
git commit -m "chore: bump version to X.Y.Z"
```

### 2. Run Full Verification
```bash
./scripts/verify-build.sh
```

### 3. Create Release Tag
```bash
git tag -a vX.Y.Z -m "Release version X.Y.Z"
git push origin vX.Y.Z
```

### 4. Publish to crates.io
```bash
cargo publish --dry-run
cargo publish
```

### 5. Create GitHub Release
- [ ] Create release from tag on GitHub
- [ ] Add release notes from CHANGELOG.md
- [ ] Attach relevant artifacts (if any)

### 6. Post-Release
- [ ] Verify package appears on crates.io
- [ ] Test installation: `cargo install ggen-marketplace`
- [ ] Update main documentation
- [ ] Announce release (if applicable)

## Feature-Specific Checklists

### P2P Feature Release
- [ ] P2P networking tested on multiple nodes
- [ ] Network partition handling verified
- [ ] Peer discovery works correctly
- [ ] Message routing is efficient
- [ ] NAT traversal tested (if applicable)

### GraphQL Feature Release
- [ ] GraphQL schema is valid
- [ ] All queries and mutations tested
- [ ] Subscriptions work correctly (if applicable)
- [ ] GraphQL playground accessible
- [ ] Rate limiting configured

### Crypto Feature Release
- [ ] Key generation works correctly
- [ ] Signature verification is accurate
- [ ] No insecure crypto usage (`.expect()`, `.unwrap()`)
- [ ] Random number generation is cryptographically secure
- [ ] Key storage recommendations documented

## Emergency Rollback Plan

If critical issues are discovered post-release:

1. **Yank the release from crates.io:**
   ```bash
   cargo yank --version X.Y.Z
   ```

2. **Create hotfix branch:**
   ```bash
   git checkout -b hotfix/vX.Y.Z+1
   ```

3. **Fix the issue and release patch version**

4. **Document the issue in CHANGELOG.md**

## Release Notes Template

```markdown
# Release vX.Y.Z

## Added
- New features and capabilities

## Changed
- Modifications to existing features

## Fixed
- Bug fixes

## Security
- Security improvements

## Breaking Changes
- API changes that break backward compatibility

## Migration Guide
- Steps to migrate from previous version
```

## Post-Release Monitoring

- [ ] Monitor issue tracker for bug reports
- [ ] Check download statistics on crates.io
- [ ] Respond to user feedback promptly
- [ ] Plan next release based on feedback

---

**Note:** This checklist should be reviewed and updated with each release to reflect current best practices.
