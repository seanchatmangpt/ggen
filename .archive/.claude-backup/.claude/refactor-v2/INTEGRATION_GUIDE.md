# Security Hardening Integration Guide

This guide shows how to integrate Agent 8's security hardening deliverables into the ggen project.

## Quick Start (5 minutes)

```bash
# 1. Add CI security workflow
mkdir -p .github/workflows
cp .claude/refactor-v2/ci-security-check.yml .github/workflows/security.yml

# 2. Add security policy
cp .claude/refactor-v2/SECURITY_POLICY.md SECURITY.md

# 3. Update README with security badge
# Add to README.md after ## Features:
cat >> README.md << 'EOF'

## Security

[![Security Audit](https://github.com/seanchatmangpt/ggen/workflows/Security%20Audit/badge.svg)](https://github.com/seanchatmangpt/ggen/actions?query=workflow%3A%22Security+Audit%22)

See [SECURITY.md](SECURITY.md) for our security policy and vulnerability reporting process.
EOF

# 4. Commit security improvements
git add .github/workflows/security.yml SECURITY.md README.md
git commit -m "security: Add security policy and CI scanning

- Add GitHub Actions security workflow
- Add SECURITY.md with vulnerability reporting process
- Document accepted risks (tokio-tar dev-only)
- Enable cargo audit in CI
"
```

## Detailed Integration Steps

### 1. CI Security Scanning

**Action**: Enable automated security scanning

```bash
# Copy workflow
cp .claude/refactor-v2/ci-security-check.yml .github/workflows/security.yml

# Test locally
cargo install cargo-audit
cargo audit --deny warnings --ignore RUSTSEC-2025-0111

# Verify it runs
act -l  # If you have act installed
```

**Expected Results**:
- ✅ Weekly vulnerability scans
- ✅ PR dependency reviews
- ⚠️ Clippy unwrap lints (will fail initially)

### 2. Clippy Security Lints (Optional - Breaking)

**Warning**: This will cause CI failures until unwraps are fixed.

```bash
# Add to Cargo.toml [lints.clippy] section
cat >> Cargo.toml << 'EOF'

[lints.clippy]
# Security-focused lints (enable after fixing unwraps)
# unwrap_used = "deny"
# expect_used = "deny"
# panic = "deny"

# Warnings (safe to enable now)
indexing_slicing = "warn"
missing_errors_doc = "warn"
EOF
```

**Gradual Rollout**:
1. Start with warnings only
2. Fix critical unwraps (P0: 6 locations)
3. Enable deny for production code
4. Allow in tests

### 3. Error Handling Tests

**Action**: Integrate security-focused tests

```bash
# Option A: Add to existing p2p tests
cat .claude/refactor-v2/error-handling-tests.rs >> src/p2p/tests.rs

# Option B: Create separate security test module
mkdir -p src/security
cp .claude/refactor-v2/error-handling-tests.rs src/security/tests.rs
echo "pub mod tests;" >> src/security/mod.rs
echo "mod security;" >> src/lib.rs

# Run tests
cargo test --lib security::
```

**Expected Results**:
- ⚠️ Some tests may fail (expected - they test error cases)
- ✅ Path traversal tests should pass
- ⚠️ P2P tests need implementations for NetworkBehaviour mocks

### 4. Security Policy

**Action**: Publish security policy

```bash
# Add policy
cp .claude/refactor-v2/SECURITY_POLICY.md SECURITY.md

# Update README
# Add this section before ## License:
cat >> README.md << 'EOF'

## Security

We take security seriously. See [SECURITY.md](SECURITY.md) for:
- Vulnerability reporting process
- Supported versions
- Known security issues
- Security best practices

**Quick Report**: Email security@ggen.io (do not use public issues)
EOF
```

### 5. Fix Critical Unwraps (P0 - Breaking Changes)

**Action**: Replace unwraps with proper error handling

**Location**: `src/p2p/registry.rs`

```rust
// BEFORE (Line 198)
let registry = P2PRegistryBuilder::new().build().unwrap();

// AFTER
let registry = P2PRegistryBuilder::new()
    .build()
    .map_err(|e| Error::RegistryInit(format!("Failed to build registry: {}", e)))?;
```

**Location**: `src/p2p/protocol.rs`

```rust
// BEFORE (Line 249)
protocol.send_request(request, "peer1".to_string()).unwrap();

// AFTER
protocol.send_request(request, "peer1".to_string())
    .map_err(|e| Error::ProtocolSend(format!("Failed to send request: {}", e)))?;
```

**All 6 Locations**:
1. `src/p2p/registry.rs:198` - `.build().unwrap()`
2. `src/p2p/registry.rs:214` - `.start().await.unwrap()`
3. `src/p2p/protocol.rs:249` - `.send_request(...).unwrap()`
4. `src/p2p/behaviour.rs:336` - `.publish_package().unwrap()`
5. `src/p2p/behaviour.rs:339` - `.search_packages().unwrap()`
6. `src/p2p/discovery.rs:136` - `.discover().await.unwrap()`

**Script to Help**:
```bash
# Find all unwraps in production code
grep -rn "\.unwrap()" src/ --include="*.rs" | grep -v "test"

# Count them
grep -r "\.unwrap()" src/ --include="*.rs" | grep -v "test" | wc -l
```

## Verification Checklist

After integration, verify:

- [ ] CI workflow runs on push to master
- [ ] `cargo audit` passes (with RUSTSEC-2025-0111 ignored)
- [ ] SECURITY.md is accessible on GitHub
- [ ] README links to security policy
- [ ] Security badge shows in README
- [ ] Tests pass (or document expected failures)
- [ ] No new clippy warnings (if lints enabled)

## Rollback Plan

If issues arise:

```bash
# Remove CI workflow
git rm .github/workflows/security.yml

# Revert README changes
git checkout README.md

# Remove security policy
git rm SECURITY.md

# Revert commit
git revert HEAD
```

## Gradual Rollout (Recommended)

### Week 1
- [x] Add SECURITY.md
- [x] Add CI workflow (without clippy lints)
- [x] Update README

### Week 2
- [ ] Fix 6 critical unwraps
- [ ] Add error handling tests
- [ ] Enable clippy warnings (not deny)

### Week 3
- [ ] Fix remaining unwraps
- [ ] Enable clippy deny for production
- [ ] Full security test coverage

### Week 4
- [ ] Review and update security policy
- [ ] Add fuzzing (optional)
- [ ] Security documentation

## Support

If you encounter issues:

1. **CI Failures**: Check `.github/workflows/security.yml` syntax
2. **Cargo Audit**: Review ignored advisories
3. **Clippy Lints**: Disable deny, use warn initially
4. **Test Failures**: Review mock implementations

## Next Steps After Integration

1. Monitor CI for security scan results
2. Address any new vulnerabilities quickly
3. Fix remaining unwraps gradually
4. Add security section to documentation
5. Consider security audit by external team

---

**Integration Time**: 5-30 minutes (depending on depth)
**Breaking Changes**: None (if clippy lints kept as warnings)
**Risk**: LOW (all changes are additive)
