# Security Policy for ggen

## Reporting Security Vulnerabilities

The ggen project takes security seriously. We appreciate your responsible disclosure of security vulnerabilities.

### Do NOT Report Publicly

**Never** open a public GitHub issue for security vulnerabilities. This may expose other users to the vulnerability.

### Report Privately

Please report security issues via email:

**Email**: `sean@chatmangpt.com`
**Subject**: `[SECURITY] ggen vulnerability - [brief description]`

### Information to Include

Provide as much detail as possible:

```
- Vulnerability type (e.g., SQL injection, XSS, race condition)
- Component affected (crate, module, function)
- Description of the vulnerability
- Steps to reproduce (if possible)
- Affected versions
- Suggested fix (if you have one)
- Your name and contact (for acknowledgment)
```

### Response Timeline

- **Acknowledgment**: Within 48 hours
- **Initial assessment**: Within 1 week
- **Fix development**: Depends on severity
- **Public disclosure**: After patch release

## Security Practices

### Code Review

All code changes go through:
- Peer review (minimum 1 reviewer)
- Automated security checks (clippy, cargo-audit)
- Test coverage analysis

### Dependencies

We maintain security through:

```bash
# Check for known vulnerabilities in dependencies
cargo audit

# Update to patched versions
cargo update
```

Dependency security is checked:
- On every pull request
- Nightly in CI
- Before each release

### Unsafe Code Policy

Unsafe code is:
- Minimized and isolated
- Fully documented with safety comments
- Reviewed thoroughly
- Tested extensively

Current unsafe usage:
- **ggen-core/src/ontology/promotion.rs**: 5 unsafe blocks (atomic operations, justified)
- **ggen-ai/src/ultrathink/mod.rs**: Thread-safe lazy initialization (replaced with OnceLock in v3.0.0)
- **ggen-ai/src/agents/**: Refactored to eliminate unsafe ptr::read patterns in v3.0.0

### Timing Security

To prevent timing attacks:

```rust
// ✗ Don't use direct comparison for secrets
if user_password == stored_password {
    // vulnerable to timing analysis
}

// ✓ Use constant-time comparison
use subtle::ConstantTimeComparison;
if user_password.ct_eq(&stored_password).unwrap_u8() == 1 {
    // Safe from timing attacks
}
```

## Vulnerability Categories

### High Priority

- Remote code execution
- SQL injection
- Cross-site scripting (XSS)
- Authentication bypass
- Data integrity compromise
- Denial of Service (resource exhaustion)

### Medium Priority

- Information disclosure
- Logic errors in critical paths
- Cryptographic weaknesses
- Race conditions in concurrent code

### Low Priority

- Cosmetic issues
- Non-critical warnings
- Edge cases with low impact
- Denial of Service (requires extreme inputs)

## Security Architecture

### Trust Boundaries

```
Untrusted Input
      ↓
[Input Validation Layer]
      ↓
[Schema Validation]
      ↓
[Kernel Decision Making]
      ↓
[Execution with Guards]
      ↓
Trusted Output
```

### Input Validation

All external inputs are validated:

```rust
// Observation validation
const MAX_OBSERVATION_SIZE: usize = 1024 * 1024; // 1MB limit

// Schema depth limit
const MAX_SCHEMA_DEPTH: usize = 256;

// Template variable injection protection
validate_template_variables(&template)?;

// File path validation
validate_path(&path, "/allowed/base")?;
```

### Cryptographic Standards

- **Hashing**: SHA-256 (minimum)
- **Signing**: HMAC-SHA256 for receipts
- **Random**: `rand` crate with `OsRng`
- **Secrets**: Never logged or displayed

### Isolation

- **Tenant isolation**: ggen-dod TenantId enforcement
- **Process isolation**: Watch service in separate tasks
- **Filesystem isolation**: Restricted to configured paths

## Known Security Considerations

### Race Conditions

Watch mode handles concurrent file changes:
- Debouncing prevents rapid re-execution
- Atomic operations for state transitions
- Tokio sync primitives for coordination

### Resource Exhaustion

Protections against DOS:

```
- Max observation size: 1MB
- Max schema depth: 256 levels
- Max fan-out per tick: 1024 items
- Max promotion rate: 100/hour
- Kernel timeout: 8ms
```

### Dependency Vulnerabilities

Monitor dependencies with:

```bash
# Regular audits
cargo audit --deny warnings

# Update maintenance
cargo outdated

# Deep inspection
cargo audit --fetch deny
```

## Release Process Security

### Pre-release Checklist

- [ ] All tests passing
- [ ] No new warnings
- [ ] `cargo audit` shows no vulnerabilities
- [ ] Changelog documenting security fixes
- [ ] Security review completed
- [ ] Backup of signing keys in secure location

### Vulnerability Announcement

For critical vulnerabilities:

1. Release patch immediately
2. Publish security advisory (GitHub)
3. Announce in release notes
4. Email maintainers of downstream projects

## Security Updates

### Version Support

| Version | Status | Security Updates |
|---------|--------|------------------|
| 3.x | Stable | ✓ Active |
| 2.x | Legacy | ✓ Critical only |
| 1.x | Unsupported | ✗ None |

### Update Cadence

- Security patches: Released within 72 hours
- Minor updates: Monthly
- Major releases: Quarterly

## Compliance

### Standards Followed

- **OWASP Top 10**: Guidance for web/API security
- **SANS Top 25**: Common software security weaknesses
- **CWE**: Common Weakness Enumeration reference

### Code Review Standards

All code must pass:

```bash
cargo fmt --check --all
cargo clippy --all -- -D warnings
cargo test --all
cargo build --release
cargo audit
```

## Security Hardening

### Enable Security Features

In your `Cargo.toml`:

```toml
[dependencies]
ggen = { version = "3.0", features = ["security-strict"] }
```

Security-strict mode:
- Stricter input validation
- Enhanced logging
- Auditing enabled
- Rate limiting enabled

## Responsible Disclosure

We follow responsible disclosure principles:

1. **Privacy**: Vulnerability details kept confidential until patch
2. **Timeliness**: Fix developed promptly
3. **Collaboration**: Work with reporter on coordination
4. **Transparency**: Public acknowledgment of reporter (if desired)

## Security Incident Response

If a vulnerability is exploited:

1. Notify affected parties immediately
2. Release patch urgently
3. Public disclosure
4. Root cause analysis
5. Preventive measures

## Questions?

For security questions (non-vulnerability):
- Open a discussion in private mode (if available)
- Email `sean@chatmangpt.com` with `[SECURITY-QUESTION]` prefix
- Avoid disclosing specifics that could enable exploits

## Acknowledgments

We acknowledge responsible security researchers who report vulnerabilities:

- Listed in release notes (with permission)
- Mentioned in CONTRIBUTORS.md
- Recognized in security advisories

Thank you for helping keep ggen secure!
