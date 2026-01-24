# Security Policy for ggen (v6.0.0)

## Reporting Security Vulnerabilities

The ggen project takes security seriously. We appreciate your responsible disclosure of security vulnerabilities.

### Do NOT Report Publicly

**Never** open a public GitHub issue for security vulnerabilities. This may expose other users to the vulnerability.

### Report Privately

Please report security issues via email:

**Email**: `sean@chatmangpt.com`
**Subject**: `[SECURITY] ggen v6 vulnerability - [brief description]`

### Information to Include

Provide as much detail as possible:

```
- Vulnerability type (e.g., path traversal, SPARQL injection, command injection)
- Component affected (crate, module, function)
- Description of the vulnerability
- Steps to reproduce (if possible)
- Affected versions (v6.0.0+)
- Suggested fix (if you have one)
- Your name and contact (for acknowledgment)
```

### Response Timeline

- **Acknowledgment**: Within 48 hours
- **Initial assessment**: Within 1 week
- **Fix development**: Depends on severity
  - Critical (Remote Code Execution, Data Breach): 72 hours
  - High (Authentication Bypass, Injection): 1 week
  - Medium (Information Disclosure): 2 weeks
  - Low (Minor Issues): Next release
- **Public disclosure**: After patch release

## Security Practices (v6.0.0)

### Code Review

All code changes go through:
- Peer review (minimum 1 reviewer for features, 2 for security-critical changes)
- Automated security checks (clippy with `-D warnings`, cargo-audit)
- Test coverage analysis (minimum 80% for security-critical paths)
- Chicago TDD methodology (state-based testing, real collaborators)

### Dependencies

We maintain security through:

```bash
# Check for known vulnerabilities in dependencies
cargo make audit

# Update to patched versions
cargo update

# Deep inspection
cargo audit --deny warnings
```

Dependency security is checked:
- On every pull request (via cargo make pre-commit)
- Nightly in CI
- Before each release
- With vendored dependencies for reproducibility

### Unsafe Code Policy

Unsafe code is:
- **Minimized and isolated** (less than 1% of codebase)
- **Fully documented** with SAFETY comments explaining invariants
- **Reviewed thoroughly** by 2+ maintainers
- **Tested extensively** with property-based testing

Current unsafe usage (v6.0.0):
- **None in production code** - v6 eliminates all unsafe blocks
- Historical unsafe code (v5.x) has been refactored to safe alternatives
- If unsafe is required in future, it must be justified with FMEA analysis

### Timing Security

To prevent timing attacks on sensitive comparisons:

```rust
// ✗ Don't use direct comparison for secrets
if user_password == stored_password {
    // vulnerable to timing analysis
}

// ✓ Use constant-time comparison
use subtle::ConstantTimeEq;
if user_password.ct_eq(&stored_password).into() {
    // Safe from timing attacks
}
```

### v6.0.0 Security Enhancements

**Major Security Improvements:**

1. **SafePath System** - Prevents path traversal attacks
2. **SPARQL Query Builder** - Prevents SPARQL injection
3. **Command Injection Prevention** - Whitelist-based command execution
4. **Rate Limiting** - DoS protection for all APIs
5. **Input Validation** - Comprehensive validation at all boundaries
6. **Error Sanitization** - No information leakage in error messages
7. **Deterministic Receipts** - Cryptographic audit trail (SHA-256)
8. **Quality Gates** - Pre-flight validation before code generation

## Vulnerability Categories

### Critical Priority (P0)

- Remote code execution
- Authentication bypass
- Data integrity compromise (RDF corruption, template injection)
- SPARQL injection enabling arbitrary data access
- Path traversal enabling arbitrary file access

### High Priority (P1)

- Command injection
- Information disclosure (internal paths, secrets)
- Privilege escalation
- Denial of Service (resource exhaustion)
- Cryptographic weaknesses

### Medium Priority (P2)

- Logic errors in critical paths
- Race conditions in concurrent code
- Missing input validation
- Inadequate error handling

### Low Priority (P3)

- Cosmetic issues
- Non-critical warnings
- Edge cases with low impact
- Documentation gaps

## Security Architecture (v6.0.0)

### Defense in Depth

```
Untrusted Input
      ↓
[SafePath Validation] ← v6 NEW
      ↓
[Input Validation Layer] (PathValidator, EnvVarValidator, InputValidator)
      ↓
[SPARQL Query Builder] ← v6 NEW (prevents injection)
      ↓
[Schema Validation] (SHACL, RDF validation)
      ↓
[Rate Limiting] ← v6 NEW
      ↓
[Business Logic Execution]
      ↓
[Error Sanitization] ← v6 NEW (prevents info leakage)
      ↓
[Deterministic Output] (cryptographic receipts)
      ↓
Trusted Output
```

### Trust Boundaries

**External Boundaries:**
- CLI arguments (validated with Clap + custom validators)
- Environment variables (sanitized, length-limited)
- Configuration files (.toml, .ttl - SHACL validated)
- Template files (.tera - syntax validated, sandboxed execution)
- RDF ontologies (.ttl, .rdf - SHACL validated)

**Internal Boundaries:**
- RDF store ↔ Query engine (SPARQL builder prevents injection)
- Template engine ↔ Filesystem (SafePath prevents traversal)
- Command execution (whitelist-based, no shell expansion)

### Input Validation (v6.0.0)

All external inputs are validated with strict limits:

```rust
// Path validation (NEW in v6)
use ggen_core::security::SafePath;
let safe_path = SafePath::new("templates/user.tmpl")?;
let content = load_template(&safe_path)?;

// SPARQL query validation (NEW in v6)
use ggen_core::sparql::QueryBuilder;
let query = QueryBuilder::new()
    .select(&["?subject", "?predicate", "?object"])
    .where_clause("?subject ?predicate ?object")
    .limit(100)
    .build()?;

// Size limits
const MAX_FILE_SIZE: usize = 10 * 1024 * 1024; // 10MB
const MAX_RDF_TRIPLES: usize = 1_000_000; // 1M triples
const MAX_TEMPLATE_DEPTH: usize = 10; // Recursion limit

// Rate limits (NEW in v6)
const MAX_REQUESTS_PER_MINUTE: u32 = 60;
const MAX_CONCURRENT_GENERATIONS: usize = 10;
```

### Cryptographic Standards

- **Hashing**: SHA-256 (minimum, for receipts and content hashing)
- **Random**: `rand` crate with `OsRng` (cryptographically secure)
- **Secrets**: Never logged, never in error messages, never in receipts
- **Receipts**: HMAC-SHA256 for integrity verification

### Isolation (v6.0.0)

- **Filesystem isolation**: SafePath restricts all file operations to allowed directories
- **Template sandboxing**: Tera templates cannot access filesystem directly
- **SPARQL isolation**: Query builder prevents cross-ontology leakage
- **Process isolation**: No shell execution, direct command invocation only

## Known Security Considerations (v6.0.0)

### Path Traversal Prevention

**SafePath System (NEW in v6):**

```rust
// ✅ SAFE: Using SafePath
use ggen_core::security::SafePath;
let template_path = SafePath::new("templates/user.tmpl")?;
let content = load_template(&template_path)?;

// ❌ UNSAFE: Direct PathBuf (deprecated in v6)
let path = PathBuf::from(user_input); // Path traversal risk!
let content = fs::read_to_string(path)?;
```

**Protection Mechanisms:**
- Canonical path resolution
- Parent directory traversal blocking (`..` detection)
- Symbolic link resolution with boundary checks
- Allowed directory whitelist enforcement

### SPARQL Injection Prevention

**Query Builder (NEW in v6):**

```rust
// ✅ SAFE: Using QueryBuilder
use ggen_core::sparql::QueryBuilder;
let query = QueryBuilder::new()
    .select(&["?name", "?age"])
    .where_clause("?person rdf:type foaf:Person")
    .filter(&format!("?name = {}", QueryBuilder::escape_literal(user_input)))
    .build()?;

// ❌ UNSAFE: String concatenation (deprecated in v6)
let query = format!("SELECT ?name WHERE {{ ?person foaf:name '{}' }}", user_input);
```

**Protection Mechanisms:**
- Parameterized queries
- Input escaping for literals and URIs
- Query complexity limits (max depth, max results)
- Syntax validation before execution

### Resource Exhaustion Protection

**Rate Limiting (NEW in v6):**

```
- Max file size: 10MB
- Max RDF triples: 1M per ontology
- Max template recursion depth: 10 levels
- Max SPARQL results: 10,000 rows
- Max concurrent generations: 10
- Max requests per minute: 60
- Generation timeout: 120 seconds
```

### Dependency Vulnerabilities

Monitor dependencies with:

```bash
# Regular audits (via cargo make)
cargo make audit  # Uses cargo audit with deny warnings

# Update maintenance
cargo update

# Deep inspection
cargo audit --fetch deny
```

**v6 Dependency Security:**
- **Vendored dependencies**: Reproducible builds, reduced supply chain risk
- **Minimal dependencies**: Only essential crates (Tokio, Oxigraph, Tera, Clap, Serde)
- **Version pinning**: Cargo.lock committed to repository
- **Security-first crates**: Only maintained, well-audited crates

## Release Process Security

### Pre-release Checklist (v6.0.0)

- [ ] All tests passing (`cargo make test`)
- [ ] No compiler warnings (`cargo make check` with `-D warnings`)
- [ ] No clippy lints (`cargo make lint`)
- [ ] Security audit clean (`cargo make audit`)
- [ ] SHACL validation passes (`cargo make speckit-validate`)
- [ ] Deterministic receipts generated (`ggen sync --audit true`)
- [ ] Changelog documenting security fixes
- [ ] Security review completed (2+ reviewers)
- [ ] Version bump in Cargo.toml (all crates)
- [ ] Git tag signed with GPG key

### Vulnerability Announcement

For critical vulnerabilities:

1. **Develop patch** immediately (within 72 hours)
2. **Private notification** to known users (via email)
3. **Release patch** with minimal disclosure
4. **Public advisory** after patch is widely deployed (7 days)
5. **Full disclosure** in release notes and SECURITY.md
6. **Credit researcher** (with permission) in acknowledgments

## Security Updates (v6.0.0)

### Version Support

| Version | Status | Security Updates | End of Life |
|---------|--------|------------------|-------------|
| 6.x | Stable | ✓ Active (all severity) | TBD |
| 5.x | Legacy | ✓ Critical only | 2026-06-01 |
| 4.x | Unsupported | ✗ None | 2025-12-31 |
| 3.x | Unsupported | ✗ None | 2025-06-30 |

### Update Cadence

- **Security patches**: Released within 72 hours (critical), 1 week (high), 2 weeks (medium)
- **Minor updates**: Monthly (v6.1.0, v6.2.0, etc.)
- **Major releases**: Quarterly (v7.0.0 planned Q2 2026)

## Compliance

### Standards Followed

- **OWASP Top 10 (2021)**: Guidance for web/API security
- **SANS Top 25 (2024)**: Common software security weaknesses
- **CWE**: Common Weakness Enumeration reference
- **NIST Cybersecurity Framework**: Risk management guidance
- **Rust Secure Code Guidelines**: Rust-specific best practices

### Code Review Standards (v6.0.0)

All code must pass:

```bash
# Via cargo make (enforces timeouts and quality gates)
cargo make check       # <5s compile check, warnings-as-errors
cargo make lint        # <60s clippy -D warnings
cargo make test        # <30s all tests passing
cargo make audit       # Security vulnerability scan
cargo make pre-commit  # All quality gates
```

**Prohibited in Production Code:**
- `unwrap()`, `expect()` (use `Result<T, E>` and `?` operator)
- `panic!()` (use graceful error handling)
- Direct shell execution (`sh -c`, replaced with whitelist commands)
- String-based SPARQL queries (use QueryBuilder)
- Direct PathBuf from user input (use SafePath)
- Hardcoded secrets or credentials

## Security Hardening (v6.0.0)

### Enable Security Features

In your `Cargo.toml`:

```toml
[dependencies]
ggen = { version = "6.0", features = ["security-strict"] }
```

Security-strict mode:
- Stricter input validation (rejects edge cases)
- Enhanced logging (all security events)
- Audit trail enabled (cryptographic receipts)
- Rate limiting enforced (all APIs)
- SPARQL complexity limits (max depth, results)

### Configuration Security

**Secure defaults in ggen.toml:**

```toml
[security]
# Path validation
allowed_template_dirs = ["templates", ".specify/templates"]
allowed_output_dirs = ["output", "generated"]

# SPARQL limits
max_sparql_results = 10000
max_sparql_depth = 10

# Rate limiting
max_requests_per_minute = 60
max_concurrent_generations = 10

# Generation limits
max_file_size_mb = 10
max_template_depth = 10
generation_timeout_seconds = 120

# Audit
enable_receipts = true
receipt_storage_path = ".ggen/receipts"
```

## Responsible Disclosure

We follow responsible disclosure principles:

1. **Privacy**: Vulnerability details kept confidential until patch
2. **Timeliness**: Fix developed promptly based on severity
3. **Collaboration**: Work with reporter on coordination and testing
4. **Transparency**: Public acknowledgment of reporter (if desired)
5. **Notification**: Inform affected parties before public disclosure

## Security Incident Response

If a vulnerability is exploited:

1. **Immediate containment**: Notify affected parties, disable vulnerable features
2. **Patch development**: Urgent fix with security team collaboration
3. **Testing**: Comprehensive validation of fix
4. **Release**: Coordinated patch release with advisory
5. **Public disclosure**: Full details after patch deployment
6. **Root cause analysis**: 5 Whys to prevent recurrence
7. **Preventive measures**: Update documentation, add tests, improve tooling

See [docs/security/INCIDENT_RESPONSE.md](docs/security/INCIDENT_RESPONSE.md) for detailed procedures.

## Questions?

For security questions (non-vulnerability):
- Open a discussion in private mode (GitHub Discussions)
- Email `sean@chatmangpt.com` with `[SECURITY-QUESTION]` prefix
- Avoid disclosing specifics that could enable exploits

For security documentation:
- [Security Architecture](docs/security/ARCHITECTURE.md)
- [Safe Coding Guidelines](docs/security/SAFE_CODING.md)
- [Security Testing](docs/security/TESTING.md)
- [v6 Migration Guide](docs/security/V6_MIGRATION.md)
- [Security Checklist](docs/security/CHECKLIST.md)

## Acknowledgments

We acknowledge responsible security researchers who report vulnerabilities:

- Listed in release notes (with permission)
- Mentioned in CONTRIBUTORS.md
- Recognized in security advisories
- Credited in CHANGELOG.md

**Hall of Fame (v6.0.0):**
- (No vulnerabilities reported yet - be the first!)

Thank you for helping keep ggen secure!

---

**Last Updated**: 2026-01-24 (v6.0.0)
**Security Contact**: sean@chatmangpt.com
**GPG Key**: (Available on request)
