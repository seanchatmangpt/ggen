# Security Integration Test Suite

End-to-end security integration tests for ggen v6.0.0.

## Overview

This test suite validates security controls across the entire ggen system using real infrastructure (file system, RDF stores, Redis) and follows Chicago TDD principles.

## Test Suites

### 1. Path Traversal Prevention (`path_traversal_tests.rs`)

Tests file system security across all file operations:

**Attack Vectors Tested:**
- Classic directory traversal (`../../../etc/passwd`)
- Windows path traversal (`..\\..\\..\\windows\\system32`)
- Absolute path injection (`/etc/passwd`)
- Symlink attacks
- Null byte injection (`file.txt\0/etc/passwd`)
- Unicode normalization attacks (fullwidth characters)

**Coverage:**
- Template file loading
- RDF file access
- Output file generation
- Configuration file access
- Error message sanitization

**Run:** `cargo make test-security`

### 2. SPARQL Injection Prevention (`sparql_injection_tests.rs`)

Tests query injection attack prevention:

**Attack Vectors Tested:**
- UNION-based injection
- Comment injection (`#`, `--`, `/* */`)
- Filter bypass (`') OR (1=1`)
- Graph traversal injection
- Property path manipulation
- SERVICE directive injection
- Blind timing attacks

**Coverage:**
- Query construction from user input
- Template variable substitution
- RDF data extraction
- Parameterized queries
- Error message sanitization

**Run:** `cargo test --test security_tests sparql_injection`

### 3. Rate Limiting (`rate_limit_integration_tests.rs`)

Tests DoS prevention with real Redis:

**Scenarios Tested:**
- Concurrent client requests
- IP-based rate limiting
- API key-based limiting
- Burst behavior
- Rate limit recovery after window expiration
- Distributed rate limiting (multiple instances)
- Rate limit headers

**Infrastructure:**
- Uses testcontainers for Redis
- Tests real rate limiter implementation
- Validates distributed state

**Run:** `cargo test --test security_tests rate_limit -- --ignored`

**Note:** Requires running ggen API server. Mark as `#[ignore]` until server implementation is complete.

### 4. Input Validation (`input_validation_tests.rs`)

Tests malicious input handling across all interfaces:

**Input Types Tested:**
- Template inputs (malformed, code execution attempts, oversized)
- RDF files (invalid syntax, XXE attacks, oversized)
- Configuration files (invalid TOML, dangerous values)
- CLI arguments (negative numbers, extremely long, special characters)
- Environment variables (malicious values, oversized)

**Validation Checks:**
- Syntax validation
- Size limits
- Character restrictions
- Type validation
- Boundary testing

**Run:** `cargo test --test security_tests input_validation`

### 5. Secrets Protection (`secrets_protection_tests.rs`)

Tests credential leakage prevention:

**Secret Types Tested:**
- API keys (`sk_live_*`)
- Passwords
- GitHub tokens (`ghp_*`)
- AWS secrets
- Private keys
- Connection strings

**Leakage Vectors Checked:**
- Application logs (stdout/stderr)
- Error messages
- Stack traces
- Generated output files
- Environment variable dumps
- Debug commands

**Redaction Patterns:**
- `sk_live_***` for API keys
- `***` for passwords
- `ghp_***` for GitHub tokens
- Generic error messages
- Sanitized stack traces

**Run:** `cargo test --test security_tests secrets_protection`

## Running Tests

### All Security Tests
```bash
cargo make test-security
```

### Individual Test Suites
```bash
# Path traversal tests
cargo test --test security_tests path_traversal

# SPARQL injection tests
cargo test --test security_tests sparql_injection

# Rate limiting tests (requires running API)
cargo test --test security_tests rate_limit -- --ignored

# Input validation tests
cargo test --test security_tests input_validation

# Secrets protection tests
cargo test --test security_tests secrets_protection
```

### With Verbose Output
```bash
cargo test --test security_tests -- --nocapture --test-threads=1
```

## CI/CD Integration

Security tests run automatically in GitHub Actions:

**Workflow:** `.github/workflows/security.yml`

**Jobs:**
1. **security-tests**: Runs all test suites
2. **dependency-audit**: `cargo audit` for known vulnerabilities
3. **sast-analysis**: Semgrep static analysis
4. **supply-chain-security**: `cargo deny` for supply chain risks
5. **security-summary**: Aggregates results

**Triggers:**
- Push to main/master/develop
- Pull requests
- Daily scheduled run (2 AM UTC)

## Test Principles (Chicago TDD)

All tests follow these principles:

1. **AAA Pattern**: Arrange → Act → Assert
2. **Real Collaborators**: Actual file system, RDF stores, Redis (no mocks)
3. **State-Based Verification**: Test observable outputs and state changes
4. **Behavior Verification**: Verify what code does, not how it does it
5. **No Meaningless Tests**: Every test verifies concrete security properties

## Test Fixtures

All tests use dedicated fixtures for isolation:

- `PathTraversalFixture`: Isolated workspace with secrets outside
- `SparqlInjectionFixture`: RDF store with sensitive data
- `RateLimitFixture`: Redis container + API client
- `InputValidationFixture`: Temporary workspace
- `SecretsFixture`: Environment with various secret types

## Expected Behavior

### What Should Fail

1. **Path Traversal**: Any attempt to access files outside workspace
2. **SPARQL Injection**: Any query manipulation that exposes sensitive data
3. **Rate Limiting**: Requests exceeding configured limits
4. **Invalid Input**: Malformed templates, RDF, config, oversized inputs
5. **Secret Leakage**: Any sensitive value appearing in logs/errors

### What Should Succeed

1. **Legitimate Paths**: Files within workspace boundaries
2. **Valid Queries**: Properly parameterized SPARQL queries
3. **Normal Usage**: Requests within rate limits
4. **Valid Input**: Well-formed templates, RDF, configuration
5. **Public Data**: Non-sensitive information in logs/output

## Security Coverage Report

Generate coverage report:

```bash
cargo tarpaulin --test security_tests --out Html
open tarpaulin-report.html
```

Expected coverage: **80%+** for security-critical code paths.

## Known Limitations

1. **Rate Limiting Tests**: Marked `#[ignore]` until API server is implemented
2. **Timing Attacks**: Basic detection only (not cryptographically secure)
3. **Network Tests**: Require running containers (Docker)
4. **Performance**: Full suite takes ~60 seconds

## Reporting Security Issues

**DO NOT** open public issues for security vulnerabilities.

Report to: security@ggen.dev (or maintainer email)

Include:
- Attack vector
- Proof of concept
- Impact assessment
- Suggested fix

## References

- [OWASP Top 10](https://owasp.org/www-project-top-ten/)
- [CWE Top 25](https://cwe.mitre.org/top25/)
- [SPARQL Injection](https://www.owasp.org/index.php/SPARQL_Injection)
- [Path Traversal](https://owasp.org/www-community/attacks/Path_Traversal)
- [Rate Limiting Best Practices](https://cloud.google.com/architecture/rate-limiting-strategies-techniques)

## Maintenance

**Update frequency:** After every security-relevant change

**Review schedule:** Monthly security audit

**Responsible:** Security team + Test Engineer
