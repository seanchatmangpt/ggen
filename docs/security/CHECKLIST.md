<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Developer Security Checklist (v6.0.0)](#developer-security-checklist-v600)
  - [Overview](#overview)
  - [Pre-Development Checklist](#pre-development-checklist)
  - [Code Development Checklist](#code-development-checklist)
    - [Input Validation](#input-validation)
    - [Safe Coding Patterns](#safe-coding-patterns)
    - [File Operations](#file-operations)
    - [SPARQL Queries](#sparql-queries)
    - [Template Rendering](#template-rendering)
    - [Error Handling](#error-handling)
    - [Rate Limiting](#rate-limiting)
    - [Cryptography](#cryptography)
    - [Dependencies](#dependencies)
  - [Testing Checklist](#testing-checklist)
    - [Unit Tests](#unit-tests)
    - [Integration Tests](#integration-tests)
    - [Property-Based Tests](#property-based-tests)
    - [Performance Tests](#performance-tests)
  - [Pre-Commit Checklist](#pre-commit-checklist)
  - [Pull Request Checklist](#pull-request-checklist)
  - [Release Checklist](#release-checklist)
  - [Deployment Checklist](#deployment-checklist)
  - [Incident Response Checklist](#incident-response-checklist)
  - [Security Best Practices Quick Reference](#security-best-practices-quick-reference)
    - [File Operations](#file-operations-1)
    - [SPARQL Queries](#sparql-queries-1)
    - [Error Handling](#error-handling-1)
    - [Secrets Management](#secrets-management)
  - [Common Security Mistakes](#common-security-mistakes)
    - [Mistake 1: Using `.unwrap()` in library code](#mistake-1-using-unwrap-in-library-code)
    - [Mistake 2: Forgetting to validate user input](#mistake-2-forgetting-to-validate-user-input)
    - [Mistake 3: Exposing internal errors to users](#mistake-3-exposing-internal-errors-to-users)
    - [Mistake 4: String concatenation for SPARQL](#mistake-4-string-concatenation-for-sparql)
    - [Mistake 5: Not enforcing rate limits](#mistake-5-not-enforcing-rate-limits)
  - [References](#references)
  - [Certification](#certification)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Developer Security Checklist (v6.0.0)

## Overview

This checklist ensures all security best practices are followed when developing for ggen v6.0.0. Use this before submitting pull requests or deploying code.

**Last Updated**: 2026-01-24
**Version**: 6.0.0
**Audience**: All developers contributing to ggen

---

## Pre-Development Checklist

Before writing code, ensure:

- [ ] **Requirements understood** - Security implications of the feature are clear
- [ ] **Threat model reviewed** - Known attack vectors are considered
- [ ] **Similar code reviewed** - Learn from existing secure implementations
- [ ] **Security docs read** - [SAFE_CODING.md](SAFE_CODING.md), [ARCHITECTURE.md](ARCHITECTURE.md)

---

## Code Development Checklist

### Input Validation

- [ ] **All user inputs validated** - CLI args, config files, environment variables
- [ ] **SafePath used for all file paths** - No direct `PathBuf::from()` with user input
- [ ] **QueryBuilder used for all SPARQL queries** - No string concatenation
- [ ] **Size limits enforced** - Files, RDF graphs, SPARQL results
- [ ] **Character validation** - Only allowed characters accepted
- [ ] **No code injection** - Templates, SPARQL, file paths validated

### Safe Coding Patterns

- [ ] **No `unwrap()` in production code** - Use `?` operator and `Result<T, E>`
- [ ] **No `expect()` in production code** - Use proper error handling
- [ ] **No `panic!()` in library code** - Use graceful error handling
- [ ] **No hardcoded secrets** - Use environment variables or secure stores
- [ ] **No secrets in logs** - Sanitize before logging
- [ ] **No secrets in error messages** - Use ErrorSanitizer

### File Operations

- [ ] **SafePath for all file operations** - Read, write, delete, list
- [ ] **Directory restrictions enforced** - `.within_directory()` used
- [ ] **Parent directories created safely** - `fs::create_dir_all()` with SafePath
- [ ] **File overwrites controlled** - Explicit `--force` flag required
- [ ] **Symbolic links resolved** - SafePath handles this automatically

### SPARQL Queries

- [ ] **QueryBuilder for all queries** - No raw SPARQL strings
- [ ] **User input escaped** - `escape_literal()` or `escape_uri()` used
- [ ] **Query complexity limited** - Max depth, max results
- [ ] **Syntax validated** - `.build()` validates before execution
- [ ] **No string concatenation** - Use builder methods only

### Template Rendering

- [ ] **Templates loaded from allowed directories only** - Via SafePath
- [ ] **Context validated** - No `__proto__`, no excessively large values
- [ ] **Sandbox enforced** - No filesystem access, no command execution
- [ ] **Recursion depth limited** - Max 10 levels
- [ ] **User input escaped** - When passed to templates

### Error Handling

- [ ] **Errors sanitized for users** - ErrorSanitizer used
- [ ] **Full errors logged internally** - For debugging
- [ ] **No path leakage** - Absolute paths removed from user messages
- [ ] **No credential leakage** - Passwords, tokens, keys removed
- [ ] **Generic messages for security errors** - Don't reveal attack details

### Rate Limiting

- [ ] **Rate limits enforced** - max_requests_per_minute, max_concurrent_generations
- [ ] **Resource limits enforced** - max_file_size, max_rdf_triples
- [ ] **Timeout enforced** - generation_timeout_seconds
- [ ] **Rate limit errors handled** - Return 429 Too Many Requests (future API)

### Cryptography

- [ ] **Secure random numbers** - Use `OsRng`, not `rand::thread_rng()`
- [ ] **Strong hashing** - SHA-256 minimum, not MD5/SHA1
- [ ] **Constant-time comparison** - For secrets, use `subtle::ConstantTimeEq`
- [ ] **No hardcoded keys** - Use environment variables or key derivation

### Dependencies

- [ ] **Minimal dependencies** - Only essential crates
- [ ] **Dependencies audited** - `cargo audit` passes
- [ ] **No deprecated crates** - Check crates.io for maintenance status
- [ ] **Versions pinned** - Cargo.lock committed to repository

---

## Testing Checklist

### Unit Tests

- [ ] **Security tests written** - For all security-critical code
- [ ] **Attack vectors tested** - Path traversal, SPARQL injection, etc.
- [ ] **Edge cases tested** - Empty inputs, large inputs, special characters
- [ ] **Error paths tested** - All error conditions exercised
- [ ] **Chicago TDD followed** - State-based, real collaborators, AAA pattern

### Integration Tests

- [ ] **End-to-end security tests** - Full workflow with malicious input
- [ ] **Cross-component tests** - Security boundaries tested
- [ ] **Rate limiting tested** - Burst requests, concurrent operations
- [ ] **Template security tested** - Injection attempts blocked

### Property-Based Tests

- [ ] **Fuzz tests written** - For SafePath, QueryBuilder, parsers
- [ ] **Random inputs tested** - Property tests with `proptest`
- [ ] **No crashes on any input** - Graceful error handling

### Performance Tests

- [ ] **DoS resistance tested** - Large files, deep graphs, complex queries
- [ ] **Timeout enforcement tested** - Long-running operations canceled
- [ ] **Resource limits tested** - Memory, CPU, disk usage

---

## Pre-Commit Checklist

Before committing code:

- [ ] **All tests passing** - `cargo make test`
- [ ] **No compiler warnings** - `cargo make check` (with `-D warnings`)
- [ ] **No linting errors** - `cargo make lint` (clippy `-D warnings`)
- [ ] **Security audit clean** - `cargo make audit`
- [ ] **Code formatted** - `cargo make fmt`
- [ ] **Documentation updated** - Docstrings, README, CHANGELOG

---

## Pull Request Checklist

Before submitting PR:

- [ ] **Security review requested** - Tag security team
- [ ] **Threat model updated** - If adding new features
- [ ] **Tests included** - For all new code
- [ ] **Security tests included** - For security-critical code
- [ ] **Documentation updated** - User-facing and internal docs
- [ ] **Breaking changes documented** - Migration guide provided
- [ ] **Changelog updated** - Security fixes noted

---

## Release Checklist

Before releasing:

- [ ] **All tests passing** - `cargo make test`
- [ ] **Security audit clean** - `cargo make audit`
- [ ] **SHACL validation passes** - `cargo make speckit-validate`
- [ ] **Deterministic receipts generated** - `ggen sync --audit true`
- [ ] **Changelog updated** - Security fixes highlighted
- [ ] **Security review completed** - 2+ reviewers
- [ ] **Version bumped** - Major/minor/patch as appropriate
- [ ] **Git tag signed** - GPG signature required

---

## Deployment Checklist

Before deploying:

- [ ] **Configuration reviewed** - No secrets in config files
- [ ] **Environment variables set** - All required secrets
- [ ] **Rate limits configured** - Appropriate for environment
- [ ] **Monitoring enabled** - Security metrics tracked
- [ ] **Alerting configured** - Rate limit violations, security errors
- [ ] **Backup created** - Rollback plan ready

---

## Incident Response Checklist

If security incident detected:

- [ ] **Incident reported** - Email security@ggen.dev
- [ ] **Severity assessed** - P0/P1/P2/P3
- [ ] **Incident commander assigned** - See [INCIDENT_RESPONSE.md](INCIDENT_RESPONSE.md)
- [ ] **Containment measures deployed** - Disable feature, rate limit
- [ ] **Evidence preserved** - Logs, receipts copied to secure location
- [ ] **Patch developed** - Fix vulnerability
- [ ] **Patch tested** - Comprehensive validation
- [ ] **Patch released** - Emergency release if P0/P1
- [ ] **Users notified** - Security advisory published
- [ ] **Post-incident review** - 5 Whys, action items

---

## Security Best Practices Quick Reference

### File Operations

```rust
// ✅ DO
let path = SafePath::new(user_input)?;
let content = fs::read_to_string(path.as_path())?;

// ❌ DON'T
let path = PathBuf::from(user_input);  // Path traversal risk!
let content = fs::read_to_string(path)?;
```

### SPARQL Queries

```rust
// ✅ DO
let query = QueryBuilder::new()
    .select(&["?s"])
    .filter(&format!("?s = {}", QueryBuilder::escape_literal(user_input)))
    .build()?;

// ❌ DON'T
let query = format!("SELECT ?s WHERE {{ ?s ?p '{}' }}", user_input);  // Injection risk!
```

### Error Handling

```rust
// ✅ DO
let sanitized = ErrorSanitizer::file_error("read", path, &err);
log::error!("{}", sanitized.internal_message());  // Full details
return Err(sanitized.user_message());  // Safe for users

// ❌ DON'T
return Err(format!("Failed to read {}: {}", path.display(), err));  // Leaks paths!
```

### Secrets Management

```rust
// ✅ DO
let api_key = env::var("GGEN_API_KEY")?;
if api_key.as_bytes().ct_eq(expected.as_bytes()).into() {  // Constant-time
    // ...
}

// ❌ DON'T
const API_KEY: &str = "sk-1234...";  // Hardcoded secret!
if api_key == expected {  // Timing attack vulnerable!
    // ...
}
```

---

## Common Security Mistakes

### Mistake 1: Using `.unwrap()` in library code

**Problem**: Panics crash the program

```rust
// ❌ BAD
let config = load_config().unwrap();  // Crash if config missing!

// ✅ GOOD
let config = load_config()?;  // Propagate error
```

### Mistake 2: Forgetting to validate user input

**Problem**: Path traversal, injection attacks

```rust
// ❌ BAD
let path = PathBuf::from(user_input);  // No validation!
fs::write(&path, content)?;

// ✅ GOOD
let path = SafePath::new(user_input)?  // Validated
    .within_directory(Path::new("output"))?;
fs::write(path.as_path(), content)?;
```

### Mistake 3: Exposing internal errors to users

**Problem**: Information disclosure

```rust
// ❌ BAD
eprintln!("Error: {}", internal_error);  // Leaks paths, details!

// ✅ GOOD
let sanitized = ErrorSanitizer::sanitize(&internal_error);
eprintln!("{}", sanitized.user_message());  // Safe for users
log::error!("{}", sanitized.internal_message());  // Full details for logs
```

### Mistake 4: String concatenation for SPARQL

**Problem**: SPARQL injection

```rust
// ❌ BAD
let query = format!("SELECT * WHERE {{ ?s foaf:name '{}' }}", user_input);

// ✅ GOOD
let query = QueryBuilder::new()
    .select(&["?s"])
    .where_clause("?s foaf:name ?name")
    .filter(&format!("?name = {}", QueryBuilder::escape_literal(user_input)))
    .build()?;
```

### Mistake 5: Not enforcing rate limits

**Problem**: Denial of service

```rust
// ❌ BAD
fn generate(input: &Input) -> Result<Output, Error> {
    // No rate limiting!
    expensive_operation(input)
}

// ✅ GOOD
fn generate(input: &Input, limiter: &RateLimiter) -> Result<Output, Error> {
    limiter.check_and_increment(client_id)?;
    expensive_operation(input)
}
```

---

## References

- [Safe Coding Guidelines](SAFE_CODING.md)
- [Security Architecture](ARCHITECTURE.md)
- [Security Testing](TESTING.md)
- [v6 Migration Guide](V6_MIGRATION.md)
- [Incident Response Plan](INCIDENT_RESPONSE.md)

---

## Certification

**I certify that I have:**
- [ ] Read and understood all security documentation
- [ ] Reviewed this checklist before submitting code
- [ ] Verified all checklist items for my changes
- [ ] Ensured no security vulnerabilities introduced

**Developer Name**: _______________
**Date**: _______________
**Pull Request**: _______________

---

**Last Updated**: 2026-01-24
**Next Review**: 2026-04-24
**Owner**: Security Team
