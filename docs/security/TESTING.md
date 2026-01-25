<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Security Testing Guide (v6.0.0)](#security-testing-guide-v600)
  - [Overview](#overview)
  - [Table of Contents](#table-of-contents)
  - [Security Test Categories](#security-test-categories)
    - [1. Unit Security Tests](#1-unit-security-tests)
    - [2. Integration Security Tests](#2-integration-security-tests)
    - [3. Property-Based Security Tests](#3-property-based-security-tests)
    - [4. Performance Security Tests](#4-performance-security-tests)
    - [5. Regression Security Tests](#5-regression-security-tests)
  - [Fuzzing Strategy](#fuzzing-strategy)
    - [AFL Fuzzing](#afl-fuzzing)
    - [LibFuzzer](#libfuzzer)
  - [Penetration Testing Checklist](#penetration-testing-checklist)
    - [Automated Penetration Testing](#automated-penetration-testing)
    - [Manual Penetration Testing](#manual-penetration-testing)
      - [Path Traversal Testing](#path-traversal-testing)
      - [SPARQL Injection Testing](#sparql-injection-testing)
      - [Template Injection Testing](#template-injection-testing)
      - [Rate Limiting Testing](#rate-limiting-testing)
  - [Dependency Scanning Process](#dependency-scanning-process)
    - [Cargo Audit](#cargo-audit)
    - [Dependency Review](#dependency-review)
  - [Static Analysis Tools](#static-analysis-tools)
    - [Clippy (Linting)](#clippy-linting)
    - [Cargo Deny](#cargo-deny)
    - [MiriRun Safety Checks)](#mirirun-safety-checks)
  - [Security Test Automation](#security-test-automation)
    - [Pre-commit Hooks](#pre-commit-hooks)
    - [CI/CD Pipeline](#cicd-pipeline)
  - [Continuous Security Testing](#continuous-security-testing)
    - [Scheduled Scans](#scheduled-scans)
    - [Security Metrics](#security-metrics)
  - [Security Test Execution](#security-test-execution)
    - [Running All Security Tests](#running-all-security-tests)
    - [Test Organization](#test-organization)
  - [References](#references)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Security Testing Guide (v6.0.0)

## Overview

This document describes the security testing strategy for ggen v6.0.0, including test categories, fuzzing strategy, penetration testing, dependency scanning, and static analysis.

**Last Updated**: 2026-01-24
**Version**: 6.0.0
**Audience**: Developers, Security Engineers, QA

---

## Table of Contents

1. [Security Test Categories](#security-test-categories)
2. [Fuzzing Strategy](#fuzzing-strategy)
3. [Penetration Testing Checklist](#penetration-testing-checklist)
4. [Dependency Scanning Process](#dependency-scanning-process)
5. [Static Analysis Tools](#static-analysis-tools)
6. [Security Test Automation](#security-test-automation)
7. [Continuous Security Testing](#continuous-security-testing)

---

## Security Test Categories

### 1. Unit Security Tests

**Purpose**: Test individual security components in isolation

**Coverage**:
- SafePath validation
- QueryBuilder injection prevention
- Input validators
- Error sanitizers
- Rate limiters

**Example**:
```rust
#[test]
fn test_safepath_blocks_traversal() {
    // Test path traversal prevention
    let result = SafePath::new("../../../etc/passwd");
    assert!(result.is_err());
    assert!(result.unwrap_err().to_string().contains("path traversal"));
}

#[test]
fn test_query_builder_escapes_literals() {
    // Test SPARQL injection prevention
    let malicious = "'; DROP TABLE users; --";
    let escaped = QueryBuilder::escape_literal(malicious);
    assert!(!escaped.contains("DROP"));
    assert!(!escaped.contains(";"));
}

#[test]
fn test_error_sanitizer_removes_paths() {
    // Test information disclosure prevention
    let internal_error = "/home/user/.config/ggen/secret.key: Permission denied";
    let sanitized = ErrorSanitizer::sanitize(internal_error);
    assert!(!sanitized.user_message().contains("/home/user"));
    assert!(sanitized.user_message().contains("secret.key"));
}
```

**Location**: `crates/ggen-core/tests/security/`

---

### 2. Integration Security Tests

**Purpose**: Test security across component boundaries

**Coverage**:
- End-to-end path validation
- SPARQL query execution with injection attempts
- Template rendering with malicious input
- Rate limiting across multiple requests

**Example**:
```rust
#[test]
fn test_e2e_template_injection_prevention() {
    // Attempt template injection
    let malicious_template = r#"
        {{ system("rm -rf /") }}
        {% include "/etc/passwd" %}
    "#;

    let result = TemplateEngine::new()
        .render_string(malicious_template, &Context::new());

    // Should fail or render safely (no system access)
    assert!(result.is_err() || !result.unwrap().contains("root:"));
}

#[test]
fn test_e2e_sparql_injection_prevention() {
    let malicious_input = "'; DROP GRAPH <http://example.com>; --";

    let query = QueryBuilder::new()
        .select(&["?s"])
        .where_clause("?s foaf:name ?name")
        .filter(&format!("?name = {}", QueryBuilder::escape_literal(malicious_input)))
        .build()
        .unwrap();

    // Execute query
    let results = execute_query(&query);

    // Should return empty results, not drop graph
    assert!(results.is_ok());
    assert!(graph_exists("http://example.com"));
}
```

**Location**: `tests/security/integration/`

---

### 3. Property-Based Security Tests

**Purpose**: Fuzz security components with random inputs

**Coverage**:
- SafePath with random paths
- QueryBuilder with random SPARQL fragments
- Input validators with random strings

**Example** (using `proptest`):
```rust
use proptest::prelude::*;

proptest! {
    #[test]
    fn safepath_never_allows_traversal(path in ".*") {
        if path.contains("..") || path.starts_with('/') {
            // Should always reject
            assert!(SafePath::new(&path).is_err());
        }
    }

    #[test]
    fn query_builder_escapes_all_literals(input in ".*") {
        let escaped = QueryBuilder::escape_literal(&input);

        // Should never contain unescaped quotes or semicolons
        assert!(!escaped.contains("';"));
        assert!(!escaped.contains("\";"));
    }

    #[test]
    fn error_sanitizer_never_leaks_paths(error_msg in ".*") {
        let sanitized = ErrorSanitizer::sanitize(&error_msg);
        let user_msg = sanitized.user_message();

        // Should never contain absolute paths
        assert!(!user_msg.starts_with('/'));
        assert!(!user_msg.contains("C:\\"));
    }
}
```

**Location**: `crates/ggen-core/tests/property/`

---

### 4. Performance Security Tests

**Purpose**: Test DoS resistance and resource limits

**Coverage**:
- Large file handling
- Deep RDF graphs
- Complex SPARQL queries
- Recursive templates

**Example**:
```rust
#[test]
fn test_large_file_rejection() {
    let large_file = vec![b'A'; 100 * 1024 * 1024];  // 100MB

    let result = validate_file_size(large_file.len());
    assert!(result.is_err());
    assert!(result.unwrap_err().to_string().contains("too large"));
}

#[test]
fn test_deep_rdf_graph_rejection() {
    let mut graph = Graph::new();

    // Create deeply nested graph (1000 levels)
    for i in 0..1000 {
        graph.add_triple(
            &format!("http://ex.com/node{}", i),
            "ex:child",
            &format!("http://ex.com/node{}", i + 1),
        );
    }

    let validator = RDFValidator::new().max_depth(100);
    let result = validator.validate(&graph);
    assert!(result.is_err());
}

#[test]
#[timeout(Duration::from_secs(5))]
fn test_complex_sparql_timeout() {
    // Complex query that should timeout
    let query = create_complex_query(depth: 100);

    let result = execute_query_with_timeout(&query, Duration::from_secs(1));
    assert!(result.is_err());
    assert!(result.unwrap_err().to_string().contains("timeout"));
}
```

**Location**: `crates/ggen-core/tests/performance/security/`

---

### 5. Regression Security Tests

**Purpose**: Ensure past vulnerabilities don't reappear

**Coverage**:
- All CVEs fixed
- All security advisories addressed
- All bug bounty findings resolved

**Example**:
```rust
/// Regression test for CVE-2025-XXXX (hypothetical)
/// Path traversal via symbolic links
#[test]
fn test_cve_2025_xxxx_symlink_traversal() {
    let tempdir = tempfile::tempdir().unwrap();
    let symlink = tempdir.path().join("symlink");

    // Create symlink pointing to /etc/passwd
    std::os::unix::fs::symlink("/etc/passwd", &symlink).unwrap();

    // Should reject symlink
    let result = SafePath::new(symlink.to_str().unwrap());
    assert!(result.is_err());
}
```

**Location**: `tests/security/regression/`

---

## Fuzzing Strategy

### AFL Fuzzing

**Target**: SafePath, QueryBuilder, RDFParser, TemplateEngine

**Setup**:
```bash
# Install AFL
cargo install cargo-afl

# Build fuzz targets
cd fuzz
cargo afl build

# Run fuzzing
cargo afl fuzz -i seeds -o findings target/debug/fuzz_safepath
```

**Fuzz Targets**:

**fuzz_safepath.rs**:
```rust
#[macro_use]
extern crate afl;

use ggen_core::security::SafePath;

fn main() {
    fuzz!(|data: &[u8]| {
        if let Ok(s) = std::str::from_utf8(data) {
            let _ = SafePath::new(s);
        }
    });
}
```

**fuzz_query_builder.rs**:
```rust
#[macro_use]
extern crate afl;

use ggen_core::sparql::QueryBuilder;

fn main() {
    fuzz!(|data: &[u8]| {
        if let Ok(s) = std::str::from_utf8(data) {
            let _ = QueryBuilder::new()
                .select(&["?s"])
                .filter(s)
                .build();
        }
    });
}
```

### LibFuzzer

**Alternative to AFL, integrated with Rust**:

```bash
# Install libfuzzer
cargo install cargo-fuzz

# Create fuzz target
cargo fuzz add fuzz_safepath

# Run fuzzing
cargo fuzz run fuzz_safepath
```

**Fuzzing Corpus**:
- Seed inputs: `fuzz/seeds/`
- Findings: `fuzz/findings/`
- Crashes: `fuzz/crashes/`

---

## Penetration Testing Checklist

### Automated Penetration Testing

**Tools**:
- OWASP ZAP (for future API endpoints)
- Burp Suite (for future web interface)
- Nuclei (template-based scanning)

### Manual Penetration Testing

**Checklist**:

#### Path Traversal Testing

- [ ] Test `..` sequences (Unix)
- [ ] Test `..\\` sequences (Windows)
- [ ] Test absolute paths (`/etc/passwd`, `C:\Windows\System32`)
- [ ] Test symbolic links
- [ ] Test URL-encoded traversal (`%2e%2e%2f`)
- [ ] Test double-encoded traversal (`%252e%252e%252f`)
- [ ] Test Unicode normalization (`\u002e\u002e\u002f`)

**Example Tests**:
```bash
# Unix path traversal
ggen sync --template="../../../etc/passwd"

# Windows path traversal
ggen sync --template="..\\..\\..\\Windows\\System32\\config\\SAM"

# Absolute path
ggen sync --template="/etc/passwd"

# Symbolic link
ln -s /etc/passwd templates/evil
ggen sync --template="evil"

# URL-encoded
ggen sync --template="%2e%2e%2f%2e%2e%2f%2e%2e%2fetc%2fpasswd"
```

#### SPARQL Injection Testing

- [ ] Test single quotes (`'`)
- [ ] Test double quotes (`"`)
- [ ] Test semicolons (`;`)
- [ ] Test comments (`#`, `--`)
- [ ] Test UNION injections
- [ ] Test DROP GRAPH attacks
- [ ] Test INSERT DATA attacks

**Example Tests**:
```bash
# Single quote injection
ggen sync --filter="name = 'admin' OR '1'='1'"

# Comment injection
ggen sync --filter="name = 'admin'--"

# DROP GRAPH injection
ggen sync --filter="'; DROP GRAPH <http://example.com>; --"

# UNION injection
ggen sync --filter="' UNION SELECT * WHERE { ?s ?p ?o } --"
```

#### Template Injection Testing

- [ ] Test code execution (`{{ system("ls") }}`)
- [ ] Test file inclusion (`{% include "/etc/passwd" %}`)
- [ ] Test prototype pollution (`__proto__`)
- [ ] Test XSS (`<script>alert(1)</script>`)

**Example Tests**:
```bash
# Code execution
ggen sync --context='{"cmd": "{{ system(\"rm -rf /\") }}"}'

# File inclusion
echo '{% include "/etc/passwd" %}' > evil.tera
ggen sync --template=evil.tera

# Prototype pollution
ggen sync --context='{"__proto__": {"polluted": true}}'
```

#### Rate Limiting Testing

- [ ] Test burst requests (>60/minute)
- [ ] Test concurrent generations (>10)
- [ ] Test large files (>10MB)
- [ ] Test deep RDF graphs (>1M triples)

**Example Tests**:
```bash
# Burst requests
for i in {1..100}; do ggen sync & done

# Large file
dd if=/dev/zero of=large.ttl bs=1M count=100
ggen sync --ontology=large.ttl
```

---

## Dependency Scanning Process

### Cargo Audit

**Purpose**: Scan for known vulnerabilities in dependencies

**Usage**:
```bash
# Scan for vulnerabilities
cargo make audit

# Scan with deny mode (fail on any vulnerability)
cargo audit --deny warnings

# Update advisory database
cargo audit --update
```

**Integration**:
- Run on every PR (CI)
- Run nightly (scheduled)
- Run before releases

### Dependency Review

**Manual Review Checklist**:
- [ ] Check crate maintainer reputation
- [ ] Review recent security advisories
- [ ] Check download statistics (avoid low-usage crates)
- [ ] Review source code for suspicious patterns
- [ ] Verify crate is actively maintained
- [ ] Check for alternative, more secure crates

**Approved Crates** (security-vetted):
- `tokio` - Async runtime
- `serde` - Serialization
- `clap` - CLI parsing
- `oxigraph` - RDF store
- `tera` - Template engine
- `thiserror` - Error handling
- `tracing` - Observability
- `subtle` - Constant-time operations

---

## Static Analysis Tools

### Clippy (Linting)

**Purpose**: Detect common security anti-patterns

**Usage**:
```bash
# Run clippy with security lints
cargo make lint

# Run clippy with all lints
cargo clippy -- -D warnings -D clippy::all -D clippy::pedantic
```

**Security-Relevant Lints**:
- `clippy::unwrap_used` - Prevents panics
- `clippy::expect_used` - Prevents panics
- `clippy::panic` - Prevents panics in library code
- `clippy::unreadable_literal` - Prevents hardcoded secrets
- `clippy::integer_arithmetic` - Prevents overflows

### Cargo Deny

**Purpose**: Enforce dependency policies

**Configuration** (`.cargo/deny.toml`):
```toml
[licenses]
# Only allow approved licenses
allow = ["MIT", "Apache-2.0", "BSD-3-Clause"]
deny = ["GPL-3.0"]  # Copyleft licenses

[bans]
# Ban specific crates with known issues
deny = [
    { name = "openssl-sys", reason = "Use rustls instead" }
]

[sources]
# Only allow crates.io
allow-org = { github = ["seanchatmangpt"] }
```

### MiriRun Safety Checks)

**Purpose**: Detect undefined behavior

**Usage**:
```bash
# Install Miri
rustup component add miri

# Run tests with Miri
cargo miri test

# Run specific test
cargo miri test test_safepath
```

**What Miri Detects**:
- Use-after-free
- Double-free
- Invalid pointer arithmetic
- Data races (in unsafe code)
- Undefined behavior

---

## Security Test Automation

### Pre-commit Hooks

**Location**: `.git/hooks/pre-commit`

```bash
#!/bin/bash

echo "Running security checks..."

# Run security tests
cargo make test-security || exit 1

# Run clippy with security lints
cargo clippy -- -D warnings || exit 1

# Run cargo audit
cargo audit --deny warnings || exit 1

echo "Security checks passed!"
```

### CI/CD Pipeline

**GitHub Actions** (`.github/workflows/security.yml`):
```yaml
name: Security

on: [push, pull_request]

jobs:
  security:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3

      - name: Install Rust
        uses: actions-rs/toolchain@v1
        with:
          toolchain: stable

      - name: Run security tests
        run: cargo make test-security

      - name: Run cargo audit
        run: cargo audit --deny warnings

      - name: Run clippy
        run: cargo clippy -- -D warnings

      - name: Check for hardcoded secrets
        uses: trufflesecurity/trufflehog@main
```

---

## Continuous Security Testing

### Scheduled Scans

**Nightly**:
- Dependency scanning (`cargo audit`)
- Fuzzing (1 hour per target)
- Performance regression tests

**Weekly**:
- Full penetration testing suite
- Manual code review of new changes
- Security documentation review

**Monthly**:
- Third-party security audit
- Threat model review
- Security training for developers

### Security Metrics

**Track**:
- Number of vulnerabilities found
- Time to fix vulnerabilities
- Test coverage for security-critical code
- Number of security test failures
- Rate limit violations in production

**Dashboard**:
- Grafana dashboard with security metrics
- Alerts for security test failures
- Trend analysis for vulnerability count

---

## Security Test Execution

### Running All Security Tests

```bash
# Run all security tests
cargo make test-security

# Run specific category
cargo make test-security-unit
cargo make test-security-integration
cargo make test-security-property

# Run with coverage
cargo make test-security-coverage
```

### Test Organization

```
tests/
├── security/
│   ├── unit/
│   │   ├── safepath_tests.rs
│   │   ├── query_builder_tests.rs
│   │   └── error_sanitizer_tests.rs
│   ├── integration/
│   │   ├── e2e_injection_tests.rs
│   │   ├── rate_limiting_tests.rs
│   │   └── template_security_tests.rs
│   ├── property/
│   │   ├── safepath_fuzz.rs
│   │   └── query_builder_fuzz.rs
│   ├── performance/
│   │   └── dos_resistance_tests.rs
│   └── regression/
│       └── cve_tests.rs
```

---

## References

- [OWASP Testing Guide](https://owasp.org/www-project-web-security-testing-guide/)
- [Rust Fuzzing Book](https://rust-fuzz.github.io/book/)
- [Cargo Audit Documentation](https://github.com/rustsec/rustsec)
- [Security Architecture](ARCHITECTURE.md)
- [Safe Coding Guidelines](SAFE_CODING.md)

---

**Last Updated**: 2026-01-24
**Next Review**: 2026-04-24
**Owner**: Security Team
