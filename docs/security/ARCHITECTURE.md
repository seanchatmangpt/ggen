<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Security Architecture (v6.0.0)](#security-architecture-v600)
  - [Overview](#overview)
  - [Table of Contents](#table-of-contents)
  - [Defense in Depth Layers](#defense-in-depth-layers)
    - [Layer 1: Input Validation](#layer-1-input-validation)
    - [Layer 2: Schema Validation](#layer-2-schema-validation)
    - [Layer 3: Rate Limiting](#layer-3-rate-limiting)
    - [Layer 4: Business Logic](#layer-4-business-logic)
    - [Layer 5: Error Sanitization](#layer-5-error-sanitization)
    - [Layer 6: Audit Logging](#layer-6-audit-logging)
    - [Layer 7: Cryptographic Receipts](#layer-7-cryptographic-receipts)
    - [Layer 8: Monitoring & Alerting](#layer-8-monitoring--alerting)
  - [Trust Boundaries](#trust-boundaries)
    - [Boundary Enforcement](#boundary-enforcement)
  - [Authentication & Authorization](#authentication--authorization)
  - [Input Validation Strategy](#input-validation-strategy)
    - [Validation Principles](#validation-principles)
    - [Validation Components](#validation-components)
  - [Rate Limiting Design](#rate-limiting-design)
    - [Rate Limiting Architecture](#rate-limiting-architecture)
    - [Rate Limiting Algorithms](#rate-limiting-algorithms)
    - [Configuration](#configuration)
  - [Audit Logging](#audit-logging)
    - [Logged Events](#logged-events)
    - [Log Enrichment](#log-enrichment)
    - [Log Security](#log-security)
  - [Threat Model](#threat-model)
    - [Threat Actors](#threat-actors)
    - [Attack Vectors](#attack-vectors)
    - [Security Assumptions](#security-assumptions)
  - [Security Controls](#security-controls)
    - [Preventive Controls](#preventive-controls)
    - [Detective Controls](#detective-controls)
    - [Corrective Controls](#corrective-controls)
  - [Security Roadmap](#security-roadmap)
    - [v6.1.0 (Q1 2026)](#v610-q1-2026)
    - [v6.2.0 (Q2 2026)](#v620-q2-2026)
    - [v7.0.0 (Q3 2026)](#v700-q3-2026)
  - [References](#references)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Security Architecture (v6.0.0)

## Overview

ggen v6.0.0 implements defense-in-depth security through multiple layers of protection. This document describes the security architecture, trust boundaries, and protection mechanisms.

**Last Updated**: 2026-01-24
**Version**: 6.0.0
**Status**: Production-Ready

---

## Table of Contents

1. [Defense in Depth Layers](#defense-in-depth-layers)
2. [Trust Boundaries](#trust-boundaries)
3. [Authentication & Authorization](#authentication--authorization)
4. [Input Validation Strategy](#input-validation-strategy)
5. [Rate Limiting Design](#rate-limiting-design)
6. [Audit Logging](#audit-logging)
7. [Threat Model](#threat-model)
8. [Security Controls](#security-controls)

---

## Defense in Depth Layers

ggen employs 8 layers of defense to protect against attacks:

```
┌─────────────────────────────────────────────────────────────┐
│ Layer 1: Input Validation (SafePath, QueryBuilder)         │
├─────────────────────────────────────────────────────────────┤
│ Layer 2: Schema Validation (SHACL, RDF)                    │
├─────────────────────────────────────────────────────────────┤
│ Layer 3: Rate Limiting (per-client, per-endpoint)          │
├─────────────────────────────────────────────────────────────┤
│ Layer 4: Business Logic (generation pipeline)              │
├─────────────────────────────────────────────────────────────┤
│ Layer 5: Error Sanitization (no info leakage)              │
├─────────────────────────────────────────────────────────────┤
│ Layer 6: Audit Logging (all operations)                    │
├─────────────────────────────────────────────────────────────┤
│ Layer 7: Cryptographic Receipts (SHA-256)                  │
├─────────────────────────────────────────────────────────────┤
│ Layer 8: Monitoring & Alerting (anomaly detection)         │
└─────────────────────────────────────────────────────────────┘
```

### Layer 1: Input Validation

**Purpose**: Prevent malicious input from entering the system

**Components**:
- **SafePath**: Validates all file paths, prevents traversal
- **QueryBuilder**: Validates SPARQL queries, prevents injection
- **InputValidator**: Validates identifiers, names, values
- **SizeValidator**: Enforces limits on file sizes, RDF graphs

**Implementation**:
```rust
// SafePath validation
use ggen_core::security::SafePath;
let path = SafePath::new(user_input)?;  // Validates before use

// SPARQL query validation
use ggen_core::sparql::QueryBuilder;
let query = QueryBuilder::new()
    .select(&["?subject"])
    .where_clause("?subject rdf:type ?type")
    .limit(100)
    .build()?;  // Type-safe, no injection

// Input validation
use ggen_core::validation::InputValidator;
let safe_name = InputValidator::validate_identifier(user_input)?;
```

**Protects Against**:
- Path traversal (CWE-22)
- SPARQL injection (CWE-89 analog)
- Command injection (CWE-78)
- Buffer overflow (CWE-120)

---

### Layer 2: Schema Validation

**Purpose**: Ensure data integrity and prevent malformed inputs

**Components**:
- **SHACL Validator**: Validates RDF graphs against SHACL shapes
- **RDF Parser**: Validates Turtle/RDF syntax
- **Template Validator**: Validates Tera template syntax
- **Configuration Validator**: Validates TOML configuration files

**Implementation**:
```rust
// SHACL validation
let validator = SHACLValidator::new(&shapes_graph)?;
validator.validate(&data_graph)?;

// RDF syntax validation
let graph = RDFParser::parse_turtle(&content)?;

// Template syntax validation
let template = TemplateValidator::validate(&template_str)?;
```

**Protects Against**:
- Data integrity violations
- Malformed RDF graphs
- Template injection
- Configuration errors

---

### Layer 3: Rate Limiting

**Purpose**: Prevent denial of service attacks

**Components**:
- **Per-client rate limiter**: Limits requests per IP/API key
- **Per-endpoint rate limiter**: Limits requests per endpoint
- **Concurrent execution limiter**: Limits parallel generations
- **Resource quota enforcer**: Limits resource usage per tenant

**Implementation**:
```rust
use ggen_core::rate_limit::RateLimiter;

let limiter = RateLimiter::new()
    .max_requests_per_minute(60)
    .max_concurrent_generations(10)
    .build();

limiter.check_and_increment(client_id, endpoint)?;
```

**Limits (v6.0.0)**:
- Max requests per minute: 60
- Max concurrent generations: 10
- Max file size: 10MB
- Max RDF triples: 1M
- Max SPARQL results: 10,000
- Generation timeout: 120 seconds

**Protects Against**:
- Denial of Service (CWE-400)
- Resource exhaustion
- Algorithmic complexity attacks

---

### Layer 4: Business Logic

**Purpose**: Execute generation pipeline securely

**Components**:
- **RDF Processing**: Load and query RDF graphs
- **Template Rendering**: Render Tera templates with sandboxing
- **Code Generation**: Generate code from templates
- **File Operations**: Write output files with SafePath

**Security Measures**:
- No eval() or dynamic code execution
- Template sandboxing (no filesystem access)
- Output directory restrictions
- Deterministic generation (same input → same output)

---

### Layer 5: Error Sanitization

**Purpose**: Prevent information disclosure through error messages

**Components**:
- **ErrorSanitizer**: Removes sensitive information from errors
- **PathSanitizer**: Removes absolute paths from error messages
- **CredentialFilter**: Removes credentials from logs

**Implementation**:
```rust
use ggen_core::security::ErrorSanitizer;

let sanitized = ErrorSanitizer::sanitize(&internal_error);
println!("{}", sanitized.user_message());  // Safe for users
log::error!("{}", sanitized.internal_message());  // Full details for logs
```

**Protects Against**:
- Information disclosure (CWE-200)
- Path exposure
- Credential leakage
- Stack trace exposure

---

### Layer 6: Audit Logging

**Purpose**: Track all security-relevant operations

**Events Logged**:
- File operations (read, write, delete)
- SPARQL queries executed
- Template rendering
- Code generation
- Rate limit violations
- Validation failures
- Authentication attempts (future)

**Log Format**:
```json
{
  "timestamp": "2026-01-24T10:15:30Z",
  "event_type": "file_operation",
  "operation": "write",
  "path": "output/generated.rs",
  "user": "system",
  "client_id": "cli-12345",
  "result": "success",
  "duration_ms": 15
}
```

**Storage**:
- Local: `.ggen/audit/*.json`
- Rotation: Daily
- Retention: 90 days
- Compression: gzip after 7 days

---

### Layer 7: Cryptographic Receipts

**Purpose**: Provide tamper-evident audit trail

**Receipt Structure**:
```json
{
  "execution_id": "550e8400-e29b-41d4-a716-446655440000",
  "timestamp": "2026-01-24T10:15:30Z",
  "manifest_hash": "sha256:1234...",
  "ontology_hash": "sha256:5678...",
  "files_generated": [
    {
      "path": "output/generated.rs",
      "content_hash": "sha256:abcd..."
    }
  ],
  "inference_rules_executed": ["rule1", "rule2"],
  "generation_rules_executed": ["gen1", "gen2"],
  "receipt_hash": "sha256:9999..."
}
```

**Cryptographic Properties**:
- **Integrity**: SHA-256 hashing prevents tampering
- **Non-repudiation**: Receipts prove generation occurred
- **Auditability**: Full provenance trail

---

### Layer 8: Monitoring & Alerting

**Purpose**: Detect anomalous behavior in real-time

**Metrics Monitored**:
- Rate limit violations
- Validation failures
- Generation failures
- Resource usage spikes
- Unusual SPARQL queries
- Suspicious file operations

**Alerting Thresholds**:
- Rate limit: >10 violations/minute
- Validation failures: >5 failures/minute
- Generation failures: >3 failures/minute
- Resource usage: >90% CPU/memory

---

## Trust Boundaries

```
┌─────────────────────────────────────────────────────────────┐
│                    UNTRUSTED ZONE                           │
│  - CLI arguments                                            │
│  - Environment variables                                    │
│  - Configuration files                                      │
│  - RDF ontologies                                           │
│  - Template files                                           │
└────────────────────┬────────────────────────────────────────┘
                     │
                     ▼ Input Validation
┌─────────────────────────────────────────────────────────────┐
│                   VALIDATION ZONE                           │
│  - SafePath validation                                      │
│  - QueryBuilder validation                                  │
│  - SHACL validation                                         │
│  - Size/rate limiting                                       │
└────────────────────┬────────────────────────────────────────┘
                     │
                     ▼ Validated Input
┌─────────────────────────────────────────────────────────────┐
│                   TRUSTED ZONE                              │
│  - RDF processing                                           │
│  - Template rendering                                       │
│  - Code generation                                          │
│  - File operations (sandboxed)                              │
└────────────────────┬────────────────────────────────────────┘
                     │
                     ▼ Generated Output
┌─────────────────────────────────────────────────────────────┐
│                   OUTPUT ZONE                               │
│  - Generated code files                                     │
│  - Cryptographic receipts                                   │
│  - Audit logs                                               │
└─────────────────────────────────────────────────────────────┘
```

### Boundary Enforcement

**CLI → Validation**:
- All CLI arguments validated with Clap derives
- Custom validators for paths, identifiers, URLs
- Size limits enforced at parse time

**Validation → Trusted**:
- Only validated data enters trusted zone
- Failed validation → immediate rejection
- No data transformation in trusted zone (already validated)

**Trusted → Output**:
- Output paths restricted via SafePath
- No overwriting without explicit `--force` flag
- Receipts generated for all operations

---

## Authentication & Authorization

**Current Status (v6.0.0)**: CLI-based, single-user model

**Future Enhancements (v6.1.0+)**:
- API key authentication
- Role-based access control (RBAC)
- OAuth2 integration
- Multi-tenant isolation

**Planned Security Model**:
```
User → API Key → Role → Permissions → Resources
```

**Roles**:
- **Admin**: Full access, can manage all resources
- **Developer**: Can generate code, read/write ontologies
- **Viewer**: Read-only access to ontologies and receipts
- **Guest**: No access (unauthenticated)

---

## Input Validation Strategy

### Validation Principles

1. **Whitelist, not blacklist**: Only allow known-good inputs
2. **Fail closed**: Reject by default, allow explicitly
3. **Early validation**: Validate at entry points, before processing
4. **Type safety**: Use types to enforce constraints at compile time

### Validation Components

**SafePath** (Path Traversal Prevention):
```rust
use ggen_core::security::SafePath;

// ✅ SAFE
let path = SafePath::new("templates/user.tmpl")?;

// ❌ REJECTED
SafePath::new("../../../etc/passwd")?;  // Error: Path traversal detected
```

**QueryBuilder** (SPARQL Injection Prevention):
```rust
use ggen_core::sparql::QueryBuilder;

// ✅ SAFE
let query = QueryBuilder::new()
    .select(&["?name"])
    .where_clause("?person foaf:name ?name")
    .filter(&QueryBuilder::escape_literal(user_input))
    .build()?;

// ❌ REJECTED
let query = format!("SELECT * WHERE {{ ?s ?p '{}' }}", user_input);
```

**InputValidator** (General Input Validation):
```rust
use ggen_core::validation::InputValidator;

// Identifier validation (alphanumeric + underscore)
let name = InputValidator::validate_identifier("my_template")?;

// Size validation
InputValidator::validate_size(content.len(), MAX_SIZE)?;

// Character validation
InputValidator::validate_no_control_chars(&input)?;
```

---

## Rate Limiting Design

### Rate Limiting Architecture

```
┌─────────────┐
│   Client    │
└──────┬──────┘
       │
       ▼
┌─────────────────────────────────────┐
│  Rate Limiter Middleware            │
│  - Check request count              │
│  - Check concurrent operations      │
│  - Update counters                  │
└──────┬──────────────────────────────┘
       │
       ├─► [Allowed] → Process request
       │
       └─► [Denied] → HTTP 429 Too Many Requests
```

### Rate Limiting Algorithms

**Token Bucket** (requests per minute):
- Tokens added at fixed rate (1 per second)
- Each request consumes 1 token
- Burst capacity: 60 tokens
- Refill rate: 1 token/second

**Semaphore** (concurrent operations):
- Max concurrent operations: 10
- Acquire semaphore before operation
- Release semaphore after completion
- Queue requests if limit reached

### Configuration

```toml
[rate_limit]
# Per-client limits
max_requests_per_minute = 60
max_requests_per_hour = 1000

# Concurrent operations
max_concurrent_generations = 10
max_concurrent_queries = 5

# Resource limits
max_file_size_mb = 10
max_rdf_triples = 1_000_000
generation_timeout_seconds = 120
```

---

## Audit Logging

### Logged Events

**Security Events**:
- Authentication attempts
- Authorization failures
- Rate limit violations
- Validation failures
- Suspicious patterns

**Operational Events**:
- File operations (read, write, delete)
- SPARQL queries
- Template rendering
- Code generation
- Configuration changes

**Performance Events**:
- Slow queries (>1s)
- Large files (>1MB)
- High resource usage (>80% CPU/memory)

### Log Enrichment

All logs include:
- Timestamp (ISO 8601)
- Event type
- Client ID
- User ID (future)
- Request ID
- Duration
- Result (success/failure)
- Error details (if applicable)

### Log Security

- **Sanitization**: No sensitive data in logs (passwords, tokens, keys)
- **Encryption**: Logs encrypted at rest (future)
- **Access control**: Logs readable only by admins
- **Retention**: 90 days for security logs, 30 days for operational logs

---

## Threat Model

### Threat Actors

1. **External Attackers**: Attempting to exploit vulnerabilities for RCE, data exfiltration
2. **Malicious Users**: Attempting to bypass rate limits, inject malicious templates/RDF
3. **Compromised Dependencies**: Supply chain attacks through dependencies
4. **Insider Threats**: Malicious developers with code access (future, when multi-tenant)

### Attack Vectors

| Attack Vector | Mitigation | Status |
|---------------|------------|--------|
| Path traversal | SafePath validation | ✅ Protected |
| SPARQL injection | QueryBuilder | ✅ Protected |
| Command injection | Whitelist commands | ✅ Protected |
| Template injection | Template sandboxing | ✅ Protected |
| DoS (resource exhaustion) | Rate limiting | ✅ Protected |
| Information disclosure | Error sanitization | ✅ Protected |
| Supply chain attack | Vendored deps, audit | ✅ Protected |
| Timing attacks | Constant-time comparison | ⚠️ Partial |

### Security Assumptions

1. **Filesystem security**: OS enforces file permissions correctly
2. **Crypto primitives**: SHA-256, OsRng are secure
3. **Rust safety**: No memory corruption vulnerabilities
4. **Dependencies**: Crates.io packages are not malicious

---

## Security Controls

### Preventive Controls

- Input validation (SafePath, QueryBuilder, InputValidator)
- Rate limiting (token bucket, semaphore)
- SHACL validation (schema enforcement)
- Template sandboxing (no filesystem access)
- Command whitelisting (only allowed commands)

### Detective Controls

- Audit logging (all operations logged)
- Monitoring (anomaly detection)
- Receipts (tamper-evident trail)
- Security tests (automated testing)

### Corrective Controls

- Error sanitization (no info leakage)
- Graceful degradation (errors don't crash)
- Incident response (documented procedures)
- Security patches (rapid response)

---

## Security Roadmap

### v6.1.0 (Q1 2026)
- [ ] API authentication (API keys)
- [ ] Multi-tenant isolation
- [ ] Enhanced audit logging
- [ ] Security dashboard

### v6.2.0 (Q2 2026)
- [ ] OAuth2 integration
- [ ] Role-based access control (RBAC)
- [ ] Encrypted audit logs
- [ ] Anomaly detection

### v7.0.0 (Q3 2026)
- [ ] End-to-end encryption
- [ ] Hardware security module (HSM) integration
- [ ] Compliance certifications (SOC2, ISO 27001)
- [ ] Bug bounty program

---

## References

- [OWASP Top 10 (2021)](https://owasp.org/Top10/)
- [SANS Top 25 (2024)](https://www.sans.org/top25-software-errors/)
- [CWE Top 25](https://cwe.mitre.org/top25/)
- [Rust Secure Code Guidelines](https://anssi-fr.github.io/rust-guide/)
- [NIST Cybersecurity Framework](https://www.nist.gov/cyberframework)

---

**Last Updated**: 2026-01-24
**Next Review**: 2026-04-24
**Owner**: Security Team
