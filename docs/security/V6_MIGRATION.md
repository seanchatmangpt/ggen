<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Security Migration Guide: v5.x → v6.0.0](#security-migration-guide-v5x-%E2%86%92-v600)
  - [Overview](#overview)
  - [Table of Contents](#table-of-contents)
  - [Breaking Changes Summary](#breaking-changes-summary)
  - [SafePath Migration](#safepath-migration)
    - [Overview](#overview-1)
    - [Breaking Change](#breaking-change)
    - [Migration Steps](#migration-steps)
    - [Common Patterns](#common-patterns)
  - [SPARQL Builder Migration](#sparql-builder-migration)
    - [Overview](#overview-2)
    - [Breaking Change](#breaking-change-1)
    - [Migration Steps](#migration-steps-1)
    - [Escaping Functions](#escaping-functions)
  - [Rate Limiting Configuration](#rate-limiting-configuration)
    - [Overview](#overview-3)
    - [New Configuration](#new-configuration)
    - [Migration Steps](#migration-steps-2)
  - [Error Handling Changes](#error-handling-changes)
    - [Overview](#overview-4)
    - [Breaking Change](#breaking-change-2)
    - [Migration Steps](#migration-steps-3)
  - [Template Security Changes](#template-security-changes)
    - [Overview](#overview-5)
    - [Breaking Change](#breaking-change-3)
    - [Migration Steps](#migration-steps-4)
  - [Configuration Changes](#configuration-changes)
    - [Removed: `output_directory`](#removed-output_directory)
    - [New: Security Configuration](#new-security-configuration)
  - [Timeline and Deprecation Schedule](#timeline-and-deprecation-schedule)
    - [v6.0.0 (January 2026) - Current](#v600-january-2026---current)
    - [v6.1.0 (Q1 2026) - Planned](#v610-q1-2026---planned)
    - [v7.0.0 (Q3 2026) - Future](#v700-q3-2026---future)
  - [Migration Checklist](#migration-checklist)
    - [Pre-Migration](#pre-migration)
    - [SafePath Migration](#safepath-migration-1)
    - [SPARQL Migration](#sparql-migration)
    - [Configuration Updates](#configuration-updates)
    - [Error Handling](#error-handling)
    - [Testing](#testing)
    - [Deployment](#deployment)
  - [Migration Support](#migration-support)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Security Migration Guide: v5.x → v6.0.0

## Overview

This guide covers security-related breaking changes when migrating from ggen v5.x to v6.0.0, including SafePath adoption, SPARQL builder migration, rate limiting configuration, and timeline/deprecation schedule.

**Last Updated**: 2026-01-24
**Source Version**: v5.x
**Target Version**: v6.0.0
**Migration Time**: 1-4 hours (depending on codebase size)

---

## Table of Contents

1. [Breaking Changes Summary](#breaking-changes-summary)
2. [SafePath Migration](#safepath-migration)
3. [SPARQL Builder Migration](#sparql-builder-migration)
4. [Rate Limiting Configuration](#rate-limiting-configuration)
5. [Error Handling Changes](#error-handling-changes)
6. [Template Security Changes](#template-security-changes)
7. [Configuration Changes](#configuration-changes)
8. [Timeline and Deprecation Schedule](#timeline-and-deprecation-schedule)
9. [Migration Checklist](#migration-checklist)

---

## Breaking Changes Summary

| Area | v5.x Behavior | v6.0.0 Behavior | Impact |
|------|---------------|-----------------|--------|
| **File Paths** | Direct `PathBuf` | `SafePath` required | 🔴 **HIGH** - All path operations |
| **SPARQL Queries** | String concatenation | `QueryBuilder` required | 🔴 **HIGH** - All queries |
| **Rate Limiting** | Optional | Enforced by default | 🟡 **MEDIUM** - API usage |
| **Error Messages** | Raw errors | Sanitized errors | 🟢 **LOW** - Error handling |
| **Templates** | Filesystem access | Sandboxed | 🟡 **MEDIUM** - Template rendering |
| **Configuration** | `output_directory` | Removed (use `SafePath`) | 🔴 **HIGH** - Config files |

---

## SafePath Migration

### Overview

**v6.0.0 requires SafePath for all file operations** to prevent path traversal attacks.

### Breaking Change

```rust
// ❌ v5.x (DEPRECATED in v6)
use std::path::PathBuf;

fn load_template(path: &str) -> Result<String, Error> {
    let full_path = PathBuf::from(path);
    fs::read_to_string(full_path)  // ⚠️ Path traversal risk!
}

// ✅ v6.0.0 (REQUIRED)
use ggen_core::security::SafePath;

fn load_template(path: &str) -> Result<String, Error> {
    let safe_path = SafePath::new(path)?;
    fs::read_to_string(safe_path.as_path())
}
```

### Migration Steps

**Step 1: Find All Path Operations**

```bash
# Find all PathBuf usages
rg "PathBuf::from" --type rust

# Find all path operations
rg "fs::(read|write|create|remove)" --type rust
```

**Step 2: Replace PathBuf with SafePath**

```rust
// Pattern 1: Simple path loading
// ❌ Before
let path = PathBuf::from(user_input);

// ✅ After
let path = SafePath::new(user_input)?;

// Pattern 2: Path joining
// ❌ Before
let path = base_dir.join(user_input);

// ✅ After
let path = SafePath::new(user_input)?
    .within_directory(&base_dir)?;

// Pattern 3: File operations
// ❌ Before
let content = fs::read_to_string(PathBuf::from(path))?;

// ✅ After
let safe_path = SafePath::new(path)?;
let content = fs::read_to_string(safe_path.as_path())?;
```

**Step 3: Update Function Signatures**

```rust
// ❌ Before
pub fn load_file(path: &Path) -> Result<String, Error> {
    fs::read_to_string(path)
}

// ✅ After
pub fn load_file(path: &SafePath) -> Result<String, Error> {
    fs::read_to_string(path.as_path())
}

// Alternative: Accept &str and validate internally
pub fn load_file(path: &str) -> Result<String, Error> {
    let safe_path = SafePath::new(path)?;
    fs::read_to_string(safe_path.as_path())
}
```

### Common Patterns

**Pattern 1: Template Loading**

```rust
// ❌ v5.x
fn load_template(name: &str) -> Result<String, Error> {
    let path = PathBuf::from("templates").join(name);
    fs::read_to_string(path)
}

// ✅ v6.0.0
fn load_template(name: &str) -> Result<String, Error> {
    let path = SafePath::new(name)?
        .within_directory(Path::new("templates"))?;
    fs::read_to_string(path.as_path())
}
```

**Pattern 2: Output File Writing**

```rust
// ❌ v5.x
fn write_output(name: &str, content: &str) -> Result<(), Error> {
    let output_dir = config.output_directory.clone();  // REMOVED in v6
    let path = PathBuf::from(output_dir).join(name);
    fs::write(path, content)
}

// ✅ v6.0.0
fn write_output(name: &str, content: &str) -> Result<(), Error> {
    let path = SafePath::new(name)?
        .within_directory(Path::new("output"))?;

    if let Some(parent) = path.as_path().parent() {
        fs::create_dir_all(parent)?;
    }

    fs::write(path.as_path(), content)
}
```

**Pattern 3: Recursive Directory Traversal**

```rust
// ❌ v5.x (vulnerable to symlink attacks)
fn find_templates(dir: &Path) -> Result<Vec<PathBuf>, Error> {
    let mut templates = Vec::new();
    for entry in fs::read_dir(dir)? {
        let entry = entry?;
        let path = entry.path();
        if path.is_file() {
            templates.push(path);
        }
    }
    Ok(templates)
}

// ✅ v6.0.0 (secure against symlinks)
use ggen_core::security::SafePath;

fn find_templates(dir: &str) -> Result<Vec<SafePath>, Error> {
    let base_dir = SafePath::new(dir)?;
    let mut templates = Vec::new();

    for entry in fs::read_dir(base_dir.as_path())? {
        let entry = entry?;
        let path = entry.path();

        // Validate each entry is within base directory
        let safe_entry = SafePath::from_path(&path)?
            .within_directory(base_dir.as_path())?;

        if safe_entry.as_path().is_file() {
            templates.push(safe_entry);
        }
    }

    Ok(templates)
}
```

---

## SPARQL Builder Migration

### Overview

**v6.0.0 requires QueryBuilder for all SPARQL queries** to prevent SPARQL injection.

### Breaking Change

```rust
// ❌ v5.x (DEPRECATED in v6)
fn find_users(name: &str) -> String {
    format!(
        "SELECT ?user WHERE {{ ?user foaf:name '{}' }}",
        name  // ⚠️ SPARQL injection risk!
    )
}

// ✅ v6.0.0 (REQUIRED)
use ggen_core::sparql::QueryBuilder;

fn find_users(name: &str) -> Result<String, Error> {
    let query = QueryBuilder::new()
        .select(&["?user"])
        .where_clause("?user foaf:name ?name")
        .filter(&format!("?name = {}", QueryBuilder::escape_literal(name)))
        .build()?;

    Ok(query)
}
```

### Migration Steps

**Step 1: Find All SPARQL Queries**

```bash
# Find string-based queries
rg "SELECT.*WHERE" --type rust

# Find format! macros with SPARQL
rg "format!.*SELECT" --type rust
```

**Step 2: Replace String Concatenation**

```rust
// Pattern 1: Simple SELECT
// ❌ Before
let query = format!("SELECT ?s WHERE {{ ?s ?p ?o }}");

// ✅ After
let query = QueryBuilder::new()
    .select(&["?s"])
    .where_clause("?s ?p ?o")
    .build()?;

// Pattern 2: With user input
// ❌ Before
let query = format!("SELECT ?s WHERE {{ ?s foaf:name '{}' }}", user_input);

// ✅ After
let query = QueryBuilder::new()
    .select(&["?s"])
    .where_clause("?s foaf:name ?name")
    .filter(&format!("?name = {}", QueryBuilder::escape_literal(user_input)))
    .build()?;

// Pattern 3: Complex query
// ❌ Before
let query = format!(
    "SELECT ?person ?age WHERE {{
        ?person rdf:type foaf:Person .
        ?person foaf:age ?age .
        FILTER (?age >= {})
    }}
    ORDER BY ?age
    LIMIT {}",
    min_age, limit
);

// ✅ After
let query = QueryBuilder::new()
    .select(&["?person", "?age"])
    .where_clause("?person rdf:type foaf:Person .")
    .where_clause("?person foaf:age ?age .")
    .filter(&format!("?age >= {}", min_age))
    .order_by("?age")
    .limit(limit)
    .build()?;
```

### Escaping Functions

**Literals (strings, numbers)**:
```rust
use ggen_core::sparql::QueryBuilder;

// Strings
let escaped = QueryBuilder::escape_literal("O'Reilly");
// Result: "\"O\\'Reilly\""

// Numbers
let escaped = QueryBuilder::escape_literal("42");
// Result: "42"

// Booleans
let escaped = QueryBuilder::escape_literal("true");
// Result: "true"
```

**URIs**:
```rust
let escaped = QueryBuilder::escape_uri("http://example.com/resource?id=123");
// Result: "<http://example.com/resource?id=123>"
```

**Variables**:
```rust
let var = QueryBuilder::validate_variable("userName")?;
// Result: "?userName"
```

---

## Rate Limiting Configuration

### Overview

**v6.0.0 enforces rate limiting by default** to prevent DoS attacks.

### New Configuration

```toml
# v6.0.0 ggen.toml
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

### Migration Steps

**Step 1: Add Rate Limiting Config**

```bash
# Generate default config
ggen init --with-defaults

# Or manually add to ggen.toml
cat >> ggen.toml <<EOF
[rate_limit]
max_requests_per_minute = 60
max_concurrent_generations = 10
EOF
```

**Step 2: Adjust Limits for Your Use Case**

```toml
# For CI/CD (higher limits)
[rate_limit]
max_requests_per_minute = 300
max_concurrent_generations = 50

# For public API (stricter limits)
[rate_limit]
max_requests_per_minute = 20
max_concurrent_generations = 3
```

**Step 3: Handle Rate Limit Errors**

```rust
use ggen_core::error::Error;

match ggen::sync(&config) {
    Ok(_) => println!("Success!"),
    Err(Error::RateLimitExceeded { retry_after }) => {
        eprintln!("Rate limit exceeded. Retry after {} seconds", retry_after);
        std::thread::sleep(Duration::from_secs(retry_after));
        // Retry...
    }
    Err(e) => eprintln!("Error: {}", e),
}
```

---

## Error Handling Changes

### Overview

**v6.0.0 sanitizes error messages** to prevent information disclosure.

### Breaking Change

```rust
// v5.x (leaked internal paths)
Err(format!("Failed to read file: {}", path.display()))
// Error: "Failed to read file: /home/user/.config/ggen/secret.key"

// v6.0.0 (sanitized)
use ggen_core::security::ErrorSanitizer;

let sanitized = ErrorSanitizer::file_error("read", &path_str, &error_msg);
Err(sanitized.user_message())
// Error: "Failed to read file: secret.key"
```

### Migration Steps

**Step 1: Replace Raw Error Messages**

```rust
// ❌ Before
return Err(Error::FileRead(format!("Failed to read {}: {}", path.display(), err)));

// ✅ After
use ggen_core::security::ErrorSanitizer;

let sanitized = ErrorSanitizer::file_error("read", path.to_str().unwrap(), &err.to_string());
log::error!("File read failed: {}", sanitized.internal_message());
return Err(Error::FileRead(sanitized.user_message()));
```

**Step 2: Update Error Types**

```rust
use thiserror::Error;

#[derive(Error, Debug)]
pub enum Error {
    #[error("File operation failed: {0}")]
    FileRead(String),  // Now contains sanitized message

    #[error("SPARQL query failed")]
    QueryFailed,  // Generic message, details in logs

    #[error("Template rendering failed")]
    TemplateFailed,  // Generic message, details in logs
}
```

---

## Template Security Changes

### Overview

**v6.0.0 sandboxes template rendering** to prevent filesystem access.

### Breaking Change

```tera
{# ❌ v5.x (allowed, dangerous) #}
{% include "/etc/passwd" %}
{{ system("rm -rf /") }}

{# ✅ v6.0.0 (blocked) #}
{# Templates cannot access filesystem or execute commands #}
```

### Migration Steps

**Templates cannot**:
- Access filesystem (`{% include %}` only allows templates in allowed directories)
- Execute commands (`system()` function removed)
- Make network requests (no HTTP client)

**Templates can**:
- Use variables and filters
- Include templates from allowed directories
- Use control flow (if/for/etc.)

**Migration**:
1. Remove filesystem access from templates
2. Pass all data via context
3. Use include only for templates in allowed directories

```tera
{# ❌ Before (v5.x) #}
{% include "/etc/passwd" %}
{{ load_file("config.toml") }}

{# ✅ After (v6.0.0) #}
{# Pass data via context instead #}
{{ config_data }}
{% include "partials/header.tera" %}  {# Only allowed directories #}
```

---

## Configuration Changes

### Removed: `output_directory`

**v5.x**:
```toml
[project]
output_directory = "generated"  # ❌ Removed in v6
```

**v6.0.0**:
```rust
// Use SafePath for output instead
let output_path = SafePath::new("output/generated.rs")?
    .within_directory(Path::new("output"))?;

fs::write(output_path.as_path(), content)?;
```

### New: Security Configuration

**v6.0.0**:
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

---

## Timeline and Deprecation Schedule

### v6.0.0 (January 2026) - Current

**Breaking Changes (Immediate)**:
- ✅ SafePath required for all file operations
- ✅ QueryBuilder required for all SPARQL queries
- ✅ Rate limiting enforced by default
- ✅ `output_directory` removed from configuration
- ✅ Template sandboxing enforced

**Deprecation Warnings**:
- None (breaking changes are immediate)

### v6.1.0 (Q1 2026) - Planned

**New Features**:
- API authentication
- Multi-tenant isolation
- Enhanced audit logging

### v7.0.0 (Q3 2026) - Future

**Planned Breaking Changes**:
- Remove deprecated error types
- Require Rust 1.75+ (from 1.70+)
- Remove legacy CLI commands

---

## Migration Checklist

### Pre-Migration

- [ ] Read breaking changes documentation
- [ ] Backup your project (`git commit`, `tar czf backup.tar.gz .`)
- [ ] Update Rust toolchain (`rustup update stable`)
- [ ] Review dependency versions (`cargo outdated`)

### SafePath Migration

- [ ] Find all `PathBuf::from()` usages
- [ ] Replace with `SafePath::new()`
- [ ] Add `.within_directory()` where appropriate
- [ ] Update function signatures to accept `SafePath`
- [ ] Test all file operations

### SPARQL Migration

- [ ] Find all SPARQL string concatenation
- [ ] Replace with `QueryBuilder`
- [ ] Use `escape_literal()` for user input
- [ ] Use `escape_uri()` for URIs
- [ ] Test all queries

### Configuration Updates

- [ ] Remove `output_directory` from `ggen.toml`
- [ ] Add `[security]` section
- [ ] Add `[rate_limit]` section
- [ ] Configure allowed directories
- [ ] Set appropriate limits

### Error Handling

- [ ] Replace raw error messages with `ErrorSanitizer`
- [ ] Log full errors internally
- [ ] Return sanitized errors to users
- [ ] Update error types

### Testing

- [ ] Run `cargo make test`
- [ ] Run `cargo make audit`
- [ ] Run `cargo make lint`
- [ ] Test with sample data
- [ ] Verify all security checks pass

### Deployment

- [ ] Update CI/CD pipelines
- [ ] Update documentation
- [ ] Train team on new patterns
- [ ] Monitor for issues

---

## Migration Support

**Need Help?**
- Documentation: [docs/security/](.)
- Email: sean@chatmangpt.com
- GitHub Discussions: [ggen/discussions](https://github.com/seanchatmangpt/ggen/discussions)
- Discord: (Coming soon)

**Common Issues**:
- [SafePath FAQ](SAFE_CODING.md#safepath-usage-patterns)
- [QueryBuilder FAQ](SAFE_CODING.md#sparql-query-construction)
- [Rate Limiting FAQ](../V6_MIGRATION_FAQ.md)

---

**Last Updated**: 2026-01-24
**Migration Support**: Until 2026-06-01
