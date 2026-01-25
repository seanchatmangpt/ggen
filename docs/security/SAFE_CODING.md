# Safe Coding Guidelines (v6.0.0)

## Overview

This document provides safe coding guidelines for ggen v6.0.0, focusing on preventing common security vulnerabilities through proper use of SafePath, SPARQL query builders, template security, and secure error handling.

**Last Updated**: 2026-01-24
**Version**: 6.0.0
**Audience**: Developers contributing to ggen

---

## Table of Contents

1. [SafePath Usage Patterns](#safepath-usage-patterns)
2. [SPARQL Query Construction](#sparql-query-construction)
3. [Template Security](#template-security)
4. [Configuration Validation](#configuration-validation)
5. [Secrets Management](#secrets-management)
6. [Error Handling](#error-handling-without-information-leakage)
7. [Input Validation](#input-validation)
8. [Secure Defaults](#secure-defaults)

---

## SafePath Usage Patterns

### Overview

**SafePath** prevents path traversal attacks by validating all file paths before use. It ensures paths stay within allowed directories and don't use dangerous patterns like `..` or symbolic links.

### Basic Usage

```rust
use ggen_core::security::SafePath;
use std::fs;

// ✅ CORRECT: Always use SafePath for user-provided paths
fn load_template(user_path: &str) -> Result<String, Error> {
    let safe_path = SafePath::new(user_path)?;
    let content = fs::read_to_string(safe_path.as_path())?;
    Ok(content)
}

// ❌ WRONG: Direct PathBuf from user input
fn load_template_unsafe(user_path: &str) -> Result<String, Error> {
    let path = PathBuf::from(user_path);  // ⚠️ Path traversal risk!
    let content = fs::read_to_string(&path)?;
    Ok(content)
}
```

### Restricting to Base Directory

```rust
use ggen_core::security::SafePath;

// ✅ CORRECT: Restrict to specific directory
fn load_template_in_dir(user_path: &str, base_dir: &Path) -> Result<String, Error> {
    let safe_path = SafePath::new(user_path)?
        .within_directory(base_dir)?;

    let content = fs::read_to_string(safe_path.as_path())?;
    Ok(content)
}

// Example usage
let template = load_template_in_dir("user.tmpl", Path::new("templates"))?;
```

### Common Patterns

**Pattern 1: Loading Templates**

```rust
use ggen_core::security::SafePath;

const TEMPLATE_DIR: &str = "templates";

fn load_template(name: &str) -> Result<String, Error> {
    let path = SafePath::new(name)?
        .within_directory(Path::new(TEMPLATE_DIR))?;

    fs::read_to_string(path.as_path())
        .map_err(|e| Error::TemplateLoad {
            name: name.to_string(),
            source: e,
        })
}
```

**Pattern 2: Writing Output Files**

```rust
use ggen_core::security::SafePath;

const OUTPUT_DIR: &str = "output";

fn write_generated_file(name: &str, content: &str) -> Result<(), Error> {
    let path = SafePath::new(name)?
        .within_directory(Path::new(OUTPUT_DIR))?;

    // Create parent directories if needed
    if let Some(parent) = path.as_path().parent() {
        fs::create_dir_all(parent)?;
    }

    fs::write(path.as_path(), content)?;
    Ok(())
}
```

**Pattern 3: Loading RDF Ontologies**

```rust
use ggen_core::security::SafePath;

const ONTOLOGY_DIR: &str = ".specify";

fn load_ontology(name: &str) -> Result<String, Error> {
    let path = SafePath::new(name)?
        .within_directory(Path::new(ONTOLOGY_DIR))?
        .with_extension("ttl")?;  // Enforce .ttl extension

    fs::read_to_string(path.as_path())
        .map_err(|e| Error::OntologyLoad {
            name: name.to_string(),
            source: e,
        })
}
```

### SafePath Security Features

**Blocked Patterns**:
- `..` (parent directory traversal)
- Absolute paths starting with `/` or `C:\`
- Symbolic links pointing outside allowed directories
- Null bytes in filenames
- Paths longer than 4096 characters

**Canonical Path Resolution**:
```rust
// SafePath automatically resolves to canonical paths
let path = SafePath::new("templates/../../../etc/passwd")?;  // ❌ Error
let path = SafePath::new("templates/./user.tmpl")?;  // ✅ OK, resolves to templates/user.tmpl
```

---

## SPARQL Query Construction

### Overview

**QueryBuilder** prevents SPARQL injection by providing type-safe query construction. Never concatenate user input directly into SPARQL queries.

### Basic Usage

```rust
use ggen_core::sparql::QueryBuilder;

// ✅ CORRECT: Use QueryBuilder
fn find_persons(name_filter: &str) -> Result<String, Error> {
    let query = QueryBuilder::new()
        .select(&["?person", "?name", "?age"])
        .where_clause("?person rdf:type foaf:Person .")
        .where_clause("?person foaf:name ?name .")
        .where_clause("?person foaf:age ?age .")
        .filter(&format!("?name = {}", QueryBuilder::escape_literal(name_filter)))
        .order_by("?name")
        .limit(100)
        .build()?;

    Ok(query)
}

// ❌ WRONG: String concatenation
fn find_persons_unsafe(name_filter: &str) -> String {
    format!(
        "SELECT ?person ?name ?age WHERE {{
            ?person rdf:type foaf:Person .
            ?person foaf:name '{}' .
            ?person foaf:age ?age .
        }}",
        name_filter  // ⚠️ SPARQL injection risk!
    )
}
```

### Escaping User Input

```rust
use ggen_core::sparql::QueryBuilder;

// Literal values (strings, numbers)
let escaped_string = QueryBuilder::escape_literal("O'Reilly");
// Result: "\"O\\'Reilly\""

let escaped_number = QueryBuilder::escape_literal("42");
// Result: "42"

// URI values
let escaped_uri = QueryBuilder::escape_uri("http://example.com/resource?id=123");
// Result: "<http://example.com/resource?id=123>"

// Variable names (should be alphanumeric + underscore only)
let var_name = QueryBuilder::validate_variable("userName")?;
// Result: "?userName"
```

### Complex Query Example

```rust
use ggen_core::sparql::QueryBuilder;

fn find_users_with_filters(
    min_age: i32,
    role: &str,
    name_prefix: &str,
) -> Result<String, Error> {
    let query = QueryBuilder::new()
        .select(&["?user", "?name", "?age", "?role"])
        .where_clause("?user rdf:type ex:User .")
        .where_clause("?user ex:name ?name .")
        .where_clause("?user ex:age ?age .")
        .where_clause("?user ex:role ?role .")
        .filter(&format!(
            "?age >= {} && ?role = {} && STRSTARTS(?name, {})",
            min_age,
            QueryBuilder::escape_literal(role),
            QueryBuilder::escape_literal(name_prefix)
        ))
        .order_by("?name")
        .limit(1000)
        .build()?;

    Ok(query)
}
```

### SPARQL Injection Prevention

**Dangerous Patterns (DO NOT USE)**:
```rust
// ❌ String interpolation
let query = format!("SELECT * WHERE {{ ?s ?p '{}' }}", user_input);

// ❌ String concatenation
let query = "SELECT * WHERE { " + user_input + " }";

// ❌ Raw queries with user input
let query = user_input;  // Never execute raw user input!
```

**Safe Patterns (USE THESE)**:
```rust
// ✅ QueryBuilder with escaping
let query = QueryBuilder::new()
    .select(&["?s", "?p", "?o"])
    .where_clause("?s ?p ?o")
    .filter(&format!("?s = {}", QueryBuilder::escape_uri(&user_input)))
    .build()?;

// ✅ Parameterized queries (future feature)
let query = QueryBuilder::new()
    .select(&["?s"])
    .where_clause("?s ex:name $name")
    .param("name", user_input)
    .build()?;
```

---

## Template Security

### Overview

Templates are powerful but can introduce security risks if not handled properly. ggen uses Tera templates with strict sandboxing.

### Safe Template Patterns

**Pattern 1: Template Loading**

```rust
use ggen_core::template::TemplateEngine;
use ggen_core::security::SafePath;

fn load_and_render(template_name: &str, context: &Context) -> Result<String, Error> {
    let template_path = SafePath::new(template_name)?
        .within_directory(Path::new("templates"))?;

    let engine = TemplateEngine::new()?;
    engine.add_template_file(template_name, template_path.as_path())?;

    let rendered = engine.render(template_name, context)?;
    Ok(rendered)
}
```

**Pattern 2: Context Validation**

```rust
use serde_json::Value;

// ✅ CORRECT: Validate context before rendering
fn validate_context(context: &Value) -> Result<(), Error> {
    // Check for dangerous keys
    if context.get("__proto__").is_some() {
        return Err(Error::InvalidContext("Prototype pollution attempt"));
    }

    // Check for excessively large values
    let json_str = serde_json::to_string(context)?;
    if json_str.len() > 1_000_000 {  // 1MB limit
        return Err(Error::InvalidContext("Context too large"));
    }

    Ok(())
}

fn render_template(name: &str, context: &Value) -> Result<String, Error> {
    validate_context(context)?;

    let engine = TemplateEngine::new()?;
    engine.render(name, context)?;
    Ok(rendered)
}
```

### Template Sandboxing

Tera templates are sandboxed to prevent:
- Filesystem access (templates cannot read/write files)
- Network access (templates cannot make HTTP requests)
- Command execution (templates cannot execute shell commands)
- Code evaluation (no `eval()` or dynamic code)

**Safe Template Features**:
```tera
{# ✅ Variables #}
{{ user_name }}

{# ✅ Filters #}
{{ email | lower }}

{# ✅ Control flow #}
{% if age >= 18 %}
    Adult
{% else %}
    Minor
{% endif %}

{# ✅ Loops #}
{% for item in items %}
    {{ item.name }}
{% endfor %}
```

**Dangerous Template Patterns (BLOCKED)**:
```tera
{# ❌ File access (not available) #}
{% include "/etc/passwd" %}

{# ❌ Command execution (not available) #}
{{ system("rm -rf /") }}

{# ❌ Code evaluation (not available) #}
{{ eval(user_input) }}
```

### Template Recursion Limits

```rust
const MAX_TEMPLATE_DEPTH: usize = 10;

let engine = TemplateEngine::new()?
    .with_max_recursion_depth(MAX_TEMPLATE_DEPTH);
```

---

## Configuration Validation

### Overview

Configuration files (`.toml`, `.ttl`) must be validated before use to prevent injection and misconfigurations.

### TOML Validation

```rust
use ggen_config::Config;
use std::fs;

// ✅ CORRECT: Validate configuration
fn load_config(path: &str) -> Result<Config, Error> {
    let safe_path = SafePath::new(path)?
        .with_extension("toml")?;

    let content = fs::read_to_string(safe_path.as_path())?;
    let config: Config = toml::from_str(&content)?;

    // Validate configuration values
    validate_config(&config)?;

    Ok(config)
}

fn validate_config(config: &Config) -> Result<(), Error> {
    // Check required fields
    if config.project_name.is_empty() {
        return Err(Error::InvalidConfig("project_name is required"));
    }

    // Check value ranges
    if config.max_file_size > 100 * 1024 * 1024 {  // 100MB
        return Err(Error::InvalidConfig("max_file_size too large"));
    }

    // Check path values
    if let Some(output_dir) = &config.output_dir {
        SafePath::new(output_dir)?;  // Validate path
    }

    Ok(())
}
```

### RDF/TTL Validation

```rust
use ggen_core::rdf::RDFValidator;

// ✅ CORRECT: Validate RDF before processing
fn load_ontology(path: &str) -> Result<Graph, Error> {
    let safe_path = SafePath::new(path)?
        .within_directory(Path::new(".specify"))?
        .with_extension("ttl")?;

    let content = fs::read_to_string(safe_path.as_path())?;

    // Parse RDF
    let graph = RDFParser::parse_turtle(&content)?;

    // Validate against SHACL shapes
    let validator = SHACLValidator::new(&shapes_graph)?;
    validator.validate(&graph)?;

    Ok(graph)
}
```

---

## Secrets Management

### Overview

Never hardcode secrets or log sensitive data. Use environment variables or secure secret stores.

### Best Practices

**DO**:
```rust
use std::env;

// ✅ Load from environment
fn get_api_key() -> Result<String, Error> {
    env::var("GGEN_API_KEY")
        .map_err(|_| Error::MissingConfig("GGEN_API_KEY not set"))
}

// ✅ Use secure comparison
use subtle::ConstantTimeEq;

fn verify_api_key(provided: &str, expected: &str) -> bool {
    provided.as_bytes().ct_eq(expected.as_bytes()).into()
}

// ✅ Clear secrets from memory
use zeroize::Zeroize;

fn use_secret(secret: &mut String) {
    // ... use secret ...
    secret.zeroize();  // Clear from memory
}
```

**DON'T**:
```rust
// ❌ Hardcoded secrets
const API_KEY: &str = "sk-1234567890abcdef";  // Never do this!

// ❌ Logging secrets
log::info!("API key: {}", api_key);  // Leaks to logs!

// ❌ Insecure comparison
if provided_key == expected_key {  // Timing attack vulnerable!
    // ...
}

// ❌ Secrets in error messages
return Err(format!("Invalid API key: {}", api_key));  // Leaks secret!
```

### Secret Scrubbing

```rust
use ggen_core::security::SecretScrubber;

// ✅ Scrub secrets from logs
fn log_request(request: &Request) {
    let scrubbed = SecretScrubber::scrub(&format!("{:?}", request));
    log::info!("Request: {}", scrubbed);
}

// SecretScrubber removes:
// - API keys (patterns: sk-, api-, token-)
// - Passwords (key names: password, passwd, pwd)
// - Tokens (key names: token, auth, bearer)
// - Base64-encoded secrets (long base64 strings)
```

---

## Error Handling Without Information Leakage

### Overview

Error messages must be informative for debugging but not leak sensitive information to users.

### Error Sanitization

```rust
use ggen_core::security::ErrorSanitizer;

// ✅ CORRECT: Sanitize errors for users
fn process_file(path: &str) -> Result<String, Error> {
    let safe_path = SafePath::new(path)
        .map_err(|e| {
            let sanitized = ErrorSanitizer::file_error("validate", path, &e.to_string());
            log::error!("Path validation failed: {}", sanitized.internal_message());
            Error::InvalidPath(sanitized.user_message())
        })?;

    let content = fs::read_to_string(safe_path.as_path())
        .map_err(|e| {
            let sanitized = ErrorSanitizer::file_error("read", path, &e.to_string());
            log::error!("File read failed: {}", sanitized.internal_message());
            Error::FileRead(sanitized.user_message())
        })?;

    Ok(content)
}

// User sees: "Failed to read file: user.tmpl"
// Log shows: "Failed to read file: /home/user/.config/ggen/templates/user.tmpl: Permission denied"
```

### Custom Error Types

```rust
use thiserror::Error;

#[derive(Error, Debug)]
pub enum Error {
    #[error("Invalid path: {0}")]
    InvalidPath(String),  // Sanitized message

    #[error("File not found: {0}")]
    FileNotFound(String),  // Sanitized message

    #[error("SPARQL query failed")]
    QueryFailed,  // Generic message, details in logs

    #[error("Template rendering failed")]
    TemplateFailed,  // Generic message, details in logs
}

impl Error {
    pub fn user_message(&self) -> String {
        // Return sanitized message for users
        ErrorSanitizer::sanitize(&self.to_string()).user_message()
    }

    pub fn internal_message(&self) -> String {
        // Return full details for logs
        format!("{:?}", self)  // Full debug output
    }
}
```

---

## Input Validation

### Validation Principles

1. **Validate early**: At entry points (CLI, config files)
2. **Whitelist, not blacklist**: Only allow known-good inputs
3. **Type safety**: Use types to enforce constraints
4. **Fail closed**: Reject by default

### Validation Patterns

**Pattern 1: Identifier Validation**

```rust
use ggen_core::validation::InputValidator;

fn validate_identifier(name: &str) -> Result<String, Error> {
    InputValidator::validate_identifier(name)?;
    Ok(name.to_string())
}

// Valid: "my_template", "Template123", "user_input"
// Invalid: "my-template" (hyphens), "123template" (starts with digit), "my template" (spaces)
```

**Pattern 2: Size Validation**

```rust
const MAX_FILE_SIZE: usize = 10 * 1024 * 1024;  // 10MB

fn validate_file_size(size: usize) -> Result<(), Error> {
    if size > MAX_FILE_SIZE {
        return Err(Error::FileTooLarge {
            size,
            max_size: MAX_FILE_SIZE,
        });
    }
    Ok(())
}
```

**Pattern 3: URL Validation**

```rust
use url::Url;

fn validate_url(url_str: &str) -> Result<Url, Error> {
    let url = Url::parse(url_str)
        .map_err(|e| Error::InvalidUrl(url_str.to_string()))?;

    // Only allow HTTP/HTTPS
    if url.scheme() != "http" && url.scheme() != "https" {
        return Err(Error::InvalidUrl("Only HTTP/HTTPS URLs allowed"));
    }

    Ok(url)
}
```

---

## Secure Defaults

### Configuration Defaults

```rust
impl Default for Config {
    fn default() -> Self {
        Self {
            // Security settings
            max_file_size: 10 * 1024 * 1024,  // 10MB
            max_rdf_triples: 1_000_000,
            max_sparql_results: 10_000,
            generation_timeout: Duration::from_secs(120),

            // Rate limiting
            max_requests_per_minute: 60,
            max_concurrent_generations: 10,

            // Paths
            allowed_template_dirs: vec!["templates".into(), ".specify/templates".into()],
            allowed_output_dirs: vec!["output".into(), "generated".into()],

            // Audit
            enable_receipts: true,
            receipt_storage: ".ggen/receipts".into(),
        }
    }
}
```

### Fail-Safe Defaults

```rust
// ✅ Deny by default, allow explicitly
const DEFAULT_ALLOW_OVERWRITE: bool = false;

fn write_file(path: &str, content: &str, allow_overwrite: bool) -> Result<(), Error> {
    let safe_path = SafePath::new(path)?;

    if safe_path.as_path().exists() && !allow_overwrite {
        return Err(Error::FileExists(path.to_string()));
    }

    fs::write(safe_path.as_path(), content)?;
    Ok(())
}
```

---

## Security Checklist

Use this checklist when writing code:

- [ ] All file paths validated with SafePath
- [ ] All SPARQL queries use QueryBuilder
- [ ] All user inputs validated (length, format, characters)
- [ ] No secrets in code, logs, or error messages
- [ ] Errors sanitized before showing to users
- [ ] Size limits enforced (files, RDF graphs, query results)
- [ ] Rate limits enforced (requests, concurrent operations)
- [ ] Templates loaded from allowed directories only
- [ ] Configuration validated before use
- [ ] Secure defaults for all settings

---

## References

- [SafePath API Documentation](../reference/security/safe_path.md)
- [QueryBuilder API Documentation](../reference/sparql/query_builder.md)
- [Security Architecture](ARCHITECTURE.md)
- [Security Testing](TESTING.md)

---

**Last Updated**: 2026-01-24
**Next Review**: 2026-04-24
