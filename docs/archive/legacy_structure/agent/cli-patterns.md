<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [CLI Patterns Guide](#cli-patterns-guide)
  - [clap-noun-verb Auto-Discovery](#clap-noun-verb-auto-discovery)
    - [Directory Structure](#directory-structure)
    - [Verb Function Pattern](#verb-function-pattern)
    - [Macro Expansion](#macro-expansion)
    - [Error Handling & Serialization](#error-handling--serialization)
    - [Command Routing & Conventions](#command-routing--conventions)
    - [Argument Type Mapping](#argument-type-mapping)
    - [Result Serialization](#result-serialization)
    - [Testing CLI Commands](#testing-cli-commands)
    - [Performance Characteristics](#performance-characteristics)
    - [Security Considerations](#security-considerations)
  - [Critical Rules](#critical-rules)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# CLI Patterns Guide

## clap-noun-verb Auto-Discovery

ggen uses a **noun-verb** command structure with automatic discovery via file-based routing.

### Directory Structure

```
crates/ggen-cli/src/cmds/
├── mod.rs          # Command registration
├── template.rs     # template noun
├── project.rs      # project noun
├── marketplace.rs  # marketplace noun
└── {noun}.rs       # Any noun creates a verb namespace
```

### Verb Function Pattern

Each noun module implements verb functions with the `#[verb]` macro:

```rust
// crates/ggen-cli/src/cmds/template.rs
use ggen_macros::verb;

#[verb]
fn generate(
    template: String,
    ontology: String,
    #[arg(short, long)]
    output: Option<String>,
) -> Result<TemplateOutput> {
    // Implementation delegates to ggen-domain
    TemplateDomain::generate(&template, &ontology, output)
}

#[verb]
fn list() -> Result<TemplateListOutput> {
    TemplateDomain::list_templates()
}

#[verb]
fn validate(path: String) -> Result<ValidateOutput> {
    TemplateDomain::validate(&path)
}
```

### Macro Expansion

The `#[verb]` macro automatically:
- Generates clap argument parsing from function signature
- Creates Result<T> return type handling
- Registers command in discovery system
- Maps to `{noun} {verb}` CLI command

**Generated command:** `ggen template generate --template NAME --ontology FILE --output DIR`

### Error Handling & Serialization

Verb functions return `Result<T>` where T derives `Serialize`:

```rust
use serde::Serialize;

#[derive(Serialize)]
pub struct TemplateOutput {
    pub template_id: String,
    pub files_generated: usize,
    pub total_size_bytes: u64,
    pub duration_ms: u64,
}

#[verb]
fn generate(...) -> Result<TemplateOutput> {
    // Errors automatically serialized to JSON
    let result = TemplateDomain::generate(...)?;
    Ok(TemplateOutput {
        template_id: result.id,
        files_generated: result.files.len(),
        total_size_bytes: result.total_bytes(),
        duration_ms: start.elapsed().as_millis() as u64,
    })
}
```

### Command Routing & Conventions

**Noun identification:**
- File name = noun (template.rs → `template` noun)
- Module path determines command namespace
- Each file is one noun

**Verb identification:**
- Public functions with `#[verb]` attribute
- Function name = verb (generate → `generate` verb)
- Function signature = arguments

**Full command:** `ggen {noun} {verb} [ARGS]`

Examples:
- `ggen template generate --template rust-model --ontology domain.ttl`
- `ggen marketplace search --query "auth" --limit 10`
- `ggen project scaffold --template next-js --output my-app`

### Argument Type Mapping

clap automatically handles:
- `String` → required positional argument
- `Option<String>` → optional flag with `--name`
- `bool` → boolean flag
- `Vec<String>` → repeatable arguments
- Custom types with `FromStr` implementation

```rust
#[verb]
fn search(
    query: String,                          // positional
    #[arg(short, long)] limit: Option<u32>, // --limit
    #[arg(short, long)] tags: Vec<String>,  // --tags (repeatable)
) -> Result<SearchOutput> {
    // ...
}

// Usage: ggen marketplace search "auth" --limit 20 --tags security --tags auth
```

### Result Serialization

All errors and outputs automatically serialize to JSON:

```
Success:
{
  "result": {
    "template_id": "rust-api",
    "files_generated": 42,
    "total_size_bytes": 185024,
    "duration_ms": 523
  }
}

Error:
{
  "error": {
    "code": "VALIDATION_ERROR",
    "message": "Template not found: invalid-template",
    "details": "Check available templates with 'ggen template list'"
  }
}
```

### Testing CLI Commands

Use `cargo make` with command-specific targets:

```bash
cargo make test-cli           # All CLI tests
cargo make test-cli-template  # Specific noun tests
cargo make cli-integration    # Full integration tests
```

Test pattern (Chicago TDD):

```rust
#[tokio::test]
async fn test_template_generate() {
    // Arrange
    let input = TemplateGenerateInput {
        template: "rust-model".to_string(),
        ontology: "domain.ttl".to_string(),
        output: Some("./output".to_string()),
    };

    // Act
    let output = Commands::template_generate(input).await;

    // Assert
    assert!(output.is_ok());
    let result = output.unwrap();
    assert!(result.files_generated > 0);
}
```

### Performance Characteristics

- **Startup time:** ≤500ms (includes clap parsing)
- **Command execution:** ≤3s for scaffolding, ≤5s for code generation
- **Memory overhead:** <50MB for typical operations

### Security Considerations

- **Path validation:** All file paths validated against directory traversal
- **Input sanitization:** Template names/queries sanitized before use
- **Authority checking:** Marketplace operations validate user permissions
- **Command injection prevention:** All shell operations use safe wrappers

---

## Critical Rules

1. **ALWAYS return Result<T>** - Never panic or unwrap in verb functions
2. **ALWAYS serialize output** - Derive Serialize on all return types
3. **NEVER use println!** - Use structured logging macros
4. **Arguments via clap** - Never manual String parsing
5. **Timeout compliance** - Respect SLA timeouts (3s scaffolding, 5s generation)
