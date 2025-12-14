# clap-noun-verb Enterprise Usage Guide

Generate production-grade Rust CLIs from RDF descriptions with domain logic protection.

## Enterprise Overview

This package is designed for **Fortune 500 scale** CLI development:
- **50+ developers** across multiple teams
- **200+ commands** without merge conflicts
- **Domain logic protection** that survives regeneration
- **FMEA-driven controls** for zero-defect quality
- **Poka-Yoke guardrails** preventing common mistakes

### Key Architecture Decision: Trait Boundary Separation

```
src/generated/    ← REGENERATE FREELY (safe to blow away)
├── nouns/
│   ├── user.rs   ← trait UserCommands (generated)
│   └── order.rs  ← trait OrderCommands (generated)

src/domain/       ← YOUR CODE (NEVER touched by generator)
├── user.rs       ← impl UserCommands for UserDomain
└── order.rs      ← impl OrderCommands for OrderDomain
```

**Why?** After 5 years, you'll have 3M+ lines of battle-tested domain logic. Regeneration must NEVER touch it.

---

## Quick Start

### 1. Describe Your CLI in Turtle RDF

Create a `.ttl` file describing your CLI structure:

```turtle
@prefix cnv: <https://ggen.dev/clap-noun-verb/> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .

# Project metadata
<http://example.com/my-cli> a cnv:CliProject ;
    cnv:projectName "my-cli" ;
    cnv:projectVersion "0.1.0" ;
    rdfs:comment "My awesome CLI tool" .

# Define a noun (resource type)
<#user> a cnv:Noun ;
    cnv:nounName "user" ;
    rdfs:comment "User management" ;
    cnv:hasVerbs <#create>, <#delete> .

# Define verbs (actions)
<#create> a cnv:Verb ;
    cnv:verbName "create" ;
    rdfs:comment "Create a new user" ;
    cnv:hasArguments <#name_arg>, <#email_arg> .

<#delete> a cnv:Verb ;
    cnv:verbName "delete" ;
    rdfs:comment "Delete a user" ;
    cnv:hasArguments <#id_arg> .

# Define arguments
<#name_arg> a cnv:Argument ;
    cnv:argumentName "name" ;
    cnv:argumentType <#StringType> .

<#email_arg> a cnv:Argument ;
    cnv:argumentName "email" ;
    cnv:argumentType <#StringType> .

<#id_arg> a cnv:Argument ;
    cnv:argumentName "id" ;
    cnv:argumentType <#u64Type> .

# Define types
<#StringType> a cnv:PrimitiveType ;
    cnv:rust-type "String" .

<#u64Type> a cnv:PrimitiveType ;
    cnv:rust-type "u64" .
```

### 2. Generate Your CLI

```bash
ggen generate \
  --template marketplace/packages/clap-noun-verb/templates/cli-project.tmpl \
  --domain my-cli.ttl \
  --output ./my-cli/
```

### 3. Review Generated Files

```
my-cli/
├── Cargo.toml       # Project manifest with dependencies
└── src/
    ├── main.rs      # CLI layer with #[noun] and #[verb] macros
    ├── domain.rs    # Domain layer with unimplemented!() stubs
    ├── error.rs     # Error types (DomainError, CliError)
    └── lib.rs       # Library re-exports
```

### 4. Implement Domain Logic

Edit `src/domain.rs` to replace `unimplemented!()` with your business logic:

```rust
// src/domain.rs
pub mod user {
    use super::*;

    pub fn create(
        name: String,
        email: String,
    ) -> Result<impl Serialize, DomainError> {
        // Your implementation here
        Ok(CreateUserResult {
            id: generate_id(),
            name,
            email,
            created_at: Utc::now(),
        })
    }

    pub fn delete(id: u64) -> Result<impl Serialize, DomainError> {
        // Your implementation here
        if !user_exists(id) {
            return Err(DomainError::NotFound(format!("User {} not found", id)));
        }
        Ok(DeleteUserResult { id, deleted: true })
    }
}
```

### 5. Build and Run

```bash
cd my-cli
cargo build
cargo run -- user create --name "Alice" --email "alice@example.com"
# Output: {"id": 1, "name": "Alice", "email": "alice@example.com", ...}
```

---

## RDF Vocabulary Reference

### Project Metadata

```turtle
<http://example.com/my-project> a cnv:CliProject ;
    cnv:projectName "project-name" ;      # Required: Cargo package name
    cnv:projectVersion "0.1.0" ;          # Optional: defaults to "0.1.0"
    rdfs:comment "Project description" .  # Optional: Cargo description
```

### Nouns (Resource Types)

```turtle
<#noun_id> a cnv:Noun ;
    cnv:nounName "resource" ;             # Required: CLI subcommand name
    rdfs:comment "Description" ;          # Optional: help text
    cnv:hasVerbs <#verb1>, <#verb2> .     # Required: at least one verb
```

### Verbs (Actions)

```turtle
<#verb_id> a cnv:Verb ;
    cnv:verbName "action" ;               # Required: CLI action name
    rdfs:comment "Description" ;          # Optional: help text
    cnv:hasArguments <#arg1>, <#arg2> .   # Optional: command arguments
```

### Arguments

```turtle
<#arg_id> a cnv:Argument ;
    cnv:argumentName "arg_name" ;         # Required: argument name
    cnv:argumentType <#type_ref> .        # Required: type reference
```

### Types

```turtle
<#i32Type> a cnv:PrimitiveType ;
    cnv:rust-type "i32" .

<#StringType> a cnv:PrimitiveType ;
    cnv:rust-type "String" .

<#PathBufType> a cnv:PrimitiveType ;
    cnv:rust-type "PathBuf" .

<#OptionalString> a cnv:PrimitiveType ;
    cnv:rust-type "Option<String>" .

<#VecString> a cnv:PrimitiveType ;
    cnv:rust-type "Vec<String>" .
```

---

## Architecture

### Three-Layer Design

```
┌─────────────────────────────────────────────────────────────┐
│                      CLI Layer (main.rs)                    │
│  - Argument parsing (#[noun], #[verb] macros)              │
│  - Input validation                                         │
│  - JSON output formatting                                   │
│  - Error presentation                                       │
└─────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────┐
│                   Domain Layer (domain.rs)                  │
│  - Pure business logic                                      │
│  - No CLI awareness                                         │
│  - Fully testable                                           │
│  - Returns Result<T, DomainError>                          │
└─────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────┐
│                   Error Layer (error.rs)                    │
│  - DomainError: Business logic errors                      │
│  - CliError: Presentation errors                           │
│  - Automatic conversion: DomainError -> CliError           │
└─────────────────────────────────────────────────────────────┘
```

### Why This Architecture?

1. **Testability**: Domain logic can be unit tested without CLI overhead
2. **Separation**: CLI concerns don't leak into business logic
3. **Reusability**: Domain module can be used as a library
4. **Type Safety**: Errors are typed, not strings
5. **Agent-Ready**: JSON output works with AI agents and automation

---

## Error Handling

### DomainError (Business Logic)

```rust
#[derive(Error, Debug, Clone, Serialize)]
pub enum DomainError {
    #[error("Validation error: {0}")]
    Validation(String),

    #[error("Not found: {0}")]
    NotFound(String),

    #[error("Operation failed: {0}")]
    OperationFailed(String),

    #[error("{0}")]
    General(String),
}
```

### CliError (Presentation)

```rust
#[derive(Error, Debug)]
pub enum CliError {
    #[error("Domain error: {0}")]
    Domain(#[from] DomainError),

    #[error("Invalid argument: {0}")]
    InvalidArgument(String),

    #[error("Serialization error: {0}")]
    Serialization(String),

    #[error("IO error: {0}")]
    Io(String),
}
```

### Exit Codes

| Error Type | Exit Code |
|------------|-----------|
| Domain errors | 1 |
| Invalid arguments | 2 |
| Serialization | 3 |
| IO errors | 4 |

---

## Testing Domain Logic

The domain layer is pure Rust - test it with standard unit tests:

```rust
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_user_create_success() {
        let result = user::create("Alice".to_string(), "alice@example.com".to_string());
        assert!(result.is_ok());
        let user = result.unwrap();
        assert_eq!(user.name, "Alice");
    }

    #[test]
    fn test_user_delete_not_found() {
        let result = user::delete(999999);
        assert!(matches!(result, Err(DomainError::NotFound(_))));
    }
}
```

---

## Advanced: Multiple Nouns

```turtle
# File: multi-noun.ttl

<#user> a cnv:Noun ;
    cnv:nounName "user" ;
    cnv:hasVerbs <#user_create>, <#user_list> .

<#project> a cnv:Noun ;
    cnv:nounName "project" ;
    cnv:hasVerbs <#project_create>, <#project_delete> .

<#task> a cnv:Noun ;
    cnv:nounName "task" ;
    cnv:hasVerbs <#task_add>, <#task_complete> .
```

Generated CLI usage:
```bash
my-cli user create --name "Alice"
my-cli project create --name "Project X"
my-cli task add --project-id 1 --title "First task"
```

---

## Dependencies

Generated projects include:

```toml
[dependencies]
clap-noun-verb = "5.3"
clap-noun-verb-macros = "5.3"
serde = { version = "1.0", features = ["derive"] }
serde_json = "1.0"
thiserror = "1.0"
```

---

## Enterprise: Adding Commands Without Merge Conflicts

### The Problem at Scale

With 50+ developers, traditional code generation fails:
- Developer A adds `user suspend` command
- Developer B adds `user activate` command
- Both modify `ontology/user.ttl`
- **Result: MERGE CONFLICT**

### The Solution: One File Per Verb

```
ontology/
└── user/
    ├── noun.ttl              ← Noun metadata (rarely changes)
    ├── OWNERS                ← Team ownership (GitHub CODEOWNERS format)
    ├── verbs/
    │   ├── create.ttl        ← Developer A's domain
    │   ├── delete.ttl        ← Developer B's domain
    │   ├── suspend.ttl       ← NEW FILE (no conflict)
    │   └── activate.ttl      ← NEW FILE (no conflict)
```

### Adding a New Command (Zero-Conflict Workflow)

```bash
# 1. Create verb definition (NEW FILE = no conflict)
cat > ontology/user/verbs/suspend.ttl << 'EOF'
@prefix cnv: <https://ggen.dev/clap-noun-verb/> .
@prefix user: <https://example.com/cli/user/> .

user:suspend a cnv:Verb ;
    cnv:verbName "suspend" ;
    rdfs:comment "Suspend a user account" ;
    cnv:hasArguments user:suspend_id, user:suspend_reason .
EOF

# 2. Regenerate (only src/generated/ changes - domain untouched)
ggen generate \
  --template clap-noun-verb/templates/generated-traits.tmpl \
  --domain ontology/cli.ttl \
  --output src/generated/ \
  --force

# 3. Implement domain logic (NEW FILE = no conflict)
cat > src/domain/user/suspend.rs << 'EOF'
use crate::error::DomainError;

pub fn suspend(id: String, reason: String) -> Result<SuspendOutput, DomainError> {
    // Your 500 lines of battle-tested production logic here
    // - Validation
    // - Audit logging (SOX compliance)
    // - Circuit breaker
    // - Feature flags
    // - Telemetry
}
EOF

# 4. Commit (3 new files, 0 modified = ZERO CONFLICTS)
git add ontology/user/verbs/suspend.ttl
git add src/domain/user/suspend.rs
git commit -m "feat(user): Add suspend command"
```

---

## FMEA: Failure Mode Analysis

### Top Failure Modes and Controls

| ID | Failure Mode | RPN | Control |
|----|--------------|-----|---------|
| F1 | Developer edits generated file | 216 | `⚠️ DO NOT EDIT` header + `.gitignore` |
| F2 | Regenerate overwrites domain logic | 140 | **Trait boundary** - domain in separate files |
| F5 | Missing error handling | 240 | Trait requires `Result<T, DomainError>` |
| F8 | Works in test, fails in prod | 252 | Feature flags + telemetry hooks |
| F9 | Merge conflict in ontology | 80 | One file per verb pattern |

### RPN (Risk Priority Number) = Severity × Occurrence × Detection
- **>200**: CRITICAL - Must have control
- **100-200**: HIGH - Implement control in Q1
- **<100**: MEDIUM - Backlog

---

## Poka-Yoke: Error-Proofing Controls

### Physical Controls (File System)
- `src/generated/` in `.gitignore` - Can't commit generated code
- CODEOWNERS per noun - Wrong team can't merge

### Sequence Controls
- Compile after regenerate - Trait signature mismatch caught immediately
- RDF validation before generate - Invalid ontology blocked

### Information Controls
- `⚠️ DO NOT EDIT` header on generated files
- `unimplemented!()` blocked in production builds (`#[cfg(not(test))]`)

---

## Team Ownership with CODEOWNERS

```
# ontology/user/OWNERS

# Identity Team owns user noun
@company/identity-team
@john.smith
@jane.doe

# Breaking changes require Platform Team
*.breaking.ttl @company/platform-team
```

**Effect**: PRs modifying `ontology/user/` require Identity Team approval.

---

## Enterprise Example

See `examples/enterprise-ops/` for a complete 5-noun, 25+ verb example:

```
enterprise-ops/
├── ontology/
│   ├── cli.ttl           ← Master file (imports all nouns)
│   ├── user/             ← Identity Team (9 verbs)
│   ├── order/            ← Commerce Team (6 verbs)
│   ├── config/           ← Platform Team (6 verbs)
│   └── report/           ← Analytics Team (5 verbs)
```

---

## Next Steps

1. **Quick Start**: Study `examples/calculator.ttl` for basic patterns
2. **Enterprise**: Study `examples/enterprise-ops/` for multi-team patterns
3. **FMEA Deep Dive**: Read `docs/enterprise/CLAP_NOUN_VERB_ENTERPRISE_ARCHITECTURE.md`
4. **Ontology Reference**: Explore `ontologies/clap-noun-verb.ttl`
5. **Runtime Docs**: https://crates.io/crates/clap-noun-verb
