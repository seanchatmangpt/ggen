# Quick Reference: Sync CLI Wrapper Pattern

**For Agents 3-6: Use this as a template for migrating commands**

## Step 1: Create Async Domain Function

```rust
// domain/utils/doctor.rs (or domain/<noun>/<verb>.rs)
use std::error::Error;

#[derive(Debug, serde::Serialize, serde::Deserialize)]
pub struct DoctorOutput {
    pub status: String,
    pub checks: Vec<DiagnosticCheck>,
}

#[derive(Debug, serde::Serialize, serde::Deserialize)]
pub struct DiagnosticCheck {
    pub name: String,
    pub passed: bool,
    pub message: String,
}

/// Run system diagnostics (async business logic)
pub async fn run_diagnostics() -> Result<DoctorOutput, Box<dyn Error>> {
    let mut checks = Vec::new();

    // Your async operations here
    let config_exists = tokio::fs::metadata("~/.ggen/config.toml").await.is_ok();

    checks.push(DiagnosticCheck {
        name: "Configuration".to_string(),
        passed: config_exists,
        message: if config_exists {
            "Configuration file found".to_string()
        } else {
            "Configuration file missing".to_string()
        },
    });

    Ok(DoctorOutput {
        status: "healthy".to_string(),
        checks,
    })
}
```

## Step 2: Create Sync CLI Wrapper

```rust
// commands/utils/doctor.rs (or commands/<noun>/<verb>.rs)
use clap_noun_verb_macros::verb;
use clap_noun_verb::Result;
use cli::runtime_helper::execute_async_verb;

// Re-export output types from domain
pub use crate::domain::utils::{DoctorOutput, DiagnosticCheck};

/// Run system diagnostics
#[verb("doctor", "utils")]
fn utils_doctor() -> Result<DoctorOutput> {
    execute_async_verb(async {
        crate::domain::utils::run_diagnostics()
            .await
            .map_err(|e| e.to_string())
    })
}
```

## Step 3: Update Module Exports

```rust
// commands/utils/mod.rs
pub mod doctor;

// domain/utils/mod.rs
pub mod doctor;
pub use doctor::*;
```

## That's It!

Three simple steps:
1. ✅ Async domain function (pure business logic)
2. ✅ Sync CLI wrapper (uses `execute_async_verb`)
3. ✅ Update module exports

## Error Handling

Always use `.map_err(|e| e.to_string())` to convert domain errors to strings:

```rust
execute_async_verb(async {
    crate::domain::mynoun::my_function()
        .await
        .map_err(|e| e.to_string())  // ← Required!
})
```

## File Organization

```
ggen/
├── cli/src/
│   └── runtime_helper.rs     ← Already created (Agent 2)
├── commands/
│   └── <noun>/
│       └── <verb>.rs         ← Sync CLI wrapper (you create this)
└── domain/
    └── <noun>/
        └── <verb>.rs         ← Async business logic (you create this)
```

## Testing

### CLI Layer (Sync)
```rust
#[test]
fn test_utils_doctor() {
    let result = utils_doctor();
    assert!(result.is_ok());
}
```

### Domain Layer (Async)
```rust
#[tokio::test]
async fn test_run_diagnostics() {
    let result = run_diagnostics().await;
    assert!(result.is_ok());
}
```

## Common Issues

### ❌ Don't do this:
```rust
#[verb("doctor", "utils")]
async fn utils_doctor() -> Result<Output> {  // ❌ Can't use async
    // ...
}
```

### ✅ Do this:
```rust
#[verb("doctor", "utils")]
fn utils_doctor() -> Result<Output> {  // ✅ Sync wrapper
    execute_async_verb(async {
        crate::domain::utils::run_diagnostics()
            .await
            .map_err(|e| e.to_string())
    })
}
```

## For More Details

See `/Users/sac/ggen/.claude/refactor-v2/sync-wrapper-pattern.md` for:
- Architecture diagrams
- Advanced error handling
- Performance considerations
- Migration checklist
- Common pitfalls

---

**Ready to migrate commands? Follow this template!**
