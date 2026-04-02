# Sync CLI Wrapper Pattern for ggen v2.0.0

## Overview

clap-noun-verb v3.0.0 requires **synchronous** verb functions, but business logic often needs async operations (file I/O, network requests, etc.). This pattern provides a clean separation:

- **CLI Layer** (sync): Thin wrappers using `runtime_helper`
- **Business Logic Layer** (async): Domain functions with full async/await

## Architecture

```
┌─────────────────────────────────────┐
│ CLI Layer (Sync)                    │
│ commands/*/mod.rs                   │
│ - Uses #[verb] macro                │
│ - Creates tokio runtime             │
│ - Calls domain functions            │
└─────────────────────────────────────┘
                 │
                 ▼ (block_on)
┌─────────────────────────────────────┐
│ Business Logic Layer (Async)        │
│ domain/*/mod.rs                     │
│ - Pure async functions              │
│ - File I/O, network, etc.           │
│ - No CLI dependencies               │
└─────────────────────────────────────┘
```

## Pattern Template

### 1. CLI Wrapper (Sync)

```rust
// commands/utils/doctor.rs
use clap_noun_verb_macros::verb;
use clap_noun_verb::Result;
use cli::runtime_helper::execute_async_verb;
use serde::{Deserialize, Serialize};

#[derive(Debug, Serialize, Deserialize)]
pub struct DoctorOutput {
    pub status: String,
    pub checks: Vec<DiagnosticCheck>,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct DiagnosticCheck {
    pub name: String,
    pub passed: bool,
    pub message: String,
}

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

### 2. Business Logic (Async)

```rust
// domain/utils/doctor.rs
use std::path::Path;
use tokio::fs;

pub struct DiagnosticCheck {
    pub name: String,
    pub passed: bool,
    pub message: String,
}

pub struct DoctorOutput {
    pub status: String,
    pub checks: Vec<DiagnosticCheck>,
}

/// Run system diagnostics (async business logic)
pub async fn run_diagnostics() -> Result<DoctorOutput, Box<dyn std::error::Error>> {
    let mut checks = Vec::new();

    // Async file check
    let config_exists = fs::metadata("~/.ggen/config.toml").await.is_ok();
    checks.push(DiagnosticCheck {
        name: "Configuration".to_string(),
        passed: config_exists,
        message: if config_exists {
            "Configuration file found".to_string()
        } else {
            "Configuration file missing".to_string()
        },
    });

    // More async checks...

    let status = if checks.iter().all(|c| c.passed) {
        "healthy".to_string()
    } else {
        "issues_found".to_string()
    };

    Ok(DoctorOutput { status, checks })
}
```

## Using runtime_helper

### Method 1: execute_async_verb (Recommended for CLI)

```rust
use cli::runtime_helper::execute_async_verb;
use clap_noun_verb::Result;

#[verb("mycommand", "mynoun")]
fn mynoun_mycommand() -> Result<Output> {
    execute_async_verb(async {
        crate::domain::mynoun::do_something()
            .await
            .map_err(|e| e.to_string())
    })
}
```

**Advantages:**
- Automatic error conversion to `NounVerbError`
- Clean, minimal boilerplate
- Type-safe

### Method 2: execute_async (For custom error handling)

```rust
use cli::runtime_helper::execute_async;
use clap_noun_verb::{Result, NounVerbError};

#[verb("mycommand", "mynoun")]
fn mynoun_mycommand() -> Result<Output> {
    execute_async(async {
        let result = crate::domain::mynoun::do_something().await?;

        // Custom error handling
        if !result.is_valid() {
            return Err("Invalid result".to_string());
        }

        Ok(result)
    })
    .map_err(NounVerbError::execution_error)
}
```

### Method 3: create_runtime (For advanced control)

```rust
use cli::runtime_helper::create_runtime;
use clap_noun_verb::{Result, NounVerbError};

#[verb("mycommand", "mynoun")]
fn mynoun_mycommand() -> Result<Output> {
    let rt = create_runtime()
        .map_err(NounVerbError::execution_error)?;

    rt.block_on(async {
        // Multiple async operations
        let data = fetch_data().await?;
        let processed = process_data(data).await?;
        let saved = save_data(processed).await?;

        Ok(saved)
    })
    .map_err(|e: String| NounVerbError::execution_error(e))
}
```

## Error Handling Pattern

### Convert domain errors to strings

```rust
// domain/utils/doctor.rs
use std::error::Error;

pub async fn run_diagnostics() -> Result<Output, Box<dyn Error>> {
    // Can use ? with any error type
    let config = load_config().await?;
    let status = check_status().await?;
    Ok(Output { config, status })
}

// CLI wrapper
#[verb("doctor", "utils")]
fn utils_doctor() -> Result<Output> {
    execute_async_verb(async {
        crate::domain::utils::run_diagnostics()
            .await
            .map_err(|e| e.to_string()) // Convert any error to String
    })
}
```

### Custom error types

```rust
// domain/utils/error.rs
#[derive(Debug)]
pub enum UtilsError {
    ConfigNotFound,
    InvalidConfig(String),
    IoError(std::io::Error),
}

impl std::fmt::Display for UtilsError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::ConfigNotFound => write!(f, "Configuration file not found"),
            Self::InvalidConfig(msg) => write!(f, "Invalid configuration: {}", msg),
            Self::IoError(e) => write!(f, "I/O error: {}", e),
        }
    }
}

impl std::error::Error for UtilsError {}

// Domain function
pub async fn run_diagnostics() -> Result<Output, UtilsError> {
    let config = load_config().await?;
    Ok(Output { config })
}

// CLI wrapper (error conversion is automatic via Display)
#[verb("doctor", "utils")]
fn utils_doctor() -> Result<Output> {
    execute_async_verb(async {
        crate::domain::utils::run_diagnostics()
            .await
            .map_err(|e| e.to_string())
    })
}
```

## File Organization

```
ggen/
├── cli/src/
│   ├── runtime_helper.rs     # This module
│   └── lib.rs                # Re-export runtime_helper
├── commands/
│   └── utils/
│       └── doctor.rs         # Sync CLI wrapper
└── domain/
    └── utils/
        ├── mod.rs
        └── doctor.rs         # Async business logic
```

## Testing

### Testing CLI Layer

```rust
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_utils_doctor() {
        let result = utils_doctor();
        assert!(result.is_ok());
    }
}
```

### Testing Business Logic Layer

```rust
#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_run_diagnostics() {
        let result = run_diagnostics().await;
        assert!(result.is_ok());

        let output = result.unwrap();
        assert!(!output.checks.is_empty());
    }
}
```

## Migration Checklist

For each command being migrated:

- [ ] Create async domain function in `domain/*/mod.rs`
- [ ] Move business logic to domain function
- [ ] Create sync CLI wrapper in `commands/*/mod.rs`
- [ ] Use `execute_async_verb` or `execute_async`
- [ ] Add proper error conversion (`.map_err(|e| e.to_string())`)
- [ ] Add tests for both layers
- [ ] Update output types (Serialize + Deserialize)
- [ ] Verify compilation

## Common Pitfalls

### ❌ Don't: Put async logic in CLI layer

```rust
// BAD: Async in CLI wrapper
#[verb("doctor", "utils")]
async fn utils_doctor() -> Result<Output> {  // ❌ Can't use async
    let data = tokio::fs::read("config").await?;
    Ok(Output { data })
}
```

### ✅ Do: Separate concerns

```rust
// GOOD: Sync wrapper, async domain
#[verb("doctor", "utils")]
fn utils_doctor() -> Result<Output> {  // ✅ Sync
    execute_async_verb(async {
        crate::domain::utils::load_config().await
            .map_err(|e| e.to_string())
    })
}
```

### ❌ Don't: Create multiple runtimes

```rust
// BAD: Multiple runtimes
#[verb("doctor", "utils")]
fn utils_doctor() -> Result<Output> {
    let rt1 = create_runtime()?;
    let data1 = rt1.block_on(fetch_data())?;  // ❌

    let rt2 = create_runtime()?;
    let data2 = rt2.block_on(process(data1))?;  // ❌ Unnecessary

    Ok(Output { data1, data2 })
}
```

### ✅ Do: Reuse runtime

```rust
// GOOD: Single runtime for all async ops
#[verb("doctor", "utils")]
fn utils_doctor() -> Result<Output> {
    execute_async_verb(async {
        let data1 = fetch_data().await?;  // ✅ Same runtime
        let data2 = process(data1).await?;  // ✅ Same runtime
        Ok(Output { data1, data2 })
    })
}
```

## Performance Considerations

- **Runtime creation overhead**: ~100-200μs (negligible for CLI)
- **block_on overhead**: Minimal, just thread parking
- **Recommendation**: Use `execute_async_verb` for simplicity unless you need fine-grained control

## References

- ASYNC_SYNC_COMPATIBILITY.md - Full compatibility guide
- clap-noun-verb v3.0.0 documentation
- Tokio runtime documentation: https://docs.rs/tokio/latest/tokio/runtime/
