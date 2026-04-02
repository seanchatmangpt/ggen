# V2.0 Architecture Pattern Template

**Proven by Agent 3**: Doctor command migration (`utils/doctor`)

## Pattern Overview

This pattern separates CLI concerns (sync, argument parsing) from business logic (async, domain operations) using a runtime bridge.

```
┌─────────────────────────────────────────────────────────────────┐
│                         USER INPUT                              │
│                     ggen utils doctor --verbose                 │
└───────────────────────────┬─────────────────────────────────────┘
                            │
                            ▼
┌─────────────────────────────────────────────────────────────────┐
│              LAYER 1: CLI Wrapper (Sync)                        │
│              File: cli/src/commands/utils/doctor.rs             │
│                                                                 │
│  • Parse arguments with clap                                   │
│  • Validate user input                                         │
│  • Transform to domain types                                   │
│  • Call runtime bridge                                         │
│                                                                 │
│  pub fn run(args: &DoctorArgs) -> Result<()> {                │
│      crate::runtime::execute(async {                          │
│          crate::domain::utils::doctor::run_doctor(            │
│              args.verbose,                                     │
│              args.check.as_deref(),                           │
│              args.env                                          │
│          ).await                                               │
│      })                                                         │
│  }                                                              │
└───────────────────────────┬─────────────────────────────────────┘
                            │
                            ▼
┌─────────────────────────────────────────────────────────────────┐
│              LAYER 2: Runtime Bridge (Agent 2)                  │
│              File: cli/src/runtime.rs                           │
│                                                                 │
│  • Create Tokio runtime                                        │
│  • Block on async future                                       │
│  • Handle errors                                               │
│  • Return to sync context                                      │
│                                                                 │
│  pub fn execute<F>(future: F) -> Result<()>                   │
│  where F: Future<Output = Result<()>>                         │
│  {                                                              │
│      let runtime = tokio::runtime::Runtime::new()?;           │
│      runtime.block_on(future)                                  │
│  }                                                              │
└───────────────────────────┬─────────────────────────────────────┘
                            │
                            ▼
┌─────────────────────────────────────────────────────────────────┐
│              LAYER 3: Domain Logic (Async)                      │
│              File: cli/src/domain/utils/doctor.rs               │
│                                                                 │
│  • Pure business logic                                         │
│  • Async operations                                            │
│  • Rich domain types                                           │
│  • Comprehensive tests                                         │
│  • No CLI dependencies                                         │
│                                                                 │
│  pub async fn run_doctor(                                      │
│      verbose: bool,                                            │
│      check_name: Option<&str>,                                │
│      show_env: bool                                            │
│  ) -> Result<()> {                                             │
│      // Business logic here                                    │
│  }                                                              │
└─────────────────────────────────────────────────────────────────┘
```

## File Structure

```
cli/src/
├── commands/                    # LAYER 1: Sync CLI Wrappers
│   ├── mod.rs
│   └── utils/
│       ├── mod.rs
│       └── doctor.rs           # ~30-100 LOC (thin wrapper)
│
├── domain/                      # LAYER 3: Async Business Logic
│   ├── mod.rs
│   └── utils/
│       ├── mod.rs
│       └── doctor.rs           # ~100-500 LOC (domain logic)
│
└── runtime.rs                   # LAYER 2: Bridge (38 LOC, reusable)
```

## Implementation Template

### Step 1: Domain Layer (Async Business Logic)

**File**: `cli/src/domain/{noun}/{verb}.rs`

```rust
//! {Verb} operation - Domain layer
//!
//! Pure business logic for {description}.

use ggen_utils::error::Result;

/// Output type for the operation
#[derive(Debug, Clone)]
pub struct {Verb}Output {
    // Rich domain types here
}

/// Main entry point (called by sync wrapper)
///
/// # Arguments
/// * `param1` - Description
/// * `param2` - Description
///
/// # Returns
/// * `Ok({Verb}Output)` - Success
/// * `Err(_)` - Business error
pub async fn execute_{verb}(
    param1: Type1,
    param2: Type2,
) -> Result<{Verb}Output> {
    // Pure business logic here
    // No CLI dependencies
    // Full async/await
    Ok(output)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_{verb}_success() {
        let result = execute_{verb}(/* args */).await;
        assert!(result.is_ok());
    }
}
```

### Step 2: CLI Wrapper (Sync Interface)

**File**: `cli/src/commands/{noun}/{verb}.rs`

```rust
//! Sync CLI wrapper for {verb} command

use clap::Args;
use ggen_utils::error::Result;

/// Arguments for the {verb} command
#[derive(Args, Debug, Clone)]
pub struct {Verb}Args {
    /// Description of flag
    #[arg(short, long)]
    pub flag1: bool,

    /// Description of option
    #[arg(short, long)]
    pub option1: Option<String>,
}

/// Execute the {verb} command (sync wrapper)
///
/// This function:
/// 1. Validates CLI arguments
/// 2. Spawns a Tokio runtime
/// 3. Calls the async domain function
/// 4. Handles errors and formats output
pub fn run(args: &{Verb}Args) -> Result<()> {
    // Use the runtime bridge to execute async domain logic
    crate::runtime::execute(async {
        crate::domain::{noun}::{verb}::execute_{verb}(
            args.flag1,
            args.option1.as_deref(),
        )
        .await
        .map(|output| {
            // Format output for CLI display
            println!("Success: {:?}", output);
        })
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_{verb}_args_defaults() {
        let args = {Verb}Args {
            flag1: false,
            option1: None,
        };
        assert!(!args.flag1);
        assert!(args.option1.is_none());
    }
}
```

### Step 3: Module Declarations

**File**: `cli/src/commands/{noun}/mod.rs`
```rust
pub mod {verb};
```

**File**: `cli/src/commands/mod.rs`
```rust
pub mod {noun};
```

**File**: `cli/src/domain/{noun}/mod.rs`
```rust
pub mod {verb};
```

**File**: `cli/src/domain/mod.rs`
```rust
pub mod {noun};
```

## Pattern Benefits

### 1. **Clear Separation of Concerns**
- CLI layer: Argument parsing, user interface
- Domain layer: Business logic, operations
- Runtime layer: Async/sync bridging

### 2. **Testability**
- Domain logic independently testable (no CLI mocking)
- Wrapper layer has simple unit tests
- Integration tests can test full stack

### 3. **Reusability**
- Domain logic reusable in other contexts (API, library)
- Runtime bridge reused by all commands
- Pattern scales to 100+ commands

### 4. **Maintainability**
- Each layer has single responsibility
- Changes isolated to appropriate layer
- Easy to understand and modify

### 5. **Type Safety**
- Rich domain types (not string bags)
- Compiler-checked transformations
- No runtime type errors

## Migration Checklist

When migrating a command to v2 architecture:

- [ ] **Read existing code**: Understand current implementation
- [ ] **Identify business logic**: Separate from CLI concerns
- [ ] **Create domain layer**:
  - File: `cli/src/domain/{noun}/{verb}.rs`
  - Make all functions `async`
  - Remove CLI dependencies
  - Use rich domain types
  - Add comprehensive tests
- [ ] **Create CLI wrapper**:
  - File: `cli/src/commands/{noun}/{verb}.rs`
  - Keep sync (no async)
  - Parse arguments with clap
  - Call `crate::runtime::execute()`
  - Add basic tests
- [ ] **Update modules**:
  - Add to `cli/src/commands/mod.rs`
  - Add to `cli/src/commands/{noun}/mod.rs`
  - Add to `cli/src/domain/mod.rs`
  - Add to `cli/src/domain/{noun}/mod.rs`
- [ ] **Verify compilation**: `cargo check --package ggen-cli-lib`
- [ ] **Run tests**: `cargo test --package ggen-cli-lib`
- [ ] **Integration**: Update main CLI dispatcher
- [ ] **Cleanup**: Delete old v1 file

## Common Patterns

### Error Handling
```rust
// Domain layer returns Result
pub async fn execute() -> Result<Output> {
    // Business logic that may fail
    let data = fetch_data().await?;
    Ok(Output { data })
}

// Wrapper propagates errors
pub fn run(args: &Args) -> Result<()> {
    crate::runtime::execute(async {
        execute().await?;
        Ok(())
    })
}
```

### Optional Parameters
```rust
// Wrapper transforms Options
pub fn run(args: &Args) -> Result<()> {
    crate::runtime::execute(async {
        execute(
            args.required_field,
            args.optional_field.as_deref(), // Transform Option<String> → Option<&str>
        ).await
    })
}
```

### Rich Return Types
```rust
// Domain returns rich types
pub struct CommandOutput {
    pub status: Status,
    pub message: String,
    pub details: Vec<Detail>,
}

// Wrapper formats for display
pub fn run(args: &Args) -> Result<()> {
    crate::runtime::execute(async {
        let output = execute().await?;
        println!("Status: {:?}", output.status);
        for detail in output.details {
            println!("  - {}", detail);
        }
        Ok(())
    })
}
```

## Performance Characteristics

- **Wrapper overhead**: Minimal (~50 microseconds to spawn runtime)
- **Domain logic**: Same as before (no performance change)
- **Memory**: One Tokio runtime per command invocation
- **Compilation**: Faster (smaller compilation units)

## Validation Criteria

A v2 migration is complete when:

1. ✅ Domain logic is fully async
2. ✅ Wrapper is fully sync
3. ✅ Uses `crate::runtime::execute()` bridge
4. ✅ No CLI dependencies in domain layer
5. ✅ Compiles without errors: `cargo check`
6. ✅ Tests pass: `cargo test`
7. ✅ Old v1 file deleted
8. ✅ Documentation updated

## Next Steps

After POC validation (Agent 3 complete):

- **Agent 4**: Migrate next subsystem using this template
- **Agent 5-12**: Continue systematic migration
- **Integration**: Bulk update main CLI after all commands migrated
- **Cleanup**: Remove old v1 command files
- **Testing**: Full integration test suite

## Reference Implementation

**Doctor Command** (Agent 3 POC):
- Domain: `cli/src/domain/utils/doctor.rs` (553 LOC)
- Wrapper: `cli/src/commands/utils/doctor.rs` (87 LOC)
- Runtime: `cli/src/runtime.rs` (38 LOC, shared)
- Status: ✅ Complete, compiles, tested

See: `.claude/refactor-v2/agent3-doctor-poc.md` for detailed analysis
