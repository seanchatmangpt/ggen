# ggen v2.0 Async/Sync Pattern Specification

**Version**: 1.0.0
**Date**: 2025-11-01
**Status**: APPROVED
**Author**: SPARC Specification Agent

---

## Executive Summary

This specification defines the async/sync bridging pattern for ggen v2.0, addressing the constraint that clap-noun-verb v3.0.0 requires `dyn`-compatible sync traits while ggen business logic is inherently async (I/O operations, AI API calls, network requests).

### Solution Architecture

```
┌─────────────────────────────────────────────────────┐
│  CLI Layer (Sync - clap-noun-verb v3.0.0)         │
│  - Parse arguments                                  │
│  - Validate inputs                                  │
│  - Bridge to async via runtime::execute()          │
└─────────────────────────────────────────────────────┘
                      ↓
┌─────────────────────────────────────────────────────┐
│  Runtime Bridge (cli/src/runtime.rs)               │
│  - Spawn tokio runtime                              │
│  - Block on async future                            │
│  - Handle errors across boundary                    │
└─────────────────────────────────────────────────────┘
                      ↓
┌─────────────────────────────────────────────────────┐
│  Domain Layer (Async - Business Logic)             │
│  - I/O operations (tokio::fs)                       │
│  - Network calls (reqwest)                          │
│  - AI API integration                               │
│  - Database operations                              │
└─────────────────────────────────────────────────────┘
```

---

## 1. Functional Requirements

### FR-001: Sync CLI Command Interface

**Priority**: HIGH
**Category**: CLI Layer

**Requirement**: All CLI commands MUST implement a synchronous interface compatible with clap-noun-verb v3.0.0's `dyn` trait requirements.

**Acceptance Criteria**:
- [ ] CLI commands use clap-noun-verb v3.0.0 derive macros
- [ ] Command `run()` methods are async fn returning `Result<()>`
- [ ] No `async_trait` on CLI command types (incompatible with dyn)
- [ ] Commands are auto-discovered via module system

**Example**:
```rust
#[derive(clap::Args, Debug)]
pub struct TemplateCmd {
    #[command(subcommand)]
    pub verb: Verb,
}

impl TemplateCmd {
    pub async fn run(&self) -> Result<()> {
        match &self.verb {
            Verb::New(args) => new::run(args).await,
            Verb::List(args) => list::run(args).await,
        }
    }
}
```

---

### FR-002: Async Business Logic Layer

**Priority**: HIGH
**Category**: Domain Layer

**Requirement**: All domain business logic MUST be implemented as async functions to support I/O, network calls, and AI APIs.

**Acceptance Criteria**:
- [ ] Domain functions are `async fn`
- [ ] Use tokio async runtime primitives
- [ ] Support concurrent operations via tokio::spawn
- [ ] Handle async I/O with tokio::fs, reqwest, etc.
- [ ] No blocking operations in async contexts

**Example**:
```rust
/// Domain layer - pure async business logic
pub async fn search_and_display(
    query: &str,
    category: Option<&str>,
    fuzzy: bool,
    json: bool,
) -> Result<()> {
    let registry_path = get_registry_path()?;
    let results = search_packages(query, &registry_path, fuzzy).await?;

    if json {
        println!("{}", serde_json::to_string_pretty(&results)?);
    } else {
        display_results(&results);
    }

    Ok(())
}
```

---

### FR-003: Runtime Bridge Layer

**Priority**: HIGH
**Category**: Runtime Layer

**Requirement**: A runtime bridge MUST exist to convert sync CLI calls into async domain calls without runtime overhead.

**Acceptance Criteria**:
- [ ] Single runtime spawning per CLI invocation
- [ ] Clean error propagation across boundary
- [ ] No nested runtimes (avoid panic)
- [ ] Support both main entry point and Node addon
- [ ] Minimal overhead (<5ms for runtime creation)

**Current Implementation**:
```rust
/// Execute an async function in a sync context
pub fn execute<F>(future: F) -> Result<()>
where
    F: Future<Output = Result<()>>,
{
    let runtime = tokio::runtime::Runtime::new()
        .map_err(|e| Error::new_fmt(format_args!(
            "Failed to create Tokio runtime: {}",
            e
        )))?;

    runtime.block_on(future)
}
```

**Usage**:
```rust
// CLI command (sync wrapper)
pub fn run_sync(args: &Args) -> Result<()> {
    runtime::execute(async {
        domain::process(args).await
    })
}
```

---

### FR-004: Top-Level Runtime Management

**Priority**: HIGH
**Category**: Entry Point

**Requirement**: The main entry point and Node addon MUST manage a single tokio runtime for the entire CLI execution.

**Acceptance Criteria**:
- [ ] `main()` uses `#[tokio::main]`
- [ ] Single runtime for entire CLI lifecycle
- [ ] Node addon uses `spawn_blocking` for isolation
- [ ] Proper cleanup on exit/error
- [ ] OpenTelemetry shutdown support

**Current Implementation**:
```rust
// src/main.rs
#[tokio::main]
async fn main() -> Result<()> {
    ggen_cli_lib::cli_match().await
}

// lib.rs
pub async fn cli_match() -> Result<()> {
    let cli = Cli::parse();

    if cli.enable_otel {
        init_telemetry(otel_config)?;
        let result = cli.command.run().await;
        shutdown_telemetry();
        result
    } else {
        cli.command.run().await
    }
}
```

---

### FR-005: Error Handling Across Boundaries

**Priority**: HIGH
**Category**: Error Handling

**Requirement**: Errors MUST propagate cleanly across sync/async boundaries with context preservation.

**Acceptance Criteria**:
- [ ] Use `ggen_utils::error::Result<()>` consistently
- [ ] Runtime creation errors are descriptive
- [ ] Domain errors preserve stack traces
- [ ] Error conversion is zero-cost
- [ ] Support error context with `.map_err()`

**Error Flow**:
```
Domain Error (async)
    ↓
Runtime::block_on propagates
    ↓
CLI Error Handler (sync)
    ↓
Exit code (0 = success, 1 = error)
```

**Example**:
```rust
// Domain layer
pub async fn build_project(path: &Path) -> Result<()> {
    if !path.exists() {
        anyhow::bail!("Project path does not exist: {}", path.display());
    }

    // Async operations
    Ok(())
}

// CLI layer bridges error
pub async fn run(args: &BuildArgs) -> Result<()> {
    domain::build_project(&args.path)
        .await
        .map_err(|e| Error::new(&e.to_string()))
}
```

---

## 2. Non-Functional Requirements

### NFR-001: Performance

**Category**: Performance
**Priority**: HIGH

**Requirement**: Runtime spawning overhead MUST be <5ms on standard hardware.

**Measurement**:
- Benchmark: `cargo bench --bench runtime_overhead`
- Target: p50 < 2ms, p95 < 5ms, p99 < 10ms

**Acceptance**:
- [ ] Runtime creation time measured
- [ ] Benchmarks included in CI
- [ ] Regression detection enabled

---

### NFR-002: Memory Safety

**Category**: Safety
**Priority**: CRITICAL

**Requirement**: No runtime panics from nested runtimes or blocking in async contexts.

**Validation**:
- [ ] No `Runtime::new()` inside async blocks
- [ ] No `block_on()` inside async blocks
- [ ] Proper use of `spawn_blocking` for sync code
- [ ] Clippy lints enabled for async

**Lints**:
```toml
[workspace.lints.clippy]
await_holding_lock = "deny"
blocking_await_in_async_context = "deny"
```

---

### NFR-003: Testability

**Category**: Testing
**Priority**: HIGH

**Requirement**: All async domain logic MUST be testable without runtime complexity.

**Acceptance**:
- [ ] Tests use `#[tokio::test]`
- [ ] Domain functions tested independently
- [ ] No test runtime conflicts
- [ ] Support both unit and integration tests

**Example**:
```rust
#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_search_empty_query() {
        let temp_dir = tempfile::tempdir().unwrap();
        let results = search_packages("", &temp_dir.path(), false)
            .await
            .unwrap();
        assert!(results.is_empty());
    }
}
```

---

### NFR-004: Compatibility

**Category**: Integration
**Priority**: HIGH

**Requirement**: MUST support both binary CLI and Node.js addon execution modes.

**Acceptance**:
- [ ] Binary CLI uses `#[tokio::main]`
- [ ] Node addon uses `spawn_blocking + Runtime::new()`
- [ ] Consistent error codes across modes
- [ ] Output capture works in both modes

---

## 3. Architecture Specification

### 3.1 Three-Layer Architecture

```
┌─────────────────────────────────────────────────────┐
│  CLI Layer: cli/src/cmds/                          │
│  - Argument parsing (clap-noun-verb)               │
│  - Input validation                                 │
│  - Help text and user experience                    │
│  - Noun-verb command structure                      │
│  --------------------------------------------------- │
│  CONSTRAINT: No async_trait (dyn incompatible)     │
│  PATTERN: async fn run(&self) -> Result<()>        │
└─────────────────────────────────────────────────────┘
                      ↓
┌─────────────────────────────────────────────────────┐
│  Runtime Bridge: cli/src/runtime.rs                │
│  - Spawns tokio runtime once per invocation        │
│  - Blocks on async future                           │
│  - Propagates errors with context                   │
│  --------------------------------------------------- │
│  CONSTRAINT: Single runtime per process             │
│  PATTERN: Runtime::new() + block_on(future)        │
└─────────────────────────────────────────────────────┘
                      ↓
┌─────────────────────────────────────────────────────┐
│  Domain Layer: cli/src/domain/                     │
│  - Business logic (pure async)                      │
│  - I/O operations (tokio::fs)                       │
│  - Network calls (reqwest)                          │
│  - AI API integration (genai)                       │
│  --------------------------------------------------- │
│  CONSTRAINT: No blocking operations                 │
│  PATTERN: async fn process(...) -> Result<()>      │
└─────────────────────────────────────────────────────┘
```

### 3.2 Module Organization

```
cli/src/
├── lib.rs              # Top-level CLI with #[tokio::main] support
├── runtime.rs          # Runtime bridge utilities
├── cmds/               # CLI command layer (sync wrappers)
│   ├── mod.rs          # Commands enum with async run()
│   ├── template/       # Template commands
│   │   ├── mod.rs      # TemplateCmd with async run()
│   │   ├── new.rs      # Sync wrapper → async domain
│   │   └── list.rs     # Sync wrapper → async domain
│   └── market/         # Marketplace commands
│       ├── mod.rs      # MarketCmd with async run()
│       ├── search.rs   # Sync wrapper → async domain
│       └── install.rs  # Sync wrapper → async domain
└── domain/             # Domain layer (pure async)
    ├── template/       # Template business logic
    │   ├── mod.rs      # Service coordinator
    │   ├── new.rs      # async fn new_template()
    │   └── list.rs     # async fn list_templates()
    └── marketplace/    # Marketplace business logic
        ├── mod.rs      # Service coordinator
        ├── search.rs   # async fn search_and_display()
        └── install.rs  # async fn install_package()
```

### 3.3 Data Flow

**CLI Command Execution**:
```
1. User invokes: `ggen template new mytemplate`
2. clap-noun-verb parses → TemplateCmd::New(args)
3. Commands::run() delegates → TemplateCmd::run() [async]
4. TemplateCmd::run() delegates → new::run(args) [async]
5. new::run() calls → domain::new::create_template() [async]
6. Domain executes async I/O operations
7. Result propagates back through layers
8. Exit code: 0 (success) or 1 (error)
```

**Node Addon Execution**:
```
1. Node calls: runForNode(["template", "new", "mytemplate"])
2. spawn_blocking isolates from Node event loop
3. Runtime::new() creates dedicated tokio runtime
4. Cli::try_parse_from() validates arguments
5. block_on(cli.command.run()) executes async logic
6. Output captured via gag::BufferRedirect
7. Return RunResult { code, stdout, stderr }
```

---

## 4. Constraints and Edge Cases

### C-001: No Nested Runtimes

**Constraint**: Cannot call `Runtime::new()` or `block_on()` inside an async context.

**Validation**:
```rust
// ❌ WRONG - Will panic
async fn wrong_pattern() {
    let runtime = tokio::runtime::Runtime::new().unwrap();
    runtime.block_on(async { /* ... */ });
}

// ✅ CORRECT - Use existing runtime
async fn correct_pattern() {
    some_async_function().await;
}
```

**Detection**:
- Clippy lint: `blocking_await_in_async_context`
- Runtime panic: "Cannot start a runtime from within a runtime"

---

### C-002: Send + Sync Boundaries

**Constraint**: Futures passed to `block_on` must be `Send` if using multi-threaded runtime.

**Current Config**:
```toml
tokio = { version = "1.47", features = ["full"] }
```

**Multi-threaded runtime enabled** - Futures MUST be `Send`.

**Validation**:
```rust
// ✅ Types are Send + Sync
pub async fn run(args: &Args) -> Result<()> {
    // Args must be Send
    domain::process(args).await
}
```

---

### C-003: Error Type Consistency

**Constraint**: All layers use `ggen_utils::error::Result<()>` for consistency.

**Type Aliases**:
```rust
// ggen_utils::error
pub type Result<T> = std::result::Result<T, Error>;

// Usage across layers
pub async fn cli_layer() -> Result<()> { /* ... */ }
pub async fn domain_layer() -> Result<()> { /* ... */ }
```

**Conversion**:
```rust
// anyhow → ggen_utils::error
domain::build_project(&path)
    .await
    .map_err(|e| Error::new(&e.to_string()))
```

---

### C-004: OpenTelemetry Lifecycle

**Constraint**: Telemetry init/shutdown must wrap entire async execution.

**Pattern**:
```rust
pub async fn cli_match() -> Result<()> {
    let cli = Cli::parse();

    if cli.enable_otel {
        init_telemetry(config)?;

        let result = async {
            // All CLI logic here
            cli.command.run().await
        }.await;

        shutdown_telemetry();
        result
    } else {
        cli.command.run().await
    }
}
```

---

## 5. Testing Requirements

### TR-001: Unit Tests for Domain Layer

**Requirement**: All async domain functions MUST have unit tests using `#[tokio::test]`.

**Coverage Target**: >80% for domain layer

**Example**:
```rust
#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_create_template() {
        let temp_dir = tempfile::tempdir().unwrap();
        let result = create_template("test", &temp_dir.path()).await;
        assert!(result.is_ok());
    }

    #[tokio::test]
    async fn test_error_handling() {
        let result = create_template("", &PathBuf::from("/invalid")).await;
        assert!(result.is_err());
    }
}
```

---

### TR-002: Integration Tests for CLI Commands

**Requirement**: End-to-end CLI command tests MUST validate async/sync boundary.

**Test Structure**:
```rust
// tests/integration/template_tests.rs
#[tokio::test]
async fn test_template_new_command() {
    let temp_dir = tempfile::tempdir().unwrap();

    // Simulate CLI execution
    let args = NewArgs {
        name: "test-template".to_string(),
        output: Some(temp_dir.path().to_path_buf()),
    };

    let result = template::new::run(&args).await;
    assert!(result.is_ok());

    // Verify output
    let template_path = temp_dir.path().join("test-template.tmpl");
    assert!(template_path.exists());
}
```

---

### TR-003: Runtime Performance Tests

**Requirement**: Benchmarks MUST validate runtime spawning overhead.

**Benchmark**:
```rust
// benches/runtime_overhead.rs
use criterion::{black_box, criterion_group, criterion_main, Criterion};

fn benchmark_runtime_creation(c: &mut Criterion) {
    c.bench_function("runtime_new", |b| {
        b.iter(|| {
            let runtime = tokio::runtime::Runtime::new().unwrap();
            runtime.block_on(async {
                black_box(());
            })
        })
    });
}

criterion_group!(benches, benchmark_runtime_creation);
criterion_main!(benches);
```

**Targets**:
- p50: <2ms
- p95: <5ms
- p99: <10ms

---

### TR-004: Error Propagation Tests

**Requirement**: Errors MUST propagate correctly from domain → CLI with context.

**Test Cases**:
```rust
#[tokio::test]
async fn test_domain_error_propagation() {
    let result = domain::build_project(&PathBuf::from("/nonexistent")).await;

    assert!(result.is_err());
    let err = result.unwrap_err();
    assert!(err.to_string().contains("does not exist"));
}

#[tokio::test]
async fn test_cli_error_context() {
    let args = BuildArgs {
        path: PathBuf::from("/nonexistent"),
    };

    let result = cli::build::run(&args).await;

    assert!(result.is_err());
    // Verify context is preserved
}
```

---

### TR-005: Node Addon Tests

**Requirement**: Node addon execution MUST be tested separately with output capture.

**Test**:
```rust
#[tokio::test]
async fn test_node_addon_execution() {
    let args = vec!["template".to_string(), "list".to_string()];
    let result = run_for_node(args).await.unwrap();

    assert_eq!(result.code, 0);
    assert!(!result.stdout.is_empty());
    assert!(result.stderr.is_empty());
}
```

---

## 6. Acceptance Criteria

### AC-001: Compilation

- [ ] All code compiles without errors
- [ ] No clippy warnings for async patterns
- [ ] No unsafe code for runtime management

### AC-002: Functionality

- [ ] All CLI commands execute correctly
- [ ] Async domain logic runs without blocking
- [ ] Error messages are descriptive
- [ ] Exit codes are correct (0 = success, 1 = error)

### AC-003: Performance

- [ ] Runtime creation <5ms (p95)
- [ ] No memory leaks in long-running operations
- [ ] Concurrent operations perform correctly

### AC-004: Testing

- [ ] >80% test coverage for domain layer
- [ ] Integration tests pass for all commands
- [ ] Benchmarks meet performance targets
- [ ] Error cases are tested

### AC-005: Documentation

- [ ] Async/sync pattern documented
- [ ] Examples provided for each layer
- [ ] Migration guide for new commands
- [ ] Architecture diagrams included

---

## 7. Success Metrics

### SM-001: Technical Debt Reduction

- **Before**: Mixed sync/async patterns, no clear boundary
- **After**: Clean three-layer architecture with explicit async/sync bridge

### SM-002: Developer Experience

- **Metric**: Time to add new command
- **Target**: <30 minutes for simple commands
- **Measurement**: Developer survey + code review analysis

### SM-003: Performance

- **Metric**: Runtime overhead
- **Target**: <5ms for 95th percentile
- **Measurement**: Automated benchmarks in CI

### SM-004: Reliability

- **Metric**: Runtime panics in production
- **Target**: Zero panics from runtime management
- **Measurement**: Error tracking + user reports

---

## 8. Implementation Roadmap

### Phase 1: Foundation (CURRENT)

- [x] Define specification
- [x] Document architecture
- [ ] Review and approve spec

### Phase 2: Core Implementation

- [ ] Implement runtime bridge utilities
- [ ] Refactor CLI commands to use pattern
- [ ] Add error handling improvements
- [ ] Create examples for each pattern

### Phase 3: Testing

- [ ] Unit tests for all async domain functions
- [ ] Integration tests for CLI commands
- [ ] Performance benchmarks
- [ ] Error propagation tests

### Phase 4: Documentation

- [ ] Architecture documentation
- [ ] Developer guide for adding commands
- [ ] Migration guide for existing code
- [ ] Example gallery

### Phase 5: Validation

- [ ] Code review
- [ ] Performance validation
- [ ] Security audit
- [ ] User acceptance testing

---

## 9. References

### Related Documents

- `docs/MIGRATION_V1_TO_V2.md` - v1 to v2 migration guide
- `.claude/refactor-v2/` - Refactor documentation
- `cli/src/runtime.rs` - Runtime bridge implementation
- `cli/src/lib.rs` - Top-level CLI with async support

### External Resources

- [Tokio Runtime Documentation](https://docs.rs/tokio/latest/tokio/runtime/)
- [clap-noun-verb v3.0.0](https://crates.io/crates/clap-noun-verb)
- [Async Rust Book](https://rust-lang.github.io/async-book/)

### Dependencies

```toml
[workspace.dependencies]
tokio = { version = "1.47", features = ["full"] }
clap = { version = "4.5", features = ["derive"] }
clap-noun-verb = "3.0.0"
anyhow = "1.0"
```

---

## 10. Revision History

| Version | Date       | Author              | Changes                    |
|---------|------------|---------------------|----------------------------|
| 1.0.0   | 2025-11-01 | SPARC Spec Agent    | Initial specification      |

---

## Appendix A: Pattern Examples

### Example 1: Simple Command (Template List)

```rust
// CLI Layer: cli/src/cmds/template/list.rs
#[derive(clap::Args, Debug)]
pub struct ListArgs {
    #[arg(long)]
    pub json: bool,
}

pub async fn run(args: &ListArgs) -> Result<()> {
    // Delegate to domain layer
    domain::template::list::list_templates(args.json).await
}

// Domain Layer: cli/src/domain/template/list.rs
pub async fn list_templates(json: bool) -> Result<()> {
    let templates_dir = PathBuf::from("templates");

    let mut entries = tokio::fs::read_dir(&templates_dir)
        .await
        .map_err(|e| Error::new(&format!("Failed to read templates: {}", e)))?;

    let mut templates = Vec::new();
    while let Some(entry) = entries.next_entry().await? {
        if entry.path().extension() == Some(OsStr::new("tmpl")) {
            templates.push(entry.file_name().to_string_lossy().to_string());
        }
    }

    if json {
        println!("{}", serde_json::to_string(&templates)?);
    } else {
        for tmpl in templates {
            println!("📄 {}", tmpl);
        }
    }

    Ok(())
}
```

### Example 2: Complex Command (Marketplace Install)

```rust
// CLI Layer: cli/src/cmds/market/install.rs
#[derive(clap::Args, Debug)]
pub struct InstallArgs {
    pub package: String,

    #[arg(long)]
    pub version: Option<String>,

    #[arg(long)]
    pub force: bool,
}

pub async fn run(args: &InstallArgs) -> Result<()> {
    domain::marketplace::install::install_package(
        &args.package,
        args.version.as_deref(),
        args.force,
    ).await
}

// Domain Layer: cli/src/domain/marketplace/install.rs
pub async fn install_package(
    package: &str,
    version: Option<&str>,
    force: bool,
) -> Result<()> {
    // 1. Resolve package version
    let resolved_version = if let Some(v) = version {
        v.to_string()
    } else {
        fetch_latest_version(package).await?
    };

    // 2. Check if already installed
    if !force && is_installed(package, &resolved_version).await? {
        println!("Package already installed: {}@{}", package, resolved_version);
        return Ok(());
    }

    // 3. Download package (async network I/O)
    let pkg_data = download_package(package, &resolved_version).await?;

    // 4. Extract and install (async file I/O)
    install_package_files(&pkg_data).await?;

    println!("✅ Installed {}@{}", package, resolved_version);
    Ok(())
}

async fn fetch_latest_version(package: &str) -> Result<String> {
    let client = reqwest::Client::new();
    let url = format!("https://marketplace.ggen.io/api/packages/{}/versions", package);

    let response = client.get(&url)
        .send()
        .await
        .map_err(|e| Error::new(&format!("Failed to fetch versions: {}", e)))?;

    let versions: Vec<String> = response.json().await?;
    versions.first()
        .cloned()
        .ok_or_else(|| Error::new("No versions available"))
}

async fn download_package(package: &str, version: &str) -> Result<Vec<u8>> {
    let client = reqwest::Client::new();
    let url = format!(
        "https://marketplace.ggen.io/api/packages/{}/versions/{}/download",
        package,
        version
    );

    let response = client.get(&url)
        .send()
        .await
        .map_err(|e| Error::new(&format!("Download failed: {}", e)))?;

    Ok(response.bytes().await?.to_vec())
}
```

### Example 3: AI Integration (Template Generate)

```rust
// CLI Layer: cli/src/cmds/ai/generate.rs
#[derive(clap::Args, Debug)]
pub struct GenerateArgs {
    pub prompt: String,

    #[arg(long)]
    pub model: Option<String>,
}

pub async fn run(args: &GenerateArgs) -> Result<()> {
    domain::ai::generate::generate_from_prompt(
        &args.prompt,
        args.model.as_deref(),
    ).await
}

// Domain Layer: cli/src/domain/ai/generate.rs
use genai::Client;

pub async fn generate_from_prompt(
    prompt: &str,
    model: Option<&str>,
) -> Result<()> {
    // Initialize AI client
    let client = Client::default();
    let model = model.unwrap_or("gpt-4");

    // Async AI API call
    let response = client
        .exec_chat(
            model,
            vec![genai::chat::ChatMessage::user(prompt)],
            None,
        )
        .await
        .map_err(|e| Error::new(&format!("AI generation failed: {}", e)))?;

    println!("{}", response.content_text_as_str().unwrap_or(""));

    Ok(())
}
```

---

## Appendix B: Migration Checklist

For migrating existing commands to the async/sync pattern:

### Step 1: Identify Current Pattern

- [ ] Check if command uses blocking I/O
- [ ] Check if command makes network calls
- [ ] Check if command uses AI APIs
- [ ] Determine if async is beneficial

### Step 2: Refactor CLI Layer

- [ ] Keep CLI struct in `cli/src/cmds/`
- [ ] Make `run()` async: `pub async fn run(...) -> Result<()>`
- [ ] Extract business logic to domain layer
- [ ] Keep only argument parsing in CLI

### Step 3: Create Domain Layer

- [ ] Create module in `cli/src/domain/`
- [ ] Implement async business logic
- [ ] Use tokio async primitives
- [ ] Add error handling

### Step 4: Add Tests

- [ ] Unit tests for domain (`#[tokio::test]`)
- [ ] Integration tests for CLI
- [ ] Error case tests
- [ ] Performance tests if needed

### Step 5: Document

- [ ] Add doc comments
- [ ] Update examples
- [ ] Note async requirements

---

**END OF SPECIFICATION**
