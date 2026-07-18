# Validation Automation

**Developer Experience & Quality of Life Automation** for validation approaches in the ggen project.

> 🚀 **Status**: Early Development - Feature complete, needs testing
> 📅 **Created**: 2026-04-07
> 🎯 **Purpose**: Make validation approaches easier to use with better UX through automation

## Overview

This library provides a unified interface for running all 4 validation approaches with:

- ✅ **Progress bars** for long-running operations
- ✅ **Colored terminal output** for visual feedback
- ✅ **Unified CLI** for all validation approaches
- ✅ **Automated report generation** (Markdown, HTML, JSON)
- ✅ **Error recovery** with helpful suggestions
- ✅ **Retry logic** for transient failures

## Validation Approaches

| Approach | Description | Agents | Typical Duration |
|----------|-------------|--------|------------------|
| **Consensus** | Byzantine fault tolerance with voting | 7 | ~700ms |
| **Property-Based** | Testing 7 complementary invariants | 7 | ~1050ms |
| **Mutation-Based** | Adversarial mutation testing | 7 | ~600ms |
| **Fuzzing-Based** | Comprehensive fuzzing | 7 | ~700ms |

## Quick Start

### As a Library

```rust
use validation_automation::ValidationAutomation;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let automation = ValidationAutomation::new().await?;

    // Run all validation approaches
    let report = automation
        .run_all_validations(
            "SELECT * FROM {graph} WHERE ?s ?p ?o",
            100  // intensity: inputs per agent
        )
        .await?;

    // Print summary
    report.print_summary();

    // Generate reports
    report.generate_markdown("VALIDATION_REPORT.md").await?;
    report.generate_html("VALIDATION_REPORT.html").await?;
    report.generate_json("VALIDATION_REPORT.json").await?;

    Ok(())
}
```

### As a CLI Tool

```bash
# Run all approaches
cargo run -- --input "SELECT * FROM {graph}" --report all

# Run specific approach
cargo run -- --approach consensus --intensity 100

# Generate reports in specific directory
cargo run -- --report all --output-dir ./reports

# Verbose output
cargo run -- --verbose
```

## CLI Options

```
Developer Experience & Quality of Life Automation for Validation

Usage: validation-automation [OPTIONS]

Options:
  -i, --input <INPUT>           Input to validate [default: SELECT * FROM {graph} WHERE ?s ?p ?o]
  -n, --intensity <INTENSITY>    Number of inputs per agent [default: 100]
  -a, --approach <APPROACH>     Validation approach to run [possible values: consensus, property, mutation, fuzzing, all]
  -r, --report <REPORT>         Generate reports (markdown, html, json, or all)
  -o, --output-dir <OUTPUT_DIR> Output directory for reports [default: .]
  -v, --verbose                 Verbose output
  -h, --help                    Print help
  -V, --version                 Print version
```

## Features

### 1. Progress Tracking

Real-time progress bars for long-running validation operations:

```
[00:00:00] [=========>                                 ] 2/7 Agent 2 fuzzing...
```

### 2. Colored Output

Visual feedback with colored terminal output:

- ✅ Green: Success
- ❌ Red: Errors
- ⚠️ Yellow: Warnings
- ℹ️ Blue: Info

### 3. Error Recovery

Automatic retry logic for transient failures:

```rust
// Retry up to 3 times for recoverable errors
runner.run_approach_with_retry(approach, input, intensity, 3).await?
```

### 4. Helpful Suggestions

Every error includes actionable suggestions:

```rust
ValidationError::ApproachFailed {
    approach: "consensus",
    details: "Network timeout",
    suggestion: "Check network connectivity and increase timeout",
}
```

### 5. Report Generation

Comprehensive reports in multiple formats:

- **Markdown**: Human-readable documentation
- **HTML**: Web-friendly with styling
- **JSON**: Machine-readable for automation

## Architecture

### Modules

| Module | Purpose |
|--------|---------|
| `runner` | Unified validation runner with approach registration |
| `progress` | Progress bars and colored terminal output |
| `error` | Error types with helpful suggestions |
| `report` | Report generation (Markdown, HTML, JSON) |
| `cli` | Command-line interface |

### Trait-Based Design

Extensible architecture using `ValidationApproachTrait`:

```rust
#[async_trait]
pub trait ValidationApproachTrait: Send + Sync {
    async fn run(&self, input: &str, intensity: usize)
        -> Result<ValidationResult, ValidationError>;

    fn name(&self) -> &str;
    fn description(&self) -> &str;
}
```

## Development

### Build

```bash
cargo build
```

### Run Tests

```bash
cargo test
```

### Run Demo

```bash
cargo run --example demo
```

### Format Code

```bash
cargo fmt
```

### Lint

```bash
cargo clippy
```

## Dependencies

- `tokio` - Async runtime
- `async-trait` - Async trait support
- `serde` / `serde_json` - Serialization
- `chrono` - Time handling
- `anyhow` - Error handling
- `indicatif` - Progress bars
- `colored` - Terminal colors
- `clap` - CLI argument parsing
- `tracing` - Structured logging

## Examples

See the `examples/` directory:

- `demo.rs` - Full demonstration of all features

## License

MIT

## Author

Created for the ggen project to improve developer experience and quality of life when working with validation approaches.

---

**Note**: This is an early-stage implementation. APIs may change as the system matures.
