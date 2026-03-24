# ggen-jidoka

Jidoka gates and andon signal system for quality control in ggen.

## Overview

This crate implements the Toyota Production System's **Jidoka** principle: automation with a human touch. Each gate performs quality checks and raises andon signals. Red signals halt the production line immediately, following the "stop-the-line" protocol.

## Features

- **Andon Signal System**: Three-level signal system (Green, Yellow, Red)
- **Quality Gates**: Compiler, Test, Lint, and SHACL validation gates
- **Production Line**: Sequential gate execution with automatic halt on red signals
- **Real-time Monitoring**: Track signal history and statistics
- **Type-safe**: No unwrap/expect in production code, Result<T, E> throughout

## Andon Signal Levels

| Signal | Meaning | Action |
|--------|---------|--------|
| 🟢 Green | All checks pass | Proceed with production |
| 🟡 Yellow | Warning detected | Stop before release |
| 🔴 Red | Critical failure | **HALT THE LINE IMMEDIATELY** |

## Quality Gates

### CompilerGate
Checks for compilation errors using `cargo check`.
- Red: Compilation errors
- Yellow: Compiler warnings
- Green: Clean compilation

### TestGate
Checks for test failures using `cargo test`.
- Red: Test failures
- Yellow: Test warnings
- Green: All tests pass

### LintGate
Checks for clippy warnings using `cargo clippy`.
- Red: Clippy errors
- Yellow: Clippy warnings
- Green: Clean lint

### SHACLGate
Validates RDF specifications using `ggen validate`.
- Red: Validation failures
- Yellow: Validation warnings
- Green: Valid specification

## Usage

### Basic Production Line

```rust
use ggen_jidoka::{
    gate::{CompilerGate, TestGate, LintGate},
    ProductionLine,
};
use std::sync::Arc;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Create production line
    let mut line = ProductionLine::new();

    // Add quality gates
    line.add_gate(Arc::new(CompilerGate::new(".")))
        .add_gate(Arc::new(TestGate::new(".")))
        .add_gate(Arc::new(LintGate::new(".")));

    // Run gates - halts on red signal
    match line.run().await {
        Ok(results) => {
            println!("✅ All gates passed!");
            for result in results {
                println!("{}: {}", result.gate_name, result.signal);
            }
        }
        Err(e) => {
            eprintln!("🚨 Line halted: {}", e);
            // Investigate and fix root cause
        }
    }

    Ok(())
}
```

### Stop on Yellow

Configure the line to halt on yellow signals:

```rust
let line = ProductionLine::new()
    .stop_on_yellow(true);
```

### Signal Monitoring

Track signal history and statistics:

```rust
use ggen_jidoka::monitor::SignalMonitor;

let monitor = SignalMonitor::new();

// Monitor a gate
let signal = monitor.monitor_gate(&gate).await?;

// Get statistics
let stats = monitor.get_stats("Compiler Gate").unwrap();
println!("Success rate: {:.1}%", stats.success_rate() * 100.0);
println!("Total checks: {}", stats.total_checks);
```

### Custom Gates

Implement your own quality gates:

```rust
use ggen_jidoka::{Gate, Signal, AndonSignal, Result};
use async_trait::async_trait;

struct CustomGate;

#[async_trait]
impl Signal for CustomGate {
    async fn check(&self) -> Result<AndonSignal> {
        // Your quality check logic
        Ok(AndonSignal::Green)
    }

    fn name(&self) -> &str {
        "Custom Gate"
    }

    fn description(&self) -> &str {
        "Custom quality check"
    }
}

#[async_trait]
impl Gate for CustomGate {
    async fn execute(&self) -> Result<AndonSignal> {
        self.check().await
    }
}
```

## Stop-the-Line Protocol

When a red signal is encountered:

1. **HALT** - Production line stops immediately
2. **INVESTIGATE** - 5 Whys root cause analysis
3. **FIX** - Address root cause, not symptom
4. **VERIFY** - Re-run checks until cleared
5. **RESUME** - Continue production

## Examples

Run the demo:

```bash
cargo run --example jidoka_demo
```

## Testing

The crate has comprehensive test coverage:

```bash
# Run all tests
cargo test

# Run with output
cargo test -- --nocapture

# Check code quality
cargo clippy -- -D warnings
```

Test coverage: 43 tests (34 unit + 9 integration)

## Architecture

```
ggen-jidoka/
├── src/
│   ├── lib.rs       # AndonSignal enum, Signal trait, core types
│   ├── gate.rs      # Gate trait and implementations
│   ├── line.rs      # ProductionLine with stop-the-line behavior
│   └── monitor.rs   # Real-time signal monitoring
├── tests/
│   └── stop_the_line_tests.rs  # Integration tests
└── examples/
    └── jidoka_demo.rs           # Usage demonstration
```

## Design Principles

- **Type-first**: Encode invariants in types
- **Zero unwrap**: No unwrap/expect in production code
- **Result-based**: All operations return Result<T, E>
- **Chicago TDD**: State-based verification with real collaborators
- **Async-first**: All gates are async for concurrency

## License

MIT
