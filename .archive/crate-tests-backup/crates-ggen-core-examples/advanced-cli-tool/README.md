# Advanced CLI Tool

Production-ready Rust CLI tool demonstrating ggen's complete lifecycle management with make.toml.

## Features

- 🚀 Async I/O with Tokio
- 📊 Multiple subcommands (process, analyze, convert, benchmark)
- 🎨 Interactive UI with progress bars
- 🔧 Configuration via TOML files
- 📝 Structured logging with tracing
- ⚡ Parallel processing with Rayon
- 🧪 Comprehensive testing
- 📈 Performance benchmarking with Criterion

## Quick Start

```bash
# Initialize and setup
cargo make init
cargo make setup

# Build and test
cargo make build
cargo make test

# Run the tool
cargo run -- --help
cargo run -- process -i ./input -o ./output
cargo run -- analyze ./data --detailed
cargo run -- benchmark -i 100 -s 10
```

## Lifecycle Commands

All commands are defined in `make.toml`:

```bash
# Development
cargo make format          # Format code
cargo make lint           # Run clippy
cargo make build          # Build debug
cargo make build-release  # Build optimized

# Testing
cargo make test           # Run all tests
cargo make test-coverage  # Generate coverage
cargo make bench          # Run benchmarks

# Quality
cargo make audit          # Security audit
cargo make doc            # Generate docs

# Deployment
cargo make deploy         # Complete pipeline
cargo make package        # Create distribution
```

## Complete Pipeline

```bash
# Run full CI pipeline
cargo make ci

# Run full pipeline with coverage and benchmarks
cargo make ci-full
```

## Configuration

Edit `config.toml` to customize:

```toml
buffer_size = 8192
max_workers = 4
compress = false

[logging]
level = "info"
json_format = false

[performance]
chunk_size = 1048576
timeout_secs = 300
```

## Usage Examples

### Process Files
```bash
cargo run -- process \
  --input ./data \
  --output ./processed \
  --workers 8 \
  --compress
```

### Analyze Directory
```bash
cargo run -- analyze ./data --detailed --format json
```

### Convert Files
```bash
cargo run -- convert input.txt output.hash --format hash
```

### Benchmark
```bash
cargo run -- benchmark --iterations 100 --size 10
```

## Lifecycle Phases

1. **init** - Install tooling (rustfmt, clippy, cargo-audit, etc.)
2. **setup** - Fetch dependencies and validate environment
3. **format** - Format code with rustfmt
4. **lint** - Lint with clippy
5. **build** - Compile project
6. **test** - Run unit and integration tests
7. **bench** - Performance benchmarking
8. **audit** - Security and dependency auditing
9. **doc** - Generate documentation
10. **deploy** - Create distribution packages

## Hooks

- **before_all** - Pre-pipeline setup
- **before_build** - Pre-build validation
- **after_build** - Post-build verification
- **before_deploy** - Pre-deployment checks
- **after_deploy** - Post-deployment tasks
- **after_all** - Pipeline cleanup

## Testing

```bash
# Unit tests
cargo make test-unit

# Integration tests
cargo make test-integration

# Coverage report
cargo make test-coverage
# Opens: coverage/index.html
```

## Benchmarking

```bash
# Run benchmarks
cargo make bench

# Create baseline
cargo make bench-baseline

# Compare against baseline
cargo make bench-compare
```

## Security

```bash
# Complete audit
cargo make audit

# Individual checks
cargo make audit-security  # Vulnerabilities
cargo make audit-deps      # Outdated deps
cargo make audit-licenses  # License check
```

## Distribution

```bash
# Create packages
cargo make package

# Creates:
# - dist/advanced-cli-tool-{OS}-{ARCH}.tar.gz
# - dist/advanced-cli-tool-{OS}-{ARCH}.tar.gz.sha256
```

## Architecture

```
src/
├── main.rs       # CLI entry point with clap
├── config.rs     # Configuration management
└── processor.rs  # Core processing logic

tests/
└── integration.rs # Integration tests

benches/
└── performance.rs # Criterion benchmarks
```

## Dependencies

- **clap** - CLI argument parsing
- **tokio** - Async runtime
- **anyhow** - Error handling
- **tracing** - Structured logging
- **serde** - Serialization
- **indicatif** - Progress bars
- **rayon** - Parallel processing
- **criterion** - Benchmarking

## License

MIT
