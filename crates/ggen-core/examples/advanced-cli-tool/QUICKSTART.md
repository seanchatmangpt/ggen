# Quick Start Guide - Advanced CLI Tool

## 🚀 5-Minute Setup

```bash
cd examples/advanced-cli-tool

# 1. Initialize (one-time setup)
cargo make init

# 2. Setup dependencies
cargo make setup

# 3. Build the project
cargo make build

# 4. Run the tool
cargo run -- --help
```

## 📋 Essential Commands

### Development
```bash
cargo make format-fix      # Auto-format code
cargo make lint           # Check code quality
cargo make build          # Build debug version
cargo make test           # Run all tests
```

### Pre-Commit
```bash
cargo make pre-commit     # Quick validation before commit
```

### Continuous Integration
```bash
cargo make ci             # Standard CI pipeline
cargo make ci-full        # Full CI with coverage & benchmarks
```

### Production
```bash
cargo make build-release  # Optimized build
cargo make deploy         # Create distribution packages
```

## 🎯 Try It Now

### 1. Create Test Data
```bash
mkdir -p /tmp/cli-test/{input,output}
echo "Hello, World!" > /tmp/cli-test/input/test.txt
echo "Sample data" > /tmp/cli-test/input/data.txt
```

### 2. Process Files
```bash
cargo run -- process \
  --input /tmp/cli-test/input \
  --output /tmp/cli-test/output \
  --workers 4
```

### 3. Analyze Directory
```bash
cargo run -- analyze /tmp/cli-test/input --detailed
```

### 4. Run Benchmark
```bash
cargo run -- benchmark --iterations 50 --size 5
```

## 📊 Lifecycle Phases

| Command | Purpose | Duration |
|---------|---------|----------|
| `cargo make init` | Install tooling | 1-2 min |
| `cargo make setup` | Fetch dependencies | 30s |
| `cargo make format` | Check formatting | 5s |
| `cargo make lint` | Static analysis | 15s |
| `cargo make build` | Compile debug | 30s |
| `cargo make test` | Run tests | 5s |
| `cargo make bench` | Benchmarks | 30s |
| `cargo make audit` | Security scan | 10s |
| `cargo make doc` | Generate docs | 20s |
| `cargo make deploy` | Package for release | 2 min |

## 🎓 Full Documentation

See [LIFECYCLE.md](./LIFECYCLE.md) for:
- Complete lifecycle documentation (41 tasks)
- Architecture details
- Best practices
- Troubleshooting guide

## 🔧 Configuration

Edit `config.toml`:
```toml
buffer_size = 8192        # Adjust for performance
max_workers = 4           # CPU core count
compress = false          # Enable compression

[logging]
level = "info"           # Logging verbosity
```

## ✅ Verify Installation

```bash
# Check all tasks are available
cargo make help

# Run quick validation
cargo make pre-commit

# Verify benchmarks work
cargo make bench
```

## 🐛 Troubleshooting

**Compilation fails?**
```bash
cargo make clean
cargo make setup
```

**Tests fail?**
```bash
RUST_LOG=debug cargo test
```

**Missing tools?**
```bash
cargo make init
```

## 📚 Next Steps

1. Read [README.md](./README.md) for features
2. Review [LIFECYCLE.md](./LIFECYCLE.md) for complete docs
3. Explore source code in `src/`
4. Run benchmarks: `cargo make bench`
5. Generate docs: `cargo make doc-open`

---

**Total Setup Time:** ~5 minutes
**Learning Curve:** 15 minutes to understand all lifecycle phases
