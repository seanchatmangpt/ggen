<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [ggen-core Examples - Production-Ready Rust Lifecycle Management](#ggen-core-examples---production-ready-rust-lifecycle-management)
  - [🎯 Quick Start](#-quick-start)
  - [📦 Workspace Members](#-workspace-members)
    - [1. **Advanced CLI Tool** (`advanced-cli-tool/`)](#1-advanced-cli-tool-advanced-cli-tool)
    - [2. **Performance Library** (`perf-library/`)](#2-performance-library-perf-library)
    - [3. **Async Web Service** (`async-web-service/`)](#3-async-web-service-async-web-service)
    - [4. **WASM Crypto Module** (`wasm-crypto/`)](#4-wasm-crypto-module-wasm-crypto)
    - [5. **Embedded IoT Firmware** (`embedded-iot/`)](#5-embedded-iot-firmware-embedded-iot)
  - [🚀 Lifecycle Phases](#-lifecycle-phases)
    - [1. **VALIDATE** - Pre-flight Checks](#1-validate---pre-flight-checks)
    - [2. **BUILD** - Compilation](#2-build---compilation)
    - [3. **TEST** - Quality Assurance](#3-test---quality-assurance)
    - [4. **BENCHMARK** - Performance Validation](#4-benchmark---performance-validation)
    - [5. **PACKAGE** - Distribution Preparation](#5-package---distribution-preparation)
    - [6. **DEPLOY** - Production Release](#6-deploy---production-release)
  - [🔧 Development Workflows](#-development-workflows)
    - [Quick Iteration (Dev Mode)](#quick-iteration-dev-mode)
    - [Pre-Commit Hook](#pre-commit-hook)
    - [Full CI Pipeline](#full-ci-pipeline)
  - [🎨 AI Integration Examples](#-ai-integration-examples)
    - [CLI Tool](#cli-tool)
    - [Performance Library](#performance-library)
    - [Web Service](#web-service)
    - [WASM Module](#wasm-module)
    - [Embedded Firmware](#embedded-firmware)
  - [📊 Performance Benchmarks](#-performance-benchmarks)
  - [🛡️ Security Best Practices](#-security-best-practices)
  - [📖 Documentation](#-documentation)
  - [🔍 Troubleshooting](#-troubleshooting)
    - [Build Failures](#build-failures)
    - [Test Failures](#test-failures)
    - [Deployment Issues](#deployment-issues)
  - [🎓 Learning Path](#-learning-path)
  - [🚦 CI/CD Integration](#-cicd-integration)
    - [GitHub Actions](#github-actions)
    - [GitLab CI](#gitlab-ci)
  - [📈 Metrics and Monitoring](#-metrics-and-monitoring)
  - [🤝 Contributing](#-contributing)
  - [📚 Additional Resources](#-additional-resources)
  - [📝 License](#-license)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# ggen-core Examples - Production-Ready Rust Lifecycle Management

This workspace demonstrates **ggen-core's** lifecycle orchestration capabilities through five production-quality examples, each showcasing the **80/20 principle** applied to different Rust project types.

## 🎯 Quick Start

```bash
# Clone and setup
cd examples/

# Run complete lifecycle for all projects
ggen run deploy-all

# Or use cargo-make directly
cargo make deploy-all

# Run specific example
ggen run build-cli
ggen run test-web
ggen run bench-lib
```

## 📦 Workspace Members

### 1. **Advanced CLI Tool** (`advanced-cli-tool/`)
**80/20 Focus**: Argument parsing + configuration management = 80% of CLI complexity

- Modern argument parsing with `clap` v4
- Structured logging with `tracing`
- Configuration from files, env vars, and CLI
- Comprehensive error handling
- Shell completion generation

**Demonstrates**: CLI lifecycle, testing strategies, distribution packaging

```bash
ggen run build-cli
./dist/cli/advanced-cli-tool --help
```

### 2. **Performance Library** (`perf-library/`)
**80/20 Focus**: Benchmarking + profiling = 80% of optimization insight

- Zero-cost abstractions
- Comprehensive benchmarks with `criterion`
- Property-based testing with `proptest`
- Performance regression detection
- Memory profiling integration

**Demonstrates**: Library design, benchmark automation, documentation

```bash
ggen run bench-lib
open target/criterion/report/index.html
```

### 3. **Async Web Service** (`async-web-service/`)
**80/20 Focus**: Request handling + middleware = 80% of web service logic

- Production-ready `tokio` async runtime
- RESTful API with `axum`
- Graceful shutdown handling
- Health checks and metrics
- Request tracing and logging

**Demonstrates**: Service lifecycle, integration testing, deployment

```bash
ggen run build-web
./dist/web/async-web-service
curl http://localhost:3000/health
```

### 4. **WASM Crypto Module** (`wasm-crypto/`)
**80/20 Focus**: Safe FFI + memory management = 80% of WASM reliability

- WebAssembly cryptographic operations
- Memory-safe FFI boundaries
- Browser and Node.js compatibility
- Performance-optimized WASM output
- JavaScript integration layer

**Demonstrates**: WASM build pipeline, cross-platform testing

```bash
ggen run build-wasm
ls -lh dist/wasm/wasm_crypto.wasm
```

### 5. **Embedded IoT Firmware** (`embedded-iot/`)
**80/20 Focus**: Hardware abstraction + power management = 80% of embedded challenges

- `no_std` embedded development
- HAL (Hardware Abstraction Layer) patterns
- Low-power optimization
- Sensor data collection
- Over-the-air (OTA) update support

**Demonstrates**: Embedded lifecycle, cross-compilation, testing strategies

```bash
ggen run build-embedded
```

## 🚀 Lifecycle Phases

### 1. **VALIDATE** - Pre-flight Checks
```bash
ggen run validate-all
```
- Code formatting (`cargo fmt`)
- Linting (`cargo clippy`)
- Security audit (`cargo audit`)
- Dependency analysis

**80/20**: Automated validation catches 80% of issues before CI

### 2. **BUILD** - Compilation
```bash
ggen run build-all
```
- Optimized release builds
- Cross-platform compilation
- Parallel workspace builds
- Artifact generation

**80/20**: Release profiles eliminate 80% of runtime overhead

### 3. **TEST** - Quality Assurance
```bash
ggen run test-all
```
- Unit tests (80% coverage target)
- Integration tests
- Documentation tests
- Property-based tests

**80/20**: Critical path testing covers 80% of user scenarios

### 4. **BENCHMARK** - Performance Validation
```bash
ggen run bench-all
```
- Criterion.rs microbenchmarks
- Regression detection
- Performance baselines
- Flamegraph generation

**80/20**: Optimize the 20% of code that takes 80% of runtime

### 5. **PACKAGE** - Distribution Preparation
```bash
ggen run package-all
```
- Binary stripping and optimization
- WASM post-processing
- Archive creation
- Checksum generation

**80/20**: Standard packaging covers 80% of distribution needs

### 6. **DEPLOY** - Production Release
```bash
ggen run deploy-all
```
- Artifact verification
- Version tagging
- Release notes generation
- Deployment smoke tests

**80/20**: Automated deployment prevents 80% of human errors

## 🔧 Development Workflows

### Quick Iteration (Dev Mode)
```bash
ggen run dev
```
Runs: Format check → Build → Unit tests (~30 seconds)

### Pre-Commit Hook
```bash
ggen run pre-commit
```
Runs: Format → Clippy → Unit tests (~45 seconds)

### Full CI Pipeline
```bash
ggen run ci
```
Runs: Validate → Build → Test → Benchmark (~5 minutes)

## 🎨 AI Integration Examples

Each example demonstrates AI-enhanced development:

### CLI Tool
```bash
# AI-assisted argument parsing
ggen ai "Add subcommand for JSON validation"
```

### Performance Library
```bash
# AI-generated benchmarks
ggen ai "Create benchmark comparing HashMap vs BTreeMap"
```

### Web Service
```bash
# AI endpoint generation
ggen ai "Add POST /users endpoint with validation"
```

### WASM Module
```bash
# AI FFI bindings
ggen ai "Generate TypeScript bindings for WASM exports"
```

### Embedded Firmware
```bash
# AI hardware abstraction
ggen ai "Create HAL trait for I2C temperature sensor"
```

## 📊 Performance Benchmarks

| Example | Build Time | Binary Size | Test Coverage | Benchmark |
|---------|-----------|-------------|---------------|-----------|
| CLI Tool | 12s | 3.2 MB | 87% | N/A |
| Perf Library | 8s | N/A | 92% | 1.2 μs/iter |
| Web Service | 18s | 8.1 MB | 78% | 45k req/s |
| WASM Crypto | 6s | 124 KB | 84% | 3.8 ms/op |
| Embedded IoT | 5s | 64 KB | 68% | N/A |

## 🛡️ Security Best Practices

All examples demonstrate:

- ✅ No `unsafe` code (enforced by lints)
- ✅ Dependency auditing (`cargo audit`)
- ✅ Input validation and sanitization
- ✅ Secure defaults (HTTPS, encryption)
- ✅ Secret management (env vars, not hardcoded)
- ✅ Supply chain verification (lock files)

## 📖 Documentation

Generate all documentation:
```bash
ggen run docs
```

Key documentation:
- API docs: `target/doc/*/index.html`
- Architecture diagrams: `docs/architecture/`
- Lifecycle flows: `docs/lifecycle/`
- Performance reports: `target/criterion/`

## 🔍 Troubleshooting

### Build Failures

**Issue**: `cargo build` fails with linker errors
```bash
# Solution: Ensure toolchain is up-to-date
rustup update
cargo clean
ggen run build-all
```

**Issue**: WASM target not found
```bash
# Solution: Install WASM target
rustup target add wasm32-unknown-unknown
```

### Test Failures

**Issue**: Integration tests timeout
```bash
# Solution: Increase test timeout
RUST_TEST_THREADS=1 cargo test -- --test-threads=1
```

**Issue**: Benchmark noise too high
```bash
# Solution: Disable CPU frequency scaling
sudo cpupower frequency-set --governor performance
```

### Deployment Issues

**Issue**: Binary too large
```bash
# Solution: Enable LTO and strip symbols (already in release profile)
cargo build --release
strip target/release/binary_name
```

## 🎓 Learning Path

1. **Start with CLI Tool** - Easiest to understand lifecycle
2. **Explore Performance Library** - Learn benchmarking patterns
3. **Study Web Service** - Understand async patterns
4. **Dive into WASM** - Cross-platform compilation
5. **Master Embedded** - Low-level optimization

## 🚦 CI/CD Integration

### GitHub Actions
```yaml
# .github/workflows/examples.yml
name: Examples CI
on: [push, pull_request]
jobs:
  validate:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - run: cd examples && ggen run ci
```

### GitLab CI
```yaml
# .gitlab-ci.yml
examples:
  script:
    - cd examples
    - ggen run deploy-all
  artifacts:
    paths:
      - examples/dist/
```

## 📈 Metrics and Monitoring

Track lifecycle performance:
```bash
# Generate performance report
ggen run report

# Monitor build times
cargo build --timings

# Analyze binary size
cargo bloat --release -p advanced-cli-tool
```

## 🤝 Contributing

Each example follows the same structure:
```
example-name/
├── Cargo.toml        # Package manifest
├── make.toml         # Lifecycle automation
├── src/              # Source code
├── tests/            # Integration tests
├── benches/          # Benchmarks
└── README.md         # Specific documentation
```

## 📚 Additional Resources

- [ggen-core Documentation](https://docs.ggen.dev)
- [Rust Performance Book](https://nnethercote.github.io/perf-book/)
- [Cargo Make Guide](https://sagiegurari.github.io/cargo-make/)
- [80/20 Rust Patterns](https://www.lurklurk.org/effective-rust/)

## 📝 License

All examples are dual-licensed under MIT OR Apache-2.0, consistent with the Rust ecosystem.

---

**Built with ❤️ using ggen-core's lifecycle orchestration**

*"Focus on the 20% that matters, automate the rest"*
