# chatman-cli Deployment Automation

## Overview

30-second deployment pipeline from ontology to crates.io with comprehensive validation and benchmarking.

## Deployment Scripts

### 1. deploy.sh (Main Deployment)

**Target Execution Time: ≤30s**

```bash
./scripts/deploy.sh [--publish] [--token CRATES_IO_TOKEN]
```

**Pipeline Steps:**
1. Validate ontology & prerequisites (5s target)
2. Load ontology from `rdf/ontology.ttl` (2s target)
3. Generate CLI from ontology using ggen (5s target)
4. Build release binary (10s target)
5. Run test suite (5s target)
6. Publish to crates.io (3s target)

**Features:**
- Automatic version extraction from ontology
- Dry-run publish validation
- Color-coded progress output
- Execution time tracking

### 2. validate.sh (Pre-Deployment Validation)

```bash
./scripts/validate.sh
```

**Validation Checks:**
- ✓ RDF ontology syntax (rapper/riot)
- ✓ SPARQL queries validation
- ✓ Cargo.toml completeness (crates.io requirements)
- ✓ License files presence
- ✓ Cargo clippy lints
- ✓ Code formatting (rustfmt)
- ✓ 43 workflow patterns verification
- ✓ Lockchain receipt schema
- ✓ Source code structure
- ✓ Dependencies security (cargo-audit)

**Exit Codes:**
- 0: All validations passed
- 1: One or more validations failed

### 3. benchmark.sh (Performance Benchmarking)

```bash
./scripts/benchmark.sh
```

**Benchmark Suites:**
1. **Hot-Path** (target: ≤2ns)
   - Hash computation
   - Pattern lookup
   - String operations
   - JSON parsing

2. **Warm-Path** (target: ≤500ms)
   - File system cached operations
   - Pattern execution with cache

3. **Cold-Path** (target: ≤500ms)
   - First-run execution
   - Cache-cleared startup

4. **Receipt Generation**
   - Cryptographic overhead
   - SHA-256 hashing performance

5. **Pattern Execution**
   - All 43 workflow patterns
   - Individual pattern benchmarks

6. **Memory & Binary Size**
   - Peak memory usage (valgrind/massif)
   - Binary size optimization
   - Strip validation

## GitHub Actions Workflow

### Automated CI/CD Pipeline

**Trigger Conditions:**
- Git tags matching `v*.*.*`
- Manual workflow dispatch

**Jobs:**

1. **validate** (Pre-deployment)
   - Install Rust toolchain (stable)
   - Cache Cargo registry & build
   - Install RDF validators (raptor2-utils)
   - Run validation script

2. **benchmark** (Performance Testing)
   - Rust toolchain setup
   - Cargo build cache
   - Execute benchmark suite

3. **deploy** (crates.io Publishing)
   - Verify CARGO_REGISTRY_TOKEN secret
   - Run deployment script with `--publish`
   - Create GitHub Release with binaries

4. **test-install** (Post-Deploy Verification)
   - Matrix: ubuntu, macos, windows
   - Install from crates.io
   - Verify installation & basic commands

### Required Secrets

- `CARGO_REGISTRY_TOKEN`: crates.io API token
- `GITHUB_TOKEN`: Automatically provided

## Usage Examples

### Local Development

```bash
# Full validation before deployment
./scripts/validate.sh

# Performance benchmarking
./scripts/benchmark.sh

# Dry-run deployment (no publish)
./scripts/deploy.sh

# Actual deployment to crates.io
./scripts/deploy.sh --publish --token $CRATES_IO_TOKEN
```

### CI/CD Workflow

```bash
# Tag release
git tag v0.1.0
git push origin v0.1.0

# GitHub Actions automatically:
# 1. Validates ontology & code
# 2. Runs benchmarks
# 3. Publishes to crates.io
# 4. Creates GitHub release
# 5. Tests installation across platforms
```

## Performance Targets

| Metric | Target | Validation |
|--------|--------|------------|
| Hot-path execution | ≤2ns | In-memory operations |
| Warm-path execution | ≤500ms | File system cached |
| Cold-path execution | ≤500ms | First run |
| Total deployment | ≤30s | End-to-end pipeline |
| Binary size | Minimal | Stripped release build |

## Prerequisites

### Development

- Rust 1.70+ toolchain
- cargo-clippy (linting)
- rustfmt (formatting)
- RDF validators: rapper or riot
- Optional: cargo-audit (security)
- Optional: valgrind (memory profiling)

### Deployment

- crates.io account & API token
- GitHub repository with Actions enabled
- Secrets configured (CARGO_REGISTRY_TOKEN)

## Troubleshooting

### Validation Failures

```bash
# Check specific validation steps
grep -A 5 "error\|failed" <(./scripts/validate.sh)

# Fix formatting
cargo fmt

# Fix clippy issues
cargo clippy --fix --allow-dirty
```

### Benchmark Performance

```bash
# Run specific benchmark
cargo bench --bench hot_path

# Profile with flamegraph
cargo install flamegraph
sudo cargo flamegraph --bench hot_path
```

### Deployment Issues

```bash
# Verify crates.io token
cargo login --token $CRATES_IO_TOKEN

# Check package before publish
cargo package --list

# Dry-run publish
cargo publish --dry-run
```

## Cargo.toml Configuration

Key crates.io requirements:

```toml
[package]
name = "chatman-cli"
version = "0.1.0"           # Extracted from ontology
edition = "2021"
description = "..."         # Required
license = "MIT OR Apache-2.0"  # Required
repository = "..."          # Required
readme = "README.md"        # Recommended
```

## File Organization

```
chatman-cli/
├── scripts/
│   ├── deploy.sh          # Main deployment (30s target)
│   ├── validate.sh        # Pre-deployment validation
│   └── benchmark.sh       # Performance benchmarking
├── .github/workflows/
│   └── deploy.yml         # CI/CD automation
├── src/
│   ├── main.rs            # CLI entry point
│   └── lib.rs             # Library code
├── benches/
│   ├── hot_path.rs        # Hot-path benchmarks
│   └── pattern_execution.rs  # Pattern benchmarks
├── rdf/
│   └── ontology.ttl       # OWL ontology (43 patterns)
├── sparql/                # SPARQL query templates
├── Cargo.toml             # Package manifest
├── LICENSE-MIT            # MIT license
├── LICENSE-APACHE         # Apache 2.0 license
└── README.md              # Package documentation
```

## Next Steps

1. Create RDF ontology: `rdf/ontology.ttl`
2. Define 43 workflow patterns in ontology
3. Add Lockchain receipt schema
4. Implement SPARQL queries
5. Test deployment pipeline locally
6. Configure GitHub secrets
7. Tag release and deploy

## References

- [Cargo Book - Publishing](https://doc.rust-lang.org/cargo/reference/publishing.html)
- [crates.io Publishing Guide](https://doc.rust-lang.org/cargo/reference/publishing.html)
- [GitHub Actions for Rust](https://github.com/actions-rs)
- [Criterion.rs Benchmarking](https://github.com/bheisler/criterion.rs)
