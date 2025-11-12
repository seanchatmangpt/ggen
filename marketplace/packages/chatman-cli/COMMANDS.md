# chatman-cli Command Reference

## Deployment Commands

### Deploy to crates.io
```bash
# Dry-run (validates everything, no publish)
./scripts/deploy.sh

# Production publish
./scripts/deploy.sh --publish --token $CRATES_IO_TOKEN

# With environment variable
export CARGO_REGISTRY_TOKEN="your-token-here"
./scripts/deploy.sh --publish
```

### Validate Package
```bash
# Run all 10 validation checks
./scripts/validate.sh

# Expected output:
# ✓ RDF ontology is valid
# ✓ Found 43 workflow patterns
# ✓ Lockchain receipt schema validated
# ✓ All tests passed
```

### Benchmark Performance
```bash
# Run all benchmark suites
./scripts/benchmark.sh

# Run specific benchmark
cargo bench --bench hot_path
cargo bench --bench pattern_execution

# With verbose output
cargo bench -- --verbose
```

## Development Commands

### Build
```bash
# Debug build
cargo build

# Release build (optimized)
cargo build --release

# Check without building
cargo check
```

### Test
```bash
# Run all tests
cargo test

# Run specific test
cargo test test_validate_ontology

# With output
cargo test -- --nocapture

# Release mode tests
cargo test --release
```

### Lint & Format
```bash
# Run clippy lints
cargo clippy

# Fix clippy issues
cargo clippy --fix

# Check formatting
cargo fmt --check

# Auto-format code
cargo fmt
```

### Documentation
```bash
# Build docs
cargo doc

# Build and open docs
cargo doc --open

# Include private items
cargo doc --document-private-items
```

## CLI Commands (After Installation)

### Validate Ontology
```bash
# Validate default ontology
chatman-cli validate

# Validate specific file
chatman-cli validate --ontology path/to/ontology.ttl
```

### List Patterns
```bash
# List all 43 workflow patterns
chatman-cli patterns list

# Show specific pattern details
chatman-cli patterns show sequence
chatman-cli patterns show parallel
```

### Execute Patterns
```bash
# Execute pattern
chatman-cli execute sequence

# With input parameters (JSON)
chatman-cli execute sequence --input '{"steps": ["a", "b", "c"]}'

# Generate cryptographic receipt
chatman-cli execute sequence --input '{"steps": ["a"]}' --receipt
```

### Generate Receipts
```bash
# Generate receipt for execution
chatman-cli receipt exec_12345

# Save to file
chatman-cli receipt exec_12345 --output receipt.json
```

### Query Ontology
```bash
# Run SPARQL query
chatman-cli query "SELECT ?pattern WHERE { ?pattern a chatman:WorkflowPattern }"

# Different output format
chatman-cli query "SELECT * WHERE { ?s ?p ?o } LIMIT 10" --format turtle
chatman-cli query "..." --format json
chatman-cli query "..." --format ntriples
```

### Verbose Mode
```bash
# Enable debug logging
chatman-cli --verbose validate
chatman-cli --verbose patterns list
chatman-cli --verbose execute sequence
```

## GitHub Actions Commands

### Manual Trigger
```bash
# From GitHub UI:
# 1. Go to Actions tab
# 2. Select "Deploy to crates.io" workflow
# 3. Click "Run workflow"
# 4. Choose branch
# 5. Set publish: true/false

# Or using gh CLI:
gh workflow run deploy.yml -f publish=true
```

### View Workflow Status
```bash
# List workflow runs
gh run list --workflow=deploy.yml

# View specific run
gh run view <run-id>

# Watch run logs
gh run watch
```

### Tag Release
```bash
# Create and push tag
git tag v0.1.0
git push origin v0.1.0

# Create annotated tag
git tag -a v0.1.0 -m "Release version 0.1.0"
git push origin v0.1.0

# Delete tag (if needed)
git tag -d v0.1.0
git push origin :refs/tags/v0.1.0
```

## Cargo Commands

### Package Management
```bash
# Create package tarball
cargo package

# List package contents
cargo package --list

# Verify package can be published
cargo publish --dry-run

# Actual publish
cargo publish --token $CRATES_IO_TOKEN
```

### Dependency Management
```bash
# Update dependencies
cargo update

# Check for outdated dependencies
cargo outdated

# Security audit
cargo audit

# Tree view of dependencies
cargo tree
```

### Clean & Maintenance
```bash
# Clean build artifacts
cargo clean

# Clean only release artifacts
cargo clean --release

# Remove specific target
cargo clean --target x86_64-unknown-linux-gnu
```

## Installation Commands

### From crates.io (Post-Publish)
```bash
# Install latest version
cargo install chatman-cli

# Install specific version
cargo install chatman-cli --version 0.1.0

# Install with all features
cargo install chatman-cli --all-features

# Install locked versions
cargo install chatman-cli --locked
```

### From Source
```bash
# Install from local source
cargo install --path .

# Install from git repository
cargo install --git https://github.com/yourusername/ggen \
  --root chatman-cli

# Uninstall
cargo uninstall chatman-cli
```

## Profiling & Analysis

### Performance Profiling
```bash
# Install profiling tools
cargo install flamegraph

# Generate flamegraph
sudo cargo flamegraph --bench hot_path

# Install criterion
cargo install cargo-criterion

# Run with criterion
cargo criterion
```

### Binary Analysis
```bash
# Check binary size
ls -lh target/release/chatman-cli

# Analyze binary symbols
nm target/release/chatman-cli

# Check if stripped
file target/release/chatman-cli

# Strip binary manually
strip target/release/chatman-cli
```

### Memory Profiling
```bash
# Install valgrind (Linux/Mac)
# Linux: sudo apt-get install valgrind
# Mac: brew install valgrind

# Memory leak check
valgrind --leak-check=full ./target/release/chatman-cli validate

# Heap profiling
valgrind --tool=massif ./target/release/chatman-cli validate
ms_print massif.out.<pid>
```

## RDF/SPARQL Tools

### Validate Ontology
```bash
# Using rapper (raptor2-utils)
rapper -i turtle -o ntriples rdf/ontology.ttl

# Using riot (Apache Jena)
riot --validate rdf/ontology.ttl

# Count triples
rapper -i turtle -c rdf/ontology.ttl
```

### Query Ontology (External Tools)
```bash
# Using sparql-query (if installed)
sparql-query --data=rdf/ontology.ttl --query=sparql/list_patterns.rq

# Using Apache Jena arq
arq --data=rdf/ontology.ttl --query=sparql/list_patterns.rq
```

## Troubleshooting Commands

### Debug Build Issues
```bash
# Verbose build
cargo build --verbose

# Very verbose build
cargo build -vv

# Clean and rebuild
cargo clean && cargo build --release
```

### Debug Test Failures
```bash
# Run tests with backtrace
RUST_BACKTRACE=1 cargo test

# Full backtrace
RUST_BACKTRACE=full cargo test

# Run single test with output
cargo test test_name -- --nocapture --test-threads=1
```

### Check Dependencies
```bash
# Verify Cargo.lock
cargo verify-project

# Generate Cargo.lock
cargo generate-lockfile

# Update Cargo.lock
cargo update
```

## Environment Variables

### Build Configuration
```bash
# Set Rust flags
export RUSTFLAGS="-C target-cpu=native"

# Set optimization level
export CARGO_PROFILE_RELEASE_OPT_LEVEL=3

# Set LTO
export CARGO_PROFILE_RELEASE_LTO=true
```

### Deployment
```bash
# crates.io token
export CARGO_REGISTRY_TOKEN="your-token"

# Custom registry
export CARGO_REGISTRIES_MY_REGISTRY_INDEX="https://my-registry.com/index"
```

### Testing
```bash
# Test threads
export RUST_TEST_THREADS=1

# Test timeout
export RUST_TEST_TIME_UNIT=60000
```

## Quick Reference

### Most Common Workflow
```bash
# 1. Development
cargo build
cargo test
cargo clippy
cargo fmt

# 2. Validation
./scripts/validate.sh

# 3. Benchmarking
./scripts/benchmark.sh

# 4. Deployment
./scripts/deploy.sh                              # Dry-run
./scripts/deploy.sh --publish --token $TOKEN     # Publish

# 5. Verification
cargo install chatman-cli
chatman-cli --version
```

### Pre-Publish Checklist
```bash
# Run all checks
./scripts/validate.sh          # ✓ Validate
cargo test --release           # ✓ Test
cargo clippy -- -D warnings    # ✓ Lint
cargo fmt --check              # ✓ Format
cargo package --list           # ✓ Review files
cargo publish --dry-run        # ✓ Dry-run
```

### Emergency Rollback
```bash
# Yank published version (doesn't delete, just warns users)
cargo yank --vers 0.1.0

# Unyank if needed
cargo yank --vers 0.1.0 --undo
```

---

**For complete documentation, see:**
- [DEPLOYMENT.md](DEPLOYMENT.md) - Full deployment guide
- [QUICK_START.md](QUICK_START.md) - Getting started
- [README.md](README.md) - Package overview
