# chatman-cli Quick Start Guide

## 🚀 30-Second Deployment

Deploy chatman-cli from ontology to crates.io in ≤30 seconds:

```bash
cd marketplace/packages/chatman-cli

# 1. Validate everything (includes ontology, SPARQL, code quality)
./scripts/validate.sh

# 2. Run performance benchmarks
./scripts/benchmark.sh

# 3. Deploy to crates.io
./scripts/deploy.sh --publish --token $CRATES_IO_TOKEN
```

## 📋 Prerequisites Checklist

- [ ] Rust 1.70+ installed (`rustc --version`)
- [ ] RDF ontology created: `rdf/ontology.ttl`
- [ ] 43 workflow patterns defined in ontology
- [ ] Lockchain receipt schema in ontology
- [ ] SPARQL queries in `sparql/` directory
- [ ] crates.io account with API token
- [ ] LICENSE files present

## 🛠️ Development Workflow

### Step 1: Create RDF Ontology

```bash
# Create ontology directory
mkdir -p rdf sparql

# Add your OWL ontology
cat > rdf/ontology.ttl << 'ONTO'
@prefix owl: <http://www.w3.org/2002/07/owl#> .
@prefix chatman: <http://chatman.ai/ontology#> .

<http://chatman.ai/ontology> a owl:Ontology ;
    owl:versionInfo "0.1.0" .

chatman:WorkflowPattern a owl:Class .
chatman:sequence a chatman:WorkflowPattern .
# ... 43 patterns total
ONTO
```

### Step 2: Validate Locally

```bash
# Run all pre-deployment checks
./scripts/validate.sh

# Expected output:
# ✓ RDF ontology is valid
# ✓ Found 43 workflow patterns
# ✓ Lockchain receipt schema validated
# ✓ Clippy checks passed
# ✓ Code formatting is correct
```

### Step 3: Benchmark Performance

```bash
# Run performance benchmarks
./scripts/benchmark.sh

# Targets:
# ✓ Hot-path: ≤2ns
# ✓ Warm-path: ≤500ms
# ✓ Cold-path: ≤500ms
```

### Step 4: Deploy

```bash
# Dry-run first (no publish)
./scripts/deploy.sh

# Actual deployment
export CARGO_REGISTRY_TOKEN="your-token"
./scripts/deploy.sh --publish --token $CARGO_REGISTRY_TOKEN

# Expected output:
# ✓ Validation complete
# ✓ Loaded ontology version: 0.1.0
# ✓ Build complete
# ✓ All tests passed
# ✓ Published chatman-cli v0.1.0 to crates.io
# ✓ Deployment complete in Xs (Target ≤30s: ACHIEVED)
```

## 🤖 GitHub Actions Automation

### Setup Secrets

1. Go to GitHub repository settings
2. Add secret: `CARGO_REGISTRY_TOKEN`
3. Value: Your crates.io API token

### Trigger Deployment

```bash
# Tag release
git tag v0.1.0
git push origin v0.1.0

# GitHub Actions will automatically:
# 1. Validate (ontology + code)
# 2. Benchmark (performance)
# 3. Deploy (crates.io)
# 4. Release (GitHub)
# 5. Test (cross-platform install)
```

## 📊 Performance Targets

| Benchmark | Target | Command |
|-----------|--------|---------|
| Hot-path | ≤2ns | `cargo bench --bench hot_path` |
| Warm-path | ≤500ms | Manual timing in benchmark.sh |
| Cold-path | ≤500ms | Manual timing in benchmark.sh |
| Receipt gen | <10ms | `cargo test receipt_generation` |
| Full deploy | ≤30s | `./scripts/deploy.sh` |

## 🔍 Validation Checks

The `validate.sh` script performs 10 comprehensive checks:

1. **RDF Ontology Syntax** - rapper/riot validation
2. **SPARQL Queries** - Query syntax verification
3. **Cargo.toml** - crates.io requirements
4. **License Files** - MIT/Apache-2.0 presence
5. **Cargo Clippy** - Lint checks (no warnings)
6. **Cargo Format** - Code formatting (rustfmt)
7. **43 Patterns** - Workflow pattern count
8. **Lockchain Schema** - Receipt schema validation
9. **Source Structure** - main.rs/lib.rs presence
10. **Security Audit** - cargo-audit (optional)

## 🐛 Troubleshooting

### Validation Fails

```bash
# Fix formatting
cargo fmt

# Fix clippy warnings
cargo clippy --fix --allow-dirty

# Check specific issues
./scripts/validate.sh 2>&1 | grep "✗"
```

### Benchmark Performance Miss

```bash
# Profile hot-path
cargo bench --bench hot_path -- --verbose

# Check binary optimization
cargo build --release
ls -lh target/release/chatman-cli

# Should be stripped (check with 'file')
file target/release/chatman-cli
```

### Deployment Timeout

```bash
# Check network
cargo publish --dry-run

# Verify token
cargo login --token $CRATES_IO_TOKEN

# Increase timeout if needed (edit deploy.sh)
```

## 📁 Project Structure

```
chatman-cli/
├── scripts/
│   ├── deploy.sh          # Main 30s deployment
│   ├── validate.sh        # 10 validation checks
│   └── benchmark.sh       # Performance testing
├── .github/workflows/
│   └── deploy.yml         # CI/CD automation
├── src/
│   ├── main.rs            # CLI commands
│   └── lib.rs             # Core library
├── benches/
│   ├── hot_path.rs        # ≤2ns benchmarks
│   └── pattern_execution.rs  # Pattern tests
├── rdf/
│   └── ontology.ttl       # OWL with 43 patterns
├── sparql/                # SPARQL queries
├── Cargo.toml             # Package manifest
├── LICENSE-MIT            # MIT license
├── LICENSE-APACHE         # Apache 2.0 license
└── README.md              # User documentation
```

## ✅ Pre-Deployment Checklist

Before running `./scripts/deploy.sh --publish`:

- [ ] RDF ontology validates with rapper/riot
- [ ] Ontology contains 43 workflow patterns
- [ ] Lockchain receipt schema defined
- [ ] All tests pass: `cargo test`
- [ ] Clippy clean: `cargo clippy -- -D warnings`
- [ ] Formatted: `cargo fmt --check`
- [ ] License files present (MIT + Apache-2.0)
- [ ] README.md complete
- [ ] CARGO_REGISTRY_TOKEN set
- [ ] Version updated in ontology
- [ ] Benchmarks meet targets

## 🎯 Next Steps

1. **Create Ontology**: Define 43 workflow patterns in `rdf/ontology.ttl`
2. **Add SPARQL**: Create queries in `sparql/` directory
3. **Implement CLI**: Expand `src/main.rs` with ontology loading
4. **Add Tests**: Create comprehensive test suite
5. **Local Validation**: Run `./scripts/validate.sh`
6. **Local Deploy**: Test `./scripts/deploy.sh` (dry-run)
7. **GitHub Setup**: Configure secrets and push tags
8. **Production Deploy**: Tag release and publish

## 📚 Resources

- [Cargo Publishing Guide](https://doc.rust-lang.org/cargo/reference/publishing.html)
- [crates.io Publishing](https://crates.io/policies)
- [GitHub Actions Rust](https://github.com/actions-rs)
- [Criterion Benchmarking](https://bheisler.github.io/criterion.rs/book/)
- [RDF/SPARQL with Oxigraph](https://github.com/oxigraph/oxigraph)

---

**Time to deploy: ≤30 seconds** ⚡
