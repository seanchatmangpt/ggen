# Cargo Make Protocol - Complete Reference

## All Available Targets

### Compilation Targets

| Target | Purpose | Timeout | Use When |
|--------|---------|---------|----------|
| `check` | Compile only | 5s | Verifying syntax, fast feedback |
| `build` | Build binary | 15s | Preparing for release |
| `build-release` | Optimized build | 30s | Production build |

### Testing Targets

| Target | Purpose | Timeout | Use When |
|--------|---------|---------|----------|
| `test` | All tests | 30s/120s | Full validation |
| `test-unit` | Unit tests only | 10s | Quick validation |
| `test-quiet` | Silent pass/fail | 30s | CI environments |
| `test-ui` | UI tests | 60s | Frontend changes |
| `bench` | Benchmarks | 5min | Performance tracking |

### Quality Targets

| Target | Purpose | Timeout | Use When |
|--------|---------|---------|----------|
| `lint` | Clippy check | 60s | Code review |
| `format` | Auto-format | 30s | Before commit |
| `format-check` | Check formatting | 10s | CI validation |
| `security-audit` | Dependency audit | 60s | Release prep |

### Integration Targets

| Target | Purpose | Timeout | Use When |
|--------|---------|---------|----------|
| `pre-commit` | Full pre-commit | 60s | Before git commit |
| `pre-push` | Full validation | 120s | Before git push |
| `ci` | Full CI pipeline | 600s | CI environment |

### Performance Targets

| Target | Purpose | Timeout | Use When |
|--------|---------|---------|----------|
| `slo-check` | Verify SLOs | 120s | Validating performance |
| `bench` | Run all benchmarks | 5min | Baseline measurement |
| `bench-compare` | Compare versions | 10min | Regression detection |

### Specification Targets

| Target | Purpose | Timeout | Use When |
|--------|---------|---------|----------|
| `speckit-check` | Validate TTL syntax | 30s | Before implementation |
| `speckit-validate` | SHACL validation | 60s | Spec completeness |
| `speckit-render` | Generate markdown | 30s | After TTL edits |

## SLO Verification

```bash
# Check all SLOs
cargo make slo-check

# Output shows:
# ✓ First build: 12.3s (target: ≤15s)
# ✓ Incremental: 1.8s (target: ≤2s)
# ✓ Unit tests: 8.9s (target: ≤10s)
# ✓ Full tests: 28.3s (target: ≤30s)
# ✓ RDF processing: 3.2s (target: ≤5s)
# ✓ Memory usage: 85MB (target: ≤100MB)
# ✓ CLI scaffolding: 2.7s (target: ≤3s)
```

## Error Interpretation

### Compilation Errors

```bash
$ cargo make check
error[E0425]: cannot find value `x` in this scope
  --> src/lib.rs:10:5
   |
10 | println!("{}", x);
   |                ^ not found in this scope

# Fix: Define variable before use
```

### Clippy Warnings (RED signal)

```bash
$ cargo make lint
warning: using `clone` on type `T` which implements `Copy`
  --> src/main.rs:5:10

# Fix: Remove unnecessary clone
```

### Test Failures (RED signal)

```bash
$ cargo make test
thread 'tests::test_cache' panicked at 'assertion failed'

# Fix: Verify test logic or implementation
```

### Timeout (System under load)

```bash
$ cargo make test
Timeout after 30s (escalation timeout: 120s allowed)

# Options:
# 1. Wait and retry (system may be overloaded)
# 2. Run on less loaded machine
# 3. Profile for optimization opportunities
```

## Environment Setup

### Required

```bash
# Rust toolchain (1.91.1+)
rustup update
rustc --version

# Cargo make
cargo install cargo-make
cargo make --version
```

### Optional

```bash
# Benchmarking with HTML reports
cargo install cargo-criterion

# Mutation testing
cargo install cargo-mutants

# Code coverage
cargo install cargo-tarpaulin
```

## Profile Configuration

### Development (Fast Iteration)

```toml
[profile.dev]
opt_level = 0              # No optimization
debug = true              # Keep debug info
incremental = true        # Cache builds
codegen_units = 256       # Parallel codegen
```

**Use for**: Active development, fastest build times

### Test (Balanced)

```toml
[profile.test]
opt_level = 0              # Fast to compile
incremental = true        # Cache builds
codegen_units = 256       # Parallel codegen
```

**Use for**: Running tests during development

### Release (Optimized)

```toml
[profile.release]
opt_level = 3              # Optimize aggressively
lto = "thin"              # Link-time optimization
strip = true              # Remove symbols
codegen_units = 16        # Optimize more
```

**Use for**: Final builds, benchmarking, releases

## Lint Levels

### Rust Compiler Lints

```rust
#![warn(missing_docs)]        // Warn on missing docs
#![deny(unsafe_code)]         // No unsafe by default
#![deny(warnings)]            // All warnings are errors
```

### Clippy Lints

```rust
#![deny(clippy::all)]         // All clippy checks
#![deny(clippy::pedantic)]    // Extra scrutiny
#![deny(clippy::unwrap_used)] // No unwrap!
#![deny(clippy::panic)]       // Panic-free
```

## Common Workflows with Cargo Make

### Feature Development

```bash
# 1. Write code
# 2. Quick check
cargo make check

# 3. Test your changes
cargo make test-unit

# 4. Full validation
cargo make format
cargo make lint

# 5. All tests
cargo make test

# 6. Ready?
cargo make pre-commit
git add .
git commit -m "feat: ..."
```

### Performance Optimization

```bash
# 1. Establish baseline
cargo make bench > baseline.txt

# 2. Make optimizations
# ... edit code ...

# 3. Verify improvement
cargo make bench > optimized.txt

# 4. Compare results
cargo make bench-compare

# 5. Check SLOs
cargo make slo-check
```

### Before Pushing

```bash
# Full CI validation
cargo make ci

# Check performance not degraded
cargo make slo-check

# Verify specifications
cargo make speckit-check

# Ready to push
git push origin feature-branch
```

## Integration with IDEs

### VS Code with Rust Analyzer

Add to `.vscode/settings.json`:

```json
{
  "rust-analyzer.checkOnSave.command": "make",
  "rust-analyzer.check.target": "check"
}
```

### Neovim with vim-cargo

Configure keymaps for cargo-make:

```vim
nnoremap <leader>cc :! cargo make check<CR>
nnoremap <leader>ct :! cargo make test-unit<CR>
nnoremap <leader>cl :! cargo make lint<CR>
```

## Performance Monitoring

### Track Build Times

```bash
# Before and after optimization
time cargo make check    # Record compilation time
time cargo make test     # Record test time
time cargo make lint     # Record lint time
```

### Memory Profiling

```bash
# Check generation memory usage
/usr/bin/time -v cargo make check

# Should show: Maximum resident set size ≤ 100MB
```

### Benchmark Trending

```bash
# Track SLOs over commits
for commit in $(git log --oneline -10 | awk '{print $1}'); do
    git checkout $commit
    cargo make slo-check >> slo-history.txt
done
```

## Troubleshooting Gallery

### Issue: "cargo: command not found"

```bash
# Solution: cargo not in PATH
export PATH="$HOME/.cargo/bin:$PATH"
cargo --version
```

### Issue: "Timeout after 30s"

```bash
# Likely cause: System under heavy load
# Solution: Run on less loaded system or retry

# Or profile to optimize:
cargo make bench | head -20
```

### Issue: "RUSTFLAGS: -D warnings"

```bash
# Means: Compiler treating warnings as errors
# Solution: Fix the underlying warning, don't suppress

cargo make lint  # See the warning
# Fix code...
cargo make check # Retry
```

### Issue: "Failed to compile due to external crate"

```bash
# Solution: Update dependencies
cargo update
cargo make check

# Or specific crate:
cargo update -p <crate-name>
```
