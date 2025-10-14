# Cargo Publish Validator - Quick Start

Fast, comprehensive validation before publishing to crates.io.

## Installation

Already included in cleanroom project at `bin/validate-crate`.

```bash
# Make executable (if needed)
chmod +x bin/validate-crate

# Test it works
./bin/validate-crate --help
```

## Basic Usage

```bash
# Run validation (default: 10s timeout)
./bin/validate-crate

# Verbose mode
./bin/validate-crate -v

# Custom timeout
./bin/validate-crate -t 30

# Custom output
./bin/validate-crate -o validation.json
```

## What It Checks

✅ **Cargo.toml** - Required fields (name, version, license, etc.)
✅ **Compilation** - `cargo check --all-features`
✅ **Formatting** - `cargo fmt --check`
✅ **Lints** - `cargo clippy -- -D warnings`
✅ **Tests** - `cargo test --all` (in cleanroom)
✅ **Documentation** - README, LICENSE files
✅ **Code Quality** - `.expect()`, `.unwrap()` usage

## Exit Codes

- `0` = Ready to publish ✅
- `1` = Validation failed ❌
- `2` = Error during validation ⚠️

## Quick Examples

### Pre-Commit Hook

```bash
# .git/hooks/pre-commit
#!/bin/bash
./bin/validate-crate || exit 1
```

### GitHub Actions

```yaml
- name: Validate
  run: |
    chmod +x bin/validate-crate
    ./bin/validate-crate -v
```

### Makefile

```makefile
validate:
	@./bin/validate-crate -v
```

### Script Integration

```bash
# Run and check results
./bin/validate-crate -o report.json

if jq -e '.ready_to_publish == true' report.json; then
    cargo publish --dry-run
    cargo publish
fi
```

## Performance

- **Target:** <10 seconds
- **Typical:** 5-8 seconds
- **Method:** Parallel execution

## Troubleshooting

### Too Slow?
```bash
./bin/validate-crate -t 5  # Reduce timeout
```

### Clippy Errors?
```bash
cargo clippy --fix --allow-dirty
```

### Test Failures?
```bash
cargo test --all -- --nocapture
```

## Documentation

- **Full Guide:** `docs/validation.md`
- **Examples:** `examples/validate_crate.sh`
- **Tests:** `tests/validator_test.rs`
- **Implementation:** `docs/validator-implementation-summary.md`

## Help

```bash
./bin/validate-crate --help
```

---

**Ready to publish?**

```bash
./bin/validate-crate && cargo publish --dry-run && cargo publish
```
