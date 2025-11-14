# Gap Detection Quick Start

**5-minute guide to automated test gap detection.**

## Installation (One-Time Setup)

```bash
# Install git hooks with gap detection
cargo make install-hooks

# Or manually
./scripts/install-git-hooks.sh
```

## Daily Usage

### Before Committing

Gap detection runs automatically:

```bash
# Stage changes
git add .

# Commit (hooks run automatically)
git commit -m "Add feature"
```

**If gaps detected**:
```
❌ Missing tests for ggen-core::templates
     Expected test file: crates/ggen-core/tests/unit/templates.rs
     Or inline tests in: crates/ggen-core/src/templates.rs
```

**Fix**:
```bash
# Create test file
touch crates/ggen-core/tests/unit/templates.rs

# Add test
echo '#[test]
fn test_basics() {
    assert!(true);
}' > crates/ggen-core/tests/unit/templates.rs

# Commit again
git commit -m "Add feature"
```

### Before Pushing

Comprehensive gap detection runs automatically:

```bash
# Push (hooks run automatically)
git push
```

### Manual Gap Detection

```bash
# Detect all gaps
cargo make detect-gaps

# View JSON report
cargo make gap-report

# Check coverage for staged files only
cargo make enforce-coverage
```

## Common Scenarios

### Adding New Module

1. **Create module**: `crates/my-crate/src/new_module.rs`
2. **Create test**: `crates/my-crate/tests/unit/new_module.rs`
3. **Add basic test**:
   ```rust
   use my_crate::new_module::*;

   #[test]
   fn test_new_module_basics() {
       assert!(true);
   }
   ```
4. **Commit**: Hooks verify coverage

### Changing Existing Code

1. **Modify code**: `crates/my-crate/src/existing.rs`
2. **Update tests**: Ensure test file exists
3. **Verify**: `cargo test --package my-crate`
4. **Commit**: Hooks verify coverage

### Public Function Without Tests

**Warning**:
```
⚠ Untested public function in ggen-domain: my_function (in my_module.rs)
```

**Fix**:
```rust
// In tests/unit/my_module.rs
#[test]
fn test_my_function() {
    let result = my_function(...);
    assert!(result.is_ok());
}
```

### Error Handling Coverage

**Warning**:
```
⚠ ggen-core has 25 Result types but only 3 error tests (expected at least 5)
```

**Fix**:
```rust
#[test]
fn test_error_case() {
    let result = function_that_fails();
    assert!(result.is_err());
}

#[test]
#[should_panic(expected = "error message")]
fn test_panic_case() {
    function_that_panics().unwrap();
}
```

## Bypassing (Emergency Only)

```bash
# Skip pre-commit (NOT RECOMMENDED)
git commit --no-verify

# Skip pre-push (NOT RECOMMENDED)
git push --no-verify
```

## CI/CD Integration

Gap detection runs automatically in CI:

```bash
# What CI runs
cargo make ci
# Includes: format, clippy, detect-gaps, test, audit, docs
```

## Thresholds (80/20 Focused)

- **Coverage**: 60% minimum (not 100%)
- **Tests per module**: 1 minimum
- **Untested functions**: ≤5 allowed
- **Error tests**: 1 per 5 Result types

## Performance

- **Pre-commit**: 2-5 seconds
- **Pre-push**: 30-60 seconds
- **Manual detection**: 30-45 seconds

## Troubleshooting

### Hooks Not Running

```bash
# Reinstall hooks
cargo make install-hooks

# Verify installation
ls -la .git/hooks/pre-commit
ls -la .git/hooks/pre-push
```

### False Positives

```bash
# Check if test file exists
ls crates/*/tests/unit/*.rs

# Check for inline tests
grep -r "#\[cfg(test)\]" crates/*/src/
```

### Slow Hook Performance

```bash
# Pre-commit should be <5s
# If slower, check what's staged:
git diff --cached --name-only

# Only staged Rust files are checked
# Large number of files = slower
```

## Integration with Existing Workflow

### With TDD (London School)

```bash
# 1. Write test first
echo '#[test]
fn test_new_feature() {
    assert_eq!(new_feature(), expected);
}' > tests/unit/new_feature.rs

# 2. Implement feature
# 3. Commit (hooks verify coverage automatically)
git add . && git commit -m "Add new feature"
```

### With Chicago TDD

```bash
# 1. Write property test
echo '#[test]
fn property_test_new_feature() {
    proptest!(|(input: String)| {
        let result = new_feature(&input);
        prop_assert!(result.is_ok());
    });
}' > tests/property/new_feature.rs

# 2. Implement feature
# 3. Commit (hooks verify coverage)
git add . && git commit -m "Add new feature"
```

## Best Practices

1. **Write tests first** - TDD prevents gaps
2. **Use inline tests** - Fast for small modules
3. **Use unit test files** - Better for complex modules
4. **Add integration tests** - Test public API
5. **Test error paths** - Not just happy paths
6. **Run manual detection** - Before big PRs

## Next Steps

1. ✅ Install hooks: `cargo make install-hooks`
2. ✅ Run detection: `cargo make detect-gaps`
3. ✅ View report: `cargo make gap-report`
4. ✅ Fix gaps: Follow error messages
5. ✅ Commit: Hooks prevent regressions
