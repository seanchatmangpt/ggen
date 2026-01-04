---
description: "Review error handling patterns in codebase. Checks for unwrap/expect violations in production, verifies Result<T,E> usage, and suggests improvements. Use for compliance audits."
allowed_tools: "Grep, Read, Glob, Bash(cargo make lint:*)"
argument_hint: "[crate-name] [strictness-level]"
---

# Error Handling Review Command

Audit error handling patterns against constitutional rules.

## Constitutional Rules

**Production Code**: NO `unwrap()` / `expect()` - Use `Result<T, E>`
**Test/Bench Code**: `unwrap()` / `expect()` ALLOWED (EXEMPT from review)

## Strictness Levels

- **strict**: Fail on any violation
- **moderate**: Report violations but show context
- **permissive**: Suggest improvements only (default)

## Review Checklist

### 1. Scan for Violations

```bash
grep -rn "\.unwrap()\|\.expect(" crates/$1/src --include="*.rs" | grep -v "tests\|benches\|#\[cfg(test)\]"
```

- Identify all production code violations
- Note line numbers and context
- Check for legitimate exemptions

### 2. Verify Error Types

Ensure all public APIs return `Result<T, E>`:

```rust
// ✅ CORRECT
pub fn parse(input: &str) -> Result<Config, Error> { }

// ❌ WRONG
pub fn parse(input: &str) -> Config { }  // Can panic!
```

### 3. Check Error Handling Pattern

```rust
// ✅ CORRECT: Production code
let cache = self.cache.lock()
    .map_err(|e| Error::new(&format!("Lock poisoned: {}", e)))?;

// ✅ CORRECT: Test code (EXEMPT)
#[test]
fn test_cache() {
    let cache = Cache::new().unwrap();  // Tests SHOULD panic
}
```

### 4. Error Types Used

Check usage of:
- `thiserror::Error` - Custom error types with derive
- `anyhow::Error` - Context-aware error propagation
- `Result<T, E>` - Standard result type

### 5. Lock Handling

Especially critical:
```rust
// ❌ WRONG: Mutex.lock().unwrap() can panic if poisoned
let data = self.data.lock().unwrap();

// ✅ CORRECT: Handle poisoning gracefully
let data = self.data.lock()
    .map_err(|e| Error::LockPoisoned(e.to_string()))?;
```

## Output Report

Generates audit report with:
- List of violations with locations
- Severity levels
- Suggested fixes
- Test exemptions verified
- Compliance score (% adherence)

## Compliance Requirements

✓ Zero unwrap() in production code
✓ Zero expect() in production code
✓ All public APIs return Result<T, E>
✓ Lock handling catches poisoning
✓ Error context provided in messages

## Success Criteria

✓ Compliance score = 100%
✓ All public APIs return Result<T, E>
✓ No panics on recoverable errors
✓ Audit report clean
