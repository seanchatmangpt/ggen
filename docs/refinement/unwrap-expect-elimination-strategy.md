# Unwrap/Expect Elimination Strategy - SPARC Refinement Phase

## Executive Summary

**Current State:**
- **Production Code**: ~170 unwrap/expect occurrences across ggen-core/src
- **Test Code**: ~57 unwrap/expect occurrences (ACCEPTABLE - tests can panic)
- **Critical Files**: resolver.rs (44), register.rs (61), lockfile.rs (25), graph.rs (21)

**Target State:**
- **Zero unwrap/expect in production code**
- **100% Result<T, E> error handling**
- **Comprehensive error context**
- **Performance-optimized error paths**

## Analysis Results

### Categorization by Type

#### 1. Test-Only Unwrap/Expect (57 occurrences) - LOW PRIORITY
**Location**: `tests/`, `benches/`, module test blocks
**Status**: ACCEPTABLE - Test code can panic to indicate failure
**Action**: Document pattern, no changes required

**Examples:**
```rust
// tests/cli.rs
let mut cmd = Command::cargo_bin("ggen").expect("Calling binary failed");
let temp_dir = TempDir::new().unwrap();

// ggen-core/tests/lifecycle_bdd.rs
executor.expect("init", Ok(CommandOutput::default()));
state_repo.save(&state).unwrap();
```

**Rationale**: Test failures should panic immediately for clarity. This is idiomatic Rust testing.

#### 2. Production Code Unwrap/Expect (170 occurrences) - HIGH PRIORITY

##### 2A. Chain/Iterator Operations (44 occurrences in resolver.rs)
**Pattern**: `.unwrap()` on `Option` from iterators
**Risk**: Panics on invalid data
**Solution**: Use `?`, `ok_or`, or proper error handling

**Before:**
```rust
let first = parts.first().unwrap();
let value = map.get("key").unwrap();
```

**After:**
```rust
let first = parts.first()
    .ok_or_else(|| anyhow::anyhow!("Empty parts vector"))?;
let value = map.get("key")
    .ok_or_else(|| anyhow::anyhow!("Missing key: {}", key))?;
```

##### 2B. Tera Template Rendering (61 occurrences in register.rs)
**Pattern**: `.unwrap()` on template rendering in tests
**Risk**: Low - all in test code
**Solution**: Already acceptable, but can improve for consistency

**Current:**
```rust
let result = tera.render_str("{{ name | camel }}", &ctx).unwrap();
```

**Better (if needed):**
```rust
let result = tera.render_str("{{ name | camel }}", &ctx)
    .expect("Template rendering failed - invalid syntax");
```

##### 2C. Lockfile/Cache Operations (25 occurrences in lockfile.rs)
**Pattern**: Nested `.unwrap_or_else(|| self.create().unwrap())`
**Risk**: HIGH - Can panic on I/O errors
**Solution**: Flatten error handling

**Before:**
```rust
let mut lockfile = self.load()?.unwrap_or_else(|| self.create().unwrap());
```

**After:**
```rust
let mut lockfile = match self.load()? {
    Some(lf) => lf,
    None => self.create()?,
};
```

##### 2D. Graph Operations (21 occurrences in graph.rs)
**Pattern**: `.unwrap()` on SPARQL query results
**Risk**: MEDIUM - Can panic on malformed queries
**Solution**: Proper error propagation

##### 2E. Serialization (6 occurrences in gpack.rs)
**Pattern**: `.unwrap()` on `serde_json::to_string`
**Risk**: LOW - serialization of known types rarely fails
**Solution**: Use `.expect()` with descriptive messages or proper error handling

**Before:**
```rust
let json = serde_json::to_string(&index).unwrap();
```

**After:**
```rust
let json = serde_json::to_string(&index)
    .context("Failed to serialize registry index")?;
```

##### 2F. Regex Compilation (3 occurrences in inject.rs)
**Pattern**: `.unwrap()` on `Regex::new()`
**Risk**: MEDIUM - Can panic on invalid patterns
**Solution**: Compile at initialization or handle errors

**Before:**
```rust
let regex = regex::Regex::new(&pattern).unwrap();
```

**After:**
```rust
let regex = regex::Regex::new(&pattern)
    .with_context(|| format!("Invalid regex pattern: {}", pattern))?;
```

##### 2G. Cryptographic Operations (7 occurrences in pqc.rs)
**Pattern**: `.unwrap()` on signature verification
**Risk**: HIGH - Security-critical code must handle errors
**Solution**: Comprehensive error handling with security context

**Before:**
```rust
let signature = SignedMessage::from_bytes(signature).unwrap();
let verified = verifier.verify(message, &signature).unwrap();
```

**After:**
```rust
let signature = SignedMessage::from_bytes(signature)
    .context("Invalid signature format")?;
let verified = verifier.verify(message, &signature)
    .context("Signature verification failed")?;
```

## Refactoring Strategy

### Phase 1: Critical Security & I/O (Priority: CRITICAL)
**Files**: `pqc.rs`, `lockfile.rs`, `cache.rs`
**Timeline**: Immediate
**Impact**: Prevents panics in critical operations

1. Replace all unwrap/expect in cryptographic operations
2. Fix lockfile create/load patterns
3. Add proper error context for I/O failures

### Phase 2: Data Processing (Priority: HIGH)
**Files**: `resolver.rs`, `graph.rs`, `inject.rs`
**Timeline**: Next iteration
**Impact**: Better error messages, no panics on bad input

1. Replace iterator unwrap() with ok_or/ok_or_else
2. Add validation error types
3. Implement regex compilation errors

### Phase 3: Serialization & Templates (Priority: MEDIUM)
**Files**: `gpack.rs`, `register.rs`, `merge.rs`
**Timeline**: Following iteration
**Impact**: Improved error diagnostics

1. Replace serde unwrap with proper error context
2. Template rendering error handling (if moving from test code)

### Phase 4: Metadata & RDF (Priority: LOW)
**Files**: `rdf/*.rs`, `project_generator/*.rs`
**Timeline**: Final cleanup
**Impact**: Completeness, edge case handling

## Implementation Patterns

### Pattern 1: Option → Result
```rust
// Before
let value = option.unwrap();

// After
let value = option
    .ok_or_else(|| anyhow::anyhow!("Expected value missing"))?;
```

### Pattern 2: Nested Unwrap → Match
```rust
// Before
let x = a.unwrap_or_else(|| b.unwrap());

// After
let x = match a {
    Some(val) => val,
    None => b?,
};
```

### Pattern 3: Chained Errors
```rust
// Before
let data = serde_json::from_str(&content).unwrap();

// After
let data = serde_json::from_str(&content)
    .with_context(|| format!("Failed to parse JSON from {}", path.display()))?;
```

### Pattern 4: Error Conversion
```rust
// Before
let regex = Regex::new(&pattern).unwrap();

// After
let regex = Regex::new(&pattern)
    .map_err(|e| Error::InvalidPattern {
        pattern: pattern.to_string(),
        source: e,
    })?;
```

### Pattern 5: Lazy Static Regex
```rust
// Before (in function)
let regex = Regex::new(r"pattern").unwrap();

// After (at module level)
lazy_static! {
    static ref PATTERN_REGEX: Regex =
        Regex::new(r"pattern").expect("Hardcoded regex is valid");
}
```

## Custom Error Types

### Core Domain Errors
```rust
#[derive(Debug, thiserror::Error)]
pub enum GgenError {
    #[error("Template not found: {pack_id}:{template_path}")]
    TemplateNotFound {
        pack_id: String,
        template_path: String,
    },

    #[error("Invalid template reference: {reference}")]
    InvalidTemplateRef { reference: String },

    #[error("Pack not found: {pack_id}")]
    PackNotFound { pack_id: String },

    #[error("Lockfile operation failed: {operation}")]
    LockfileError {
        operation: String,
        #[source]
        source: anyhow::Error,
    },

    #[error("Signature verification failed")]
    SignatureVerificationFailed {
        #[source]
        source: Box<dyn std::error::Error + Send + Sync>,
    },

    #[error("Invalid regex pattern: {pattern}")]
    InvalidPattern {
        pattern: String,
        #[source]
        source: regex::Error,
    },

    #[error("Graph query failed: {query}")]
    GraphQueryError {
        query: String,
        #[source]
        source: anyhow::Error,
    },
}
```

## Performance Considerations

### 1. Error Allocation
- Use `anyhow::Error` for most cases (single allocation)
- Reserve custom error types for typed error handling
- Avoid unnecessary string allocations in hot paths

### 2. Error Context
- Use `with_context` for lazy evaluation
- Prefer closures over immediate format! in context

```rust
// Good - lazy evaluation
.with_context(|| format!("Failed to process {}", path.display()))?;

// Avoid - eager evaluation
.context(format!("Failed to process {}", path.display()))?;
```

### 3. Happy Path Optimization
- Keep error paths cold (unlikely branch prediction)
- Use `#[cold]` annotation on error construction helpers
- Minimize error type size for better cache efficiency

## Validation Criteria

### Zero Warnings Checklist
- [ ] No clippy::unwrap_used warnings
- [ ] No clippy::expect_used warnings (except in tests)
- [ ] All Result types properly propagated
- [ ] Error context added to all I/O operations
- [ ] Security-critical paths have comprehensive error handling

### Test Coverage
- [ ] Error paths covered by tests
- [ ] Error messages validated
- [ ] Error context preserved through call stack
- [ ] Panic-free under invalid input

### Documentation
- [ ] Error handling patterns documented
- [ ] Custom error types documented with examples
- [ ] Migration guide for common patterns
- [ ] Best practices guide updated

## Rollout Plan

### Step 1: Enable Clippy Lints (Week 1)
```toml
[workspace.lints.clippy]
unwrap_used = "deny"
expect_used = "deny"
```

### Step 2: Fix Critical Files (Week 1-2)
1. `pqc.rs` - Security critical
2. `lockfile.rs` - I/O critical
3. `resolver.rs` - Core functionality

### Step 3: Fix High Priority (Week 2-3)
1. `graph.rs` - Data processing
2. `inject.rs` - Regex handling
3. `cache.rs` - I/O operations

### Step 4: Complete Remaining (Week 3-4)
1. Serialization files
2. RDF operations
3. Project generators

### Step 5: Validation (Week 4)
1. Run full test suite
2. Benchmark error path performance
3. Verify zero warnings
4. Security audit of error handling

## Success Metrics

- **Zero** unwrap/expect in production code
- **100%** Result<T> error propagation
- **Zero** warnings from clippy lints
- **< 5%** performance overhead from error handling
- **100%** test coverage of error paths
- **Zero** panics in production workloads

## References

- [Rust Error Handling Best Practices](https://doc.rust-lang.org/book/ch09-00-error-handling.html)
- [anyhow Documentation](https://docs.rs/anyhow/latest/anyhow/)
- [thiserror Documentation](https://docs.rs/thiserror/latest/thiserror/)
- [Clippy Lints - unwrap_used](https://rust-lang.github.io/rust-clippy/master/index.html#unwrap_used)

---

**Status**: Ready for implementation
**Owner**: SPARC Refinement Agent
**Next Action**: Begin Phase 1 refactoring
