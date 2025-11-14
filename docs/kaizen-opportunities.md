# Kaizen Opportunities Identified

## Step 1: Identify Improvement Opportunity ✅

### Opportunity 1: Extract Magic Numbers in Cache Sizes (graph.rs)

**Location**: `crates/ggen-core/src/graph.rs:293-295`

**Current Code**:
```rust
let plan_cache_size =
    NonZeroUsize::new(100).ok_or_else(|| anyhow::anyhow!("Invalid cache size"))?;
let result_cache_size =
    NonZeroUsize::new(1000).ok_or_else(|| anyhow::anyhow!("Invalid cache size"))?;
```

**Issue**: Magic numbers `100` and `1000` are not self-documenting
**Value**: Improves readability, maintainability, self-documentation
**Risk**: Low - simple refactoring, no logic change

### Opportunity 2: Extract Magic Number in Backoff Timing (registry.rs)

**Location**: `crates/ggen-core/src/registry.rs:306`

**Current Code**:
```rust
let backoff_ms = 100 * 2u64.pow(attempt - 1); // 100ms, 200ms, 400ms
```

**Issue**: Magic number `100` (base backoff milliseconds) is not self-documenting
**Value**: Improves readability, makes it easier to adjust backoff timing
**Risk**: Low - simple refactoring, no logic change

## Step 2: Plan Change

### Improvement Plan

**What**: Extract magic numbers to named constants
**Why**: 
- Makes code more readable
- Easier to change configuration values
- Self-documenting code
- Follows Rust best practices

**How**:
1. Add constants at module level:
   - `const DEFAULT_PLAN_CACHE_SIZE: usize = 100;`
   - `const DEFAULT_RESULT_CACHE_SIZE: usize = 1000;`
   - `const BASE_BACKOFF_MS: u64 = 100;`
2. Replace magic numbers with constants
3. Verify compilation and tests

**Risk**: Low - simple refactoring, no logic change

### Safety Checks

- ✅ No logic changes (pure refactoring)
- ✅ Tests exist for affected code
- ✅ Change is isolated (doesn't affect other code)
- ✅ Can be easily reverted if needed

