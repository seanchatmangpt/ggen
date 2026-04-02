# Audit Trail Re-enablement Analysis

**Date:** 2026-03-31
**Status:** BLOCKED - Dependency Conflict Resolution Required

## Current State

The audit trail functionality in `ggen-ai/src/governance/mod.rs` is disabled due to a SQLite dependency conflict that was noted in the TODO comments:

```rust
// pub mod audit;  // TODO: Re-enable when SQLite dependency conflict is resolved
// audit_trail: Arc<AuditTrail>,  // TODO: Re-enable when SQLite dependency conflict is resolved
```

## Investigation Findings

### 1. Audit Module Exists and is Complete

**File:** `/Users/sac/ggen/crates/ggen-ai/src/governance/audit.rs`
- **Status:** ✅ Fully implemented (451 lines)
- **Dependencies:** Uses `rusqlite` for SQLite database operations
- **Features:**
  - Comprehensive event logging (18 event types)
  - SQLite persistence with indexed queries
  - Retention policy cleanup
  - Full test coverage with `tempfile` for real I/O

### 2. SQLite Dependency Analysis

**Current Situation:**
- ✅ **`sqlx` v0.8** is available in workspace dependencies (used by `ggen-api`)
- ❌ **`rusqlite`** is NOT in workspace dependencies
- ❌ **No `rusqlite` dependency** in `ggen-ai/Cargo.toml`

**Comment in ggen-ai/Cargo.toml (line 34-35):**
```toml
# Note: SQLite database access moved to optional feature due to conflicts with sqlx v0.7
# ggen-ai focuses on LLM integration; database operations belong in domain/api layers
```

### 3. Dependency Conflict Analysis

**Potential Conflict:** `rusqlite` vs `sqlx`
- Both are SQLite libraries for Rust
- `sqlx` is async-first (used by `ggen-api` for async database operations)
- `rusqlite` is synchronous (used by `audit.rs` for simple embedded database)

**Why This Might Be a Conflict:**
1. **Linking conflicts:** Both libraries link to SQLite C library
2. **Version mismatches:** Different SQLite versions may cause symbol conflicts
3. **Runtime conflicts:** Two different SQLite implementations in same binary

### 4. Alternative Approaches

#### Option A: Use `sqlx` instead of `rusqlite` (RECOMMENDED)
**Pros:**
- ✅ Already in workspace (v0.8)
- ✅ Async/await compatible (matches project architecture)
- ✅ Consistent with `ggen-api` database layer
- ✅ Better type safety with compile-time query checking

**Cons:**
- ❌ Requires rewriting `audit.rs` to use async `sqlx` API
- ❌ More complex API than `rusqlite`

**Implementation:**
```toml
# In ggen-ai/Cargo.toml
[dependencies]
sqlx = { workspace = true } # Already available!
```

```rust
// Rewrite audit.rs to use sqlx:
use sqlx::{SqlitePool, sqlite::SqliteConnectOptions};

pub struct AuditTrail {
    pool: SqlitePool,
    // ...
}
```

#### Option B: Add `rusqlite` as dependency (RISKY)
**Pros:**
- ✅ Minimal code changes (audit.rs already written)
- ✅ Simple synchronous API

**Cons:**
- ❌ Potential runtime conflicts with `sqlx`
- ❌ Synchronous operations block async runtime
- ❌ Adds dependency bloat

#### Option C: Move audit trail to `ggen-domain` or `ggen-api` (ARCHITECTURALLY CORRECT)
**Pros:**
- ✅ Separates concerns (ggen-ai focuses on LLM integration)
- ✅ Database operations live in domain layer
- ✅ Can use `sqlx` consistently across domain layer

**Cons:**
- ❌ Requires moving code between crates
- ❌ Updates to cross-crate dependencies

## Recommended Solution

### Phase 1: Quick Fix - Use `sqlx` in `ggen-ai`

1. **Add `sqlx` dependency to `ggen-ai/Cargo.toml`:**
```toml
[dependencies]
sqlx = { workspace = true }
```

2. **Rewrite `audit.rs` to use async `sqlx`:**
   - Replace `rusqlite::Connection` with `sqlx::SqlitePool`
   - Convert sync operations to async
   - Use `sqlx::query!` macros for compile-time safety

3. **Update `governance/mod.rs`:**
   - Uncomment `pub mod audit;`
   - Uncomment `audit_trail: Arc<AuditTrail>`
   - Remove type aliases `AuditEvent = ()`, etc.

### Phase 2: Architectural Cleanup - Move to Domain Layer

1. **Move `audit.rs` to `ggen-domain/src/governance/audit.rs`**
2. **Update imports in `ggen-ai` to use `ggen-domain::governance::AuditTrail`**
3. **Remove database operations from `ggen-ai` entirely**

## Blocker Summary

**Current Blocker:** The `audit.rs` module uses `rusqlite` which is not in dependencies, and adding it may conflict with existing `sqlx` v0.8 dependency.

**Resolution Path:**
1. ✅ **Immediate:** Rewrite `audit.rs` to use `sqlx` (async, consistent)
2. ✅ **Long-term:** Move audit functionality to `ggen-domain` layer

## Next Steps

1. **Decision required:** Choose Option A (sqlx rewrite) or Option C (move to domain)
2. **Implementation:** Update dependencies and rewrite audit.rs
3. **Testing:** Verify compilation with `cargo make check -p ggen-ai`
4. **Validation:** Run governance tests to ensure audit trail works

## Files Requiring Changes

1. `/Users/sac/ggen/crates/ggen-ai/Cargo.toml` - Add `sqlx` dependency
2. `/Users/sac/ggen/crates/ggen-ai/src/governance/audit.rs` - Rewrite to use `sqlx`
3. `/Users/sac/ggen/crates/ggen-ai/src/governance/mod.rs` - Re-enable audit module

---

**Status:** Awaiting decision on resolution approach.
**Estimated Effort:** 2-4 hours for Option A (sqlx rewrite), 4-6 hours for Option C (move to domain).
