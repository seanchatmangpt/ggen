# Agent 10: Quick Fix Guide for Compilation Blockers

**For Agent 12 (Final Validation)**: Fix these 6 issues to unblock benchmarks

---

## Issue 1: Missing Module `ai`

**Error**:
```
error[E0583]: file not found for module `ai`
  --> cli/src/commands/mod.rs:35:1
   |
35 | pub mod ai;
```

**Fix Option A** (Remove declaration):
```rust
// cli/src/commands/mod.rs
// Line 35: REMOVE OR COMMENT OUT
// pub mod ai;
```

**Fix Option B** (Create stub):
```rust
// cli/src/commands/ai.rs
pub mod ai {
    // TODO: Implement AI commands
}
```

**Recommendation**: Option A (remove) - module not implemented yet

---

## Issue 2: Missing Module `project`

**Error**:
```
error[E0583]: file not found for module `project`
  --> cli/src/commands/mod.rs:37:1
   |
37 | pub mod project;
```

**Fix Option A** (Remove declaration):
```rust
// cli/src/commands/mod.rs
// Line 37: REMOVE OR COMMENT OUT
// pub mod project;
```

**Fix Option B** (Create stub):
```rust
// cli/src/commands/project.rs
pub mod project {
    // TODO: Implement project commands
}
```

**Recommendation**: Option A (remove) - module not implemented yet

---

## Issue 3: Missing Module `utils`

**Error**:
```
error[E0583]: file not found for module `utils`
  --> cli/src/commands/mod.rs:39:1
   |
39 | pub mod utils;
```

**Fix Option A** (Remove declaration):
```rust
// cli/src/commands/mod.rs
// Line 39: REMOVE OR COMMENT OUT
// pub mod utils;
```

**Fix Option B** (Create stub):
```rust
// cli/src/commands/utils.rs
pub mod utils {
    // TODO: Implement utility commands
}
```

**Recommendation**: Option A (remove) - module not implemented yet

---

## Issue 4: Marketplace Search Function Signature

**Error**:
```
error[E0061]: this function takes 3 arguments but 8 arguments were supplied
  --> cli/src/commands/marketplace/search.rs:62:9
```

**Current (Wrong)**:
```rust
// cli/src/commands/marketplace/search.rs:62
search_and_display(
    &args.query,
    args.category.as_deref(),    // Extra
    args.keyword.as_deref(),     // Extra
    args.author.as_deref(),      // Extra
    args.fuzzy,                  // Extra
    args.detailed,
    args.json,                   // Extra
    args.limit,                  // Extra
)
```

**Domain Function**:
```rust
// cli/src/domain/marketplace/search.rs:5
pub async fn search_and_display(_query: &str, _detailed: bool, _json: bool) -> Result<()>
```

**Fix Option A** (Update caller to match signature):
```rust
// cli/src/commands/marketplace/search.rs:62
search_and_display(
    &args.query,
    args.detailed,
    args.json,
)
```

**Fix Option B** (Stub implementation):
```rust
// cli/src/domain/marketplace/search.rs:5
pub async fn search_and_display(
    _query: &str,
    _category: Option<&str>,
    _keyword: Option<&str>,
    _author: Option<&str>,
    _fuzzy: bool,
    _detailed: bool,
    _json: bool,
    _limit: usize,
) -> Result<()> {
    // TODO: Implement marketplace search
    println!("Marketplace search not yet implemented");
    Ok(())
}
```

**Recommendation**: Option A (simpler, faster)

---

## Issue 5: Marketplace Publish Path Type

**Error**:
```
error[E0308]: mismatched types
  --> cli/src/commands/marketplace/publish.rs:43:13
   |
   | &args.path,
   | ^^^^^^^^^^ expected `&Path`, found `&String`
```

**Current (Wrong)**:
```rust
// cli/src/commands/marketplace/publish.rs:43
publish_and_report(
    &args.path,  // &String
    ...
)
```

**Domain Function**:
```rust
// cli/src/domain/marketplace/publish.rs:6
pub async fn publish_and_report(_path: &Path, ...) -> Result<()>
```

**Fix** (Convert String to Path):
```rust
// cli/src/commands/marketplace/publish.rs:43
use std::path::Path;

publish_and_report(
    Path::new(&args.path),  // Convert &String to &Path
    ...
)
```

**Alternative Fix** (Use as_ref):
```rust
publish_and_report(
    args.path.as_ref(),  // Coerce &String to &Path
    ...
)
```

**Recommendation**: Use `Path::new(&args.path)` (explicit and clear)

---

## Issue 6: Cargo.toml Dev-Dependency Configuration

**Error**:
```
error: dev-dependencies are not allowed to be optional: `clnrm`
```

**Current (Wrong)**:
```toml
# Cargo.toml
[dev-dependencies]
clnrm = { version = "...", optional = true }  # ❌ Can't be optional
```

**Fix**:
```toml
# Cargo.toml
[dev-dependencies]
clnrm = "..."  # Remove 'optional = true'
```

**Or** (if clnrm should be optional, move to dependencies):
```toml
[dependencies]
clnrm = { version = "...", optional = true }  # ✅ OK in [dependencies]

[dev-dependencies]
# Don't declare clnrm here
```

**Recommendation**: Remove `optional = true` from `[dev-dependencies]`

---

## Quick Fix Script

```bash
#!/bin/bash
# Quick fix for Agent 10 blockers

echo "Fixing CLI compilation errors..."

# Issue 1-3: Comment out missing modules
sed -i.bak 's/^pub mod ai;$/\/\/ pub mod ai;  \/\/ TODO: Implement/' cli/src/commands/mod.rs
sed -i.bak 's/^pub mod project;$/\/\/ pub mod project;  \/\/ TODO: Implement/' cli/src/commands/mod.rs
sed -i.bak 's/^pub mod utils;$/\/\/ pub mod utils;  \/\/ TODO: Implement/' cli/src/commands/mod.rs

# Issue 4: Fix marketplace search (manual edit required)
echo "⚠️  Manual edit required: cli/src/commands/marketplace/search.rs:62"
echo "    Change to: search_and_display(&args.query, args.detailed, args.json)"

# Issue 5: Fix marketplace publish (manual edit required)
echo "⚠️  Manual edit required: cli/src/commands/marketplace/publish.rs:43"
echo "    Change to: publish_and_report(Path::new(&args.path), ...)"

# Issue 6: Fix Cargo.toml (manual edit required)
echo "⚠️  Manual edit required: Cargo.toml [dev-dependencies]"
echo "    Remove 'optional = true' from clnrm dependency"

echo ""
echo "After manual fixes, run:"
echo "  cargo build --release"
echo "  ./.claude/refactor-v2/run-benchmarks.sh all"
```

---

## Verification Steps

After applying fixes:

### Step 1: Verify Compilation
```bash
cargo build --release --bin ggen
```

**Expected**: ✅ No errors

### Step 2: Verify Binary Works
```bash
target/release/ggen --version
target/release/ggen --help
target/release/ggen template --help
```

**Expected**: All commands succeed

### Step 3: Run Benchmarks
```bash
./.claude/refactor-v2/run-benchmarks.sh all
```

**Expected**: All benchmarks execute

### Step 4: Validate SLOs
```bash
./.claude/refactor-v2/run-benchmarks.sh validate
```

**Expected**: All 6 SLOs PASS

### Step 5: Save Baseline
```bash
./.claude/refactor-v2/run-benchmarks.sh save v2.0.0
```

**Expected**: Baseline saved to `target/criterion/`

---

## Time Estimate

- **Issues 1-3** (remove module declarations): 2 minutes
- **Issue 4** (fix marketplace search): 5 minutes
- **Issue 5** (fix marketplace publish): 3 minutes
- **Issue 6** (fix Cargo.toml): 1 minute
- **Verification** (build + test): 5 minutes
- **Run benchmarks**: 15 minutes
- **Total**: ~30 minutes

---

## Success Criteria

✅ **Compilation succeeds**:
```bash
cargo build --release --bin ggen
# No errors
```

✅ **Binary works**:
```bash
target/release/ggen --version
# ggen 1.2.0
```

✅ **Benchmarks run**:
```bash
cargo bench --bench v2_performance
# All benchmarks complete
```

✅ **SLOs validated**:
```bash
./.claude/refactor-v2/run-benchmarks.sh validate
# All 6 SLOs: PASS
```

---

**Quick Reference**: This is the fast-path to unblock performance validation. Agent 12 should complete these fixes before final validation.
