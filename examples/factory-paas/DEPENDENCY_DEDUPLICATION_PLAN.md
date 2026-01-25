# Dependency Deduplication Plan - Phase 1 (CRITICAL)

**Priority**: 🔴 CRITICAL
**Estimated Effort**: 3-5 days
**Goal**: Reduce 160 duplicates to <20

---

## Critical Duplicates to Fix

### Top Priority (Fix First)

These have the most transitive dependencies:

#### 1. axum (3 versions → 1)
```toml
# In workspace Cargo.toml [workspace.dependencies]
axum = "0.8"
axum-core = "0.5"

# Update all crates to use workspace version
# Files to modify:
# - crates/ggen-marketplace-v2/Cargo.toml (uses 0.7)
# - Check which crate uses 0.6 (likely via opentelemetry-otlp)
```

**Impact**: ~50-80 transitive dependencies reduced

#### 2. tonic (2 versions → 1)
```toml
# In workspace Cargo.toml [workspace.dependencies]
tonic = "0.14"

# Problem: opentelemetry-otlp v0.14 depends on tonic v0.9
# Solution: Check if newer opentelemetry-otlp exists
# OR: Feature-gate OTEL to remove from default build
```

**Impact**: ~30-50 transitive dependencies reduced

#### 3. dashmap (2 versions → 1)
```toml
# In workspace Cargo.toml [workspace.dependencies]
dashmap = "6.1"

# Update all crates using dashmap 5.5
```

**Impact**: Direct dependency, minimal transitive impact

#### 4. derive_more (3 versions → 1)
```toml
# In workspace Cargo.toml [workspace.dependencies]
derive_more = "1.0"

# Drop v0.99 and v2.1
# Update proc-macro usage
```

**Impact**: Proc-macro compilation time reduction

#### 5. darling (2 versions → 1)
```toml
# In workspace Cargo.toml [workspace.dependencies]
darling = "0.21"
darling_core = "0.21"
darling_macro = "0.21"

# Update from v0.20
```

**Impact**: Proc-macro compilation time reduction

---

## Implementation Steps

### Step 1: Add Workspace Version Pins

**File**: `/home/user/ggen/Cargo.toml`

**Location**: After line 148 (`base64 = "0.22"`)

```toml
# Add to [workspace.dependencies] section:

# Deduplicated versions (Phase 1 - Critical)
axum = "0.8"
axum-core = "0.5"
tonic = "0.14"
dashmap = "6.1"
derive_more = "1.0"
darling = "0.21"
darling_core = "0.21"
darling_macro = "0.21"
bitflags = "2.10"
config = "0.15"
convert_case = "0.10"
```

### Step 2: Update Crate Dependencies

**Find affected crates**:
```bash
# Find which crates use old versions
grep -r "axum = " crates/*/Cargo.toml
grep -r "tonic = " crates/*/Cargo.toml
grep -r "dashmap = " crates/*/Cargo.toml
grep -r "derive_more = " crates/*/Cargo.toml
```

**Update pattern**:
```toml
# Before
[dependencies]
axum = "0.7"

# After
[dependencies]
axum = { workspace = true }
```

### Step 3: Handle OTEL Conflict

**Problem**: `opentelemetry-otlp v0.14` depends on `tonic v0.9`

**Option A**: Update OTEL (if newer version exists)
```bash
# Check for newer versions
cargo search opentelemetry-otlp
```

**Option B**: Feature-gate OTEL (recommended)
```toml
# In workspace Cargo.toml
[features]
default = []
otel = [
  "opentelemetry",
  "opentelemetry-otlp",
  "opentelemetry_sdk",
  "tracing-opentelemetry"
]
```

Then update crates to only enable with feature:
```toml
[dependencies]
opentelemetry = { workspace = true, optional = true }
opentelemetry-otlp = { workspace = true, optional = true }
```

### Step 4: Verify Deduplication

```bash
# Check duplicates after changes
cargo tree --duplicates | grep -E "^[a-z]" | wc -l

# Target: <20 (from 160)
```

### Step 5: Test Build

```bash
# Clean build
cargo clean

# Build with timing
export RUSTC_WRAPPER=sccache
time cargo build --lib

# Should be faster than >600s
```

---

## Expected Results

### Metrics Before/After

| Metric | Before | Target After | Success Criteria |
|--------|--------|--------------|------------------|
| Duplicates | 160 | <20 | ✅ Met |
| Total deps | 1,011 | ~800 | ✅ 20% reduction |
| Cargo.lock | 11,454 lines | ~9,000 | ✅ 21% reduction |
| Build time | >600s | <400s | ✅ 33% improvement |

### Files Modified (Estimated)

- `Cargo.toml` (workspace root): +10 lines
- ~15-20 crate Cargo.toml files: Update dependencies to `{ workspace = true }`
- Total changes: ~30-50 lines across 20 files

---

## Risks & Mitigation

### Risk 1: Breaking Changes in Version Upgrades

**Example**: axum 0.6 → 0.8 may have API changes

**Mitigation**:
1. Read CHANGELOG for each major version bump
2. Fix compilation errors incrementally
3. Run full test suite after each upgrade
4. Use compiler errors as guide

### Risk 2: OTEL Dependency Conflicts

**Example**: opentelemetry-otlp may not support tonic 0.14

**Mitigation**:
1. Check compatibility first: `cargo tree -p opentelemetry-otlp`
2. If incompatible, feature-gate OTEL instead of upgrading
3. Document OTEL as optional feature

### Risk 3: Transitive Dependency Conflicts

**Example**: Forcing axum 0.8 may conflict with other deps

**Mitigation**:
1. Run `cargo check` frequently during changes
2. If conflicts arise, use `cargo tree -i <package>` to identify source
3. May need to update multiple crates together

---

## Validation Checklist

Before marking Phase 1 complete:

- [ ] Workspace Cargo.toml updated with version pins
- [ ] All affected crate Cargo.tomls updated to use `workspace = true`
- [ ] `cargo check` passes cleanly
- [ ] `cargo tree --duplicates | wc -l` shows <20
- [ ] Build time measured and <400s
- [ ] Full test suite passes: `cargo test`
- [ ] Changes committed and pushed

---

## Detailed Duplicate List

### All 160 Duplicates (for reference)

**Critical (transitive impact)**:
- axum (3 versions)
- axum-core (3 versions)
- tonic (2 versions)
- derive_more (3 versions)
- darling ecosystem (2 versions each)

**Moderate**:
- dashmap (2 versions)
- bitflags (3 versions)
- config (2 versions)
- convert_case (2 versions)
- base64 (2 versions) - already noted

**Low priority** (small crates, minimal impact):
- block-buffer (2 versions)
- crypto-common (2 versions)
- ...and ~140 more minor duplicates

**Strategy**: Fix critical first (top 10), then iterate through moderate, then low priority.

---

## Automation Opportunities

### Script to Find Duplicate Usage

```bash
#!/bin/bash
# find_duplicate_usage.sh

# Find which crates use old versions of duplicated dependencies
for dep in axum tonic dashmap derive_more darling; do
  echo "=== $dep ==="
  grep -rn "$dep = " crates/*/Cargo.toml
  echo ""
done
```

### Script to Update to Workspace Versions

```bash
#!/bin/bash
# update_to_workspace.sh

# Automatically update dependencies to use workspace versions
# (Manual review recommended)

for dep in axum tonic dashmap derive_more darling; do
  find crates -name "Cargo.toml" -exec sed -i "s/$dep = \"[^\"]*\"/$dep = { workspace = true }/g" {} \;
done
```

**Warning**: Review changes carefully before committing!

---

## Next Steps After Phase 1

Once deduplication is complete:

1. **Phase 2**: cargo +nightly udeps (remove unused)
2. **Phase 3**: Feature-gate OTEL
3. **Phase 4**: Measure final performance
4. **Phase 5**: Update documentation

---

## References

- [PERFORMANCE_ANALYSIS.md](PERFORMANCE_ANALYSIS.md) - Full analysis
- [cargo tree --duplicates](https://doc.rust-lang.org/cargo/commands/cargo-tree.html#cargo-tree---duplicates)
- [Workspace dependencies](https://doc.rust-lang.org/cargo/reference/workspaces.html#the-dependencies-table)

---

**Last Updated**: 2026-01-24
**Status**: Ready to begin implementation
**Priority**: Start immediately - this is the critical path to build performance
