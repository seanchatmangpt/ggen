# ⚡ GGEN QUICK FIX CHECKLIST - CRITICAL ISSUES

**Status:** 🔴 **BUILD BROKEN** - Immediate action required
**Priority:** P0 blocker preventing all work

---

## 🚨 EMERGENCY FIX (DO THIS FIRST - 4 Hours)

### Step 1: Fix Build Failure (ggen-marketplace)
```bash
# Navigate to project root
cd /Users/sac/ggen

# See detailed errors
cargo build --package ggen-marketplace 2>&1 | tee build-errors.log

# Common fixes needed:
# 1. Add missing imports in tantivy_engine.rs
# 2. Fix type mismatches (Vec<Facet> vs types::Facet)
# 3. Remove unused variables (9 instances)
# 4. Fix struct field errors

# Verify fix
cargo build --workspace
cargo test --workspace --lib
```

**Files to Fix:**
- `ggen-marketplace/src/search/tantivy_engine.rs` (primary)
- `ggen-marketplace/src/backend/p2p.rs`
- `ggen-marketplace/src/storage/filesystem.rs`

**Success Criteria:**
- ✅ `cargo build --workspace` passes
- ✅ Zero compilation errors
- ✅ Tests can run

---

## 🔥 DAY 1 QUICK WINS (After Build is Fixed - 2 Hours)

### Step 2: Fix Compiler Warnings
```bash
# Apply suggested fixes
cargo clippy --fix --allow-dirty --allow-staged
cargo build --workspace
```

**Expected Fixes:**
- Remove unused variables: `searcher` → `_searcher`
- Remove unnecessary `mut` (3 instances)

---

### Step 3: Update Dependencies
```bash
# Update to latest compatible versions
cargo update

# Check for outdated packages
cargo outdated --workspace

# Run security audit
cargo audit

# Test after updates
cargo test --workspace
```

---

## 📊 DAY 2-3 HIGH-VALUE FIXES (16 Hours Total)

### Step 4: Consolidate Documentation (Day 2 - 8h)
```bash
# Current: 503 files
# Target: ≤100 files (80% reduction)

# Audit docs
find docs -name "*.md" | wc -l

# Create consolidated structure
mkdir -p docs/archive
mkdir -p docs/guides
mkdir -p docs/api

# Move old/internal docs to archive
# Keep only essential docs in main docs/

# Update README.md with clear navigation
# Fix internal doc links
```

**Keep These Docs (20% that matter):**
- README.md
- getting-started.md
- cli-reference.md
- architecture.md
- guides/ (marketplace, lifecycle, templates, ai)
- api/ (reference docs)

---

### Step 5: Consolidate Examples (Day 3 - 8h)
```bash
# Current: 31 examples
# Target: 6-8 core examples

# Test all examples
for dir in examples/*/; do
  (cd "$dir" && cargo build 2>&1 | tee test.log)
done

# Keep top 6-8 examples:
# 1. hello-world (5 min)
# 2. rust-cli (10 min)
# 3. microservices (15 min)
# 4. ai-generation (10 min)
# 5. full-stack (20 min)
# 6. advanced-rust (15 min)

# Move others to examples/archive/
mkdir -p examples/archive
# Move less-used examples

# Update examples/README.md with decision tree
```

---

## 📋 WEEK 2 POLISH (10 Hours)

### Step 6: Fix Deprecated Code
```bash
# Review 4 files with deprecation markers
grep -rn "deprecated\|DEPRECATED" ggen-core/src/ ggen-ai/src/

# Files to review:
# - ggen-core/src/cleanroom/forensics.rs
# - ggen-core/src/poc.rs (likely can be removed)
# - ggen-core/src/graph.rs
# - ggen-ai/src/streaming.rs

# Update or remove deprecated code
```

---

### Step 7: Fix Documentation Links
```bash
# Install link checker
npm install -g markdown-link-check

# Check all docs
find docs -name "*.md" -exec markdown-link-check {} \;

# Fix broken links
# Update internal references after doc consolidation
# Remove/replace dead external links
```

---

## ✅ VERIFICATION CHECKLIST

After each step, verify:

**Build Health:**
- [ ] `cargo build --workspace` → PASS
- [ ] `cargo test --workspace --lib` → PASS
- [ ] `cargo clippy` → Zero warnings
- [ ] `cargo audit` → Zero vulnerabilities

**Documentation:**
- [ ] README.md is up-to-date
- [ ] All internal links work
- [ ] ≤100 active documentation files
- [ ] Clear navigation structure

**Examples:**
- [ ] ≤8 core examples
- [ ] All core examples build successfully
- [ ] examples/README.md guides users

**Code Quality:**
- [ ] Zero compiler warnings
- [ ] No deprecated code in critical paths
- [ ] Dependencies updated
- [ ] Tests passing

---

## 🎯 SUCCESS METRICS

**Before (Current):**
- ❌ Build: FAILING
- ❌ Tests: CANNOT RUN
- ⚠️ Docs: 503 files
- ⚠️ Examples: 31 projects
- ⚠️ Warnings: 9 instances

**After (48 Hours Target):**
- ✅ Build: PASSING
- ✅ Tests: RUNNING
- ✅ Docs: ≤100 files
- ✅ Examples: ≤8 core
- ✅ Warnings: 0

**After (1 Week Target):**
- ✅ All P0, P1, P2 issues resolved
- ✅ Production-ready for v1.0 release
- ✅ Clean codebase
- ✅ Clear documentation

---

## 📞 ESCALATION

**If build fix takes > 6 hours:**
- Review compiler errors in detail
- Consider reverting recent commits
- Check git history for last working build
- Consult with original implementer

**If stuck:**
- Review detailed error log: `cargo build 2>&1 | tee errors.log`
- Search for similar issues: `cargo search tantivy`
- Check dependencies: `cargo tree --package ggen-marketplace`

---

## 📊 TIME ESTIMATES

| Task | Priority | Effort | Dependencies |
|------|----------|--------|--------------|
| Fix build | P0 | 4h | None - START HERE |
| Fix warnings | P0 | 1h | Build working |
| Update deps | P1 | 4h | Build working |
| Consolidate docs | P1 | 8h | Build working |
| Consolidate examples | P1 | 8h | Build working |
| Fix deprecated | P2 | 3h | Build working |
| Fix doc links | P2 | 4h | Docs consolidated |

**Total:** ~32 hours to resolve all critical issues

**Critical Path:** Build fix → Warnings → Tests → Docs/Examples

---

**Created:** 2025-10-30T04:36:00Z
**See Full Report:** docs/testing/CRITICAL_ISSUES_REPORT.md
