# The Vital Few: 80/20 Improvements for v5.2.0

## Gemba Walk Results: v5.1.0 Analysis

**Status**: Production Ready ✅
**Gap Found**: Integration completeness - features implemented but integration may be partial
**Opportunity**: 20% effort = 80% user impact

---

## TOP 3 CRITICAL BLOCKERS (Do These First)

### 1. Template Rendering Integration
**Impact**: CRITICAL - No code generated without this
**Current**: Pipeline structure exists but template rendering unclear
**Action**: Verify that execute_full_sync() actually calls template rendering
**Test**: `ggen sync` produces .rs files with actual code
**Effort**: 4-6 hours
**Why First**: Blocks all code generation

### 2. Watch Mode Integration
**Impact**: HIGH - Key development workflow
**Current**: FileWatcher implementation complete but executor integration unclear
**Action**: Verify execute_watch_mode() properly loops and triggers regeneration
**Test**: Change ontology file → automatic regeneration
**Effort**: 2-4 hours
**Why Second**: Enables development loop

### 3. Merge Mode Wiring
**Impact**: HIGH - Hybrid manual/generated code
**Current**: merge.rs implemented but executor may not call it
**Action**: Confirm execute_full_sync() calls merge_sections()
**Test**: `ggen sync --merge` produces properly marked sections
**Effort**: 1-2 hours
**Why Third**: Enables human+machine collaboration

---

## NEXT 2 HIGH PRIORITY ITEMS

### 4. Audit Trail Recording
**Impact**: HIGH - Safety and compliance
**Check**: Does executor record audit details?
**Effort**: 1-2 hours

### 5. Conditional Execution (SPARQL ASK)
**Impact**: MEDIUM - Advanced filtering
**Check**: Where do SPARQL ASK conditions get evaluated?
**Effort**: 3-4 hours

---

## FINAL 5 MEDIUM PRIORITY ITEMS

6. Multi-flag integration test (2-3 hrs)
7. CLI documentation enhancements (0.5 hrs)
8. Watch mode real FS test (2-3 hrs)
9. Audit trail recovery documentation (2-3 hrs)
10. Feature completeness matrix (1 hr)

---

## Total Investment for v5.2.0

| Phase | Items | Hours | Days |
|-------|-------|-------|------|
| **CRITICAL** | 1-3 | 7-12 | 1-1.5 |
| **HIGH** | 4-5 | 4-6 | 0.5-1 |
| **MEDIUM** | 6-10 | 8-11 | 1-1.5 |
| **TOTAL** | 10 | 19-29 | 2-3 |

---

## Verification Test

```bash
# Run this to verify each feature tier is working

# Tier 1: Generation (CRITICAL)
ggen sync  # Should produce files

# Tier 2: Modes (HIGH)
ggen sync --dry-run --audit  # Preview + track
ggen sync --force --audit     # Overwrite + audit
ggen sync --merge --audit     # Hybrid + track
ggen sync --watch --timeout 1000  # Live monitoring

# Tier 3: Advanced (MEDIUM)
ggen sync --validate-only  # Check without generate
ggen sync --condition "ASK { ?x a :Type }"  # Filter rules
ggen sync --rule my_rule   # Single rule

# All tests must pass before release
cargo make test
```

---

## Success Criteria for v5.2.0

When all 10 items are complete, v5.1.0 evolves into v5.2.0 with:

✅ All features fully integrated and tested
✅ All flag combinations documented and working
✅ Clear error messages and recovery procedures
✅ Verified performance meets SLOs
✅ Full audit trail capability
✅ Safe destructive operations (--force with --audit)
✅ Smooth developer experience (--watch mode)
✅ Advanced filtering (--condition)

---

**Next Step**: Run verification checklist against codebase to confirm which items are already complete vs need implementation.

**Expected Timeline**: 2-3 days of focused development to v5.2.0 GA.
