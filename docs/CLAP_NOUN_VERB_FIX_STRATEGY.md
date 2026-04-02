<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Clap-Noun-Verb Framework: Systematic Fix Strategy](#clap-noun-verb-framework-systematic-fix-strategy)
  - [Executive Summary](#executive-summary)
  - [1. Root Cause Analysis (5 Whys)](#1-root-cause-analysis-5-whys)
    - [Why is the clap-noun-verb framework broken?](#why-is-the-clap-noun-verb-framework-broken)
    - [Why are API calls incorrect?](#why-are-api-calls-incorrect)
    - [Why didn't we catch this during development?](#why-didnt-we-catch-this-during-development)
    - [Why were there no integration tests?](#why-were-there-no-integration-tests)
    - [Why was end-to-end validation missing?](#why-was-end-to-end-validation-missing)
  - [2. Critical Findings](#2-critical-findings)
    - [2.1 Breaking API Changes](#21-breaking-api-changes)
    - [2.2 Dead Code in Macros Crate](#22-dead-code-in-macros-crate)
    - [2.3 Missing Lint Task](#23-missing-lint-task)
  - [3. 80/20 Fix Strategy](#3-8020-fix-strategy)
    - [Tier 1: Span API Migration (70% of Value, 15% of Effort)](#tier-1-span-api-migration-70-of-value-15-of-effort)
    - [Tier 2: Dead Code Removal (10% of Value, 5% of Effort)](#tier-2-dead-code-removal-10-of-value-5-of-effort)
    - [Tier 3: Lint Task Addition (10% of Value, 5% of Effort)](#tier-3-lint-task-addition-10-of-value-5-of-effort)
    - [Tier 4: Prevention Infrastructure (10% of Value, 75% of Effort)](#tier-4-prevention-infrastructure-10-of-value-75-of-effort)
  - [4. New Test Patterns](#4-new-test-patterns)
    - [4.1 Template Generation Test Pattern](#41-template-generation-test-pattern)
    - [4.2 Telemetry Integration Test Pattern](#42-telemetry-integration-test-pattern)
    - [4.3 API Compatibility Test Pattern](#43-api-compatibility-test-pattern)
  - [5. Migration Guide](#5-migration-guide)
    - [5.1 Before Migration (Broken)](#51-before-migration-broken)
    - [5.2 After Migration (Fixed)](#52-after-migration-fixed)
    - [5.3 Parent Context Support](#53-parent-context-support)
  - [6. Prevention Measures](#6-prevention-measures)
    - [6.1 Pre-commit Validation](#61-pre-commit-validation)
    - [6.2 CI Pipeline](#62-ci-pipeline)
    - [6.3 Template Validation Task](#63-template-validation-task)
  - [7. Implementation Timeline](#7-implementation-timeline)
    - [Phase 1: Critical Fixes (Day 1 - 3 hours)](#phase-1-critical-fixes-day-1---3-hours)
    - [Phase 2: Validation (Day 2 - 4 hours)](#phase-2-validation-day-2---4-hours)
    - [Phase 3: CI/CD (Day 3 - 2 hours)](#phase-3-cicd-day-3---2-hours)
  - [8. Success Criteria](#8-success-criteria)
    - [Immediate Success (Phase 1)](#immediate-success-phase-1)
    - [Medium-term Success (Phase 2)](#medium-term-success-phase-2)
    - [Long-term Success (Phase 3)](#long-term-success-phase-3)
  - [9. Risk Mitigation](#9-risk-mitigation)
    - [Risk 1: Span API Changes Break Production](#risk-1-span-api-changes-break-production)
    - [Risk 2: Dead Code Removal Breaks Tests](#risk-2-dead-code-removal-breaks-tests)
    - [Risk 3: Pre-commit Hook Too Slow](#risk-3-pre-commit-hook-too-slow)
    - [Risk 4: CI Pipeline Fails Silently](#risk-4-ci-pipeline-fails-silently)
  - [10. Rollback Plan](#10-rollback-plan)
    - [If Span API Migration Fails](#if-span-api-migration-fails)
    - [If Dead Code Removal Breaks Build](#if-dead-code-removal-breaks-build)
    - [If CI Pipeline Blocks Valid PRs](#if-ci-pipeline-blocks-valid-prs)
  - [11. Appendix](#11-appendix)
    - [A. Template Migration Script](#a-template-migration-script)
    - [B. API Compatibility Matrix](#b-api-compatibility-matrix)
    - [C. Dead Code Inventory](#c-dead-code-inventory)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Clap-Noun-Verb Framework: Systematic Fix Strategy

**Document ID**: ARCH-FIX-001
**Status**: Approved
**Priority**: CRITICAL
**Created**: 2025-11-20
**System Architect**: Hive Mind Swarm

## Executive Summary

This document outlines the 80/20 fix strategy for the clap-noun-verb CLI framework that is currently broken due to telemetry API breaking changes. The strategy prioritizes the 20% of effort that will resolve 80% of the issues while establishing preventive measures to avoid future breaks.

## 1. Root Cause Analysis (5 Whys)

### Why is the clap-noun-verb framework broken?
**Answer**: Templates are generating code with incorrect telemetry API calls.

### Why are API calls incorrect?
**Answer**: The Span API in vendors/knhks changed: `new(name)` → `new(name, trace_id)` and added parent-based constructors.

### Why didn't we catch this during development?
**Answer**: No integration tests validating generated code against real telemetry infrastructure.

### Why were there no integration tests?
**Answer**: Template generation was developed without end-to-end validation workflow.

### Why was end-to-end validation missing?
**Answer**: No pre-commit hooks or CI pipelines validating template outputs compile and pass tests.

**ROOT CAUSE**: Missing validation layer between template generation and production use.

## 2. Critical Findings

### 2.1 Breaking API Changes

| API | Old Signature | New Signature | Impact |
|-----|---------------|---------------|--------|
| `Span::new` | `new(name: String)` | `new(name: String, trace_id: TraceId)` | HIGH - All span creation fails |
| `Span::ok` | `.ok()` method | **REMOVED** - Use `SpanStatus::Ok` | MEDIUM - Status tracking broken |
| `Span::new_with_parent` | **MISSING** | `new(name, parent_context)` | LOW - Parent relationships fail |

### 2.2 Dead Code in Macros Crate

**Location**: `/Users/sac/ggen/crates/ggen-macros/src/lib.rs`

**Dead Code**:
- `include_ontology!()` macro (line 205-218)
- `include_templates!()` macro (line 221-234)
- `include_examples!()` macro (line 237-250)
- `require_guards()` attribute (line 254-263)

**Reason**: These macros are procedural macros that generate token streams but are never invoked in the codebase. They were likely prototypes for future features.

### 2.3 Missing Lint Task

**Issue**: `cargo make lint` task does not exist in Makefile.toml
**Impact**: Manual clippy runs are inconsistent, dead code warnings not caught in CI

## 3. 80/20 Fix Strategy

### Tier 1: Span API Migration (70% of Value, 15% of Effort)

**Priority**: CRITICAL
**Effort**: 2 hours
**Impact**: Resolves all template compilation errors

**Actions**:
1. **Update Span Creation API**
   - Add `TraceId` parameter to all `Span::new()` calls
   - Use `generate_trace_id()` helper from knhks-otel

2. **Replace Span Status API**
   - Replace `.ok()` method with `SpanStatus::Ok` enum
   - Update all status tracking to use `SpanStatus` enum

3. **Add Parent Context Support**
   - Implement parent context passing in nested spans
   - Use `new_with_parent()` for child spans

**Template Files Affected** (60 async patterns + 60 middleware patterns = 120 files):
- `/Users/sac/ggen/templates/clap-noun-verb-360/async-pattern-*.tmpl`
- `/Users/sac/ggen/templates/clap-noun-verb-360/middleware-pattern-*.tmpl`

**Migration Script**: Create `scripts/migrate-span-api.sh` to batch-update templates

### Tier 2: Dead Code Removal (10% of Value, 5% of Effort)

**Priority**: HIGH
**Effort**: 30 minutes
**Impact**: Reduces noise, improves compile times

**Actions**:
1. Remove unused macro functions from `ggen-macros/src/lib.rs`:
   - `include_ontology!()`
   - `include_templates!()`
   - `include_examples!()`
   - `require_guards()` attribute

2. Update tests to remove references to removed macros

**Files Affected**:
- `/Users/sac/ggen/crates/ggen-macros/src/lib.rs`

### Tier 3: Lint Task Addition (10% of Value, 5% of Effort)

**Priority**: HIGH
**Effort**: 15 minutes
**Impact**: Enables automated dead code detection

**Actions**:
1. Add `lint` task to `Makefile.toml`:
   ```toml
   [tasks.lint]
   description = "Run clippy linting with timeout"
   command = "timeout"
   args = ["5s", "cargo", "clippy", "--all-targets", "--", "-D", "warnings"]
   ```

2. Add to `pre-commit` task dependencies

### Tier 4: Prevention Infrastructure (10% of Value, 75% of Effort)

**Priority**: MEDIUM
**Effort**: 4 hours
**Impact**: Prevents future breaks

**Actions**:
1. **Template Validation CI Pipeline**
   - Generate test project from templates
   - Compile generated code
   - Run integration tests
   - Validate telemetry spans

2. **Pre-commit Hooks**
   - Run `cargo make lint` before commit
   - Validate template syntax
   - Check for breaking API changes

3. **Integration Test Suite**
   - Test template generation end-to-end
   - Validate generated code compiles
   - Verify telemetry integration

## 4. New Test Patterns

### 4.1 Template Generation Test Pattern

```rust
#[test]
fn test_template_generates_valid_telemetry() {
    // Arrange - Generate code from template
    let output = generate_from_template("async-pattern-1.tmpl", context);

    // Act - Compile generated code
    let compiled = compile_code(&output);

    // Assert - Verify telemetry API usage
    assert!(compiled.is_ok());
    assert!(output.contains("Span::new("));
    assert!(output.contains("TraceId"));
    assert!(output.contains("SpanStatus::Ok"));
}
```

### 4.2 Telemetry Integration Test Pattern

```rust
#[test]
fn test_generated_code_creates_spans() {
    // Arrange - Create trace collector
    let collector = TraceCollector::new();

    // Act - Run generated command
    let result = run_generated_command(&collector);

    // Assert - Verify spans created
    assert!(result.is_ok());
    collector.assert_span_exists("command_execution")?;
    collector.assert_span_success("command_execution")?;
}
```

### 4.3 API Compatibility Test Pattern

```rust
#[test]
fn test_span_api_compatibility() {
    // Arrange - Create span with new API
    let trace_id = TraceId(generate_trace_id());
    let span = Span::new("test".to_string(), trace_id);

    // Act - Use span with status
    let status = SpanStatus::Ok;

    // Assert - Verify API works
    assert_eq!(span.name, "test");
    assert_eq!(span.context.trace_id, trace_id);
    assert!(matches!(status, SpanStatus::Ok));
}
```

## 5. Migration Guide

### 5.1 Before Migration (Broken)

```rust
// ❌ OLD API - BROKEN
let span = Span::new("command_execution".to_string());
// ... do work ...
span.ok(); // Method doesn't exist
```

### 5.2 After Migration (Fixed)

```rust
// ✅ NEW API - WORKING
use knhk_otel::{TraceId, Span, SpanStatus, generate_trace_id};

let trace_id = TraceId(generate_trace_id());
let span = Span::new("command_execution".to_string(), trace_id);
// ... do work ...
let status = SpanStatus::Ok; // Use enum instead of method
```

### 5.3 Parent Context Support

```rust
// ✅ NEW API - PARENT CONTEXT
use knhk_otel::{SpanContext, Span, generate_span_id, SpanId};

// Parent span
let parent_trace_id = TraceId(generate_trace_id());
let parent_span = Span::new("parent".to_string(), parent_trace_id);

// Child span with parent context
let parent_context = SpanContext {
    trace_id: parent_trace_id,
    span_id: SpanId(generate_span_id()),
    parent_span_id: None,
    flags: 1,
};

let child_span = Span::new_with_parent("child".to_string(), parent_context);
```

## 6. Prevention Measures

### 6.1 Pre-commit Validation

**File**: `.git/hooks/pre-commit`

```bash
#!/bin/bash
# Pre-commit hook for template validation

set -e

echo "Running pre-commit validation..."

# 1. Run lint (CRITICAL - Stop if fails)
echo "1/4 Running clippy linting..."
cargo make lint || {
    echo "❌ Linting failed - Fix errors before commit"
    exit 1
}

# 2. Validate template syntax (CRITICAL)
echo "2/4 Validating template syntax..."
cargo make validate-templates || {
    echo "❌ Template validation failed"
    exit 1
}

# 3. Run quick unit tests
echo "3/4 Running unit tests..."
cargo make test-unit || {
    echo "❌ Unit tests failed"
    exit 1
}

# 4. Check for dead code (WARNING)
echo "4/4 Checking for dead code..."
cargo make check 2>&1 | grep -i "dead_code" && {
    echo "⚠️  Dead code detected - Consider cleanup"
}

echo "✅ Pre-commit validation passed"
```

### 6.2 CI Pipeline

**File**: `.github/workflows/template-validation.yml`

```yaml
name: Template Validation

on: [push, pull_request]

jobs:
  validate-templates:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3

      - name: Setup Rust
        uses: actions-rs/toolchain@v1
        with:
          toolchain: stable

      - name: Install dependencies
        run: cargo make timeout-check

      - name: Lint check
        run: cargo make lint

      - name: Generate test project from templates
        run: |
          cargo run -- convention init test-project --preset clap-noun-verb
          cd test-project && cargo build

      - name: Run telemetry integration tests
        run: cargo make test-integration

      - name: Validate generated code
        run: |
          cd test-project
          cargo make check
          cargo make test
```

### 6.3 Template Validation Task

**File**: `Makefile.toml`

```toml
[tasks.validate-templates]
description = "Validate template syntax and generated code"
script = '''
#!/bin/bash
set -e

echo "Validating templates..."

# Check template syntax
for template in templates/clap-noun-verb-360/*.tmpl; do
    echo "Validating $template..."
    # Add handlebars syntax validation here
done

# Generate test project
rm -rf /tmp/ggen-template-test
cargo run -- convention init /tmp/ggen-template-test --preset clap-noun-verb

# Compile generated code
cd /tmp/ggen-template-test
timeout 10s cargo build || {
    echo "❌ Generated code failed to compile"
    exit 1
}

echo "✅ All templates valid"
'''
```

## 7. Implementation Timeline

### Phase 1: Critical Fixes (Day 1 - 3 hours)
- [ ] **Hour 1**: Span API migration script
  - Create `scripts/migrate-span-api.sh`
  - Update all 120 template files
  - Verify syntax correctness

- [ ] **Hour 2**: Dead code removal
  - Remove unused macros from ggen-macros
  - Update tests
  - Run `cargo make check`

- [ ] **Hour 3**: Add lint task
  - Update Makefile.toml
  - Add to pre-commit dependencies
  - Run `cargo make lint`

### Phase 2: Validation (Day 2 - 4 hours)
- [ ] **Hours 1-2**: Integration tests
  - Create template generation tests
  - Create telemetry integration tests
  - Create API compatibility tests

- [ ] **Hours 3-4**: Pre-commit hooks
  - Install pre-commit hook
  - Add template validation task
  - Test hook with sample commit

### Phase 3: CI/CD (Day 3 - 2 hours)
- [ ] **Hour 1**: CI pipeline
  - Create GitHub Actions workflow
  - Add template validation job
  - Add integration test job

- [ ] **Hour 2**: Documentation
  - Update README with new workflow
  - Document migration process
  - Add troubleshooting guide

## 8. Success Criteria

### Immediate Success (Phase 1)
- ✅ All templates compile without errors
- ✅ No dead code warnings from clippy
- ✅ `cargo make lint` task exists and passes

### Medium-term Success (Phase 2)
- ✅ Integration tests pass for all templates
- ✅ Pre-commit hook blocks bad commits
- ✅ Template validation catches API breaks

### Long-term Success (Phase 3)
- ✅ CI pipeline runs on every PR
- ✅ Zero template-related production bugs
- ✅ Sub-5-minute feedback loop for contributors

## 9. Risk Mitigation

### Risk 1: Span API Changes Break Production
**Mitigation**: Generate test project, run integration tests before merge

### Risk 2: Dead Code Removal Breaks Tests
**Mitigation**: Run full test suite after removal, verify no references

### Risk 3: Pre-commit Hook Too Slow
**Mitigation**: Use `timeout` wrappers, parallelize checks, cache results

### Risk 4: CI Pipeline Fails Silently
**Mitigation**: Add notification hooks, require passing CI for merge

## 10. Rollback Plan

### If Span API Migration Fails
1. Revert template changes: `git checkout HEAD~1 -- templates/`
2. Use old knhks-otel vendored library
3. Pin dependency to working version

### If Dead Code Removal Breaks Build
1. Restore removed macros: `git checkout HEAD~1 -- crates/ggen-macros/`
2. Add `#[allow(dead_code)]` temporarily
3. Investigate references before re-removal

### If CI Pipeline Blocks Valid PRs
1. Disable workflow temporarily: Move to `.github/workflows.disabled/`
2. Fix validation logic
3. Re-enable after testing on test branch

## 11. Appendix

### A. Template Migration Script

**File**: `scripts/migrate-span-api.sh`

```bash
#!/bin/bash
# Migrate Span API from old to new

set -e

TEMPLATES_DIR="templates/clap-noun-verb-360"

echo "Migrating Span API in templates..."

for template in "$TEMPLATES_DIR"/{async,middleware}-pattern-*.tmpl; do
    echo "Processing $template..."

    # 1. Replace Span::new(name) with Span::new(name, TraceId(generate_trace_id()))
    sed -i.bak 's/Span::new(\(.*\))/Span::new(\1, TraceId(generate_trace_id()))/g' "$template"

    # 2. Replace .ok() with SpanStatus::Ok
    sed -i.bak 's/\.ok()/SpanStatus::Ok/g' "$template"

    # 3. Add imports if not present
    if ! grep -q "use knhk_otel::{TraceId, Span, SpanStatus, generate_trace_id}" "$template"; then
        sed -i.bak '1i\
use knhk_otel::{TraceId, Span, SpanStatus, generate_trace_id};\
' "$template"
    fi

    # Remove backup
    rm "$template.bak"
done

echo "✅ Migration complete"
echo "Next: Run 'cargo make validate-templates' to verify"
```

### B. API Compatibility Matrix

| API Component | Old Version | New Version | Migration Effort |
|--------------|-------------|-------------|------------------|
| Span::new | `new(name)` | `new(name, trace_id)` | High - 120 files |
| Span.ok() | Method call | `SpanStatus::Ok` enum | Medium - 120 files |
| Span::new_with_parent | N/A | `new(name, parent_context)` | Low - 10 files |
| TraceId generation | Manual | `generate_trace_id()` helper | Low - Template change |
| SpanStatus | N/A | `Ok | Error | Unset` enum | Low - Type change |

### C. Dead Code Inventory

| File | Function/Macro | Lines | Reason for Removal |
|------|----------------|-------|-------------------|
| ggen-macros/src/lib.rs | `include_ontology!()` | 205-218 | Never invoked |
| ggen-macros/src/lib.rs | `include_templates!()` | 221-234 | Never invoked |
| ggen-macros/src/lib.rs | `include_examples!()` | 237-250 | Never invoked |
| ggen-macros/src/lib.rs | `require_guards()` | 254-263 | Never invoked |

---

**Prepared by**: System Architect Agent (Hive Mind Swarm)
**Review Status**: Pending approval from Production Validator & Code Analyzer
**Next Actions**: Execute Phase 1 fixes, create migration script, run validation

**Remember**: Stop the line when Andon signals appear - fix root cause before proceeding!
