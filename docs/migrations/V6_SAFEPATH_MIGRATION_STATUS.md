<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [SafePath Migration Status (v6.0.0)](#safepath-migration-status-v600)
  - [Constitutional Rules (Poka-Yoke)](#constitutional-rules-poka-yoke)
  - [SafePath API Reference](#safepath-api-reference)
    - [Basic Operations](#basic-operations)
    - [Migration Patterns](#migration-patterns)
      - [Pattern 1: PathBuf::from(s) → SafePath::new(s)?](#pattern-1-pathbuffroms-%E2%86%92-safepathnews)
      - [Pattern 2: path.join(p) → path.join(p)?](#pattern-2-pathjoinp-%E2%86%92-pathjoinp)
      - [Pattern 3: Function signatures accepting PathBuf](#pattern-3-function-signatures-accepting-pathbuf)
      - [Pattern 4: Function signatures returning PathBuf](#pattern-4-function-signatures-returning-pathbuf)
      - [Pattern 5: Struct fields](#pattern-5-struct-fields)
  - [Migration Progress](#migration-progress)
    - [Phase 1 - Core Modules (Week 1)](#phase-1---core-modules-week-1)
      - [ggen-utils (COMPLETE)](#ggen-utils-complete)
      - [ggen-config (COMPLETE ✅)](#ggen-config-complete-)
      - [ggen-core/src (HIGH PRIORITY)](#ggen-coresrc-high-priority)
        - [Core Files](#core-files)
      - [ggen-core/src/codegen (HIGH PRIORITY)](#ggen-coresrccodegen-high-priority)
      - [ggen-core/src/protection (SECURITY CRITICAL)](#ggen-coresrcprotection-security-critical)
      - [ggen-core/src/v6 (V6 PIPELINE)](#ggen-coresrcv6-v6-pipeline)
    - [Phase 2 - Domain & CLI (Week 2)](#phase-2---domain--cli-week-2)
      - [ggen-domain](#ggen-domain)
      - [ggen-cli](#ggen-cli)
    - [Phase 3 - Tests & Examples (Week 3)](#phase-3---tests--examples-week-3)
      - [Integration Tests](#integration-tests)
      - [Examples](#examples)
    - [Phase 4 - Supporting Crates (Week 4)](#phase-4---supporting-crates-week-4)
      - [ggen-e2e](#ggen-e2e)
      - [ggen-ai](#ggen-ai)
      - [ggen-dspy](#ggen-dspy)
      - [ggen-spec-validator](#ggen-spec-validator)
      - [ggen-cli-validation](#ggen-cli-validation)
      - [Marketplace Packages](#marketplace-packages)
  - [Overall Progress Summary](#overall-progress-summary)
  - [Andon Signals (Stop the Line)](#andon-signals-stop-the-line)
    - [CRITICAL (Red) - Must stop immediately](#critical-red---must-stop-immediately)
    - [HIGH (Yellow) - Should stop](#high-yellow---should-stop)
    - [Verification Before Proceeding](#verification-before-proceeding)
  - [Breaking Changes & Migration Guide](#breaking-changes--migration-guide)
    - [API Changes](#api-changes)
      - [Before (PathBuf)](#before-pathbuf)
      - [After (SafePath)](#after-safepath)
    - [Error Handling](#error-handling)
    - [Conversion Helpers](#conversion-helpers)
  - [Testing Strategy (Chicago TDD)](#testing-strategy-chicago-tdd)
    - [Test Requirements](#test-requirements)
  - [Risk Mitigation](#risk-mitigation)
    - [High-Risk Areas (Require Extra Scrutiny)](#high-risk-areas-require-extra-scrutiny)
    - [Rollback Plan](#rollback-plan)
  - [Completion Criteria](#completion-criteria)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# SafePath Migration Status (v6.0.0)

**Objective**: Migrate entire codebase from raw `PathBuf` to type-safe `SafePath` wrapper.

**Scope**: 861 file operations across 27 crates.

**Timeline**: Phase 1 (Week 1) - Core modules.

---

## Constitutional Rules (Poka-Yoke)

- ✅ No regression in error messages
- ✅ Preserve existing behavior (observable outputs)
- ✅ Zero runtime overhead (newtype pattern)
- ✅ Comprehensive error context (path traversal prevention)
- ✅ All tests must pass after each migration batch
- ✅ Use Chicago TDD - verify behavior, not just compilation

---

## SafePath API Reference

### Basic Operations
```rust
use ggen_utils::SafePath;

// Creation
let path = SafePath::new("templates/file.tmpl")?;           // Relative paths
let abs_path = SafePath::new_absolute("/tmp/output")?;      // Absolute paths allowed
let cwd = SafePath::current_dir()?;                         // Current directory

// Joining paths
let joined = path.join("subdir")?;                          // Safe join (validates)

// Conversions
let pathbuf: PathBuf = path.into_path_buf();                // Consume and convert
let pathbuf_clone: PathBuf = path.as_path_buf();            // Clone and convert
let path_ref: &Path = path.as_path();                       // Borrow as Path

// Filesystem operations
assert!(path.exists());                                     // Check existence
assert!(path.is_file());                                    // Check if file
assert!(path.is_dir());                                     // Check if directory
let name = path.file_name();                                // Get filename
let ext = path.extension();                                 // Get extension
let parent = path.parent()?;                                // Get parent dir
```

### Migration Patterns

#### Pattern 1: PathBuf::from(s) → SafePath::new(s)?
```rust
// Before
let path = PathBuf::from("templates/file.tmpl");

// After
let path = SafePath::new("templates/file.tmpl")?;
```

#### Pattern 2: path.join(p) → path.join(p)?
```rust
// Before
let joined = path.join("subdir");

// After
let joined = path.join("subdir")?;  // Note: SafePath::join renamed to match old API
```

#### Pattern 3: Function signatures accepting PathBuf
```rust
// Before
pub fn load_template(path: PathBuf) -> Template

// After
pub fn load_template(path: &SafePath) -> Result<Template, TemplateError>
```

#### Pattern 4: Function signatures returning PathBuf
```rust
// Before
pub fn generate(&mut self) -> Result<PathBuf>

// After
pub fn generate(&mut self) -> Result<SafePath>
```

#### Pattern 5: Struct fields
```rust
// Before
pub struct GenContext {
    pub template_path: PathBuf,
    pub output_root: PathBuf,
}

// After
pub struct GenContext {
    pub template_path: SafePath,
    pub output_root: SafePath,
}
```

---

## Migration Progress

### Phase 1 - Core Modules (Week 1)

#### ggen-utils (COMPLETE)
- [x] src/safe_path.rs - Implementation (pre-existing, 100+ tests)
- [x] src/lib.rs - Export SafePath
- [x] No PathBuf usage in ggen-utils (utility crate)

**Status**: ✅ COMPLETE (0/0 functions migrated)

---

#### ggen-config (COMPLETE ✅)
**Target**: 1 file, ~10 PathBuf usages

- [x] src/parser.rs (MIGRATED)
  - [x] ConfigLoader::path field (PathBuf → SafePath)
  - [x] Update new() to use SafePath::new_absolute()
  - [x] Update find_config_file() return type (PathBuf → SafePath)
  - [x] Update find_config_file() implementation (SafePath::current_dir, join, parent)
  - [x] Add ggen_utils::SafePath import
  - [x] Remove std::path::PathBuf import (only Path needed now)

**Status**: ✅ COMPLETE (6/6 operations) - Pending verification

---

#### ggen-core/src (HIGH PRIORITY)
**Target**: 20 files, ~400 PathBuf usages

##### Core Files
- [ ] **generator.rs** (31 PathBuf::from calls) - CRITICAL
  - [ ] GenContext::template_path (PathBuf → SafePath)
  - [ ] GenContext::output_root (PathBuf → SafePath)
  - [ ] GenContext::new() signature
  - [ ] Generator::generate() return type
  - [ ] All PathBuf::from() calls → SafePath::new()?

- [ ] **template.rs** (1 PathBuf::from)
  - [ ] Template loading logic

- [ ] **streaming_generator.rs** (12 PathBuf::from)
  - [ ] StreamingGenerator::new() parameters
  - [ ] Template directory handling
  - [ ] Output directory handling

- [ ] **snapshot.rs** (11 PathBuf::from)
  - [ ] Snapshot file paths
  - [ ] SnapshotManager directory handling

- [ ] **gpack.rs** (2 PathBuf::from)
  - [ ] GpackManifest::load_from_file()
  - [ ] discover_* methods return types

- [ ] **cache.rs** (2 PathBuf::from)
  - [ ] Cache::with_dir() parameter

- [ ] **preprocessor.rs** (2 PathBuf::from)
  - [ ] Preprocessor::with_freeze() parameter

- [ ] **lockfile.rs** (PathBuf field)
  - [ ] Lockfile::with_path() parameter

- [ ] **pipeline.rs** (3 PathBuf::from)
  - [ ] Pipeline path handling

- [ ] **merge.rs** (4 PathBuf::from)
  - [ ] File merging operations

- [ ] **manifest/types.rs** (2 PathBuf::from)
  - [ ] Manifest path handling

**Status**: 🟡 NOT STARTED (0/70+ operations)

---

#### ggen-core/src/codegen (HIGH PRIORITY)
**Target**: 24 files, ~150 PathBuf usages

- [ ] **pipeline.rs** (7 PathBuf::from)
  - [ ] CodegenPipeline path handling
  - [ ] Template resolution
  - [ ] Output path management

- [ ] **executor.rs** (20+ PathBuf operations)
  - [ ] Executor::execute() paths
  - [ ] Template path resolution
  - [ ] Output file writing

- [ ] **incremental.rs** (2 PathBuf::from)
  - [ ] Incremental compilation cache paths

- [ ] **watch.rs** (6 PathBuf::from)
  - [ ] File watching paths
  - [ ] Change detection

- [ ] **merge.rs** (4 PathBuf::from)
  - [ ] Code merging operations

- [ ] **mod.rs** (PathBuf exports)
  - [ ] Public API types

**Status**: 🟡 NOT STARTED (0/40+ operations)

---

#### ggen-core/src/protection (SECURITY CRITICAL)
**Target**: 2 files, ~5 PathBuf usages

- [ ] **path.rs** (1 PathBuf::from) - CRITICAL for SafePath integration
  - [ ] PathProtection integration with SafePath
  - [ ] Validation alignment

- [ ] **generator_integration.rs** (4 PathBuf::from)
  - [ ] Generator protection wrappers

**Status**: 🟡 NOT STARTED (0/5 operations)

---

#### ggen-core/src/v6 (V6 PIPELINE)
**Target**: 5 files, ~20 PathBuf usages

- [ ] **pipeline.rs** (4 PathBuf::from)
  - [ ] V6Pipeline path handling
  - [ ] Stage path resolution

- [ ] **receipt.rs** (2 PathBuf::from)
  - [ ] Receipt generation paths

- [ ] **epoch.rs** (4 PathBuf::from)
  - [ ] Epoch timestamp paths

- [ ] **passes/emission.rs** (3 PathBuf::from)
  - [ ] Code emission paths

- [ ] **passes/canonicalization.rs** (9 PathBuf::from)
  - [ ] Canonical path handling
  - [ ] Format checking paths

- [ ] **passes/receipt_gen.rs** (3 PathBuf::from)
  - [ ] Receipt output paths

**Status**: 🟡 NOT STARTED (0/25 operations)

---

### Phase 2 - Domain & CLI (Week 2)

#### ggen-domain
**Target**: ~200 PathBuf usages across 15 files

- [ ] src/template/render_with_rdf.rs (5 PathBuf::from)
- [ ] src/template/generate.rs (4 PathBuf::from)
- [ ] src/template/mod.rs (1 PathBuf::from)
- [ ] src/project/new.rs (2 PathBuf::from)
- [ ] src/packs/repository.rs (4 PathBuf::from)
- [ ] src/packs/metadata.rs (3 PathBuf::from)
- [ ] src/packs/installer.rs (4 PathBuf::from)
- [ ] src/packs/install.rs (2 PathBuf::from)
- [ ] src/packs/generator.rs (1 PathBuf::from)
- [ ] src/packs/composer.rs (3 PathBuf::from)
- [ ] src/packs/compose.rs (1 PathBuf::from)
- [ ] src/ontology/validate.rs (1 PathBuf::from)
- [ ] src/ontology/generate.rs (2 PathBuf::from)
- [ ] src/ontology/extract.rs (3 PathBuf::from)
- [ ] src/marketplace/*.rs (~20 PathBuf::from)

**Status**: 🟡 NOT STARTED (0/200+ operations)

---

#### ggen-cli
**Target**: ~150 PathBuf usages across 10 files

- [ ] src/cmds/sync.rs (2 PathBuf::from)
- [ ] src/cmds/template.rs (7 PathBuf::from)
- [ ] src/cmds/ontology.rs (4 PathBuf::from)
- [ ] src/cmds/init.rs (5 PathBuf::from)
- [ ] src/cmds/graph.rs (5 PathBuf::from)
- [ ] src/cmds/project.rs (1 PathBuf::from)
- [ ] src/cmds/workflow.rs (1 PathBuf::from)
- [ ] src/cmds/packs.rs (1 PathBuf::from)
- [ ] src/conventions/watcher.rs (5 PathBuf::from)
- [ ] src/conventions/planner.rs (1 PathBuf::from)

**Status**: 🟡 NOT STARTED (0/150+ operations)

---

### Phase 3 - Tests & Examples (Week 3)

#### Integration Tests
**Target**: ~200 PathBuf usages

- [ ] tests/*.rs - All integration tests
- [ ] crates/ggen-core/tests/*.rs
- [ ] crates/ggen-cli/tests/*.rs
- [ ] crates/ggen-domain/tests/*.rs

**Status**: 🟡 NOT STARTED (0/200+ operations)

---

#### Examples
**Target**: ~50 PathBuf usages

- [ ] examples/sparql-engine/src/main.rs (1 PathBuf::from)
- [ ] examples/advanced-error-handling/src/*.rs (2 PathBuf::from)
- [ ] examples/advanced-fullstack-integration/src/main.rs (1 PathBuf::from)
- [ ] examples/ggen-usage-wrapping/examples/*.rs (1 PathBuf::from)
- [ ] examples/e2e-demo/demo_e2e.rs (2 PathBuf::from)

**Status**: 🟡 NOT STARTED (0/50+ operations)

---

### Phase 4 - Supporting Crates (Week 4)

#### ggen-e2e
- [ ] src/runner.rs (1 PathBuf::from)
- [ ] src/result.rs (1 PathBuf::from)
- [ ] src/golden.rs (4 PathBuf::from)
- [ ] src/fixture.rs (1 PathBuf::from)
- [ ] src/error.rs (1 PathBuf::from)
- [ ] src/container.rs (2 PathBuf::from)
- [ ] src/comparison.rs (2 PathBuf::from)

**Status**: 🟡 NOT STARTED (0/12 operations)

---

#### ggen-ai
- [ ] src/swarm/orchestration.rs (2 PathBuf::from)
- [ ] src/agents/core/regeneration.rs (1 PathBuf::from)
- [ ] src/rdf/mod.rs (1 PathBuf::from)
- [ ] src/prompts/loader.rs (3 PathBuf::from)
- [ ] src/dspy/evaluation/mod.rs (2 PathBuf::from)

**Status**: 🟡 NOT STARTED (0/9 operations)

---

#### ggen-dspy
- [ ] src/config/cache.rs (1 PathBuf::from)

**Status**: 🟡 NOT STARTED (0/1 operations)

---

#### ggen-spec-validator
- [ ] src/lib.rs (2 PathBuf::from)

**Status**: 🟡 NOT STARTED (0/2 operations)

---

#### ggen-cli-validation
- [ ] src/security.rs (2 PathBuf::from)

**Status**: 🟡 NOT STARTED (0/2 operations)

---

#### Marketplace Packages
- [ ] marketplace/packages/multi-tenant-saas/tests/*.rs (1 PathBuf::from)

**Status**: 🟡 NOT STARTED (0/1 operations)

---

## Overall Progress Summary

| Phase | Crates | Files | Operations | Status | Completion |
|-------|--------|-------|------------|--------|------------|
| **Phase 1** | 3 | 40+ | ~400 | 🟡 IN PROGRESS | 1/400 (0.25%) |
| **Phase 2** | 2 | 25+ | ~350 | 🔴 NOT STARTED | 0/350 (0%) |
| **Phase 3** | Tests | ~50 | ~250 | 🔴 NOT STARTED | 0/250 (0%) |
| **Phase 4** | 6 | 20+ | ~50 | 🔴 NOT STARTED | 0/50 (0%) |
| **TOTAL** | **11+** | **135+** | **~1050** | **🟡 0.1%** | **1/1050** |

---

## Andon Signals (Stop the Line)

### CRITICAL (Red) - Must stop immediately
- ❌ `cargo make check` fails (compiler errors)
- ❌ `cargo make test` fails (test failures)
- ❌ Behavioral regression detected (outputs changed)

### HIGH (Yellow) - Should stop
- ⚠️ `cargo make lint` warnings (clippy)
- ⚠️ Performance regression >10% (benchmark failure)
- ⚠️ Error message quality degraded

### Verification Before Proceeding
```bash
# After each batch migration (10-20 functions):
cargo make check          # Must pass (no errors/warnings)
cargo make test-unit      # Must pass (all tests green)
cargo make lint           # Must pass (zero clippy warnings)

# After completing each file:
cargo make test           # Full test suite must pass
cargo make slo-check      # Performance SLOs must be met

# After completing each crate:
cargo make ci             # Full CI pipeline must pass
```

---

## Breaking Changes & Migration Guide

### API Changes

#### Before (PathBuf)
```rust
pub fn load_template(path: PathBuf) -> Template {
    // ...
}

let path = PathBuf::from("template.tmpl");
let template = load_template(path);
```

#### After (SafePath)
```rust
pub fn load_template(path: &SafePath) -> Result<Template, TemplateError> {
    // ...
}

let path = SafePath::new("template.tmpl")?;
let template = load_template(&path)?;
```

### Error Handling

SafePath operations can fail, so all call sites must handle errors:

```rust
// Before: infallible
let path = PathBuf::from("template.tmpl");
let joined = path.join("subdir");

// After: fallible
let path = SafePath::new("template.tmpl")?;
let joined = path.join("subdir")?;
```

### Conversion Helpers

When interfacing with external APIs still using PathBuf:

```rust
// SafePath → PathBuf
let safe_path = SafePath::new("template.tmpl")?;
let path_buf: PathBuf = safe_path.into_path_buf();  // Consumes
let path_buf_clone: PathBuf = safe_path.as_path_buf();  // Clones

// PathBuf → SafePath
let path_buf = PathBuf::from("template.tmpl");
let safe_path = SafePath::new(path_buf)?;  // Validates
```

---

## Testing Strategy (Chicago TDD)

For each migrated module:

1. **Arrange**: Identify all PathBuf usage patterns
2. **Act**: Replace with SafePath equivalents
3. **Assert**: Verify behavior unchanged (same outputs, same errors, same side effects)

### Test Requirements
- ✅ All existing tests must pass (regression prevention)
- ✅ New tests for SafePath validation (security)
- ✅ Error path tests (invalid paths rejected)
- ✅ Performance tests (zero overhead verified)

---

## Risk Mitigation

### High-Risk Areas (Require Extra Scrutiny)
1. **ggen-core/src/protection/path.rs** - Security-critical path validation
2. **ggen-core/src/v6/pipeline.rs** - Core generation pipeline
3. **ggen-core/src/generator.rs** - Primary code generation entry point
4. **ggen-cli/src/cmds/*.rs** - User-facing CLI commands

### Rollback Plan
- Each migration is a separate commit
- If Andon signal appears, rollback last commit
- Fix root cause before proceeding
- Re-run full validation suite

---

## Completion Criteria

Migration is complete when:
- ✅ Zero raw `PathBuf::from()` calls in production code (only `SafePath::new()`)
- ✅ Zero `.join()` on PathBuf (only `SafePath::join()`)
- ✅ All function signatures use `SafePath` or `&SafePath`
- ✅ All struct fields use `SafePath`
- ✅ `cargo make ci` passes (full CI pipeline green)
- ✅ `cargo make slo-check` passes (performance targets met)
- ✅ All tests pass (0 failures, 0 regressions)
- ✅ Documentation updated (API docs, migration guide)
- ✅ Security review passed (path traversal prevention verified)

---

**Last Updated**: 2026-01-24
**Status**: Phase 1 IN PROGRESS (ggen-utils complete, ggen-config/ggen-core pending)
