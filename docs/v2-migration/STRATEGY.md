# ggen v2.0.0 Migration Strategy
**V2 Completion Architect - Hive Mind Swarm**
**Date**: 2025-11-02
**Method**: 80/20 Principle + Complexity Analysis

---

## Executive Summary

**Current State**: v2.0.0 architecture **72% complete** (72/100 scorecard)
**Blocking Issues**: 4 compilation errors in domain/utils layer
**Timeline to Production**: **2-6 hours** depending on chosen phase
**Risk Level**: **LOW** (isolated errors, working domain layer exists)

**Recommendation**: Execute **Phase 1 (P0)** to achieve production release in 2-3 hours.

---

## Architecture Analysis

### Current v2 Architecture (3-Layer Pattern)

```
┌─────────────────────────────────────────────────────┐
│         cmds/ (CLI Router - 1,052 LOC)              │
│  ┌────────┬────────┬────────┬────────┬─────────┐   │
│  │Template│  AI    │ Graph  │Marketpl│ Project │   │
│  │ Args   │ Args   │ Args   │ace Args│  Args   │   │
│  └───┬────┴────┬───┴────┬───┴────┬───┴────┬────┘   │
│      │         │        │        │        │         │
└──────┼─────────┼────────┼────────┼────────┼─────────┘
       │         │        │        │        │
       ▼         ▼        ▼        ▼        ▼
┌─────────────────────────────────────────────────────┐
│       domain/ (Business Logic - 8,619 LOC)          │
│  ┌────────┬────────┬────────┬────────┬─────────┐   │
│  │template│   ai   │ graph  │marketpl│ project │   │
│  │ (async)│(async) │(async) │ace     │ (async) │   │
│  │        │        │        │(async) │         │   │
│  └────┬───┴────┬───┴────┬───┴────┬───┴────┬────┘   │
│       │        │        │        │        │         │
└───────┼────────┼────────┼────────┼────────┼─────────┘
        │        │        │        │        │
        ▼        ▼        ▼        ▼        ▼
┌─────────────────────────────────────────────────────┐
│     runtime/ (Async/Sync Bridge - 94 LOC)           │
│              runtime::execute()                      │
└─────────────────────────────────────────────────────┘
```

### Completion Status by Subsystem

| Subsystem | CLI Layer | Domain Layer | Status | Errors |
|-----------|-----------|--------------|--------|--------|
| **Template** | ✅ 100% (5 files) | ✅ 100% (8 files) | ✅ COMPLETE | 0 |
| **AI** | ✅ 100% (1 file) | ✅ 100% (2 files) | ✅ COMPLETE | 0 |
| **Graph** | ⚠️ 80% (1 file) | ✅ 100% (5 files) | ⚠️ PARTIAL | 1 |
| **Marketplace** | ✅ 100% (1 file) | ✅ 100% (6 files) | ✅ COMPLETE | 0 |
| **Project** | ✅ 100% (1 file) | ✅ 100% (6 files) | ✅ COMPLETE | 0 |
| **Utils** | ❌ 0% (missing) | ⚠️ 67% (2/3 files) | ❌ BROKEN | 3 |
| **Hook** | ❌ 0% (missing) | ✅ 100% (5 files) | ❌ NO CLI | 0 |
| **CI** | ❌ 0% (exists, not migrated) | ✅ 100% (2 files) | ❌ NO CLI | 0 |

**Total**: 9 command modules, 5 complete (56%), 4 incomplete (44%)

---

## Compilation Error Analysis

### Error Inventory (4 Total)

```bash
$ cargo build 2>&1 | grep "^error\[E"
error[E0432]: unresolved imports (6 types) in domain/utils/doctor.rs:11
error[E0432]: unresolved import run_doctor in domain/utils/doctor.rs:34
error[E0432]: unresolved imports (3 types) in domain/utils/env.rs:11
error[E0412]: cannot find type `GraphCmd` in cmds/graph.rs:40
```

### Root Cause: Domain Layer Incomplete

**File**: `cli/src/domain/utils/doctor.rs`
**Problem**: Attempts to import from `super::super::super::domain::utils::doctor`, but that module doesn't export types

**Fix Complexity**: **LOW** (2-3 hours)
- Missing type definitions in domain layer
- Missing re-exports in domain/utils/mod.rs
- Pattern already exists in domain/marketplace (working reference)

**File**: `cli/src/domain/utils/env.rs`
**Problem**: Same as doctor.rs (missing types in domain layer)

**Fix Complexity**: **LOW** (1-2 hours)
- Copy pattern from marketplace domain layer

**File**: `cli/src/cmds/graph.rs`
**Problem**: References non-existent `GraphCmd` type

**Fix Complexity**: **TRIVIAL** (<30 min)
- Change `GraphCmd` to `GraphArgs` (already exists)
- Typo in cmds/mod.rs:40

---

## Migration Complexity Matrix

### Noun Command Complexity Analysis

| Noun | Verbs | Domain LOC | CLI LOC | Dependencies | Complexity | Est. Hours |
|------|-------|------------|---------|--------------|------------|------------|
| **template** | 8 | 2,387 | 358 | ggen-core | ✅ DONE | 0 |
| **marketplace** | 5 | 1,624 | 412 | reqwest, serde | ✅ DONE | 0 |
| **ai** | 1 | 412 | 86 | genai | ✅ DONE | 0 |
| **project** | 5 | 1,892 | 287 | ggen-core | ✅ DONE | 0 |
| **graph** | 4 | 1,128 | 94 | oxigraph | ⚠️ 1 error | 0.5 |
| **utils** | 2 | 823 | 0 | None | ❌ 3 errors | 2-3 |
| **hook** | 4 | 487 | 0 | tokio | ❌ No CLI | 1-2 |
| **ci** | 1 | 267 | 0 | tokio | ❌ No CLI | 1-2 |
| **audit** | 1 | 189 | 0 | tokio | ❌ No CLI | 1 |

### Dependency Graph

```
Utils ────┐
          ├──→ No external deps (easiest)
Hook ─────┤
          │
CI ───────┘

Template ──→ ggen-core (RDF, templates) ✅ DONE
Project ───→ ggen-core (RDF, templates) ✅ DONE
Graph ─────→ oxigraph (SPARQL) ⚠️ 1 error
Marketplace→ reqwest, serde ✅ DONE
AI ────────→ genai ✅ DONE
```

**Insight**: Utils, Hook, CI have NO external dependencies → **lowest migration risk**

---

## Phased Migration Plan

### Phase 1 (P0) - Production Release [2-3 hours]
**Goal**: Fix blocking errors → ship v2.0.0 with 67% feature coverage
**Priority**: CRITICAL
**Risk**: LOW

**Tasks**:
1. ✅ Fix `GraphCmd` → `GraphArgs` typo (5 min)
   - File: `cli/src/cmds/mod.rs:40`
   - Change: One line

2. ✅ Fix domain/utils/doctor.rs (1 hour)
   - Add type exports to `cli/src/domain/utils/mod.rs`
   - Copy pattern from `domain/marketplace/mod.rs`
   - Types: `SystemCheck`, `SystemChecker`, `CheckStatus`, etc.

3. ✅ Fix domain/utils/env.rs (1 hour)
   - Add type exports to `cli/src/domain/utils/mod.rs`
   - Types: `EnvironmentManager`, `GgenEnvironment`, etc.

4. ✅ Add utils CLI wrapper (30 min)
   - File: `cli/src/cmds/utils.rs`
   - Pattern: Copy from `marketplace.rs` (already exists as stub)
   - Verbs: `doctor`

5. ✅ Build validation (30 min)
   - `cargo build --release`
   - `cargo test --lib`
   - Fix any remaining type mismatches

**Deliverables**:
- ✅ Clean build
- ✅ 6/9 command modules working (67%)
- ✅ All critical features (template, project, marketplace, AI)
- ✅ Production-ready binary

**What's Not Included**:
- Hook commands (not user-facing)
- CI commands (internal tooling)
- Audit commands (security scanning)

---

### Phase 2 (P1) - Extended Coverage [+2 hours]
**Goal**: Add hook and CI commands → 89% coverage
**Priority**: HIGH
**Risk**: LOW

**Tasks**:
1. ✅ Add hook CLI wrapper (1 hour)
   - File: `cli/src/cmds/hook.rs`
   - Verbs: `create`, `list`, `remove`, `monitor`
   - Pattern: Copy marketplace.rs structure

2. ✅ Add CI CLI wrapper (1 hour)
   - File: `cli/src/cmds/ci.rs`
   - Verbs: `workflow`
   - Pattern: Copy template.rs structure

**Deliverables**:
- ✅ 8/9 command modules (89%)
- ✅ Hook management commands
- ✅ CI workflow automation

**Dependencies**: Phase 1 complete

---

### Phase 3 (P2) - Full Coverage [+1 hour]
**Goal**: Add audit commands → 100% coverage
**Priority**: MEDIUM
**Risk**: LOW

**Tasks**:
1. ✅ Add audit CLI wrapper (1 hour)
   - File: `cli/src/cmds/audit.rs`
   - Verbs: `security`
   - Pattern: Copy AI.rs structure (single verb)

**Deliverables**:
- ✅ 9/9 command modules (100%)
- ✅ Security audit commands
- ✅ Complete v2 architecture

**Dependencies**: Phase 2 complete

---

## Reusable Patterns

### Pattern 1: Args Struct Template

```rust
// cli/src/cmds/{noun}.rs
use clap::Subcommand;
use ggen_utils::error::Result;
use crate::runtime;

#[derive(clap::Args, Debug)]
pub struct {Noun}Args {
    #[command(subcommand)]
    pub command: {Noun}Commands,
}

#[derive(Subcommand, Debug)]
pub enum {Noun}Commands {
    /// Verb description
    Verb(VerbArgs),
}

impl {Noun}Args {
    pub fn run(self) -> Result<()> {
        match self.command {
            {Noun}Commands::Verb(args) => runtime::execute(
                crate::domain::{noun}::verb::execute(args.into())
            ),
        }
    }
}
```

**LOC**: ~25 lines per noun module
**Time**: 15-30 min per module

### Pattern 2: Domain Layer Integration

```rust
// cli/src/domain/{noun}/verb.rs
use ggen_utils::error::Result;
use async_trait::async_trait;

pub struct VerbInput {
    // Fields from CLI args
}

pub async fn execute(input: VerbInput) -> Result<()> {
    // Business logic here
    Ok(())
}
```

**LOC**: 30-100 lines per verb (depends on complexity)
**Time**: 30 min - 2 hours per verb

### Pattern 3: Error Handling

```rust
// Avoid type alias confusion
use std::result::Result as StdResult;

// For validators that return String errors
fn validate_foo(s: &str) -> StdResult<(), String> {
    // ...
}

// For main execution that returns ggen errors
use ggen_utils::error::Result;

fn execute() -> Result<()> {
    // ...
}
```

---

## Validation Strategy

### Build Validation (Per Phase)

```bash
# Step 1: Clean build
cargo clean
cargo build --release 2>&1 | tee build.log

# Success criteria:
# - 0 compilation errors
# - Binary size ~8-10 MB
# - Build time <30 seconds (incremental)

# Step 2: Library tests
cargo test --lib 2>&1 | tee test-lib.log

# Success criteria:
# - 127+ tests passing
# - 0 failures
# - <10 seconds execution time
```

### Runtime Validation (Per Command)

```bash
# Template commands
./target/release/ggen template list
./target/release/ggen template new my-template
./target/release/ggen template generate --template foo

# Project commands
./target/release/ggen project new my-project
./target/release/ggen project gen --output ./out

# Marketplace commands
./target/release/ggen marketplace search rust
./target/release/ggen marketplace list

# AI commands
./target/release/ggen ai generate "create a REST API"

# Graph commands
./target/release/ggen graph query "SELECT ?s ?p ?o WHERE { ?s ?p ?o }"

# Utils commands (Phase 1+)
./target/release/ggen utils doctor

# Hook commands (Phase 2+)
./target/release/ggen hook list
./target/release/ggen hook create pre-commit

# CI commands (Phase 2+)
./target/release/ggen ci workflow validate
```

### Performance Validation

```bash
# Benchmark template generation
cargo bench --bench template_generation

# Success criteria (from prior validation):
# - 10 triples: 10-15ms
# - 100 triples: 15-25ms
# - 1,000 triples: 25-40ms
# - 10,000 triples: 35-50ms

# Runtime overhead benchmark
cargo bench --bench runtime_overhead

# Success criteria:
# - Async/sync bridging: <100ns
# - Function call overhead: <50ns
```

### E2E Validation

```bash
# RDF rendering pipeline
cargo test rdf_rendering_e2e

# Success criteria:
# - 10/11 tests passing (91%)
# - Core rendering validated
# - Error handling validated
# - Performance within SLOs
```

---

## Risk Assessment

### High-Risk Areas (None)

**None identified**. All errors are isolated to domain/utils layer with working reference implementations.

### Medium-Risk Areas

1. **Graph command type error**
   - Impact: Blocks graph noun entirely
   - Mitigation: Trivial fix (typo correction)
   - Likelihood: 100% fixable in <5 min

2. **Utils domain layer incomplete**
   - Impact: Blocks utils noun (doctor, env commands)
   - Mitigation: Copy pattern from marketplace
   - Likelihood: 90% fixable in 2-3 hours

### Low-Risk Areas

1. **Hook CLI wrapper missing**
   - Impact: Hook commands unavailable
   - Mitigation: Optional feature, not blocking v2.0.0
   - Likelihood: 100% fixable in 1 hour

2. **CI CLI wrapper missing**
   - Impact: CI commands unavailable
   - Mitigation: Internal tooling, not user-facing
   - Likelihood: 100% fixable in 1 hour

3. **Audit CLI wrapper missing**
   - Impact: Audit commands unavailable
   - Mitigation: Optional security feature
   - Likelihood: 100% fixable in 1 hour

### Risk Mitigation Strategy

**Rollback Plan** (if Phase 1 fails):
1. Revert utils domain changes
2. Remove utils from cmds/mod.rs enum
3. Ship v2.0.0 with 56% coverage (5/9 modules)
4. Mark utils, hook, ci, audit as "experimental" in v2.1.0

**Incremental Validation**:
- Build after each file change
- Test each verb after CLI wrapper added
- Commit after each successful module

---

## Timeline Estimates

### Phase 1 (P0) - Critical Path

```
Hour 1:
├─ 0:00-0:05 → Fix GraphCmd typo
├─ 0:05-0:35 → Fix domain/utils/doctor.rs exports
├─ 0:35-1:00 → Fix domain/utils/env.rs exports
└─ Milestone: Utils domain layer complete

Hour 2:
├─ 1:00-1:30 → Add cmds/utils.rs CLI wrapper
├─ 1:30-1:45 → Build validation (cargo build --release)
├─ 1:45-2:00 → Library test validation (cargo test --lib)
└─ Milestone: Clean build achieved

Hour 3:
├─ 2:00-2:30 → Runtime validation (test each command)
├─ 2:30-2:45 → Performance benchmarks
├─ 2:45-3:00 → Update RELEASE_NOTES.md
└─ Milestone: v2.0.0 PRODUCTION READY
```

**Total**: 3 hours
**Success Probability**: 95%

### Phase 2 (P1) - Extended

```
Hour 4:
├─ 0:00-1:00 → Add cmds/hook.rs CLI wrapper
└─ Milestone: Hook commands available

Hour 5:
├─ 0:00-1:00 → Add cmds/ci.rs CLI wrapper
└─ Milestone: CI commands available
```

**Total**: +2 hours (5 hours cumulative)
**Success Probability**: 90%

### Phase 3 (P2) - Complete

```
Hour 6:
├─ 0:00-1:00 → Add cmds/audit.rs CLI wrapper
└─ Milestone: 100% command coverage
```

**Total**: +1 hour (6 hours cumulative)
**Success Probability**: 85%

---

## Migration Templates

### Template: CLI Noun Module (cmds/{noun}.rs)

```rust
//! {Noun} command implementation
//!
//! Provides CLI interface for {description}.

use clap::{Args, Subcommand};
use ggen_utils::error::Result;
use crate::runtime;

#[derive(Args, Debug)]
pub struct {Noun}Args {
    #[command(subcommand)]
    pub command: {Noun}Commands,
}

#[derive(Subcommand, Debug)]
pub enum {Noun}Commands {
    /// {Verb1 description}
    Verb1(Verb1Args),

    /// {Verb2 description}
    Verb2(Verb2Args),
}

#[derive(Args, Debug)]
pub struct Verb1Args {
    /// Argument description
    #[arg(short, long)]
    pub arg1: Option<String>,
}

#[derive(Args, Debug)]
pub struct Verb2Args {
    /// Argument description
    #[arg(short, long)]
    pub arg2: Option<String>,
}

impl {Noun}Args {
    pub fn run(self) -> Result<()> {
        match self.command {
            {Noun}Commands::Verb1(args) => {
                runtime::execute(crate::domain::{noun}::verb1::execute(args.into()))
            }
            {Noun}Commands::Verb2(args) => {
                runtime::execute(crate::domain::{noun}::verb2::execute(args.into()))
            }
        }
    }
}
```

**Usage**:
1. Replace `{Noun}` with capitalized noun (e.g., `Utils`)
2. Replace `{noun}` with lowercase noun (e.g., `utils`)
3. Add verbs with descriptions
4. Update domain layer calls

**Time**: 15-20 minutes per noun

### Template: Domain Module Re-exports

```rust
// cli/src/domain/{noun}/mod.rs

pub mod verb1;
pub mod verb2;

// Re-export types for CLI layer
pub use verb1::{Verb1Input, Verb1Output};
pub use verb2::{Verb2Input, Verb2Output};

// Re-export common types
pub use types::{CommonType1, CommonType2};
```

**Usage**:
1. List all verb modules
2. Re-export Input/Output types
3. Re-export shared types

**Time**: 5-10 minutes per noun

---

## Testing Checklist

### Per-Command Testing

```
Template Commands:
□ ggen template list
□ ggen template new {name}
□ ggen template generate --template {name}
□ ggen template lint {path}
□ ggen template show {name}
□ ggen template regenerate
□ ggen template generate-tree

Marketplace Commands:
□ ggen marketplace search {query}
□ ggen marketplace install {package}
□ ggen marketplace list
□ ggen marketplace publish {path}
□ ggen marketplace update

AI Commands:
□ ggen ai generate {prompt}

Project Commands:
□ ggen project new {name}
□ ggen project gen
□ ggen project apply
□ ggen project plan
□ ggen project init

Graph Commands:
□ ggen graph load {file}
□ ggen graph query {sparql}
□ ggen graph export
□ ggen graph visualize

Utils Commands (Phase 1+):
□ ggen utils doctor
□ ggen utils env

Hook Commands (Phase 2+):
□ ggen hook list
□ ggen hook create {type}
□ ggen hook remove {id}
□ ggen hook monitor

CI Commands (Phase 2+):
□ ggen ci workflow {action}

Audit Commands (Phase 3+):
□ ggen audit security
```

### Acceptance Criteria

**Per Phase**:
- [ ] Cargo build succeeds with 0 errors
- [ ] Cargo test --lib passes with 0 failures
- [ ] All commands in phase execute without panic
- [ ] Help text displays correctly for each command
- [ ] Error messages are user-friendly

**Final (All Phases)**:
- [ ] E2E tests maintain 90%+ pass rate
- [ ] Performance benchmarks within SLOs
- [ ] Binary size <12 MB
- [ ] Documentation updated (RELEASE_NOTES.md)

---

## Success Metrics

### Phase 1 (P0) Success Criteria

| Metric | Target | Validation |
|--------|--------|------------|
| **Build Status** | 0 errors | `cargo build --release` |
| **Command Coverage** | 67% (6/9) | Help text for each noun |
| **Test Pass Rate** | 90%+ | `cargo test --lib` |
| **Critical Features** | 100% | Template, project, marketplace, AI working |
| **Performance** | <50ms template gen | `cargo bench` |
| **Binary Size** | <10 MB | `ls -lh target/release/ggen` |

### Phase 2 (P1) Success Criteria

| Metric | Target | Validation |
|--------|--------|------------|
| **Command Coverage** | 89% (8/9) | Hook + CI commands working |
| **Hook Management** | All 4 verbs | `ggen hook --help` |
| **CI Automation** | Workflow verb | `ggen ci --help` |

### Phase 3 (P2) Success Criteria

| Metric | Target | Validation |
|--------|--------|------------|
| **Command Coverage** | 100% (9/9) | All nouns available |
| **Security Audit** | Security verb | `ggen audit --help` |
| **Completeness** | v2 architecture complete | All layers validated |

---

## Coordination Hooks

### Pre-Migration
```bash
npx claude-flow@alpha hooks pre-task \
  --description "V2 Migration Strategy - Phase {N}"

npx claude-flow@alpha hooks session-restore \
  --session-id "v2-migration-swarm"
```

### During Migration
```bash
# After each file modified
npx claude-flow@alpha hooks post-edit \
  --file "{path}" \
  --memory-key "v2-migration/phase-{N}/{module}"

# After each module complete
npx claude-flow@alpha hooks notify \
  --message "{Noun} module migrated successfully"
```

### Post-Migration
```bash
npx claude-flow@alpha hooks post-task \
  --task-id "v2-migration-phase-{N}"

npx claude-flow@alpha hooks session-end \
  --export-metrics true
```

---

## References

- **Current Scorecard**: `.claude/refactor-v2/SCORECARD.md` (72/100 score)
- **Agent 7 Report**: `.claude/refactor-v2/agent7-entry-point.md` (clap-noun-verb migration)
- **Completion Report**: `.claude/refactor-v2/V2.0.0-FINAL-COMPLETION-REPORT.md` (validation results)
- **Domain Layer**: `cli/src/domain/` (8,619 LOC business logic)
- **CLI Layer**: `cli/src/cmds/` (1,052 LOC routing)
- **Runtime Bridge**: `cli/src/runtime.rs` (94 LOC async/sync)

---

## Appendix: Command Inventory

### Fully Migrated (5 modules)

1. **Template** (8 verbs)
   - list, new, generate, lint, show, regenerate, generate-tree, render-with-rdf

2. **Marketplace** (5 verbs)
   - search, install, list, publish, update

3. **AI** (1 verb)
   - generate

4. **Project** (5 verbs)
   - new, gen, apply, plan, init

5. **Graph** (4 verbs)
   - load, query, export, visualize

### Partially Migrated (1 module)

6. **Utils** (2 verbs - domain exists, CLI broken)
   - doctor, env

### Not Migrated (3 modules)

7. **Hook** (4 verbs - domain exists, no CLI)
   - create, list, remove, monitor

8. **CI** (1 verb - domain exists, no CLI)
   - workflow

9. **Audit** (1 verb - domain exists, no CLI)
   - security

---

**End of Strategy Document**

This strategy provides a clear, actionable plan to complete v2.0.0 migration using the 80/20 principle: Phase 1 delivers 67% functionality with 80% of user value in just 2-3 hours.
