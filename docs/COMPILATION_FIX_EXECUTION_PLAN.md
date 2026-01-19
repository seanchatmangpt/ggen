# SYSTEMATIC COMPILATION FIX EXECUTION PLAN

**Generated**: 2025-11-20
**Total Errors**: 259 compilation errors
**Strategy**: Hybrid 80/20 Parallel Execution (TRIZ Solution E)
**Estimated Time**: 195 minutes (3.25 hours) with parallelization
**Time Saved**: 75 minutes (28% reduction vs sequential)

---

## EXECUTIVE SUMMARY

This plan synthesizes FMEA (Failure Mode & Effects Analysis), TRIZ (Inventive Problem Solving), and ANDON (Stop-the-Line Quality) methodologies into a coordinated parallel execution strategy.

**Key Insights**:
- **FMEA**: Top RPN errors = 800 (Default traits), 630 (Kernel modules), 432 (Telemetry API)
- **TRIZ**: Solution E (Hybrid 80/20) = 5 parallel sub-tasks, 10 hours total → optimized to 3.25 hours
- **ANDON**: RED cord on ggen_cli_tests (180 errors), YELLOW on graph_tests (48 errors)

---

## PARALLEL EXECUTION ARCHITECTURE

```
┌────────────────────────────────────────────────────────────────┐
│                    WAVE 1: CRITICAL PATH                       │
│                      (90 minutes)                              │
├────────────────┬──────────────────┬───────────────────────────┤
│   Task 1a      │     Task 1b      │        Task 1c            │
│   (30 min)     │     (45 min)     │        (15 min)           │
│                │                  │                           │
│  Default       │  Kernel Module   │   Export from             │
│  Trait         │  Stubs           │   lib.rs                  │
│  Implementation│                  │                           │
│                │                  │                           │
│  FIX: 48 errors│  FIX: 100+ errors│   FIX: Unblocks           │
│  (graph_tests) │  (ggen_cli_tests)│        compilation        │
└────────────────┴──────────────────┴───────────────────────────┘
                           ↓
┌────────────────────────────────────────────────────────────────┐
│                    WAVE 2: SECONDARY ISSUES                    │
│                      (120 minutes - PARALLEL)                  │
├──────────────────┬─────────────────┬─────────────────────────┤
│    Task 2a       │     Task 2b     │       Task 2c           │
│    (45 min)      │     (45 min)    │       (30 min)          │
│                  │                 │                         │
│ Middleware/IO    │  Telemetry API  │   Contract Types        │
│ Type Stubs       │  Alignment      │                         │
│                  │                 │                         │
│ FIX: 40+ errors  │  FIX: 15 errors │   FIX: 3-15 errors      │
└──────────────────┴─────────────────┴─────────────────────────┘
                           ↓
┌────────────────────────────────────────────────────────────────┐
│                 WAVE 3: VALIDATION & CLEANUP                   │
│                      (60 minutes - SEQUENTIAL)                 │
├──────────────────┬─────────────────┬─────────────────────────┤
│    Task 3a       │     Task 3b     │       Task 3c           │
│    (15 min)      │     (30 min)    │       (15 min)          │
│                  │                 │                         │
│ Compile Check    │  Test Execution │   Documentation         │
│                  │                 │                         │
│ VERIFY: 0 errors │  CATEGORIZE:    │   DOCUMENT: Stubs       │
│                  │  test failures  │   vs implementations    │
└──────────────────┴─────────────────┴─────────────────────────┘
```

---

## CRITICAL PATH ANALYSIS

**Sequential Execution (No Parallelization)**:
```
1a (30) → 1b (45) → 1c (15) → 2a (45) → 2b (45) → 2c (30) → 3a (15) → 3b (30) → 3c (15)
= 270 minutes (4.5 hours)
```

**Parallel Execution (Optimized)**:
```
1a (30) → 1b (45) → 1c (15) → max(2a,2b,2c) (45) → 3a (15) → 3b (30) → 3c (15)
= 195 minutes (3.25 hours)
```

**Critical Path**: `1a → 1b → 1c → 3a` = **105 minutes (1.75 hours)**

**Parallelization Savings**: **75 minutes (28% faster)**

---

## WAVE 1: UNBLOCK CRITICAL PATH (90 minutes)

### Task 1a: Default Trait Implementation (30 minutes)
**Agent**: `coder`
**Priority**: CRITICAL (RPN = 800)
**ANDON Signal**: YELLOW (graph_tests.rs)

**Objective**: Implement `Default` trait for `OutputSchema` and `InputSchema`

**Files to Modify**:
- `crates/ggen-domain/src/graph/types.rs` (or wherever schema types are defined)

**Implementation**:
```rust
#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct OutputSchema {
    // existing fields...
}

#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct InputSchema {
    // existing fields...
}
```

**Verification**:
```bash
cargo make check
# EXPECTED: 48 fewer errors in graph_tests.rs
```

**Dependencies**: None (can run in parallel)

**Fixes**: 48 errors in `graph_tests.rs`

---

### Task 1b: Kernel Module Stubs (45 minutes)
**Agent**: `code-analyzer`
**Priority**: CRITICAL (RPN = 630)
**ANDON Signal**: RED (ggen_cli_tests.rs - 180 errors)

**Objective**: Create stub modules for kernel subsystem

**Files to Create**:
1. `crates/ggen-domain/src/kernel/mod.rs`
2. `crates/ggen-domain/src/kernel/session.rs`
3. `crates/ggen-domain/src/kernel/attestation.rs`
4. `crates/ggen-domain/src/kernel/quotas.rs`
5. `crates/ggen-domain/src/kernel/capability.rs`

**Implementation Strategy**:
1. Extract type signatures from test files
2. Create minimal stub types with `#[derive(Debug, Clone)]`
3. Implement stub methods that return `Ok(())` or default values
4. Mark with `#[allow(dead_code)]` for now

**Example Stub** (session.rs):
```rust
use crate::{Error, Result};
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Session {
    pub id: String,
    // Add fields as needed from test expectations
}

impl Session {
    pub fn new(id: impl Into<String>) -> Self {
        Self { id: id.into() }
    }
}

#[derive(Debug, Clone)]
pub struct SessionManager {
    // Stub fields
}

impl SessionManager {
    pub fn new() -> Result<Self> {
        Ok(Self {})
    }

    pub fn create_session(&self, _id: &str) -> Result<Session> {
        Ok(Session::new(_id))
    }

    pub fn get_session(&self, _id: &str) -> Result<Option<Session>> {
        Ok(None)
    }
}
```

**Verification**:
```bash
cargo make check
# EXPECTED: 100+ fewer errors in ggen_cli_tests.rs
```

**Dependencies**: Task 1a must complete first (lib.rs exports depend on types existing)

**Fixes**: 100+ errors in `ggen_cli_tests.rs`

---

### Task 1c: Export from lib.rs (15 minutes)
**Agent**: `reviewer`
**Priority**: CRITICAL (Unblocks compilation)
**ANDON Signal**: GREEN (enables other modules)

**Objective**: Export kernel modules and verify module tree

**Files to Modify**:
- `crates/ggen-domain/src/lib.rs`

**Implementation**:
```rust
// Add to lib.rs
pub mod kernel;

// Re-export commonly used kernel types
pub use kernel::{
    Session, SessionManager,
    Attestation, AttestationManager,
    QuotaManager, ResourceQuota,
    Capability, CapabilityManager,
};
```

**Verification**:
```bash
cargo make check
# EXPECTED: All kernel types now resolve
```

**Dependencies**: Tasks 1a and 1b must complete

**Fixes**: Enables 1a and 1b to fully compile

---

## WAVE 2: RESOLVE SECONDARY ISSUES (120 minutes - PARALLEL)

### Task 2a: Middleware/IO Type Stubs (45 minutes)
**Agent**: `coder`
**Priority**: HIGH (RPN = 432)
**ANDON Signal**: YELLOW

**Objective**: Create middleware and IO abstraction stubs

**Files to Create/Modify**:
- `crates/ggen-domain/src/middleware.rs`
- `crates/ggen-domain/src/io.rs`

**Types to Implement**:
```rust
// middleware.rs
pub struct MiddlewareChain {
    // Stub
}

pub struct MiddlewareExecutor {
    // Stub
}

// io.rs
pub trait AsyncReader {
    // Stub methods
}

pub trait AsyncWriter {
    // Stub methods
}

pub struct BufferedIO {
    // Stub
}

pub struct StreamProcessor {
    // Stub
}
```

**Verification**:
```bash
cargo make check
# EXPECTED: 40+ fewer errors
```

**Dependencies**: Wave 1 completion

**Fixes**: 40+ errors

---

### Task 2b: Telemetry API Alignment (45 minutes)
**Agent**: `code-analyzer`
**Priority**: HIGH (RPN = 280)
**ANDON Signal**: YELLOW

**Objective**: Fix `TelemetryManager::new()` and `Span::new()` signatures to match test expectations

**Files to Modify**:
- `crates/ggen-core/src/telemetry.rs` (or wherever telemetry is defined)

**Strategy**:
1. Extract exact signatures from test files
2. Update `TelemetryManager::new()` to match
3. Update `Span::new()` to match
4. Ensure return types align

**Example Fix**:
```rust
// Before: pub fn new() -> Self
// After:  pub fn new(config: TelemetryConfig) -> Result<Self>

impl TelemetryManager {
    pub fn new(config: TelemetryConfig) -> Result<Self> {
        // Implementation...
    }
}
```

**Verification**:
```bash
cargo make check
# EXPECTED: 15 fewer errors related to telemetry
```

**Dependencies**: Wave 1 completion

**Fixes**: 15 errors

---

### Task 2c: Contract Types (30 minutes)
**Agent**: `coder`
**Priority**: MEDIUM (RPN = 400)
**ANDON Signal**: YELLOW

**Objective**: Implement missing contract types

**Files to Modify**:
- `crates/ggen-domain/src/ahi_contract.rs`

**Types to Implement**:
```rust
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AvailableResources {
    pub cpu: f64,
    pub memory: u64,
    pub disk: u64,
    // Add fields as needed
}

impl Default for AvailableResources {
    fn default() -> Self {
        Self {
            cpu: 1.0,
            memory: 1024,
            disk: 10240,
        }
    }
}
```

**Verification**:
```bash
cargo make check
# EXPECTED: 3-15 fewer errors
```

**Dependencies**: Wave 1 completion

**Fixes**: 3-15 errors

---

## WAVE 3: VALIDATION & CLEANUP (60 minutes - SEQUENTIAL)

### Task 3a: Compile Check (15 minutes)
**Agent**: `production-validator`
**Priority**: CRITICAL
**ANDON Signal**: RED if any errors remain

**Objective**: Verify zero compilation errors

**Commands**:
```bash
cargo make clean
cargo make check
cargo make lint
```

**Success Criteria**:
- ✅ `cargo make check` returns exit code 0
- ✅ Zero `error[E...]` patterns in output
- ✅ All 259 errors resolved

**If Failures Detected**:
1. Run `cargo make check 2>&1 | grep "error\[E"` to extract remaining errors
2. Create detailed todos for systematic fixing
3. Categorize by module and severity
4. Return to appropriate wave for fixes

**Dependencies**: Wave 2 completion

**Fixes**: Ensures all 259 errors resolved

---

### Task 3b: Test Execution (30 minutes)
**Agent**: `tester`
**Priority**: HIGH
**ANDON Signal**: YELLOW for test failures (vs RED for compilation)

**Objective**: Execute tests and categorize failures

**Commands**:
```bash
cargo make test 2>&1 | tee test_results.log
```

**Analysis Tasks**:
1. **Separate compilation vs test failures**:
   - Compilation failures = RED (must fix immediately)
   - Test logic failures = YELLOW (document and queue)

2. **Categorize test failures**:
   - Unit test failures (fast feedback)
   - Integration test failures (cross-module)
   - Property test failures (edge cases)

3. **Root cause analysis**:
   - Why did test fail? (expected vs actual)
   - Is stub implementation insufficient?
   - Is test expectation incorrect?

**Deliverable**:
- `docs/TEST_FAILURE_ANALYSIS.md` with categorized failures
- Priority queue for fixing test logic (post-compilation)

**Dependencies**: Task 3a completion (compilation must succeed)

**Fixes**: Separates compilation from test logic issues

---

### Task 3c: Feature Gate Documentation (15 minutes)
**Agent**: `reviewer`
**Priority**: MEDIUM
**ANDON Signal**: GREEN (documentation)

**Objective**: Document which modules are stubs vs fully implemented

**Deliverable**: `docs/COMPILATION_FIX_SUMMARY.md`

**Contents**:
```markdown
# Compilation Fix Summary

**Date**: 2025-11-20
**Total Errors Fixed**: 259
**Time Taken**: [actual time]
**Strategy**: Hybrid 80/20 Parallel Execution

## Module Status

### ✅ Fully Implemented
- graph (with Default traits)
- [list modules]

### ⚠️ Stub Implementations (Minimal Viable)
- kernel/session
- kernel/attestation
- kernel/quotas
- kernel/capability
- middleware
- io

### 📋 Implementation Roadmap

**Phase 1** (Next Sprint):
- kernel/session: Full session lifecycle management
- kernel/attestation: Cryptographic attestation

**Phase 2** (Future):
- middleware: Composable middleware chains
- io: Async I/O abstractions

## Test Results

**Total Tests**: [count]
**Passing**: [count]
**Failing**: [count]

**Failure Categories**:
- Stub limitations: [count]
- Test logic issues: [count]
- Integration issues: [count]
```

**Dependencies**: Task 3b completion

**Fixes**: Prevents future confusion about module maturity

---

## EXECUTION CHECKLIST

### Pre-Execution (5 minutes)
- [ ] All agents available and ready
- [ ] File locations confirmed
- [ ] Communication channels open (memory store)
- [ ] Backup created: `git stash push -m "pre-fix-backup"`

### Wave 1 Execution (90 minutes)
**Task 1a: Default Traits**
- [ ] Located schema type definitions
- [ ] Added `#[derive(Default)]` to OutputSchema
- [ ] Added `#[derive(Default)]` to InputSchema
- [ ] Added any other schema types requiring Default
- [ ] Verified: `cargo make check` shows 48 fewer errors
- [ ] Status stored in memory: `swarm/wave1/task1a/status`

**Task 1b: Kernel Stubs**
- [ ] Created `crates/ggen-domain/src/kernel/mod.rs`
- [ ] Created `crates/ggen-domain/src/kernel/session.rs` with Session, SessionManager
- [ ] Created `crates/ggen-domain/src/kernel/attestation.rs` with Attestation, AttestationManager
- [ ] Created `crates/ggen-domain/src/kernel/quotas.rs` with QuotaManager, ResourceQuota
- [ ] Created `crates/ggen-domain/src/kernel/capability.rs` with Capability, CapabilityManager
- [ ] All stubs have `#[derive(Debug, Clone)]`
- [ ] All stubs have stub methods returning `Ok(())` or defaults
- [ ] Verified: `cargo make check` shows 100+ fewer errors
- [ ] Status stored in memory: `swarm/wave1/task1b/status`

**Task 1c: lib.rs Exports**
- [ ] Added `pub mod kernel;` to lib.rs
- [ ] Added `pub use kernel::*;` to lib.rs
- [ ] Verified: `cargo make check` passes cleanly
- [ ] Status stored in memory: `swarm/wave1/task1c/status`

**Wave 1 Checkpoint**:
- [ ] All Wave 1 tasks complete
- [ ] `cargo make check` runs successfully
- [ ] Error count reduced by ~148 errors (48 + 100)
- [ ] No new errors introduced

### Wave 2 Execution (120 minutes - PARALLEL)
**Task 2a: Middleware/IO Stubs**
- [ ] Created `crates/ggen-domain/src/middleware.rs`
- [ ] Implemented MiddlewareChain, MiddlewareExecutor
- [ ] Created `crates/ggen-domain/src/io.rs`
- [ ] Implemented AsyncReader, AsyncWriter, BufferedIO, StreamProcessor
- [ ] Added exports to lib.rs
- [ ] Verified: `cargo make check` shows 40+ fewer errors
- [ ] Status stored in memory: `swarm/wave2/task2a/status`

**Task 2b: Telemetry API**
- [ ] Located TelemetryManager definition
- [ ] Updated `TelemetryManager::new()` signature to match tests
- [ ] Located Span definition
- [ ] Updated `Span::new()` signature to match tests
- [ ] Verified: `cargo make check` shows 15 fewer errors
- [ ] Status stored in memory: `swarm/wave2/task2b/status`

**Task 2c: Contract Types**
- [ ] Located ahi_contract.rs
- [ ] Implemented AvailableResources struct
- [ ] Implemented Default for AvailableResources
- [ ] Added any missing contract fields
- [ ] Verified: `cargo make check` shows 3-15 fewer errors
- [ ] Status stored in memory: `swarm/wave2/task2c/status`

**Wave 2 Checkpoint**:
- [ ] All Wave 2 tasks complete
- [ ] `cargo make check` runs successfully
- [ ] Error count reduced by ~58-73 errors (40 + 15 + 3-15)
- [ ] No new errors introduced

### Wave 3 Execution (60 minutes - SEQUENTIAL)
**Task 3a: Compile Check**
- [ ] Ran `cargo make clean`
- [ ] Ran `cargo make check`
- [ ] Exit code = 0 (success)
- [ ] Zero `error[E...]` patterns in output
- [ ] All 259 errors resolved
- [ ] If failures: Created detailed todos for systematic fixing
- [ ] Status stored in memory: `swarm/wave3/task3a/status`

**Task 3b: Test Execution**
- [ ] Ran `cargo make test 2>&1 | tee test_results.log`
- [ ] Tests execute successfully (no compilation failures)
- [ ] Categorized test failures:
  - [ ] Unit test failures: [count]
  - [ ] Integration test failures: [count]
  - [ ] Property test failures: [count]
- [ ] Root cause analysis completed
- [ ] Created `docs/TEST_FAILURE_ANALYSIS.md`
- [ ] Status stored in memory: `swarm/wave3/task3b/status`

**Task 3c: Documentation**
- [ ] Created `docs/COMPILATION_FIX_SUMMARY.md`
- [ ] Documented stub modules
- [ ] Documented fully implemented modules
- [ ] Created implementation roadmap
- [ ] Status stored in memory: `swarm/wave3/task3c/status`

**Wave 3 Checkpoint**:
- [ ] All Wave 3 tasks complete
- [ ] Compilation succeeds
- [ ] Tests executable (may have logical failures)
- [ ] Documentation complete

### Final Validation
- [ ] All 259 compilation errors resolved
- [ ] Tests executable (no compilation blockers)
- [ ] Actual test failures visible and categorized
- [ ] Path forward documented
- [ ] Total time: < 4 hours
- [ ] Git commit created with summary

---

## SUCCESS CRITERIA

### Must Have (CRITICAL)
- ✅ **259 compilation errors resolved** (`cargo make check` exit code 0)
- ✅ **Tests executable** (no compilation failures preventing test runs)
- ✅ **Actual test failures visible** (can distinguish compilation vs logic issues)
- ✅ **Path forward documented** (COMPILATION_FIX_SUMMARY.md exists)
- ✅ **Total time < 4 hours** (efficiency requirement met)

### Should Have (HIGH PRIORITY)
- ✅ **All stubs marked clearly** (documentation prevents confusion)
- ✅ **Implementation roadmap created** (next steps clear)
- ✅ **Root cause analysis for test failures** (prevents recurrence)
- ✅ **Memory storage of all statuses** (swarm coordination)

### Nice to Have (OPTIONAL)
- ✅ **Zero clippy warnings** (code quality)
- ✅ **All tests passing** (100% green)
- ✅ **Performance benchmarks** (regression detection)

---

## RISK MITIGATION

### Risk 1: Dependencies Between Waves
**Impact**: HIGH
**Probability**: MEDIUM
**Mitigation**:
- Task 1c MUST verify 1a/1b complete before running
- Checkpoint with `cargo make check` after each wave
- Store completion status in memory for coordination

**Validation**:
```bash
# Before Task 1c runs:
test -f crates/ggen-domain/src/kernel/session.rs || exit 1
test -f crates/ggen-domain/src/kernel/attestation.rs || exit 1
# etc.
```

### Risk 2: Stub Types Have Wrong Signatures
**Impact**: MEDIUM
**Probability**: HIGH
**Mitigation**:
- Extract exact signatures from test files before implementing
- Compare test expectations vs stub definitions
- Iterate with small feedback loops (compile after each type)

**Validation**:
```bash
# After each stub creation:
cargo make check 2>&1 | grep "expected.*found" | head -10
# Reveals signature mismatches
```

### Risk 3: New Errors Discovered During Fixes
**Impact**: MEDIUM
**Probability**: MEDIUM
**Mitigation**:
- Document all new errors immediately
- Add to queue with priority
- Run `cargo check` after each wave to detect cascading errors

**Validation**:
```bash
# After each wave:
ERROR_COUNT=$(cargo make check 2>&1 | grep -c "error\[E")
echo "Remaining errors: $ERROR_COUNT"
# Track delta to ensure progress
```

### Risk 4: Parallel Tasks Introduce Merge Conflicts
**Impact**: LOW
**Probability**: LOW
**Mitigation**:
- Tasks 2a, 2b, 2c work on different files
- No overlapping module modifications
- Sequential Wave 3 prevents race conditions

**Validation**:
- File assignment matrix (no overlaps)
- Git branch per task (if needed)

### Risk 5: Time Estimates Are Optimistic
**Impact**: MEDIUM
**Probability**: MEDIUM
**Mitigation**:
- 20% time buffer built into estimates
- Critical path clearly identified (can skip optional tasks)
- Checkpoint after each wave (can reassess)

**Validation**:
- Track actual time vs estimated at each checkpoint
- Adjust remaining estimates based on actuals

---

## DEPENDENCY GRAPH (Visual)

```
        ┌─────────────────────────────────────────┐
        │         START (t=0)                     │
        └───────────┬─────────────────────────────┘
                    │
        ┌───────────┴───────────┐
        │                       │
        ▼                       ▼
  ┌──────────┐          ┌──────────┐
  │   1a     │          │   1b     │ (can run parallel if no shared files)
  │ (30 min) │          │ (45 min) │
  └────┬─────┘          └────┬─────┘
       │                     │
       └─────────┬───────────┘
                 │
                 ▼
           ┌──────────┐
           │   1c     │
           │ (15 min) │
           └────┬─────┘
                │
    ┌───────────┼───────────┐
    │           │           │
    ▼           ▼           ▼
┌────────┐  ┌────────┐  ┌────────┐
│  2a    │  │  2b    │  │  2c    │ (PARALLEL)
│(45 min)│  │(45 min)│  │(30 min)│
└───┬────┘  └───┬────┘  └───┬────┘
    │           │           │
    └───────────┼───────────┘
                │
                ▼
          ┌──────────┐
          │   3a     │
          │ (15 min) │
          └────┬─────┘
               │
               ▼
          ┌──────────┐
          │   3b     │
          │ (30 min) │
          └────┬─────┘
               │
               ▼
          ┌──────────┐
          │   3c     │
          │ (15 min) │
          └────┬─────┘
               │
               ▼
        ┌──────────────┐
        │   SUCCESS    │
        └──────────────┘
```

**Critical Path** (cannot be parallelized):
`1a (30) → 1b (45) → 1c (15) → 3a (15) = 105 minutes`

**Parallel Opportunities**:
- Wave 2: `max(2a, 2b, 2c) = 45 minutes` (vs 120 sequential)
- Saves: 75 minutes

---

## AGENT ASSIGNMENTS

| Wave | Task | Agent | Specialization | Duration |
|------|------|-------|----------------|----------|
| 1 | 1a | `coder` | Trait implementation | 30 min |
| 1 | 1b | `code-analyzer` | Module architecture | 45 min |
| 1 | 1c | `reviewer` | Export verification | 15 min |
| 2 | 2a | `coder` | Type stubs | 45 min |
| 2 | 2b | `code-analyzer` | API alignment | 45 min |
| 2 | 2c | `coder` | Contract types | 30 min |
| 3 | 3a | `production-validator` | Compilation check | 15 min |
| 3 | 3b | `tester` | Test execution | 30 min |
| 3 | 3c | `reviewer` | Documentation | 15 min |

---

## MEMORY COORDINATION KEYS

All agents store status in memory for swarm coordination:

| Key | Description | Format |
|-----|-------------|--------|
| `swarm/orchestrator/execution-plan` | Full execution plan | JSON |
| `swarm/wave1/task1a/status` | Task 1a completion status | `{complete: bool, errors_fixed: int, time_taken: int}` |
| `swarm/wave1/task1b/status` | Task 1b completion status | `{complete: bool, errors_fixed: int, time_taken: int}` |
| `swarm/wave1/task1c/status` | Task 1c completion status | `{complete: bool, errors_fixed: int, time_taken: int}` |
| `swarm/wave2/task2a/status` | Task 2a completion status | `{complete: bool, errors_fixed: int, time_taken: int}` |
| `swarm/wave2/task2b/status` | Task 2b completion status | `{complete: bool, errors_fixed: int, time_taken: int}` |
| `swarm/wave2/task2c/status` | Task 2c completion status | `{complete: bool, errors_fixed: int, time_taken: int}` |
| `swarm/wave3/task3a/status` | Task 3a completion status | `{complete: bool, errors_remaining: int, time_taken: int}` |
| `swarm/wave3/task3b/status` | Task 3b completion status | `{complete: bool, test_failures: int, time_taken: int}` |
| `swarm/wave3/task3c/status` | Task 3c completion status | `{complete: bool, docs_created: bool, time_taken: int}` |

---

## POST-EXECUTION ANALYSIS

After completion, capture metrics for continuous improvement:

### Performance Metrics
- **Total time**: [actual] vs 195 minutes (estimated)
- **Error resolution rate**: 259 errors / [actual time] = [errors/hour]
- **Parallelization efficiency**: [actual parallel time] / [sequential time]
- **Checkpoint overhead**: Time spent on validation vs implementation

### Quality Metrics
- **Defect escape rate**: New errors introduced / 259 fixed
- **Rework rate**: Fixes that required re-fixing
- **Test coverage**: Tests executable / total tests
- **Documentation completeness**: Stubs documented / total stubs

### Process Insights
- **Which estimates were accurate?**
- **Which tasks took longer than expected? Why?**
- **Did parallel execution work as planned?**
- **What would we do differently next time?**

**Store in**: `swarm/post-execution/analysis`

---

## NEXT STEPS (After Compilation Fixed)

1. **Address Test Failures** (from Task 3b analysis)
   - Prioritize by impact and effort
   - Use Chicago TDD for fixes
   - Verify with `cargo make test`

2. **Implement Stub Modules** (from Task 3c roadmap)
   - kernel/session (highest priority)
   - kernel/attestation
   - middleware
   - io

3. **Performance Optimization** (if SLOs not met)
   - Run `cargo make slo-check`
   - Benchmark critical paths
   - Optimize hot code

4. **Security Audit** (before release)
   - Run `cargo make audit`
   - Fix vulnerabilities
   - Update dependencies

---

## APPENDIX A: COMMAND REFERENCE

### Build Commands (ALWAYS USE `cargo make`)
```bash
cargo make timeout-check    # Verify timeout command exists
cargo make check           # Quick compilation check (5s timeout)
cargo make lint            # Clippy linting
cargo make test            # All tests (10s unit + 30s integration)
cargo make test-unit       # Unit tests only (10s timeout)
cargo make slo-check       # Verify performance SLOs
cargo make audit           # Security vulnerability checks
cargo make ci              # Full CI pipeline
```

### Diagnostic Commands
```bash
# Count remaining errors
cargo make check 2>&1 | grep -c "error\[E"

# Extract error details
cargo make check 2>&1 | grep "error\[E" | head -20

# Find specific error types
cargo make check 2>&1 | grep "cannot find type"
cargo make check 2>&1 | grep "trait.*not implemented"

# Test execution with categorization
cargo make test 2>&1 | tee test_results.log
grep "FAILED" test_results.log
grep "test result:" test_results.log
```

---

## APPENDIX B: ANDON SIGNAL REFERENCE

### Signal Interpretation

| Color | Meaning | Action | Example |
|-------|---------|--------|---------|
| 🔴 RED | CRITICAL - Stop immediately | Fix before proceeding | Compilation errors, test crashes |
| 🟡 YELLOW | HIGH - Should stop | Investigate and fix | Warnings, test failures |
| 🟢 GREEN | OK - Continue | Monitor | Passing tests, clean compilation |

### Signal Workflow
1. **Monitor**: Run checks continuously (`cargo make check`, `cargo make test`)
2. **Stop**: When signal appears, immediately stop current work
3. **Investigate**: Use root cause analysis (5 Whys) to understand signal
4. **Fix**: Address root cause, not just symptom
5. **Verify**: Re-run checks to confirm signal cleared

**Never proceed with RED or YELLOW signals present.**

---

## APPENDIX C: TYPE SIGNATURE EXTRACTION GUIDE

When creating stubs, extract exact signatures from test usage:

```bash
# Find how Session is used in tests
grep -r "Session::" crates/*/tests/ -A 2 -B 2

# Find how SessionManager is instantiated
grep -r "SessionManager::new" crates/*/tests/ -A 2 -B 2

# Find struct field expectations
grep -r "session\." crates/*/tests/ | grep -o "session\.[a-z_]*"
```

**Compare test expectations vs stub definitions to ensure alignment.**

---

**END OF EXECUTION PLAN**
