# Build Optimization Agent Orchestration Plan (10-Agent Parallel Swarm)

**Date**: 2026-01-25
**Project**: ggen v6.0.0 (30-crate Rust workspace)
**Status**: Ready for parallel execution
**SLO Targets**: First build ≤15s, incremental ≤2s, RDF ≤5s/1k triples

---

## Executive Summary

**Current State**: 🔴 CRITICAL - Build times 8-24x over SLO, artifact lock contention blocking operations
- Compilation: ~120s+ vs 5-10s target
- Memory: 1.5-2 GB vs 100 MB target
- File locks: Preventing parallel execution

**Optimization Strategy**: 10-agent parallel swarm with coordinated handoffs and conflict detection
- **Phase 1**: Specification closure & baseline measurement (3 agents)
- **Phase 2**: Parallel implementations (7 agents with synchronized start)
- **Phase 3**: Integration, validation, verification (all agents)

**Expected Outcome**: 3-5x build speedup (120s → 30-40s baseline, with path to 15s)

---

## 10 Agent Responsibilities & Specialization

### Tier 1: Specification & Analysis (Foundation Layer)

#### 1. **Specification Validator** (`specification` agent)
**Role**: Define optimization requirements and success criteria
**Dependencies**: None (runs first)
**Deliverables**:
- Specification closure checklist (100% coverage in `.specify/`)
- Performance baseline requirements (pre-optimization metrics)
- Gate acceptance criteria for all Phase 2 agents
- Risk assessment for changes

**Synchronization Points**:
- Gate #1: After spec closure → signals Phase 2 start
- Gate #2: After baseline measurement → provides before/after comparison framework

**Handoff to**: Performance Benchmarker (baseline metrics), System Architect (design constraints)

**Key Responsibilities**:
```
1. Verify .specify/specs/NNN-optimize-build-times/ exists with TTL closure
2. Document success metrics:
   - File lock contention metric (target: 0% blocking)
   - Memory usage peak (target: ≤100 MB)
   - Build time (target: first ≤15s, incremental ≤2s)
   - Compilation speedup factor (target: 3-5x)
3. Identify all 3 critical bottlenecks in specification
4. Create acceptance gates for each bottleneck:
   - Bottleneck #1: Artifact lock isolation (40-60% improvement potential)
   - Bottleneck #2: Memory optimization (30-50% improvement)
   - Bottleneck #3: Workspace coupling (30-40% improvement)
5. Generate TTL triples for each optimization approach
6. Produce specification closure report
```

---

#### 2. **Performance Benchmarker** (`performance-benchmarker` agent)
**Role**: Establish baseline metrics and measure improvements
**Dependencies**: Specification Validator (uses baseline requirements)
**Runs Concurrently With**: System Architect, Code Analyzer
**Deliverables**:
- Baseline measurements (clean build, incremental, memory peak)
- Benchmark infrastructure setup (Criterion, JSON output)
- Pre/post comparison framework
- Performance regression detection thresholds

**Synchronization Points**:
- Pre-Phase 2: Provide baseline for comparison
- Post-Phase 2: Collect post-optimization metrics
- Final gate: Verify 3-5x improvement achieved

**Handoff to**: All Phase 2 agents (baseline context), Performance Validator (pre/post data)

**Key Responsibilities**:
```
1. Run clean build baseline with artifact locks present:
   - cargo clean && cargo check --workspace (measure time, file locks, memory)
   - Document: compilation time, lock contention events, peak memory
2. Establish incremental baseline (single-file change):
   - Modify single file in ggen-core
   - Measure rebuild time and memory usage
3. Profile RDF processing SLO:
   - Run benches/comprehensive_slo_benchmarks.rs
   - Validate RDF load <1s, SPARQL <100ms, cache <5ms
4. Create baseline report with:
   - Compilation timeline (which crates, in what order, duration)
   - File lock contention frequency (count blocking events)
   - Memory usage per rustc process
   - CPU utilization patterns
5. Generate Criterion HTML report (JSON baseline for comparison)
6. Identify 5 crates contributing most to build time
7. Store baseline in: .ggen/benchmarks/baseline-2026-01-25.json
```

---

#### 3. **System Architect** (`system-architect` agent)
**Role**: Design optimization strategy and architecture changes
**Dependencies**: Specification Validator (requirements)
**Runs Concurrently With**: Performance Benchmarker, Code Analyzer
**Deliverables**:
- Optimization architecture design (Phase 2 blueprint)
- Crate reorganization recommendations (if applicable)
- Makefile.toml changes specification
- Cargo.toml feature-gating strategy
- Risk mitigation for breaking changes

**Synchronization Points**:
- Provide design → Phase 2 agents use design as spec
- Collision detection point: Verify no conflicting Makefile changes

**Handoff to**: Build Config Optimizer, Incremental Build Optimizer, Dependency Optimizer

**Key Responsibilities**:
```
1. Design artifact lock mitigation:
   - CARGO_TARGET_DIR isolation strategy
   - Per-task target directory naming
   - Cleanup/GC strategy for /tmp/cargo-target-* directories
   - Mutex/coordination approach (if needed)
2. Design memory optimization approach:
   - Evaluate split-debuginfo=packed impact
   - Analyze thin LTO vs full LTO tradeoff
   - Identify heavy dependencies (ring, tera, rustls, config)
   - Feature-gating strategy for optional heavy deps
3. Design workspace coupling reduction:
   - Analyze ggen-utils/ggen-config as bridge crates
   - Evaluate crate isolation boundaries
   - Consider splitting high-frequency change crates
   - Workspace exclude/include patterns
4. Create Phase 2 agent assignments based on design:
   - Which crates to optimize (sort by memory/lock impact)
   - Which Makefile targets to modify
   - Which Cargo.toml features to change
5. Design rollback strategy (if optimization breaks something)
6. Identify potential conflicts:
   - Makefile changes that might conflict
   - Feature flag combinations that might be incompatible
   - Crate reorganization impact on dependent crates
7. Produce architecture document with diagrams
```

---

### Tier 2: Analysis & Detection (Validation Layer)

#### 4. **Code Analyzer** (`code-analyzer` agent)
**Role**: Analyze codebase for optimization opportunities
**Dependencies**: Specification Validator
**Runs Concurrently With**: Performance Benchmarker, System Architect
**Deliverables**:
- Compilation dependency analysis
- Crate coupling metrics
- Optimization opportunity identification
- Technical debt assessment for optimization changes

**Synchronization Points**:
- Pre-Phase 2: Provide codebase insights for prioritization
- Collision detection: Identify conflicts in optimization approaches

**Handoff to**: Build Config Optimizer, Code Optimizer, Test Optimizer

**Key Responsibilities**:
```
1. Analyze Cargo.toml dependency graph:
   - Generate dependency tree focusing on lock contention paths
   - Identify "bridge" crates (dependencies of many others)
   - Measure graph depth and fanout (which crates change most frequently)
2. Profile workspace structure:
   - Count transitive dependencies per crate
   - Identify optional features that could be feature-gated
   - Analyze heavy compile-time dependencies (ring, tera, rustls, config)
3. Detect optimization conflicts:
   - Which crates can be optimized independently
   - Which optimizations might conflict (e.g., different LTO strategies)
   - Feature flag combinations that need coordination
4. Identify compiler settings opportunities:
   - Current codegen-units setting (evaluate impact)
   - Debug symbol handling (split-debuginfo potential)
   - LTO strategy review
5. Analyze test dependencies:
   - Which test crates contribute most to build time
   - Opportunities for parallel test isolation
6. Produce codebase analysis report:
   - Dependency graph visualization (top 20 edges by weight)
   - Crate compilation time contributions (estimated)
   - Feature-gating opportunities ranked by effort/impact
```

---

#### 5. **Conflict Detector** (internal role - handled by Task Orchestrator)
**Monitoring During Parallel Phase 2**:
- Makefile.toml changes (detect overlapping modifications)
- Cargo.toml feature flag changes (ensure consistency)
- Crate organization changes (validate against design)
- Target directory naming (ensure unique isolation)

---

### Tier 3: Implementation (Execution Layer)

#### 6. **Build Configuration Optimizer** (`coder` agent specialization)
**Role**: Optimize Makefile.toml and Cargo.toml settings
**Dependencies**: System Architect (design), Code Analyzer (codebase insights)
**Synchronization Points**:
- Start: After design provided by System Architect
- Checkpoint #1: Verify no Makefile conflicts with other agents
- Checkpoint #2: Verify Cargo.toml feature flags coordinated

**Deliverables**:
- Modified Makefile.toml with optimized targets
- Cargo.toml feature-gating changes
- CARGO_TARGET_DIR isolation implementation
- Compiler flags optimization (debuginfo, LTO, codegen-units)

**Key Responsibilities**:
```
1. Implement CARGO_TARGET_DIR isolation in Makefile.toml:
   - Add per-task env var: CARGO_TARGET_DIR=/tmp/cargo-target-${RANDOM}
   - Ensure cleanup/GC in post-task hooks
   - Test parallel execution without file locks
2. Optimize compiler settings:
   - Evaluate split-debuginfo=packed impact on debug builds
   - Adjust LTO strategy (thin vs full)
   - Profile codegen-units (current vs optimal)
   - Add profile-specific overrides in Cargo.toml
3. Implement feature-gating for heavy dependencies:
   - Create feature flags for optional crypto, TLS, templating
   - Ensure test features include all optional dependencies
   - Document which features enable which capabilities
4. Create new Makefile targets for optimized builds:
   - cargo make build-fast (minimal features, max parallelization)
   - cargo make build-debug-thin (debug with thin LTO)
   - cargo make check-isolated (per-task target dirs)
5. Update timeout values based on expected improvements:
   - New timeouts must remain conservative (80% of expected time)
6. Verify backward compatibility:
   - Existing Makefile targets still work
   - New targets don't break CI/CD workflows
```

---

#### 7. **Incremental Build Optimizer** (`coder` agent specialization)
**Role**: Optimize incremental build speed and cache strategies
**Dependencies**: System Architect (design), Code Analyzer (workspace analysis)
**Synchronization Points**:
- Start: After System Architect design
- Checkpoint: Verify no conflicts with Build Config Optimizer
- Integration: Work with Test Optimizer on test artifact caching

**Deliverables**:
- Incremental build cache strategy improvements
- Workspace exclude/include patterns (if applicable)
- Crate isolation boundary definitions
- Incremental test artifact caching

**Key Responsibilities**:
```
1. Analyze and reduce workspace coupling:
   - Profile changes to ggen-utils impact on rebuild time
   - Identify which crates can be built independently
   - Propose workspace reorganization (if needed)
2. Implement crate isolation:
   - Define clear dependency boundaries
   - Identify and break unnecessary transitive dependencies
   - Create feature-gated optional dependencies where applicable
3. Optimize incremental build paths:
   - Analyze which crate changes trigger full workspace rebuilds
   - Implement selective compilation targets for development
   - Create fast-feedback targets for common workflows
4. Cache strategy improvements:
   - Implement artifact caching between CI runs
   - Create incremental baseline artifacts
   - Optimize workspace metadata caching
5. Document incremental build workflow:
   - Fastest workflow for common development tasks
   - How to trigger minimal recompilation
   - Feature flags to enable/disable for speed
```

---

#### 8. **Dependency Optimizer** (`coder` agent specialization)
**Role**: Optimize and feature-gate heavy dependencies
**Dependencies**: System Architect (design), Code Analyzer (identification)
**Synchronization Points**:
- Start: After architecture design
- Checkpoint: Verify feature flag changes don't conflict
- Integration: Coordinate with Build Config Optimizer

**Deliverables**:
- Feature-gated heavy dependencies (ring, tera, rustls, config)
- Optional dependency analysis and removal opportunities
- Compilation unit reorganization (if applicable)
- Dependency graph documentation

**Key Responsibilities**:
```
1. Feature-gate memory-intensive dependencies:
   - ring (crypto): Feature-gate non-essential crypto paths
   - tera (templates): Optional templating for minimal builds
   - rustls (TLS): Optional TLS for CLI-only builds
   - config (configuration): Evaluate necessity, feature-gate if possible
2. Identify unnecessary transitive dependencies:
   - Search for redundant crates (duplicated functionality)
   - Remove unused direct dependencies
   - Consolidate similar functionality
3. Optimize dependency compilation:
   - Identify crates with high compile-time cost relative to usage
   - Evaluate lower-cost alternatives (if applicable)
   - Profile to quantify impact of each heavy dependency
4. Create feature combinations for different use cases:
   - full (all features, for testing and release)
   - minimal (core only, fastest to build)
   - headless (no UI dependencies)
   - integration (required for CI/production)
5. Document feature flag combinations:
   - Which combinations are valid
   - Build time improvement per feature disabled
   - Which features required for different workflows
6. Verify no functionality loss:
   - Tests must run with all feature combinations
   - Core functionality available in minimal build
```

---

#### 9. **Code Optimizer** (`coder` agent specialization)
**Role**: Optimize hot-path code for compile-time reduction
**Dependencies**: Code Analyzer (identification), Performance Benchmarker (baseline)
**Synchronization Points**:
- Start: After Code Analyzer identifies high-impact crates
- Checkpoint: Verify no performance regressions (runtime)
- Integration: Work with Test Optimizer on new code tests

**Deliverables**:
- Monomorphization reduction in high-impact crates
- Const generic optimization opportunities
- Template metaprogramming simplification
- Compile-time code reduction

**Key Responsibilities**:
```
1. Profile hot crates for compilation cost:
   - Analyze ggen-core, ggen-cli, ggen-domain (top 3 slowest)
   - Measure monomorphization cost (generic instantiations)
   - Identify template metaprogramming overhead
2. Reduce monomorphization where possible:
   - Use trait objects instead of generics for compilation boundary
   - Create type aliases to reduce monomorphization
   - Use dyn Trait for optional codepaths
3. Optimize const generics:
   - Profile const generic instantiations
   - Evaluate const specialization opportunities
   - Simplify const trait bounds
4. Simplify macro-heavy code:
   - Profile procedural macros (ggen-macros) compilation cost
   - Simplify macro implementations where possible
   - Consider proc-macro vs derive optimization
5. Reduce template code duplication:
   - Identify duplicate generic implementations
   - Consolidate shared type bounds
   - Use wrapper types to reduce monomorphization
6. Measure improvements:
   - Build time before/after per crate
   - Ensure no runtime performance regressions
   - Validate binary size doesn't increase significantly
```

---

#### 10. **Test Optimizer** (`tester` agent specialization)
**Role**: Optimize test infrastructure for parallel execution
**Dependencies**: Code Analyzer, Performance Benchmarker
**Synchronization Points**:
- Start: After baseline established
- Checkpoint: Verify test speedup with artifact isolation
- Integration: Coordinate with Build Config Optimizer on test features

**Deliverables**:
- Parallel test execution configuration
- Test artifact caching strategy
- Test isolation for concurrent execution
- Test feature optimization (minimal test features)

**Key Responsibilities**:
```
1. Analyze test execution bottlenecks:
   - Profile test compilation time (dependencies, macros)
   - Identify test crates with heavy dependencies
   - Measure sequential vs parallel test execution
2. Optimize test features:
   - Create test-only features (reduce test compile time)
   - Feature-gate heavy test dependencies
   - Optimize test fixture generation
3. Implement parallel test isolation:
   - Ensure tests can run with CARGO_TARGET_DIR isolation
   - Verify concurrent test execution (no file lock issues)
   - Configure --test-threads for deterministic tests
4. Optimize test artifact caching:
   - Cache test artifacts across CI runs
   - Implement incremental test rebuild strategy
   - Minimize test artifact size
5. Create test speed targets:
   - Unit test suite: <150s (with rebuild allowance)
   - Integration tests: <30s
   - Full test suite: <120s
6. Implement SLO testing:
   - Create cargo make test-slo target
   - Validate RDF processing ≤5s/1k triples
   - Monitor test suite performance trends
```

---

### Tier 4: Validation & Verification

#### 11. **Production Validator** (`production-validator` agent)
**Role**: Validate production readiness and integration
**Dependencies**: All Phase 2 agents (collects outputs)
**Synchronization Points**:
- Start: After all Phase 2 agents complete
- Gate: Verify all optimizations integrated correctly
- Final: Sign-off on production readiness

**Deliverables**:
- Production readiness checklist (100% passing)
- Integration validation report
- Regression test results
- Rollback plan (if needed)
- Release notes for optimizations

**Key Responsibilities**:
```
1. Validate all optimizations integrated correctly:
   - Verify no conflicting Makefile/Cargo changes
   - Ensure all feature flags consistent
   - Check CARGO_TARGET_DIR usage throughout
2. Run full test suite on optimized build:
   - cargo make test (must pass 100%)
   - cargo make pre-commit (must pass 100%)
   - cargo make slo-check (must meet all targets)
3. Verify no performance regressions:
   - Runtime performance (use benchmarks)
   - Binary size (compare before/after)
   - Memory usage at runtime (not just compilation)
4. Integration validation:
   - Cross-platform testing (Linux verified, check macOS/Windows)
   - Feature flag combinations (test all critical combinations)
   - CI/CD pipeline integration (verify workflows still work)
5. Risk assessment:
   - Identify remaining risks after optimizations
   - Confidence level for each optimization
   - Rollback readiness (can we revert if needed)
6. Produce final report:
   - Before/after metrics (compilation time, memory, lock contention)
   - SLO compliance matrix (target vs actual)
   - Recommendations for Phase 3 (if needed)
   - Sign-off on production deployment
```

---

## Orchestration Topology & Communication

```
Phase 1: Specification & Analysis (Sequential Foundation)
┌─────────────────────────────────────────────────────┐
│ Specification Validator (Gate #1)                   │
│ - Define success criteria, spec closure             │
│ - Identify all 3 critical bottlenecks               │
│ - Gate: ALL agents can proceed only after           │
└──────────────────┬──────────────────────────────────┘
                   │ (Gate: Spec closure confirmed)
     ┌─────────────┼─────────────┐
     │             │             │
     ▼             ▼             ▼
┌──────────┐ ┌──────────┐ ┌──────────┐
│Performance│ │System    │ │Code      │
│Benchmarker│ │Architect │ │Analyzer  │
└──────┬───┘ └──────┬───┘ └──────┬───┘
       │ (baseline) │ (design)   │ (analysis)
       └────────────┼────────────┘
                    │ (Gate #2: All analysis complete, baseline ready)
                    ▼
Phase 2: Parallel Implementation (7 Agents, Synchronized Start)
       ┌────────────┬────────────┬────────────┬────────────┐
       │            │            │            │            │
       ▼            ▼            ▼            ▼            ▼
    ┌────┐      ┌────────┐  ┌────────┐  ┌────────┐   ┌────┐
    │Build│      │Incremental│Dependency  │Code      │Test│
    │Config│     │Optimizer  │Optimizer   │Optimizer │Opt │
    │Opt   │     └────────┘  └────────┘  └────────┘   └────┘
    └──┬──┘
       │ (Makefile changes)
       │ (Parallel execution with conflict detection)
       │
       ├─────────────────── Conflict Detection ─────────────────┐
       │  - Monitor Makefile.toml changes (detect overlaps)     │
       │  - Monitor Cargo.toml features (ensure consistency)    │
       │  - Detect crate reorganization conflicts               │
       │  - Verify CARGO_TARGET_DIR isolation naming            │
       └──────────────────────────────────────────────────────┘
                    │ (Checkpoint: All implementations ready)
                    ▼
Phase 3: Validation & Release (Sequential Finalization)
┌──────────────────────────────────────────────────────┐
│ Production Validator                                 │
│ - Verify all optimizations integrated correctly      │
│ - Run full test suite (must pass)                    │
│ - Validate SLO compliance (all targets met)          │
│ - Sign-off on production readiness                   │
└──────────────────┬───────────────────────────────────┘
                   │
                   ▼ (Final Gate: Ready for deployment)
            Release & Deploy
```

---

## Dependency Graph & Synchronization Points

### Pre-Phase 2 Gates (Blocking)
1. **Specification Closure Gate**: Spec Validator must confirm 100% coverage
2. **Baseline Measurement Gate**: Benchmarker must provide baseline metrics
3. **Architecture Design Gate**: System Architect must provide design document

### Phase 2 Synchronization (Parallel with Conflict Detection)

**Start Condition**: All Phase 1 gates passed

**Concurrent Execution**: 7 agents run in parallel
```
Build Config Optimizer (Makefile, Cargo, features)
├─ Depends on: System Architect design
├─ Conflicts with: Incremental Optimizer (workspace changes),
│                  Dependency Optimizer (feature flags)
└─ Synchronization: Conflict detection every 30 minutes

Incremental Build Optimizer (crate isolation, caching)
├─ Depends on: System Architect design
├─ Conflicts with: Build Config Optimizer (workspace changes)
└─ Synchronization: Conflict detection every 30 minutes

Dependency Optimizer (feature-gating, dep removal)
├─ Depends on: Code Analyzer (identification)
├─ Conflicts with: Build Config Optimizer (feature flags)
└─ Synchronization: Conflict detection every 30 minutes

Code Optimizer (monomorphization, const generics)
├─ Depends on: Code Analyzer (high-impact crates)
├─ No conflicts (code-only changes)
└─ Independent execution

Test Optimizer (parallel execution, caching)
├─ Depends on: Code Analyzer
├─ Conflicts with: Build Config Optimizer (test features)
└─ Synchronization: Conflict detection every 30 minutes
```

### Phase 2 Conflict Detection Protocol

**Frequency**: Polling every 30 minutes during Phase 2

**Detection Strategy**:
```bash
# Makefile conflict detection
git diff Makefile.toml | grep "^\+\|^\-" | grep -E "\[tasks\.|env\]" > /tmp/makefile-changes.txt
# Alert if >1 agent modifying same task block

# Cargo.toml conflict detection
git diff Cargo.toml | grep "features =" | grep "^\+\|^\-" > /tmp/cargo-feature-changes.txt
# Alert if conflicting feature definitions

# Target directory naming validation
grep "CARGO_TARGET_DIR" .claude/settings.json | wc -l
# Ensure unique isolation per agent
```

**Escalation Path**:
- **Minor conflict** (same block edited by multiple agents): Agent 1 wins, Agent 2 integrates changes
- **Major conflict** (conflicting feature definitions): Pause affected agents, escalate to System Architect
- **Critical conflict** (breaking change): Halt Phase 2, investigate root cause, replan

### Post-Phase 2 Gate (Sequential)

**Merge & Validation**:
1. All Phase 2 agent outputs collected
2. Production Validator integrates changes
3. Full test suite runs (must pass 100%)
4. SLO compliance verified (all targets met)
5. Regression tests passed

**Release Gate**: Only after all checks pass

---

## Handoff Protocol (Agent-to-Agent Communication)

### Memory Storage Format (Claude Flow)

Each agent stores outputs in memory for downstream agents:

```bash
# Specification Validator → Others
npx claude-flow@alpha hooks post-edit \
  --memory-key "swarm/build-optimization/spec-closure" \
  --value '{
    "spec_complete": true,
    "bottleneck_1": "artifact-lock-contention",
    "bottleneck_2": "memory-intensive-deps",
    "bottleneck_3": "workspace-coupling",
    "success_criteria": {...}
  }'

# Performance Benchmarker → Others
npx claude-flow@alpha hooks post-edit \
  --memory-key "swarm/build-optimization/baseline-metrics" \
  --value '{
    "baseline_compilation_time": "120s",
    "baseline_memory_peak": "1.5GB",
    "baseline_lock_events": 42,
    "file_baseline": ".ggen/benchmarks/baseline-2026-01-25.json"
  }'

# System Architect → Phase 2 Agents
npx claude-flow@alpha hooks post-edit \
  --memory-key "swarm/build-optimization/architecture-design" \
  --value '{
    "cargo_target_dir_isolation": true,
    "split_debuginfo_strategy": "packed",
    "feature_gating_required": ["ring", "tera", "rustls"],
    "crate_isolation_boundaries": {...}
  }'
```

### Agent Work Log (Evidence Trail)

Each agent logs completion evidence:

```json
{
  "agent": "build-config-optimizer",
  "phase": 2,
  "start_time": "2026-01-25T14:00:00Z",
  "end_time": "2026-01-25T16:30:00Z",
  "duration": 150,
  "changes": {
    "Makefile.toml": {
      "lines_modified": 47,
      "tasks_added": 3,
      "env_vars_added": 2
    },
    "Cargo.toml": {
      "features_added": ["minimal", "debug-fast"],
      "profile_overrides": 2
    }
  },
  "verification": {
    "cargo_make_check": "PASS",
    "no_makefile_conflicts": true,
    "backward_compatible": true
  },
  "evidence_file": ".ggen/evidence/build-config-optimizer-2026-01-25.json"
}
```

---

## Resource Allocation Strategy

### CPU & Memory Budgets

```
Total System Resources:
- CPU cores: 8 (assume Linux dev machine)
- RAM: 16 GB
- Disk I/O: 250 MB/s (SSD)

Phase 1 (Specification & Analysis) - Sequential
- Specification Validator: 1 CPU, <500MB RAM (30 min)
- Performance Benchmarker: 4 CPUs, 2GB RAM (45 min, runs clean build)
- System Architect: 1 CPU, <500MB RAM (30 min)
- Code Analyzer: 2 CPUs, <1GB RAM (45 min)

Phase 2 (Implementation) - Parallel
- Build Config Optimizer: 1 CPU, <500MB RAM
- Incremental Build Optimizer: 1 CPU, <500MB RAM
- Dependency Optimizer: 1 CPU, <500MB RAM
- Code Optimizer: 2 CPUs, 1GB RAM (compilation testing)
- Test Optimizer: 2 CPUs, 1GB RAM (test runs)
- Total: 8 CPUs, ~4GB RAM (balanced, no contention)
```

### Per-Agent Working Directories

```
/home/user/ggen/.ggen/agents/
├── build-config-optimizer/
│   ├── Makefile.toml.backup
│   ├── Cargo.toml.backup
│   ├── changes.diff
│   └── validation.log
├── incremental-build-optimizer/
│   ├── workspace-analysis.json
│   ├── crate-isolation-plan.md
│   └── changes.diff
├── dependency-optimizer/
│   ├── feature-gating-plan.md
│   ├── dep-removal-candidates.json
│   └── changes.diff
└── [... 5 more agent directories ...]
```

---

## Conflict Detection & Resolution

### Conflict Types & Resolution

#### Type 1: Makefile Task Conflicts
**Symptom**: Two agents modifying same `[tasks.X]` block

**Example**:
```
Build Config Optimizer: Adds CARGO_TARGET_DIR to [tasks.check]
Test Optimizer: Also modifies [tasks.check] for parallel test execution
```

**Resolution**:
1. Detect conflict in polling (30-min interval)
2. Agent 1 (Build Config Optimizer) merges changes
3. Agent 2 (Test Optimizer) pulls merged version, rebases changes
4. Verify: No functional regression in merged task

#### Type 2: Feature Flag Conflicts
**Symptom**: Conflicting feature definitions in Cargo.toml

**Example**:
```
Dependency Optimizer: features = ["minimal", "debug-fast"]
Build Config Optimizer: features = ["minimal", "headless"]
```

**Resolution**:
1. Detect conflict in polling
2. Escalate to System Architect for union of features
3. Document feature combination compatibility
4. Both agents accept merged features

#### Type 3: Crate Reorganization Conflicts
**Symptom**: Changes to workspace member list or dependencies

**Example**:
```
Incremental Build Optimizer: Removes ggen-utils from workspace temporarily
Code Analyzer: Assumes ggen-utils remains in workspace
```

**Resolution**:
1. Pause Incremental Build Optimizer pending resolution
2. System Architect adjudicates breaking changes
3. Document impact on dependent crates
4. Proceed only if backward compatible

### Conflict Detection Algorithm

```bash
#!/bin/bash
# Runs every 30 minutes during Phase 2

detect_conflicts() {
  # 1. Check for Makefile conflicts
  local makefile_conflicts=$(git diff Makefile.toml | \
    grep "^\+\[tasks\." | cut -d. -f2 | sort | uniq -d | wc -l)

  # 2. Check for feature flag conflicts
  local feature_conflicts=$(git diff Cargo.toml | \
    grep "^+.*features = " | wc -l)

  # 3. Check for crate list conflicts
  local crate_conflicts=$(git diff Cargo.toml | \
    grep "^[+-]members = " | wc -l)

  if [ $makefile_conflicts -gt 0 ] || \
     [ $feature_conflicts -gt 1 ] || \
     [ $crate_conflicts -gt 1 ]; then
    echo "CONFLICT_DETECTED=true"
    return 1
  fi

  echo "CONFLICT_DETECTED=false"
  return 0
}
```

---

## Verification Checkpoints & Gates

### Pre-Execution Checklist (Specification Validator)

```
✓ .specify/specs/007-optimize-build-times/ directory exists
✓ feature.ttl has 100% spec closure (all 3 bottlenecks documented)
✓ entities.ttl defines OptimizationStrategy triples
✓ plan.ttl specifies Phase 1-3 milestones
✓ tasks.ttl breaks down each bottleneck into actionable tasks
✓ Success criteria quantified (3-5x speedup, lock contention=0, etc.)
✓ Risk assessment completed
✓ Rollback plan documented
```

### Phase 1 Completion Checklist (After Specification Validator)

```
✓ Specification closure gate passed
✓ Baseline metrics captured in .ggen/benchmarks/baseline-*.json
✓ Architecture design document completed
✓ Codebase analysis available to Phase 2 agents
✓ Conflict detection protocol tested
✓ All agents ready for parallel execution
```

### Phase 2 Checkpoint (Every 30 minutes)

```
✓ No conflicting changes to Makefile.toml
✓ No incompatible feature flag definitions
✓ No breaking crate reorganization
✓ CARGO_TARGET_DIR isolation naming unique per agent
✓ All agents making progress (commits every hour)
✓ No agent blocked (escalate if blocked >1 hour)
```

### Phase 2 Completion Checklist (All agents report done)

```
✓ Build Config Optimizer: Makefile & Cargo changes complete
✓ Incremental Build Optimizer: Crate isolation boundaries defined
✓ Dependency Optimizer: Feature-gating complete
✓ Code Optimizer: Monomorphization reductions done
✓ Test Optimizer: Parallel execution configured
✓ All changes committed to feature branch
✓ No git conflicts remaining
✓ All Andon signals cleared (no warnings/errors)
```

### Phase 3 Validation Checklist (Production Validator)

```
✓ cargo make check passes cleanly (no errors, no warnings)
✓ cargo make test passes 100% (all tests green)
✓ cargo make pre-commit passes (full quality gate)
✓ cargo make slo-check validates:
    ✓ First build ≤15s (was 120s, target 3-5x improvement)
    ✓ Incremental build ≤2s
    ✓ RDF processing ≤5s/1k triples
    ✓ Memory peak ≤100MB
✓ cargo make lint passes (zero warnings)
✓ cargo make audit passes (no vulnerabilities)
✓ Integration tests pass (cross-platform if possible)
✓ Performance regressions: None detected
✓ Binary size: Within 5% of baseline
✓ Feature flag combinations tested: All critical combinations pass
✓ Rollback plan verified (can revert if needed)
```

---

## Risk Mitigation & Rollback Strategy

### Pre-Execution Risks

| Risk | Severity | Mitigation | Owner |
|------|----------|-----------|-------|
| Spec closure incomplete | CRITICAL | Specification Validator verifies 100% coverage | Spec Validator |
| Conflicting Makefile changes | HIGH | Conflict detection every 30 min | Task Orchestrator |
| Feature flag incompatibility | HIGH | Cargo.toml syntax validation | Build Config Opt |
| Breaking crate changes | HIGH | Architecture review before approval | System Architect |
| Rollback inability | MEDIUM | Version control backup, test rollback | Prod Validator |

### Rollback Plan

**If Phase 2 produces breaking changes**:

```bash
# 1. Identify which agent change broke things
git bisect start HEAD~7 HEAD  # Find problematic commit

# 2. Revert breaking commit
git revert <commit-hash>

# 3. Run full test suite to verify working state
cargo make test

# 4. Document what broke and why
# Add to RISK_LOG.md

# 5. Re-plan optimization (System Architect redesign)
# Avoid the breaking change pattern
```

---

## Timeline & Milestones

### Phase 1: Foundation (Days 1-2)
- **Day 1, 0-2h**: Specification Validator (spec closure)
- **Day 1, 2-3h**: Performance Benchmarker (baseline measurement)
- **Day 1, 2-3h**: System Architect (design, parallel with Benchmarker)
- **Day 1, 2-3h**: Code Analyzer (analysis, parallel with Benchmarker)
- **Day 2, 0-2h**: Review & gate approval (all Phase 1 outputs reviewed)

### Phase 2: Optimization (Days 3-5)
- **Day 3, 0-8h**: All 5 Phase 2 agents execute in parallel
- **Day 3-5**: Ongoing conflict detection (30-min intervals)
- **Day 5, 0-4h**: Final integration and git merge

### Phase 3: Validation (Day 6)
- **Day 6, 0-4h**: Production Validator (integration testing)
- **Day 6, 4-8h**: Final gate approval & release

**Total**: 6-7 calendar days (parallel execution reduces to ~5 days wall time)

---

## Success Metrics & KPIs

### Primary KPI: Build Time Improvement

| Metric | Baseline | Target | Success Criteria |
|--------|----------|--------|-----------------|
| Clean build time | ~120s | ≤15s | 8x improvement |
| Incremental build | ~120s | ≤2s | 60x improvement |
| File lock contention | 42 events | 0 events | 100% reduction |
| Memory peak | 1.5-2 GB | ≤100 MB | 15-20x reduction |

### Secondary KPIs

| Metric | Baseline | Target |
|--------|----------|--------|
| Test suite time | Blocked | <150s |
| RDF processing | Not measured | ≤5s/1k triples |
| CI/CD pipeline | Not measured | <3 min total |
| Code compilation | Not measured | ≤10s per change |

### Quality KPIs (Must maintain)

| Metric | Target |
|--------|--------|
| Test pass rate | 100% |
| No compiler warnings | 0 |
| Binary size increase | <5% |
| Runtime performance | ±5% variance acceptable |

---

## Agents & Tool Integration

### Claude Code Task Tool

```
Task("specification-validator",
     "Specification & baseline requirements...",
     "specification")
Task("performance-benchmarker",
     "Establish baseline metrics...",
     "performance-benchmarker")
Task("system-architect",
     "Design optimization strategy...",
     "system-architect")
Task("code-analyzer",
     "Analyze codebase bottlenecks...",
     "code-analyzer")
[... 5 more Phase 2 agents ...]
Task("production-validator",
     "Validate production readiness...",
     "production-validator")
```

### Memory Management

All agents use Claude Flow hooks for cross-agent communication:
```
npx claude-flow@alpha hooks post-edit --memory-key "swarm/build-opt/..." --value "{...}"
npx claude-flow@alpha hooks session-restore --session-id "build-opt-swarm"
```

---

## Next Steps

1. **Approve orchestration plan** (this document)
2. **Spawn Phase 1 agents** (Spec Validator + Benchmarker + System Architect + Code Analyzer)
3. **Wait for Phase 1 gates** (all 4 agents complete, outputs validated)
4. **Spawn Phase 2 agents** (7 parallel implementers)
5. **Monitor conflict detection** (30-min interval polling)
6. **Phase 2 completion** (all 7 agents complete, merge validated)
7. **Spawn Production Validator** (final integration & approval)
8. **Deploy optimized build system** (merge to main, update CI/CD)

---

**Plan Created**: 2026-01-25
**Status**: Ready for parallel execution (awaiting orchestrator approval)
**Confidence Level**: HIGH (based on comprehensive analysis)
