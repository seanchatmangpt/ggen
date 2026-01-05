# EPIC 9: Parallel Agents Skill (2026 Bleeding Edge Edition)

**Auto-trigger**: Whenever you see "parallel", "agents", "independent", "collision", "convergence", "fan-out", "EPIC 9", "bb80-parallel"

**Version**: 2.0.0 (2026 Edition with ggen sync integration)

**SLO**: Complete EPIC 9 cycle in 2-3 hours (vs 5+ hours sequential)

## Core Concept

EPIC 9 is an **atomic cognitive cycle** enabling 2.8-4.4x speedup through true parallelism:

```
1. FAN-OUT (Spawn 10+ independent agents)
         ↓
2. INDEPENDENT CONSTRUCTION (All work in parallel, NO coordination)
         ↓
3. COLLISION DETECTION (Analyze overlaps: structural & semantic)
         ↓
4. CONVERGENCE (Apply selection pressure, synthesize best solution)
         ↓
5. REFACTORING & SYNTHESIS (Merge, discard, rewrite as needed)
         ↓
6. CLOSURE (Validate all phases complete or no output)
```

**Key Insight**: Specification closure + deterministic generation = true parallelism without coordination overhead.

This skill teaches when to use parallel agents, how to orchestrate them, and how to validate results with receipts (not narratives).

## Golden Rule: Parallel-First for Non-Trivial Tasks

**Default: Use EPIC 9**

Only skip for trivial tasks (whitelist below).

## Trivial Task Whitelist

Tasks that skip EPIC 9:
- Reading a single file (no analysis)
- Running a single existing command
- Displaying help text
- Answering factual questions

**Everything else = Non-Trivial → Use EPIC 9**

## When to Spawn 10 Agents

| Task Type | Count | Roles | Why |
|-----------|-------|-------|-----|
| **Implementation** | 10 | 2 architects, 2 implementers, 2 testers, 2 reviewers, 2 validators | Multiple approaches prevent blind spots |
| **Architecture** | 10 | 2 designers, 2 system architects, 2 security reviewers, 2 performance analysts, 2 maintainability experts | Design decisions need diverse perspective |
| **Problem-Solving** | 10 | Spawn diverse cognitive styles (analytical, creative, conservative, aggressive, etc.) | Novel solutions emerge from collision |

## Fan-Out Phase: Spawning Independent Agents

**Critical rule**: Agents must NOT coordinate.

```
Task("Architect-1", "[FULL SPECIFICATION] Design architecture independently. Do NOT coordinate with other agents.", "architect")
Task("Architect-2", "[FULL SPECIFICATION] Design architecture independently. Do NOT coordinate with other agents.", "architect")
Task("Implementer-1", "[FULL SPECIFICATION] Implement independently. Do NOT coordinate with other agents.", "coder")
// ... repeat for all 10
```

Each agent receives:
- Complete specification (no hints about other agents)
- Clear instruction: work in isolation
- Request for complete artifact (not partial)

## Independent Construction Phase

All agents work in **parallel without coordination**:

```
Time →
Agent 1: ████████████ (produces artifact 1)
Agent 2:    ████████ (produces artifact 2)
Agent 3:       ████████ (produces artifact 3)
...
Agent 10:          ████████ (produces artifact 10)

All parallel. NO interaction.
```

Agents do NOT:
- Ask each other questions
- Compare notes
- Merge work mid-stream
- Optimize for agreement

Agents DO:
- Work independently
- Make all decisions solo
- Produce complete artifacts
- Document rationale

## Collision Detection Phase

After all agents report, analyze overlaps:

```json
{
  "architecture": {
    "agents_identical": 7,
    "agents_different": 3,
    "status": "GREEN_COLLISION",
    "confidence": "High - 70% convergence on same design"
  },
  "testing": {
    "agents_identical": 4,
    "agents_different": 6,
    "status": "YELLOW_COLLISION",
    "confidence": "Moderate - multiple valid strategies"
  },
  "error_handling": {
    "agents_identical": 1,
    "agents_different": 9,
    "status": "RED_COLLISION",
    "confidence": "Low - needs reconciliation"
  }
}
```

Collision categories:
- **GREEN** (≥60% identical): High confidence, use consensus
- **YELLOW** (30-60% overlap): Moderate, analyze trade-offs
- **RED** (<30% overlap): Low, apply selection pressure

**Collision is GOOD** - it signals where agents converged independently.

## Convergence Phase

Separate reconciliation process (not voting):

Apply **selection pressure**:
1. Invariants satisfied? (Must pass)
2. Coverage ≥80%? (Required)
3. Minimality? (Simplest structure wins)
4. Readability? (Self-documenting)
5. Performance? (Meets SLOs)

Result: Single synthesized artifact (not compromise, not vote).

## Why Parallel Works

| Aspect | Sequential | Parallel (EPIC 9) |
|--------|-----------|-------------------|
| **Speed** | Plan 1h, Code 2h, Test 1h = 4h | Fan-out instantly, Parallel 2h, Collision 30min, Convergence 30min = ~2h |
| **Diversity** | 1 perspective | 10 perspectives |
| **Risk** | Single blind spot | Collisions reveal blind spots |
| **Confidence** | "Looks good" | "7 agents independently converged" |
| **Rework** | Iterate if wrong | Selection pressure finds best first-pass |

## 🚀 BREAKTHROUGH: ggen sync Enables True Parallelism

This is the **game-changing insight** that makes EPIC 9 work at 2.8-4.4x speedup:

### The Deterministic Generation Pattern

```
Specification (TTL) is deterministic input
             ↓
10 independent agents can generate FROM THE SAME SPEC
             ↓
All 10 outputs are identical, correct implementations
             ↓
Agents converge on same solution → HIGH CONFIDENCE
```

### Why This Is Revolutionary

**Traditional Parallel Development (fails)**:
```
Agent 1: Writes CLI handler (interpretation A)
Agent 2: Writes CLI handler (interpretation B)
→ Need coordination meeting
→ Merge conflicts
→ Rework and iteration
→ Sequential bottleneck
Result: Parallelism FAILS
```

**ggen sync Pattern (succeeds)**:
```
Shared Specification: api_spec.ttl (single source of truth, verified closed)
       ↓
Agent 1: ggen sync → generates handler.rs (hash: abc123)
Agent 2: ggen sync → generates handler.rs (hash: abc123)
Agent 3: ggen sync → generates handler.rs (hash: abc123)
...
Agent 10: ggen sync → generates handler.rs (hash: abc123)
       ↓
All agents' code is IDENTICAL (compiled from same spec)
→ No coordination needed
→ No merge conflicts
→ No iteration
→ TRUE PARALLELISM
Result: 2.8-4.4x speedup achieved
```

### The Three Enablers

1. **Specification Closure**: RDF specs are verified 100% complete before generation
   - No ambiguity → No divergent interpretations
   - Run `/speckit-verify` → Must pass before fan-out

2. **Deterministic Generation**: `ggen sync` is a pure function
   - Same input (TTL spec) → Same output (code)
   - Reproducible across agents, environments, time

3. **Formal Semantics**: RDF is formal logic, not natural language
   - SPARQL queries are precise
   - Templates are deterministic
   - No room for "creative interpretation"

### Example: REST API Generation with 10 Agents

**Step 1: Verify Specification Closure**
```bash
/speckit-verify api_spec.ttl

[Receipt] Closure Score: 100%
[Receipt] SHACL Validation: ✓ (0 violations)
[Receipt] Required Properties: ✓ (all present)
[Receipt] Status: READY FOR EPIC 9
```

**Step 2: Fan-Out (Single Message, All Agents)**
```javascript
// All agents receive IDENTICAL instructions
Task("Agent-1", "Generate API from api_spec.ttl using ggen sync. Report hash.", "coder")
Task("Agent-2", "Generate API from api_spec.ttl using ggen sync. Report hash.", "coder")
Task("Agent-3", "Generate API from api_spec.ttl using ggen sync. Report hash.", "coder")
// ... Task("Agent-10", ...)
```

**Step 3: Independent Construction (All Parallel)**
```bash
# Agent 1
cd agent-1-workspace
ggen sync
sha256sum src/handlers/users.rs  # abc123def456...

# Agent 2 (simultaneously)
cd agent-2-workspace
ggen sync
sha256sum src/handlers/users.rs  # abc123def456... (SAME!)

# Agent 10 (simultaneously)
cd agent-10-workspace
ggen sync
sha256sum src/handlers/users.rs  # abc123def456... (SAME!)
```

**Step 4: Collision Detection**
```json
{
  "collision_report": {
    "handlers/users.rs": {
      "agents_identical": 10,
      "sha256": "abc123def456...",
      "status": "GREEN_COLLISION",
      "confidence": "Maximum - 100% convergence"
    },
    "models.rs": {
      "agents_identical": 10,
      "sha256": "789ghi012jkl...",
      "status": "GREEN_COLLISION",
      "confidence": "Maximum - 100% convergence"
    }
  },
  "overall_status": "PERFECT_COLLISION",
  "recommendation": "Use any agent's output (all identical)"
}
```

**Step 5: Convergence (Instant)**
```
Selection Pressure: N/A (all outputs identical)
Winner: Agent 1 (arbitrary choice, all are identical)
Verification: cargo make check on Agent 1 output
[Receipt] cargo make check: ✓ (4.2s)
[Receipt] cargo make test: ✓ (347/347 pass)
[Receipt] cargo make lint: ✓ (0 violations)
Status: CONVERGED WITH HIGH CONFIDENCE
```

**Result**: 10 agents worked in **true parallelism** with **zero coordination overhead**.

---

## Integration with ggen Patterns

### With ggen sync (Primary Pattern)

**Always use ggen sync for generation tasks in EPIC 9**:

```bash
# Each agent independently
ggen sync                    # Generate from spec
ggen sync --validate-only    # Verify spec before generation
ggen sync --dry-run          # Preview changes
ggen sync --audit            # Generate audit trail

# Collision detection compares:
# - Generated file hashes (structural collision)
# - Audit trails (process collision)
# - SLO compliance (performance collision)
```

**Watch Mode for Iterative Specification Refinement**:
```bash
# Agent workflow with watch mode
ggen sync --watch --verbose

# On TTL change:
# 1. Auto-regenerate (1-2s incremental)
# 2. Agent reports new hash
# 3. Collision detector tracks convergence
# Result: Real-time collision tracking
```

### With Cargo Make (Receipt Collection)

**Agents collect receipts, not opinions**:

```bash
# Agent 1 (parallel with Agents 2-10)
cargo make check      # [Receipt] ✓ 4.2s
cargo make test       # [Receipt] ✓ 347/347 pass
cargo make lint       # [Receipt] ✓ 0 violations
cargo make slo-check  # [Receipt] ✓ All SLOs met

# Collision detector analyzes receipts:
{
  "check_slo": {
    "agents_pass": 10,
    "mean_time": "4.1s",
    "status": "GREEN_COLLISION"
  },
  "test_pass_rate": {
    "agents_100%": 10,
    "status": "GREEN_COLLISION"
  }
}
```

### With Chicago TDD (State-Based Testing)

**Agents write state-based tests independently**:

```rust
// Agent 1
#[test]
fn test_lockfile_upsert() {
    let manager = LockfileManager::new(temp_dir.path());
    manager.upsert("pkg", "1.0.0", "sha256", "url").unwrap();
    let entry = manager.get("pkg").unwrap().unwrap();
    assert_eq!(entry.version, "1.0.0");
}

// Agent 2 (writes similar test independently)
#[test]
fn test_lockfile_upsert() {
    let manager = LockfileManager::new(temp_dir.path());
    manager.upsert("pkg", "1.0.0", "sha256", "url").unwrap();
    assert_eq!(manager.get("pkg").unwrap().unwrap().version, "1.0.0");
}

// Collision Detection:
// - Both test same invariant (version stored correctly)
// - Slightly different assertion structure (minor)
// - Collision confidence: 95% (both verify same behavior)
```

### With RDF-First Specs (Prerequisite)

**MANDATORY workflow**:

```bash
# Phase 0: Specification Closure (BEFORE EPIC 9)
/speckit-verify feature.ttl

# If closure < 100%:
# → STOP
# → Clarify with user
# → Update TTL
# → Re-verify
# → Only proceed when closure = 100%

# Phase 1: Fan-Out (After closure verified)
# All agents receive feature.ttl (single source of truth)

# Phase 2: Independent Construction
# Agents generate from TTL using ggen sync

# Phase 3: Collision Detection
# Compare generated artifacts

# Phase 4: Convergence
# Select best (if divergence) or use any (if perfect collision)
```

### With Andon Signals (Quality Gates)

**Agents monitor for RED signals independently**:

```bash
# Agent 3 encounters RED signal
cargo make test
# Output: test lockfile_upsert ... FAILED

# Agent response:
1. Mark test failure in collision report
2. Capture error details
3. Continue (don't stop other agents)

# Collision Detection Phase:
{
  "test_results": {
    "agents_pass": 7,
    "agents_fail": 3,
    "status": "YELLOW_COLLISION",
    "failure_pattern": "All 3 failures on lockfile_upsert test",
    "root_cause": "Specification ambiguity in locking semantics"
  }
}

# Convergence Phase:
# → Investigate why 3 agents failed same test
# → Likely spec is incomplete (closure was not 100%)
# → Return to specification, clarify locking semantics
```

## Common Patterns (Real-World Workflows)

### Pattern 1: REST API Generation from Specification

**Context**: Generate complete REST API with handlers, models, routes, tests

**Prerequisites**:
```bash
# Verify specification closure
/speckit-verify .specify/specs/042-api/feature.ttl
[Receipt] Closure Score: 100%
[Receipt] SHACL Validation: ✓
[Receipt] Status: READY FOR EPIC 9
```

**Fan-Out** (single message, 10 agents):
```javascript
// All spawned in ONE message (CONCURRENT EXECUTION RULE)
Task("Agent-1", "Generate REST API from .specify/specs/042-api/feature.ttl using ggen sync. Run cargo make check && cargo make test. Report results with receipts.", "coder")
Task("Agent-2", "Generate REST API from .specify/specs/042-api/feature.ttl using ggen sync. Run cargo make check && cargo make test. Report results with receipts.", "coder")
// ... Task("Agent-3" through "Agent-10") ...
```

**Independent Construction** (all agents work in parallel):
```bash
# Agent 1 workflow
cd /workspace/agent-1
ggen sync
cargo make check      # [Receipt] ✓ 4.1s
cargo make test       # [Receipt] ✓ 89/89 pass
sha256sum src/**/*.rs # Report hashes

# Agent 2 workflow (simultaneous)
cd /workspace/agent-2
ggen sync
cargo make check      # [Receipt] ✓ 4.3s
cargo make test       # [Receipt] ✓ 89/89 pass
sha256sum src/**/*.rs # Report hashes

# ... Agents 3-10 (all simultaneous)
```

**Collision Detection**:
```json
{
  "collision_report": {
    "src/handlers/users.rs": {
      "agents_identical": 10,
      "sha256": "a1b2c3...",
      "status": "GREEN_COLLISION",
      "confidence": "Maximum"
    },
    "src/models.rs": {
      "agents_identical": 10,
      "sha256": "d4e5f6...",
      "status": "GREEN_COLLISION"
    },
    "tests/integration_tests.rs": {
      "agents_identical": 8,
      "agents_different": 2,
      "majority_sha256": "g7h8i9...",
      "status": "YELLOW_COLLISION",
      "divergence": "Agent 3,7 added extra edge case tests"
    }
  },
  "receipts": {
    "cargo_make_check": {
      "agents_pass": 10,
      "mean_slo": "4.2s",
      "status": "GREEN"
    },
    "cargo_make_test": {
      "agents_100%_pass": 10,
      "status": "GREEN"
    }
  },
  "overall_confidence": "High - 95% structural collision, all receipts GREEN"
}
```

**Convergence**:
```
Selection Pressure Applied:
1. Handlers/Models: All identical → Use any agent's output
2. Tests: Merge Agent 3's edge cases (added value)
3. Verification: cargo make pre-commit on merged output
   [Receipt] cargo make check: ✓
   [Receipt] cargo make test: ✓ (92/92 pass, +3 from Agent 3)
   [Receipt] cargo make lint: ✓
   [Receipt] Coverage: 87% (above 80% threshold)

Winner: Agent 1 output + Agent 3 tests
Status: CONVERGED - READY FOR DEPLOYMENT
Time: 2.5 hours (vs 6 hours sequential)
Speedup: 2.4x
```

---

### Pattern 2: Bug Investigation with Diverse Analysis

**Context**: Production panic in cache invalidation, root cause unknown

**Prerequisites**:
```bash
# Gather evidence first
/review-errors crates/ggen-core/src/cache.rs
[Receipt] Found 3 unwrap() calls in production code (VIOLATION)
[Receipt] Panic trace points to line 247
```

**Fan-Out** (10 agents, diverse cognitive styles):
```javascript
// Single message with all agents
Task("Agent-1", "Analyze cache panic using top-down approach (from panic trace). Provide evidence.", "investigator")
Task("Agent-2", "Analyze cache panic using bottom-up approach (from cache state machine). Provide evidence.", "investigator")
Task("Agent-3", "Analyze cache panic using dataflow analysis. Provide evidence.", "investigator")
Task("Agent-4", "Analyze cache panic using race condition detection. Provide evidence.", "investigator")
Task("Agent-5", "Analyze cache panic using property-based testing. Provide evidence.", "investigator")
// ... Agents 6-10 with different approaches ...
```

**Independent Construction**:
```bash
# Agent 1: Top-down
grep -r "unwrap()" crates/ggen-core/src/cache.rs
# Line 247: let cache = self.cache.lock().unwrap();
# Root cause: Lock poisoning when panic occurs during critical section
[Receipt] Evidence: Stack trace shows panic while lock held

# Agent 4: Race condition (simultaneously)
cargo make test -- --test-threads=1  # Run serially
# All tests pass
cargo make test -- --test-threads=8  # Run parallel
# 3/89 tests fail with "PoisonError"
[Receipt] Evidence: Race condition confirmed under concurrency

# Agent 7: Property-based (simultaneously)
# Wrote proptest that triggers panic in 2/100 runs
[Receipt] Evidence: Minimal failing case: concurrent read during write
```

**Collision Detection**:
```json
{
  "root_cause_identified": {
    "agents_converged": 7,
    "root_cause": "Lock poisoning from unwrap() on Mutex::lock()",
    "evidence_sources": ["stack_trace", "race_condition_test", "proptest"],
    "status": "GREEN_COLLISION",
    "confidence": "Very High - 7/10 agents independently identified same root cause"
  },
  "fix_proposed": {
    "agents_converged": 6,
    "fix": "Replace unwrap() with map_err(|e| Error::new(...))?",
    "status": "YELLOW_COLLISION",
    "divergence": "4 agents also suggested LockGuard timeout"
  }
}
```

**Convergence**:
```rust
// Selected fix (consensus from 6 agents)
let cache = self.cache.lock()
    .map_err(|e| Error::new(&format!("Cache lock poisoned: {}", e)))?;

// Additional improvement (from Agent 4's timeout suggestion)
let cache = self.cache.lock()
    .map_err(|e| Error::new(&format!("Cache lock poisoned: {}", e)))?;
// + Add timeout in future iteration

[Receipt] Fix applied
[Receipt] cargo make test: ✓ (89/89 pass, 100 runs with --test-threads=8)
[Receipt] cargo make test -- --ignored: ✓ (stress tests pass)
[Receipt] Proptest: ✓ (10000 iterations, 0 failures)
Status: BUG FIXED WITH HIGH CONFIDENCE
```

---

### Pattern 3: Test Strategy Design for New Feature

**Context**: Design comprehensive test strategy for new RDF inference engine

**Prerequisites**:
```bash
/speckit-verify .specify/specs/055-inference/feature.ttl
[Receipt] Closure Score: 100%
[Receipt] Status: READY FOR EPIC 9
```

**Fan-Out** (10 agents, specialized test expertise):
```javascript
Task("Agent-1", "Design property-based tests for RDF inference. Use proptest.", "tester")
Task("Agent-2", "Design state machine tests for inference lifecycle. Use chicago-tdd-tools.", "tester")
Task("Agent-3", "Design fuzzing tests for SPARQL query input. Use cargo-fuzz.", "tester")
Task("Agent-4", "Design E2E tests for inference pipeline. Use testcontainers + gVisor.", "tester")
Task("Agent-5", "Design mutation tests for inference correctness. Use cargo-mutants.", "tester")
Task("Agent-6", "Design performance tests for inference SLOs. Use criterion.", "tester")
Task("Agent-7", "Design snapshot tests for inference output. Use insta.", "tester")
Task("Agent-8", "Design integration tests for inference with cache. Use chicago-tdd.", "tester")
Task("Agent-9", "Design error handling tests for inference failures. Use chicago-tdd.", "tester")
Task("Agent-10", "Design concurrency tests for parallel inference. Use loom.", "tester")
```

**Independent Construction** (all agents write test strategies in parallel):
```bash
# Each agent produces:
# 1. Test plan (markdown)
# 2. Example tests (Rust code)
# 3. Coverage estimates
# 4. SLO targets
```

**Collision Detection**:
```json
{
  "test_coverage_overlap": {
    "agents_testing_core_invariants": 8,
    "core_invariants": [
      "Inference produces valid RDF triples",
      "CONSTRUCT queries are idempotent",
      "Inference terminates (no infinite loops)"
    ],
    "status": "GREEN_COLLISION",
    "confidence": "High - 8 agents identified same invariants"
  },
  "test_strategy_diversity": {
    "unique_approaches": 7,
    "complementary_coverage": "95%",
    "overlapping_coverage": "40%",
    "status": "IDEAL - Good diversity + overlap"
  },
  "test_quality_estimates": {
    "agents_estimating_80%+_coverage": 9,
    "mean_estimate": "84%",
    "status": "GREEN"
  }
}
```

**Convergence**:
```
Selection Pressure Applied:
1. Core invariants: All 8 agents' property-based tests (high value)
2. Edge cases: Agent 3's fuzzing (found input validation gaps)
3. Performance: Agent 6's criterion benchmarks (SLO enforcement)
4. Concurrency: Agent 10's loom tests (race condition detection)
5. Integration: Agent 8's cache interaction tests (system-level)

Final Test Suite Composition:
- Property-based: 12 tests (Agents 1, 5, 9)
- State machine: 8 tests (Agent 2)
- Fuzzing: Continuous (Agent 3)
- E2E: 6 scenarios (Agent 4)
- Mutation: Coverage target 80% (Agent 5)
- Performance: 4 benchmarks (Agent 6)
- Snapshot: Golden files for 10 cases (Agent 7)
- Integration: 15 tests (Agent 8)
- Error handling: 20 tests (Agent 9)
- Concurrency: 7 loom tests (Agent 10)

[Receipt] Total tests: 88
[Receipt] Estimated coverage: 87%
[Receipt] Estimated SLO: <30s test execution
[Receipt] Mutation score target: 80%

Status: TEST STRATEGY CONVERGED
Next: Implement tests using ggen sync
```

---

### Pattern 4: Architecture Decision (Microservices vs Monolith)

**Context**: Decide architecture for new marketplace service

**Prerequisites**:
```bash
# Requirements specification verified
/speckit-verify .specify/specs/060-marketplace-v2/requirements.ttl
[Receipt] Closure Score: 100%
[Receipt] Non-functional requirements: ✓ (latency, throughput, scalability)
```

**Fan-Out** (10 agents, diverse architectural perspectives):
```javascript
Task("Agent-1", "Design microservices architecture for marketplace. Provide trade-off analysis with evidence.", "architect")
Task("Agent-2", "Design monolith architecture for marketplace. Provide trade-off analysis with evidence.", "architect")
Task("Agent-3", "Design serverless architecture for marketplace. Provide trade-off analysis with evidence.", "architect")
Task("Agent-4", "Design event-driven architecture for marketplace. Provide trade-off analysis with evidence.", "architect")
Task("Agent-5", "Design CQRS architecture for marketplace. Provide trade-off analysis with evidence.", "architect")
// ... Agents 6-10 with hybrid approaches ...
```

**Independent Construction**:
```bash
# Each agent produces:
# 1. Architecture diagram (Mermaid)
# 2. Component specifications (TTL)
# 3. Trade-off analysis (latency, cost, complexity)
# 4. SLO projections
# 5. Risk assessment (FMEA)
```

**Collision Detection**:
```json
{
  "architectural_convergence": {
    "pure_microservices": 2,
    "pure_monolith": 1,
    "hybrid_microservices_monolith": 5,
    "event_driven_microservices": 2,
    "status": "YELLOW_COLLISION",
    "insight": "5 agents converged on hybrid approach (modular monolith + microservices for bounded contexts)"
  },
  "slo_projections": {
    "agents_meeting_latency_slo": 7,
    "agents_failing_latency_slo": 3,
    "convergence": "Hybrid approaches meet SLOs, pure microservices exceed latency budget"
  },
  "complexity_analysis": {
    "lowest_complexity": "Modular monolith (Agent 2, 6, 8)",
    "highest_value_per_complexity": "Hybrid (Agent 6, 7, 8, 9, 10)"
  }
}
```

**Convergence**:
```
Selection Pressure Applied:
1. Meets SLOs? (Mandatory gate)
   → Pure microservices: ❌ (latency 450ms, target 200ms)
   → Hybrid: ✅ (latency 180ms)
   → Monolith: ✅ (latency 120ms)

2. Scalability? (Required for marketplace)
   → Hybrid: ✅ (can scale bounded contexts independently)
   → Monolith: ⚠️ (vertical scaling only)

3. Team cognitive load? (DfLSS principle)
   → Hybrid: ✅ (Agents 6-10 estimated 6 months to production)
   → Pure microservices: ❌ (Agents 1,3 estimated 18 months)

4. Cost analysis?
   → Hybrid: ✅ ($2400/month at 10K users)
   → Pure microservices: ❌ ($8900/month)

Winner: Hybrid Architecture (Agents 6, 7, 8, 9, 10 consensus)
- Core marketplace: Modular monolith
- Payment processing: Separate microservice (PCI compliance)
- Analytics: Separate microservice (high write load)
- Search: Separate microservice (Elasticsearch)

[Receipt] Architecture Decision Record created
[Receipt] 5 agents independently converged on same hybrid approach
[Receipt] SLO projections: ✅ (all targets met)
[Receipt] FMEA risk score: 23 (acceptable, <50 threshold)
[Receipt] Team capacity: ✅ (6 months to production)

Status: ARCHITECTURE DECISION CONVERGED WITH HIGH CONFIDENCE
Next: Generate component specifications with ggen sync
```

## Failure Conditions & Recovery

**EPIC 9 fails if any of these occur**:

### Critical Failures (Stop Immediately)

1. **❌ Specification Closure < 100%**
   ```bash
   /speckit-verify feature.ttl
   [Receipt] Closure Score: 73%
   [Receipt] Missing: acceptance_criteria, error_handling_strategy
   [Receipt] Status: NOT READY FOR EPIC 9

   # Recovery:
   # → STOP fan-out
   # → Clarify with user
   # → Update feature.ttl
   # → Re-verify closure
   # → Only proceed when closure = 100%
   ```

2. **❌ <10 Agents Spawned**
   ```javascript
   // WRONG: Only 3 agents (not enough diversity)
   Task("Agent-1", "...", "coder")
   Task("Agent-2", "...", "coder")
   Task("Agent-3", "...", "coder")

   // Recovery:
   // → Always spawn 10+ agents for non-trivial tasks
   // → If task is truly trivial, skip EPIC 9 entirely
   ```

3. **❌ Agents Coordinating (Breaking Independence)**
   ```bash
   # Agent 2 output: "Waiting for Agent 1 to finish..."
   # Agent 5 output: "Discussed with Agent 3, decided to..."

   # Recovery:
   # → This violates INDEPENDENT CONSTRUCTION phase
   # → Restart fan-out with clear "NO coordination" instruction
   # → Ensure agents work in true isolation
   ```

### Warning Failures (Investigate)

4. **⚠️ No Collision Detected (Complete Divergence)**
   ```json
   {
     "collision_report": {
       "agents_identical": 0,
       "agents_different": 10,
       "status": "RED_COLLISION",
       "confidence": "Zero - complete divergence"
     }
   }

   // Root cause: Specification ambiguity (closure was false positive)
   // Recovery:
   // 1. Analyze divergence patterns
   // 2. Identify ambiguous requirements
   // 3. Return to specification, clarify
   // 4. Re-run EPIC 9 with updated spec
   ```

5. **⚠️ Convergence Failed (Selection Pressure Inconclusive)**
   ```bash
   # 5 approaches, all meet SLOs, no clear winner

   # Recovery:
   # → Add more selection criteria (maintainability, cognitive load)
   # → Consult user for preference
   # → Document trade-offs in ADR
   # → Select approach with lowest risk (FMEA)
   ```

6. **⚠️ Final Artifact Violates Invariants**
   ```bash
   cargo make check
   # error[E0277]: trait bound not satisfied

   cargo make test
   # test core_invariant_preserved ... FAILED

   # Recovery:
   # → Convergence synthesized incompatible parts
   # → Re-analyze collision detection for compatibility
   # → Select single agent's complete output (don't merge)
   # → If all agents fail: spec is incorrect (return to closure)
   ```

7. **⚠️ Coverage <80%**
   ```bash
   cargo make test
   [Receipt] Test coverage: 67% (below 80% threshold)

   # Recovery:
   # → Identify untested paths
   # → Extract test strategies from agents that covered those paths
   # → Merge additional tests
   # → Re-verify coverage
   ```

### Performance Failures (Iterate Specification)

8. **⚠️ SLO Violations**
   ```bash
   cargo make slo-check
   [Receipt] check: 8.2s (target <5s) ❌
   [Receipt] test: 45s (target <30s) ❌

   # Recovery:
   # → Analyze which agents met SLOs
   # → Identify performance-critical design decisions
   # → Apply selection pressure favoring performance
   # → If all agents fail SLOs: requirements are infeasible
   ```

9. **⚠️ Andon Signal: RED During Construction**
   ```json
   {
     "andon_signals": {
       "agents_red": 8,
       "common_error": "error[E0308]: mismatched types",
       "root_cause": "Specification defined incompatible types"
     }
   }

   // Recovery:
   // → 8 agents hitting same error = spec issue (not agent error)
   // → Return to specification
   // → Fix type definitions
   // → Re-run EPIC 9
   ```

### Recovery Decision Tree

```
Failure detected
    ↓
Is spec closure < 100%?
    Yes → Return to spec, clarify, re-verify
    No  ↓
Are agents coordinating?
    Yes → Restart with "NO coordination" instruction
    No  ↓
Did >50% agents converge on same error?
    Yes → Spec has bug, return to closure
    No  ↓
Did >50% agents produce valid outputs?
    Yes → Apply selection pressure to valid outputs
    No  ↓
Did ANY agent produce valid output?
    Yes → Use that agent's approach, investigate why others failed
    No  ↓
EPIC 9 FAILS → Task is not ready for parallel execution
    → Specification is fundamentally incomplete
    → User intervention required
```

## Commands & Slash Command Reference

### Essential Commands (2026 Edition)

**Specification Verification** (MANDATORY before EPIC 9):
```bash
/speckit-verify .specify/specs/NNN-feature/feature.ttl
# Verifies closure score = 100%
# SHACL validation
# Required properties check
# Must pass before fan-out

/speckit-check
# Validate all RDF specs in .specify/ directory
# Generate markdown from TTL
# Verify feature completeness
```

**EPIC 9 Orchestration**:
```bash
/bb80-parallel "[Full specification here with all context]"
# Spawns 10+ agents
# Orchestrates atomic cognitive cycle
# Manages collision detection + convergence
# Enforces all 6 phases
# SLO: Complete cycle in 2-3 hours

# Example:
/bb80-parallel "Generate REST API from .specify/specs/042-api/feature.ttl. Each agent: run ggen sync, cargo make check, cargo make test. Report hashes and receipts."
```

**Collision Detection & Convergence**:
```bash
/collision-detect
# Analyzes overlaps between agent outputs
# Structural collision (file hashes)
# Semantic collision (behavior)
# Receipt collision (validation results)
# Outputs JSON collision report

/convergence
# Synthesizes best solution from collision report
# Applies selection pressure criteria
# Merges complementary outputs
# Validates final artifact
# Produces deployment-ready result
```

**Quality Assurance**:
```bash
/test-audit
# Comprehensive test audit using ggen-test-audit
# Mutation testing analysis
# Assertion coverage detection
# False positive detection
# Use when improving test quality

/review-errors [path]
# Review error handling patterns
# Check unwrap/expect violations
# Verify Result<T,E> usage
# Suggest improvements

/optimize [path]
# Optimize Rust code for performance
# Analyze hot paths
# Suggest improvements
# Verify SLOs

/bench-compare [commit1] [commit2]
# Compare benchmark results across commits
# Show performance regressions/improvements
# Track performance trends
```

### ggen Commands (Core Workflow)

```bash
# Code Generation
ggen sync                    # Full generation from spec
ggen sync --dry-run          # Preview without writing
ggen sync --watch            # Watch TTL, auto-regenerate
ggen sync --validate-only    # SHACL validation gate
ggen sync --rule NAME        # Run specific generation rule
ggen sync --audit            # Detailed audit trail

# Project Scaffolding
ggen init                    # Interactive project init
ggen init --template NAME    # Scaffold from marketplace template
ggen init --list             # List available templates
```

### Cargo Make Commands (Validation)

```bash
# Fast Feedback Loop
cargo make check             # Compilation check (<5s SLO)
cargo make test-unit         # Unit tests only (<10s SLO)
cargo make lint              # Clippy + format (<60s SLO)

# Full Validation
cargo make test              # All tests (<30s SLO)
cargo make pre-commit        # Full validation pipeline
cargo make ci                # Complete CI pipeline
cargo make slo-check         # Verify all SLO compliance

# Performance
cargo make bench             # Run all 14 benchmark suites
```

---

## When to Override EPIC 9

**ONLY skip EPIC 9 for truly trivial tasks**:

### Whitelist (Skip EPIC 9)
- Reading a single file (no analysis)
- Running a single existing command (`cargo make test`)
- Displaying help text (`ggen --help`)
- Answering factual questions ("What is EPIC 9?")
- Listing directory contents (`ls`)

### Blacklist (ALWAYS use EPIC 9)
- Any code generation or modification
- Any architecture or design decision
- Any test strategy or implementation
- Any bug investigation or debugging
- Any performance optimization
- Any specification authoring
- Any API design
- Any refactoring (>10 lines changed)
- Any integration work

**Rule of thumb**: If task will take >15 minutes sequentially, use EPIC 9.

**Exception for emergencies**:
```
Emergency hotfix AND
Time pressure (production down) AND
Clear fix identified (no uncertainty) AND
User explicitly approves override

→ ONLY THEN skip EPIC 9
→ Document override decision (ADR)
→ Plan EPIC 9 post-mortem for permanent fix
```

**For production work**: EPIC 9 is default. No exceptions without user approval.

## Best Practices (2026 Edition)

### 1. Specification Closure is Non-Negotiable

```bash
# ✅ CORRECT: Verify first
/speckit-verify feature.ttl
# [Receipt] Closure Score: 100%
# → Proceed to EPIC 9

# ❌ WRONG: Skip verification
# "Spec looks complete enough, let's start"
# → Agents will diverge, collision detection will fail
```

**Why**: Closure verification prevents 80% of EPIC 9 failures.

### 2. Single Message Fan-Out (CONCURRENT EXECUTION RULE)

```javascript
// ✅ CORRECT: All agents in ONE message
[Single Message]:
  Task("Agent-1", "...", "coder")
  Task("Agent-2", "...", "coder")
  // ... Task("Agent-3" through "Agent-10") ...

// ❌ WRONG: Sequential messages
[Message 1]: Task("Agent-1", "...", "coder")
[Message 2]: Task("Agent-2", "...", "coder")  // Too slow!
```

**Why**: Concurrent execution achieves 2.8-4.4x speedup. Sequential loses all benefit.

### 3. Identical Instructions for All Agents

```javascript
// ✅ CORRECT: Same spec for all
const spec = "Generate API from feature.ttl using ggen sync";
Task("Agent-1", spec, "coder")
Task("Agent-2", spec, "coder")
// ... all receive same spec

// ❌ WRONG: Different instructions
Task("Agent-1", "Generate handlers", "coder")
Task("Agent-2", "Generate models", "coder")  // This is division of labor, not parallelism!
```

**Why**: Collision detection requires agents working on the SAME problem from different perspectives.

### 4. Receipt-Based Validation (Never Narratives)

```bash
# ✅ CORRECT: Receipts
[Receipt] cargo make check: ✓ 4.1s
[Receipt] cargo make test: ✓ 89/89 pass
[Receipt] SHA256: abc123def456...

# ❌ WRONG: Narratives
"Code looks good"
"Tests probably pass"
"Should be fine"
```

**Why**: Receipts are objective, reproducible, auditable. Narratives are opinions.

### 5. Enforce No Coordination

```javascript
// ✅ CORRECT: Explicit isolation instruction
Task("Agent-1", "Design architecture independently. Do NOT coordinate with other agents. Do NOT wait for others.", "architect")

// ❌ WRONG: Implicit assumption
Task("Agent-1", "Design architecture", "architect")
// → Agent may try to coordinate
```

**Why**: Coordination destroys parallelism. Enforce isolation explicitly.

### 6. Use ggen sync for Generation (Determinism)

```bash
# ✅ CORRECT: Deterministic generation
ggen sync  # Same spec → same output every time

# ❌ WRONG: Hand-coding
vim src/handlers.rs  # Each agent writes different code
```

**Why**: ggen sync enables 100% collision (all agents produce identical output from same spec).

### 7. Collect Hashes for Collision Detection

```bash
# ✅ CORRECT: Report hashes
sha256sum src/**/*.rs
# abc123... src/handlers/users.rs
# def456... src/models.rs

# ❌ WRONG: No verification
"I generated the files"
```

**Why**: Hashes enable precise structural collision detection.

### 8. Apply Selection Pressure (Not Voting)

```bash
# ✅ CORRECT: Criteria-based selection
1. Meets SLOs? (gate)
2. Lowest complexity? (minimality)
3. Highest coverage? (quality)
4. Fewest invariants violated? (correctness)
→ Winner: Agent 7 (meets all criteria)

# ❌ WRONG: Voting
"5 agents voted for approach A, so use A"
→ Compromise solution, not best solution
```

**Why**: Selection pressure finds objectively best solution. Voting produces compromise.

### 9. Watch Mode for Spec Iteration

```bash
# ✅ CORRECT: Watch mode during spec refinement
ggen sync --watch --verbose
# → Auto-regenerate on TTL changes
# → Fast feedback (1-2s incremental)

# ❌ WRONG: Manual regeneration
# Edit TTL
ggen sync  # Manual
# Edit TTL again
ggen sync  # Manual again
```

**Why**: Watch mode enables rapid iteration during specification development.

### 10. Document Collision Patterns

```json
// ✅ CORRECT: Structured collision report
{
  "handlers.rs": {
    "agents_identical": 10,
    "confidence": "Maximum"
  },
  "tests.rs": {
    "agents_identical": 7,
    "divergence": "3 agents added edge cases"
  }
}

// ❌ WRONG: Vague summary
"Most agents converged"
```

**Why**: Structured reports enable systematic convergence decisions.

---

## Anti-Patterns (What NOT to Do)

### ❌ Anti-Pattern 1: Iteration Instead of Closure

```
WRONG:
1. Write vague spec
2. Run EPIC 9
3. Agents diverge
4. Iterate spec
5. Run EPIC 9 again
6. Still diverging...

CORRECT:
1. Verify spec closure = 100%
2. Run EPIC 9 once
3. Perfect collision
4. Deploy
```

**Why**: Iteration signals incomplete specification. Fix spec first.

### ❌ Anti-Pattern 2: Division of Labor (Not Parallelism)

```
WRONG:
Agent 1: Write handlers
Agent 2: Write models
Agent 3: Write tests
→ This is sequential work divided, not parallel exploration

CORRECT:
All 10 agents: Generate complete API (handlers + models + tests)
→ Collision reveals best complete solution
```

**Why**: Division of labor has sequential dependencies. Parallelism requires independent work.

### ❌ Anti-Pattern 3: Premature Convergence

```
WRONG:
Agent 1 finishes first
→ "Use Agent 1's output, don't wait for others"

CORRECT:
Wait for ALL agents to complete
→ Analyze collisions across all 10
→ Apply selection pressure
```

**Why**: Early agents may be wrong. Collision detection requires all perspectives.

### ❌ Anti-Pattern 4: Merging Incompatible Parts

```
WRONG:
Agent 1: REST API (actix-web)
Agent 5: REST API (axum)
Convergence: "Merge Agent 1's handlers + Agent 5's middleware"
→ Frameworks are incompatible!

CORRECT:
Selection Pressure: Which framework meets SLOs?
→ Choose Agent 1 OR Agent 5 (complete output)
→ Don't merge incompatible artifacts
```

**Why**: Merging can create frankencode. Select complete, coherent solutions.

### ❌ Anti-Pattern 5: Ignoring Andon Signals

```
WRONG:
Agent 3: error[E0308]: mismatched types
Agent 7: error[E0308]: mismatched types
Agent 9: error[E0308]: mismatched types
→ "3 agents failed, use the 7 that passed"

CORRECT:
3+ agents hitting SAME error = spec bug
→ STOP the line (Andon)
→ Return to specification
→ Fix type definitions
→ Re-run EPIC 9
```

**Why**: Common failures reveal specification bugs, not agent errors.

### ❌ Anti-Pattern 6: Narrative Reviews

```
WRONG:
"Agent 1's code looks clean"
"Agent 5's approach feels more maintainable"
"I prefer Agent 8's style"

CORRECT:
Agent 1: cargo make lint: ✓ 0 violations
Agent 5: cargo make test: ✓ 92/92 pass (highest coverage)
Agent 8: cargo make bench: ✓ Fastest (180ms vs 220ms)
→ Select Agent 5 (highest coverage + meets SLOs)
```

**Why**: Opinions are subjective. Receipts are objective.

### ❌ Anti-Pattern 7: Skipping Validation

```
WRONG:
Convergence produces final artifact
→ Deploy without validation

CORRECT:
Convergence produces final artifact
→ cargo make pre-commit (full validation)
→ [Receipt] All checks pass
→ THEN deploy
```

**Why**: Convergence can synthesize invalid combinations. Always validate.

### ❌ Anti-Pattern 8: Using EPIC 9 for Trivial Tasks

```
WRONG:
"Read CLAUDE.md"
→ Spawn 10 agents to read file
→ Waste 2 hours

CORRECT:
"Read CLAUDE.md"
→ Read file directly (30 seconds)
```

**Why**: EPIC 9 overhead (2-3 hours) only makes sense for non-trivial tasks.

---

## Quick Reference Card

### EPIC 9 in 6 Steps

```
┌─────────────────────────────────────────────────────────────┐
│ PHASE 0: PREREQUISITE                                       │
│ /speckit-verify feature.ttl → Closure = 100%               │
│ If < 100%: STOP, clarify, update, re-verify                │
└─────────────────────────────────────────────────────────────┘
                          ↓
┌─────────────────────────────────────────────────────────────┐
│ PHASE 1: FAN-OUT (Single Message)                          │
│ Task("Agent-1", "[spec]", "role")                          │
│ Task("Agent-2", "[spec]", "role")                          │
│ ... Task("Agent-10", "[spec]", "role")                     │
│ SLO: <30s to spawn all agents                              │
└─────────────────────────────────────────────────────────────┘
                          ↓
┌─────────────────────────────────────────────────────────────┐
│ PHASE 2: INDEPENDENT CONSTRUCTION (All Parallel)           │
│ Agent 1: ggen sync → cargo make check → report hash        │
│ Agent 2: ggen sync → cargo make check → report hash        │
│ ... (all simultaneous, NO coordination)                    │
│ SLO: 1-2 hours parallel work                               │
└─────────────────────────────────────────────────────────────┘
                          ↓
┌─────────────────────────────────────────────────────────────┐
│ PHASE 3: COLLISION DETECTION                               │
│ /collision-detect → JSON report                            │
│ - Structural (file hashes)                                 │
│ - Semantic (behavior)                                      │
│ - Receipt (validation)                                     │
│ SLO: <30m to analyze                                       │
└─────────────────────────────────────────────────────────────┘
                          ↓
┌─────────────────────────────────────────────────────────────┐
│ PHASE 4: CONVERGENCE                                       │
│ /convergence → Apply selection pressure                    │
│ 1. Meets SLOs? (gate)                                      │
│ 2. Highest coverage? (quality)                             │
│ 3. Lowest complexity? (maintainability)                    │
│ 4. Fewest violations? (correctness)                        │
│ SLO: <30m to converge                                      │
└─────────────────────────────────────────────────────────────┘
                          ↓
┌─────────────────────────────────────────────────────────────┐
│ PHASE 5: REFACTORING & SYNTHESIS                           │
│ Merge complementary parts (if applicable)                  │
│ Validate merged output: cargo make pre-commit              │
│ SLO: <30m to synthesize + validate                         │
└─────────────────────────────────────────────────────────────┘
                          ↓
┌─────────────────────────────────────────────────────────────┐
│ PHASE 6: CLOSURE                                           │
│ [Receipt] cargo make check: ✓                              │
│ [Receipt] cargo make test: ✓                               │
│ [Receipt] cargo make lint: ✓                               │
│ [Receipt] Collision confidence: High                       │
│ Status: READY FOR DEPLOYMENT                               │
└─────────────────────────────────────────────────────────────┘

Total SLO: 2-3 hours (vs 5+ hours sequential)
Speedup: 2.8-4.4x
```

### Collision Detection Thresholds

| Collision % | Status | Interpretation | Action |
|-------------|--------|----------------|--------|
| **90-100%** | 🟢 GREEN | Perfect collision | Use any agent's output |
| **60-89%** | 🟢 GREEN | High collision | Use majority approach |
| **30-59%** | 🟡 YELLOW | Moderate collision | Analyze trade-offs, apply selection pressure |
| **10-29%** | 🟠 ORANGE | Low collision | Likely spec ambiguity, investigate |
| **0-9%** | 🔴 RED | Complete divergence | Spec is incomplete, return to closure |

### Selection Pressure Criteria (Priority Order)

1. **Invariants satisfied?** (Must pass - binary gate)
2. **SLOs met?** (Must pass - binary gate)
3. **Coverage ≥80%?** (Quality gate)
4. **Lowest complexity?** (Simplicity principle)
5. **Fewest violations?** (Correctness)
6. **Best performance?** (Efficiency)
7. **Highest maintainability?** (Long-term value)

### Receipt Categories (Evidence Hierarchy)

| Evidence Type | Reliability | Example |
|---------------|-------------|---------|
| **Compilation** | 95% | `cargo make check: ✓` |
| **Test Output** | 90% | `89/89 tests pass` |
| **Linter** | 85% | `0 clippy violations` |
| **Coverage** | 80% | `87% line coverage` |
| **Benchmarks** | 80% | `180ms p99 latency` |
| **Hash** | 99% | `sha256: abc123...` |
| **Narrative** | 0% | "Code looks good" ❌ |

---

## Key Takeaways

### The Golden Rules

1. **Specification closure is prerequisite for EPIC 9** → `/speckit-verify` MUST pass
2. **EPIC 9 is default for non-trivial tasks** → >15 min sequential = use EPIC 9
3. **Deterministic validation replaces human review** → Collect receipts, not narratives
4. **Parallelism requires independence** → NO coordination between agents
5. **Collision detection reveals truth** → High collision = high confidence
6. **Selection pressure finds best** → Objective criteria, not voting
7. **ggen sync enables true parallelism** → Same spec → same output → perfect collision

### The Speedup Formula

```
Sequential Time: Plan (1h) + Code (2h) + Test (1h) + Review (1h) = 5 hours
EPIC 9 Time: Spec (30m) + Parallel (2h) + Collision (30m) + Convergence (30m) = 3.5 hours
Speedup: 5 / 3.5 = 1.43x (conservative)

With perfect collision (ggen sync):
EPIC 9 Time: Spec (30m) + Parallel (1.5h) + Collision (15m) + Convergence (15m) = 2.5 hours
Speedup: 5 / 2.5 = 2.0x (typical)

With high diversity tasks (architecture decisions):
Sequential: Plan (3h) + Research (4h) + Analysis (2h) + Decision (1h) = 10 hours
EPIC 9: Spec (1h) + Parallel (2h) + Collision (30m) + Convergence (30m) = 4 hours
Speedup: 10 / 4 = 2.5x (high-value tasks)

Maximum observed: 4.4x (perfect conditions)
Minimum viable: 1.5x (high coordination overhead)
Target: 2.8x (realistic average)
```

### When EPIC 9 Wins

- **Non-trivial tasks** (>15 min sequential)
- **High uncertainty** (multiple valid approaches)
- **Architecture decisions** (need diverse perspectives)
- **Complex debugging** (root cause unknown)
- **Test strategy design** (need comprehensive coverage)
- **API design** (need to explore alternatives)
- **Performance optimization** (need to compare approaches)

### When EPIC 9 Loses

- **Trivial tasks** (<15 min sequential)
- **Specification not ready** (closure <100%)
- **Clear single solution** (no alternatives to explore)
- **Emergency hotfixes** (time pressure > optimality)
- **Exploratory spikes** (understanding domain, not building)

---

## Integration with Other Skills

**Prerequisite Skills**:
- `bb80-specification-closure` → Verify spec before EPIC 9
- `rdf-ontologies` → Author TTL specifications

**Concurrent Skills**:
- `cargo-make-protocol` → Collect receipts during construction
- `chicago-tdd-pattern` → State-based testing in agents
- `poka-yoke-patterns` → Error-proofing mechanisms

**Post-EPIC-9 Skills**:
- `bb80-deterministic-receipts` → Validate convergence output
- `bb80-invariant-construction` → Verify invariants preserved

---

## Constitutional Equation

```
EPIC 9 = Spec₁₀₀% × (Agent₁...₁₀)ᵖᵃʳᵃˡˡᵉˡ × Collision × Convergence
```

Where:
- `Spec₁₀₀%` = Specification closure verified at 100%
- `(Agent₁...₁₀)ᵖᵃʳᵃˡˡᵉˡ` = 10+ agents working independently (no coordination)
- `Collision` = Overlap analysis (structural + semantic + receipt)
- `Convergence` = Selection pressure applied (criteria-based, not voting)

**Result**: 2.8-4.4x speedup + higher confidence + better coverage

**Failure modes**: Any term = 0 → EPIC 9 = 0 (no output)

---

**Version**: 2.0.0 (2026 Edition)
**Last Updated**: 2026-01-05
**Changelog**: Added ggen sync integration, receipt-based validation, real-world patterns, failure recovery, anti-patterns, quick reference
