---
description: Synthesize multiple agent solutions into single optimal result
---

# /convergence

Synthesize parallel agent outputs into single optimal solution using selection pressure.

## Usage

```bash
/convergence "[collision analysis report or path to artifacts]"
```

or automatically triggered after `/collision-detect` in `/bb80-parallel` cycle.

## Parameters

- `"[collision analysis]"`: Collision report or path to agent artifacts to reconcile

## What It Does

Runs the `bb80-convergence-orchestrator` agent to:

1. **Apply Selection Pressure**: Coverage, invariants, minimality, elegance, performance
2. **Merge Complementary Approaches**: Take best from each agent
3. **Discard Inferior Alternatives**: Remove dominated solutions
4. **Synthesize Best Solution**: Single, polished, final artifact
5. **Erase Authorship**: No agent attribution, only final result

## Output

Final converged artifact ready for implementation:

```json
{
  "convergence_status": "SUCCESS",
  "selected_approach": "Monoidal composition with async streams (from Agent 3)",
  "reasoning": "95% specification coverage, minimal structure (1200 LOC), best performance (4.8s SLO<5s ✓)",
  "merged_from": ["Agent 3", "Agent 5 (error handling)", "Agent 8 (test strategy)"],
  "discarded": ["Agent 1 (incomplete coverage)", "Agent 6 (inferior performance)"],
  "refactoring_applied": "Extracted common validation trait, unified error handling across 3 error paths",
  "invariants_verified": true,
  "coverage_percentage": 95,
  "slo_compliance": true,
  "artifact": {
    "code": "crates/ggen-core/src/rdf/validation.rs",
    "tests": "crates/ggen-core/tests/rdf_validation_tests.rs",
    "docs": "Specification coverage confirmed"
  },
  "ready_for_deployment": true
}
```

## Selection Pressure Criteria (Applied in Order)

Applied in decreasing priority:

1. **Invariant Satisfaction** (Non-negotiable)
   - Does artifact maintain all specification invariants?
   - All error cases handled?
   - Type system enforces constraints?
   - → FAIL if any invariant violated

2. **Coverage** (Breadth)
   - What % of specification does it cover?
   - All edge cases included?
   - All acceptance criteria met?
   - → FAIL if <80% coverage

3. **Minimality** (Elegance)
   - Is this the simplest structure?
   - Can abstractions be removed?
   - Is every line justified?
   - → Prefer simple over clever

4. **Readability** (Maintainability)
   - Self-documenting?
   - Clear intent?
   - Follows project conventions?
   - → Code should speak clearly

5. **Performance** (Efficiency)
   - Meets SLO targets?
   - Memory usage acceptable?
   - Concurrency model sound?
   - → Benchmarks must pass

## Convergence Process

```
Input: Collision analysis + Agent artifacts
  ↓
Apply selection pressure to each decision point
  ├─ Architecture: Stream-based (8 agents) vs. Batch (2 agents)
  │  Selection: Stream-based has better performance (4.8s < 5.0s)
  │
  ├─ Testing: Property-based (5) vs. State machine (3) vs. Fuzzing (2)
  │  Selection: Merge property-based + state machine (complementary)
  │
  └─ Error handling: Result<T,E> (7) vs. anyhow (2) vs. custom (1)
     Selection: Result<T,E> (standard, type-safe)
  ↓
Synthesize best parts into single artifact
  ├─ Core algorithm from Agent 3 (best coverage)
  ├─ Error handling from Agent 5 (cleanest)
  ├─ Test strategy from Agent 8 (most comprehensive)
  └─ Refactor for coherence
  ↓
Validate final artifact
  ├─ All invariants satisfied? ✓
  ├─ Coverage ≥80%? ✓ (95%)
  ├─ Performance targets met? ✓
  └─ Tests pass? ✓ (342/342)
  ↓
Output: Ready for deployment
```

## Example: Detailed Convergence

```bash
/convergence ./agent_outputs/collision_report.json

═══════════════════════════════════════════════════════════
CONVERGENCE ORCHESTRATION
═══════════════════════════════════════════════════════════

DECISION POINT 1: Architecture Pattern
──────────────────────────────────────

Candidates from collision analysis:
  A) Stream-based (Agents 1,2,3,4,5,6,7,8)
  B) Batch-based (Agents 9,10)

Applying selection pressure:
  ✓ Invariant: Both maintain Result<T,E> ✓
  ✓ Coverage: A=95%, B=88% → Winner: A (95% > 88%)
  ✓ Minimality: A=1200 LOC, B=950 LOC → Review both
  ✓ Performance: A=4.8s, B=6.2s → Winner: A (meets SLO)
  ✓ Readability: A uses traits (clear), B uses loops (less clear)

Decision: SELECT A (Stream-based)
Rationale: Best coverage (95%), meets performance SLO, more readable

DECISION POINT 2: Testing Strategy
──────────────────────────────────

Candidates:
  A) Property-based only (Agents 1,2,3,4,5)
  B) State machine only (Agents 6,7,8)
  C) Fuzzing only (Agents 9,10)

Applying selection pressure:
  ✓ Invariant: All find bugs ✓
  ✓ Coverage: A=85%, B=90%, C=70%
    B has better coverage, but...
  ✓ Minimality: Can merge A+B (complementary)
  ✓ Readiness: A is ready now, B adds 2 weeks

Decision: MERGE A+B (Property-based + State machine)
Rationale: Complementary strategies, total coverage 95%+, both ready now

DECISION POINT 3: Error Handling
────────────────────────────────

Candidates:
  A) Result<T, E> (Agents 1..7)
  B) anyhow::Result (Agents 8,9)
  C) Custom error trait (Agent 10)

Applying selection pressure:
  ✓ Invariant: A is Result<T,E>, B loses type info, C is overengineered
  ✓ Coverage: A covers all errors, others miss some
  ✓ Minimality: A is simplest and standard
  ✓ Readability: A is clearest to ggen developers
  ✓ Ecosystem: ggen already uses A elsewhere (consistency)

Decision: SELECT A (Result<T, E>)
Rationale: Invariant requirements, consistency with codebase

═══════════════════════════════════════════════════════════
SYNTHESIS: Merging Best Parts
═══════════════════════════════════════════════════════════

Taking:
  ✓ Core implementation: Agent 3 (stream-based, 95% coverage)
  ✓ Error handling: Agent 5 (cleanest Result<T,E> implementation)
  ✓ Property tests: Agent 2 (most comprehensive)
  ✓ State machine tests: Agent 7 (catches edge cases)
  ✓ Performance: Agent 6 (optimized hot path)

Discarding:
  ✗ Agent 1: Incomplete coverage (80%)
  ✗ Agent 9: Uses anyhow (inconsistent)
  ✗ Agent 10: Over-engineered (unnecessary trait)

Refactoring:
  - Extracted common validation trait (used by 3 modules)
  - Unified error handling across validation + generation
  - Merged property-based + state machine test suites
  - Optimized hot path from Agent 6

═══════════════════════════════════════════════════════════
VALIDATION: Confirming Synthesized Artifact
═══════════════════════════════════════════════════════════

[Receipt] cargo make check: ✓ (clean compilation)
[Receipt] cargo make test: ✓ (347/347 tests pass)
[Receipt] cargo make lint: ✓ (0 clippy violations)
[Receipt] Specification coverage: 95% (target ≥80%)
[Receipt] SLO compliance: ✓
  - Compilation: 4.2s < 5s
  - Tests: 28s < 30s
  - Lint: 58s < 60s

═══════════════════════════════════════════════════════════
FINAL ARTIFACT READY FOR DEPLOYMENT ✓
═══════════════════════════════════════════════════════════

Location: crates/ggen-core/src/rdf/validation.rs
Tests: crates/ggen-core/tests/rdf_validation_tests.rs
Performance: Validates 1000 triples in 87ms (SLO <100ms) ✓

Authorship: Converged result (synthesized from Agents 3,5,2,7,6)
           Individual author attribution erased

Status: READY FOR IMPLEMENTATION ✓
```

## Convergence NOT Consensus

**Important distinction**:

- **Consensus**: All agents vote, compromise wins (often mediocre)
- **Convergence**: Selection pressure applied, best solution wins (optimal)

Convergence orchestrator:
- Is NOT one of the agents
- Makes decisions based on formal criteria
- Can discard majority opinion if inferior
- Produces optimal result, not compromise

## Failure Modes

```
Error: Convergence failed to reach decision
Reason: Candidate A and B both score equally
Action: Re-run with different selection pressure weights
        OR return to specification (ambiguity?)
```

```
Error: Final artifact violates specification invariant
Reason: No candidate satisfies all invariants
Action: Problem in specification or incomplete agent search
        Return to /speckit-verify or spawn additional agents
```

## When to Use

- **After /collision-detect**: Automatic in `/bb80-parallel` cycle
- **Manual synthesis**: Re-run with different selection criteria
- **Trade-off analysis**: Compare different approaches
- **Alternative selection**: Try different optimizations

## Integration

```
/bb80-parallel [spec]
  ├─ PHASE 1-3: Fan-out, construction, collision
  │
  └─ PHASE 4: Convergence ← YOU ARE HERE
     ├─ Apply selection pressure
     ├─ Synthesize best solution
     └─ Ready for implementation
        ↓
     PHASE 5: Refactoring & polish
     PHASE 6: Closure & validation
```

## See Also

- `/bb80-parallel` - Orchestrate full EPIC 9 cycle
- `/collision-detect` - Analyze overlaps (run first)
- `bb80-convergence-orchestrator` agent - What this command invokes
- `bb80-deterministic-receipts` skill - Validation approach
