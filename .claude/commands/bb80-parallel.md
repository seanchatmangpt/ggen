---
description: Spawn EPIC 9 parallel agents for task
---

# /bb80-parallel

Launch the EPIC 9 atomic cognitive cycle for non-trivial tasks.

## Usage

```bash
/bb80-parallel "[task description with full context]"
```

## Parameters

- `"[task description]"`: Complete specification/task description. Must be clear and unambiguous for agents to work independently.

## What It Does

Orchestrates the complete atomic cognitive cycle:

1. **Fan-Out**: Spawn 10+ independent agents
2. **Independent Construction**: Each works in parallel, no coordination
3. **Collision Detection**: Analyze overlaps in outputs
4. **Convergence**: Apply selection pressure to synthesize best solution
5. **Refactoring**: Merge, discard, rewrite as needed
6. **Closure**: Validate all phases complete

## Output

Returns:
- Individual agent artifacts (10+ independent solutions)
- Collision analysis (overlap detection and categorization)
- Convergence result (synthesized best solution)
- Closure status (ready for implementation or blocked)

## Example

```bash
/bb80-parallel "Implement RDF validation layer for ggen specs.
Requirements:
- Validate all .ttl files against SHACL constraints
- Detect schema violations
- Report errors with file:line:col
- Performance: validate 1000+ triples in <100ms
- Error handling: Return Result<T, ValidationError>
Acceptance Criteria:
- All schema violations caught
- Performance target met
- Full Chicago TDD test coverage"

Output:
═══════════════════════════════════════════════════════════
EPIC 9 ATOMIC COGNITIVE CYCLE
═══════════════════════════════════════════════════════════

PHASE 1: FAN-OUT ✓
  Spawned 10 independent agents:
  ├─ 2 architects (RDF design patterns)
  ├─ 2 implementers (code implementation)
  ├─ 2 testers (test strategy)
  ├─ 2 reviewers (risk analysis)
  └─ 2 validators (invariant checking)

PHASE 2: INDEPENDENT CONSTRUCTION ✓
  Agent 1: Completed architecture design (stream-based validation)
  Agent 2: Completed architecture design (batch-based validation)
  Agent 3: Completed implementation (15 files, 1200 LOC)
  ... [all 10 agents report artifacts]

PHASE 3: COLLISION DETECTION ✓
  Architecture: GREEN_COLLISION (8 agents: stream-based, 2 agents: batch)
    → High confidence: 80% converged on streaming
  Implementation: YELLOW_COLLISION (4 different approaches, 6 similar)
  Testing: YELLOW_COLLISION (property-based vs. state-machine)
  Error handling: GREEN_COLLISION (all agents: Result<T, E>)

PHASE 4: CONVERGENCE ✓
  Selection Pressure Applied:
    Candidate 1 (Agent 3): Streaming, 1200 LOC, 94% test coverage ✓
    Candidate 2 (Agent 5): Batch, 950 LOC, 87% coverage
    Decision: Select Candidate 1 (higher coverage, proven faster)

  Synthesis: Merged batch error handling from Agent 5
  Final: 1250 LOC, 95% coverage, streaming + robust errors

PHASE 5: REFACTORING ✓
  Merged complementary parts
  Polished interfaces
  Ready for integration

PHASE 6: CLOSURE ✓
  [Receipt] cargo make check: ✓
  [Receipt] cargo make test: ✓ (342/342 tests)
  [Receipt] cargo make lint: ✓ (0 violations)
  [Receipt] Specification coverage: 100%

═══════════════════════════════════════════════════════════
FINAL ARTIFACT READY FOR DEPLOYMENT ✓

Files: crates/ggen-core/src/rdf/validation.rs (ready)
Tests: crates/ggen-core/tests/rdf_validation_tests.rs (ready)
Performance: Validates 1000 triples in 87ms (SLO <100ms) ✓
```

## When to Use

✅ **Always for non-trivial tasks**:
- Feature implementation
- Architecture decisions
- Bug investigation
- Performance optimization
- Major refactoring

❌ **Skip for trivial tasks**:
- Reading one file
- Running one script
- Displaying help
- Answering factual questions

## Pre-Requisites

1. **Specification must be closed** - Run `/speckit-verify` first
2. **Clear task description** - Agents need complete context
3. **No ambiguity** - Undefined terms will cause agent divergence

## Integration

```bash
# Step 1: Verify spec is complete
/speckit-verify 005
→ Output: Closure 100% ✓

# Step 2: Launch parallel cycle
/bb80-parallel "[specification from feature.ttl]"
→ Output: All 6 EPIC 9 phases complete

# Step 3: Inspect final artifact
# Review synthesized output, ready for deployment
```

## Failure Modes

If cycle fails:

```
Error: Collision detection found ZERO overlap
→ All 10 agents produced completely different solutions
→ Indicates incomplete specification
→ Return to /speckit-verify
→ Resolve ambiguities
→ Retry /bb80-parallel
```

## Performance Expectations

- Fan-out: <10 seconds
- Independent construction: 2-15 minutes (parallel)
- Collision detection: 1-5 minutes
- Convergence: 2-10 minutes
- Total: 3-20 minutes (vs. 2-8 hours sequential)

## See Also

- `/speckit-verify` - Verify closure first
- `/collision-detect` - Detailed collision analysis
- `/convergence` - Re-run convergence with different criteria
- `bb80-parallel-agents` skill - Learn parallel patterns
- `bb80-specification-closure` skill - Closure checklist
