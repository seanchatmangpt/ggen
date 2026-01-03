---
description: Analyze parallel agent outputs for overlaps and divergences
---

# /collision-detect

Analyze overlaps in parallel agent outputs. Detect structural and semantic convergence.

## Usage

```bash
/collision-detect "[path to agent artifacts directory]"
```

or after `/bb80-parallel` automatically triggered.

## Parameters

- `"[path]"`: Directory containing agent output artifacts to analyze

## What It Does

Runs the `bb80-collision-detector` agent to:

1. **Structural Overlap Detection**: Identical or dominance-equivalent solutions
2. **Semantic Overlap Analysis**: Different approaches converging on same conclusions
3. **Divergence Identification**: Disagreements requiring reconciliation
4. **Collision Categorization**: GREEN (high confidence), YELLOW (moderate), RED (low)
5. **Confidence Scoring**: 0-100% per decision point

## Output

```json
{
  "collision_analysis": {
    "architecture_design": {
      "status": "GREEN_COLLISION",
      "overlap_percentage": 80,
      "consensus_approach": "Monoidal composition with async streams",
      "agents_converged": 8,
      "dissenters": 2,
      "dissent_reason": "Proposed imperative loop alternative"
    },
    "implementation_approach": {
      "status": "YELLOW_COLLISION",
      "overlap_percentage": 50,
      "approaches": [
        { "style": "trait-based abstraction", "count": 5 },
        { "style": "function composition", "count": 3 },
        { "style": "macro metaprogramming", "count": 2 }
      ]
    },
    "error_handling_strategy": {
      "status": "RED_COLLISION",
      "overlap_percentage": 20,
      "divergence": [
        { "approach": "Result<T, E>", "agents": 7 },
        { "approach": "anyhow::Result", "agents": 2 },
        { "approach": "custom error enum", "agents": 1 }
      ]
    }
  },
  "high_confidence_decisions": ["architecture_design", "core_algorithm"],
  "requires_reconciliation": ["implementation_approach", "error_handling_strategy"],
  "ready_for_convergence": true
}
```

## Collision Categories

| Status | Overlap | Meaning | Action |
|--------|---------|---------|--------|
| **GREEN** | ≥60% | High confidence consensus | Use consensus directly |
| **YELLOW** | 30-60% | Moderate overlap, valid alternatives | Analyze trade-offs |
| **RED** | <30% | Significant divergence | Apply selection pressure |

## Collision Semantics

**Collision is GOOD**:
- Structural overlap = Independent agents converged (high signal)
- Semantic overlap = Different paths, same conclusion (validates approach)
- Divergence = Trade-offs to evaluate (valuable info)

**No collision = PROBLEM**:
- All agents diverged completely
- Indicates incomplete specification
- Cannot proceed to convergence
- Return to spec validation

## Example Output

```bash
/collision-detect ./agent_outputs/

═══════════════════════════════════════════════════════════
COLLISION DETECTION ANALYSIS
═══════════════════════════════════════════════════════════

ARCHITECTURE DECISIONS
──────────────────────

Design Pattern Selection: GREEN_COLLISION ✓
  Status: HIGH CONFIDENCE
  Consensus: Monoidal composition with async streams
  Agent agreement: 8/10 (80%)

  Breakdown:
    ✓ 8 agents: "Stream-based validation"
    ~ 2 agents: "Batch-based validation"

  Signal: 80% convergence on streaming approach
  Confidence: Very High
  Recommendation: Proceed with streaming (try batch as alternative)

API Design: YELLOW_COLLISION
  Status: MODERATE CONFIDENCE
  Approaches:
    ✓ 5 agents: Trait-based abstraction
    ~ 3 agents: Function composition
    ~ 2 agents: Macro metaprogramming

  Signal: Multiple valid approaches, trade-offs exist
  Confidence: Moderate
  Recommendation: Analyze trade-offs (complexity vs. ergonomics)

ERROR HANDLING: RED_COLLISION
  Status: LOW CONFIDENCE
  Approaches:
    ✓ 7 agents: Result<T, E> with custom Error enum
    ~ 2 agents: anyhow::Result
    ~ 1 agent: Custom error trait

  Signal: Significant divergence, needs reconciliation
  Confidence: Low
  Recommendation: Apply selection pressure (verify spec constraint)

TESTING STRATEGY: YELLOW_COLLISION
  Status: MODERATE CONFIDENCE
  Approaches:
    ✓ 5 agents: Property-based testing (proptest)
    ~ 3 agents: State machine testing
    ~ 2 agents: Fuzzing approach

  Signal: Multiple valid strategies, complement each other
  Confidence: Moderate
  Recommendation: Merge complementary strategies (proptest + state machine)

═══════════════════════════════════════════════════════════
SUMMARY
═══════════════════════════════════════════════════════════

High Confidence Decisions (use as-is):
  ✓ Architecture design (streaming): 80% convergence
  ✓ Core algorithm: 9/10 agents converged

Requires Trade-off Analysis (select best):
  ~ API design (trait vs. composition)
  ~ Testing strategy (property vs. state machine)

Requires Selection Pressure (apply criteria):
  ✗ Error handling (Result vs. anyhow)

Ready for Convergence Phase: YES ✓
  Collision detection found sufficient overlap to proceed.
  Convergence orchestrator can now synthesize best solution.

Next: /convergence
```

## When to Use

- **After /bb80-parallel**: Automatically run during cycle
- **Manual analysis**: Reanalyze agent outputs with different criteria
- **Debugging**: Understand why agents converged/diverged
- **Decision making**: Choose between alternatives based on overlap

## Integration

```
/bb80-parallel [spec]
  ├─ PHASE 1: Fan-out (spawn 10 agents)
  ├─ PHASE 2: Independent construction (all agents work)
  │
  └─ PHASE 3: Collision detection ← YOU ARE HERE
     ├─ Analyze structural overlaps
     ├─ Categorize as GREEN/YELLOW/RED
     └─ Gate convergence on results
        ↓
     PHASE 4: Convergence (if collision found)
     PHASE 5: Refactoring
     PHASE 6: Closure
```

## See Also

- `/bb80-parallel` - Orchestrate full EPIC 9 cycle
- `/convergence` - Synthesize agents using collision analysis
- `bb80-parallel-agents` skill - Learn when collision happens
- `bb80-collision-detector` agent - What this command invokes
