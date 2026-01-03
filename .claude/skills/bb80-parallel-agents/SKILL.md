# EPIC 9: Parallel Agents Skill

**Auto-trigger**: Whenever you see "parallel", "agents", "independent", "collision", "convergence", "fan-out"

## Core Concept

EPIC 9 is an **atomic cognitive cycle** with mandatory phases:

```
FAN-OUT → INDEPENDENT CONSTRUCTION → COLLISION DETECTION → CONVERGENCE → REFACTORING → CLOSURE
```

This skill teaches when to use parallel agents and how to orchestrate them.

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

## Integration with ggen Patterns

**With Cargo Make**:
```bash
# Agents each run cargo make check independently
# Collision detector analyzes which passed/failed
# Convergence selects approach with best SLOs
```

**With Chicago TDD**:
```rust
// Agent 1 test: assert_eq!(cache.len(), 0)
// Agent 2 test: assert_eq!(cache.len(), 0)
// Agent 3 test: assert_eq!(cache.len(), 0)
// Collision: All agents agree on invariant
// Confidence: Very high
```

**With RDF-First Specs**:
```
1. Verify spec closure (bb80-specification-validator)
2. Fan-out agents with spec.ttl
3. Collision detect on design approaches
4. Convergence synthesizes best
```

## Common Patterns

### Pattern 1: Architecture Decision
```
Spawn 10 agents
├─ 2 microservices architects
├─ 2 monolith architects
├─ 2 serverless architects
├─ 2 event-driven architects
└─ 2 message-queue architects

Wait for collision analysis
→ "5 agents converged on microservices + event-driven hybrid"
→ Select that approach with high confidence
```

### Pattern 2: Bug Investigation
```
Spawn 10 agents analyzing same bug
├─ 2 top-down analysis (from symptoms)
├─ 2 bottom-up analysis (from code)
├─ 2 dataflow analysis
├─ 2 state machine analysis
└─ 2 performance analysis

Wait for collision analysis
→ "7 agents identified same root cause"
→ Fix with high confidence
```

### Pattern 3: Test Strategy
```
Spawn 10 agents designing tests
├─ 2 property-based testers
├─ 2 state machine testers
├─ 2 fuzzing specialists
├─ 2 E2E testers
└─ 2 performance testers

Collision analysis reveals overlaps
→ Convergence selects complementary strategies
→ Final test suite covers all angles
```

## Failure Conditions

**No output if any of these fail**:

1. ❌ <10 agents spawned
2. ❌ No collision detected (all agents diverged)
3. ❌ Collision detected but convergence failed
4. ❌ Final artifact violates invariants
5. ❌ Coverage <80%

On failure: Return to specification validation. Spec is incomplete.

## Commands to Use

```bash
/bb80-parallel [task-description]     # Spawn EPIC 9 cycle
/collision-detect [artifacts...]      # Analyze overlaps
/convergence [collision-report]       # Synthesize result
/speckit-verify [spec.ttl]            # Verify closure first
```

## When to Override

**Only for**:
- Emergency hotfixes (speed > optimality)
- Exploratory spikes (understanding domain)
- Trivial tasks (reading a file)

**For production work**: EPIC 9 is default. Overriding requires explicit approval.

## Key Takeaway

Parallel-first means:
- Spawn agents first (before detailed planning)
- Let them work independently
- Analyze collisions when done
- Convergence finds the best solution
- 2.8-4.4x speedup + better confidence

No iteration needed if specification is closed.
