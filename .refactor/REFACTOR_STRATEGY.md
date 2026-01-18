# ggen CLAUDE.md Refactor Strategy (RFC)
## Bleeding Edge Best Practices from qlever_poc

**Date**: 2026-01-03
**Branch**: `claude/refactor-claude-docs-1zp8v`
**Status**: RFC (Ready for Review)

---

## Executive Summary

This refactoring integrates **three paradigm shifts** from qlever_poc into ggen's constitution:

1. **Big Bang 80/20 Philosophy** - Single-pass construction in low-entropy domains; iteration signals incomplete specification
2. **EPIC 9: Atomic Cognitive Cycle** - Mandatory multi-agent collaboration with collision detection and convergence (not consensus)
3. **Deterministic Validation** - Receipts replace review; benchmarks replace narratives; guards replace trust

**Impact**: Shifts ggen from sequential (plan→code→test→review) to **parallel-first, specification-driven, collision-aware** architecture.

---

## Part 1: Current State Analysis

### ggen's Current Philosophy

| Aspect | Current |
|--------|---------|
| **Execution Model** | Sequential: Plan → Code → Test → Review |
| **Specification** | Present but not closure-enforced (can iterate) |
| **Multi-Agent Pattern** | Serial task delegation (not parallel) |
| **Validation** | Review-based (humans judge correctness) |
| **Iteration** | Normal, expected, part of workflow |
| **Determinism** | SLOs enforced, but narrative-driven decisions common |

### qlever_poc Innovations

| Aspect | qlever_poc |
|--------|-----------|
| **Execution Model** | Parallel: Fan-out → Independent → Collision → Convergence → Refactoring → Closure |
| **Specification** | REQUIRED closure before implementation (iteration is defect) |
| **Multi-Agent Pattern** | 10 independent agents in parallel, no coordination until convergence |
| **Validation** | Deterministic: Benchmarks, guards, receipts (reproducible) |
| **Iteration** | Signal of incomplete specification, not normal workflow |
| **Determinism** | Selection pressure-based, not consensus-based |

---

## Part 2: The Three Paradigm Shifts

### Shift 1: Big Bang 80/20 (Specification-First)

**Current ggen approach:**
```
[Vague requirement] → Plan → Code → Test → [Iterate if broken]
```

**qlever_poc approach:**
```
[Specification closure verification] → [Single-pass construction] → [Validation receipts]
```

**Application to ggen**:
- Formal specification closure before any implementation (BEFORE agent dispatch)
- Specification incompleteness is detected and reported (not iterated around)
- Code generation is "compilation from compressed manifold" (specification → code, not discovery)
- Iteration only happens if specification was incomplete (defect signal)

**CLAUDE.md Changes**:
```markdown
### Specification Closure (NEW)

**CRITICAL**: No implementation begins without specification closure.

Specification closure means:
- All inputs characterized (domain, constraints, edge cases)
- All outputs specified (behavior, invariants, success criteria)
- Ambiguities resolved (clarified or scoped out)
- Completeness verified (checklist or formal proof)

If iteration occurs, specification was incomplete. This is NOT normal workflow; it is diagnostic.

**Process**:
1. Formalize requirements (RDF .ttl for ggen)
2. Run `/speckit-verify` (check closure)
3. On incomplete signal: STOP, clarify, update .ttl
4. Only after closure ✓: Proceed to implementation
```

---

### Shift 2: EPIC 9 - Atomic Cognitive Cycle (Parallel-First)

**Current ggen approach:**
```
Plan agent writes plan → Code agent reads plan → Test agent tests code → Reviewer reviews all
```

**qlever_poc approach:**
```
[Fan-out 10 agents] → [Independent construction, no coordination] → [Collision detection]
→ [Convergence via selection pressure] → [Refactoring] → [Closure]
```

**Application to ggen**:

For non-trivial tasks (default):

```
1. FAN-OUT: Spawn 10 independent agents in parallel:
   - 2 architects exploring different design patterns
   - 2 implementers coding different approaches
   - 2 testers designing different test strategies
   - 2 validators checking different invariants
   - 2 reviewers analyzing different risk angles

2. INDEPENDENT CONSTRUCTION: All 10 work in isolation
   - No coordination messages between agents
   - Each produces complete artifacts
   - Can overlap, conflict, or converge (all valuable)

3. COLLISION DETECTION: Analyze what agents produced
   - Structural overlap: Same code/design produced independently? (HIGH CONFIDENCE)
   - Semantic overlap: Different approaches, same conclusions? (MODERATE CONFIDENCE)
   - Divergence: Disagreement on architecture? (NEEDS RECONCILIATION)

4. CONVERGENCE: Separate process (not original agents)
   - Selection pressure: Coverage, invariants, minimality, elegance
   - Result is synthesized, not voted
   - Authorship erased (ownership → synthesis)

5. REFACTORING: Merge, discard, rewrite as needed
   - Only final artifact survives
   - Intermediate steps destructed

6. CLOSURE: All phases complete or no output
```

**CLAUDE.md Changes**:
```markdown
### EPIC 9: Atomic Cognitive Cycle (Mandatory for Non-Trivial)

[Same structure as qlever_poc, adapted for ggen crates/specs]

### Agents-First Pattern (Recommended)

For implementation tasks:
1. Spawn Task() with rust-coder, test-engineer, reviewer, speckit-architect
2. Each works independently without coordination
3. Detect collisions in outputs (same design? same tests? same findings?)
4. Convergence process selects best approach(es)
5. Refactor merged result

This achieves:
- Parallelism (4x faster context gathering)
- Diversity (multiple perspectives before commitment)
- Confidence (collision = high signal)
- Minimality (selection pressure finds elegant solutions)
```

---

### Shift 3: Deterministic Validation (Evidence > Narrative)

**Current ggen approach:**
```
Compiler ✓ → Tests ✓ → Clippy ✓ → [Human review] → Approved
```

**qlever_poc approach:**
```
Benchmarks → Guards → Receipts (reproducible, no human narrative needed)
```

**Application to ggen**:

```markdown
### Deterministic Validation (No Narratives)

Replace "I think it's correct" with "Here are the receipts":

1. **Benchmarks**: SLO performance validated
   - cargo make slo-check passes
   - Benchmark suites show no regressions
   - Memory profiling acceptable

2. **Guards**: Invariants enforced
   - Type system prevents invalid states
   - Andon signals (RED) trigger immediate stop
   - Path protection prevents overwrites
   - FMEA analysis shows no P1 risks

3. **Receipts** (Reproducible Evidence):
   - Test output with timestamps
   - Clippy report with zero violations
   - Benchmark HTML reports
   - Code coverage metrics
   - Integration test logs

NEVER: "I reviewed the code and it looks good"
ALWAYS: "[Receipt] cargo make lint passed with 0 warnings. [Receipt] All 347 tests passed."
```

---

## Part 3: Concrete Changes to CLAUDE.md

### Section A: Reorganize Top-Level Structure

**Current**:
```
🎯 Core Identity
🚨 CRITICAL: THE VITAL FEW
  1. Concurrent Execution Rule
  2. Cargo Make Rule
  3. Andon Signal Rule
  4. Error Handling Rule
  5. Chicago TDD Rule
  6. Claude Code Operating Rules
```

**New (Bleeding-Edge)**:
```
🎯 Core Identity
🚨 CRITICAL: THE VITAL THREE (80% of value)
  1. Big Bang 80/20 (Specification-First)
  2. EPIC 9: Atomic Cognitive Cycle (Parallel-First)
  3. Deterministic Validation (Evidence-First)

📋 CONSTITUTIONAL LAWS (20% supporting detail)
  [Previous rules, reorganized as supporting detail]

🔧 TOOLS & PATTERNS (Reference)
  [Cargo Make, Error Handling, Chicago TDD, etc. as tooling, not law]
```

### Section B: Add EPIC 9 as First-Class Concept

**New Section (Full Copy from qlever_poc, adapted for ggen)**:

```markdown
## EPIC 9: Atomic Cognitive Cycle (Multi-Agent Construction Law)

### Non-Trivial Task Default

For any task not explicitly trivial (reading one file, displaying help):

1. **FAN-OUT (Gate)**: Spawn 10+ independent agents
2. **INDEPENDENT CONSTRUCTION**: Each works in isolation
3. **COLLISION DETECTION**: Structural + semantic overlap analysis
4. **CONVERGENCE**: Selection pressure synthesis (not consensus)
5. **REFACTORING**: Merge, discard, rewrite
6. **CLOSURE**: All phases complete or no output

### Why This Works

- **Parallelism**: 10 agents gathering context = 2.8-4.4x speed
- **Diversity**: Multiple approaches before commitment
- **Confidence**: Collision detection = high-signal overlap
- **Minimality**: Selection pressure finds elegant solutions
- **Determinism**: Process is reproducible, not opinion-based
```

### Section C: Create "Trivial Task" Whitelist

```markdown
### Trivial Tasks (Whitelist)

Only these tasks skip EPIC 9 parallel processing:

1. Reading a single file (no analysis, no synthesis)
2. Running a single existing script or command
3. Displaying help text or status information
4. Answering a factual question from existing knowledge

**Default = Non-Trivial → EPIC 9 applies**
```

### Section D: Refactor "Concurrent Execution Rule"

**Current**:
```
Golden Rule: "1 MESSAGE = ALL RELATED OPERATIONS"
```

**New**:
```
Golden Rules:
1. EPIC 9: Non-trivial tasks use parallel agents (MANDATORY)
2. Atomic Message: All related operations in ONE message (MANDATORY)
3. Specification Closure: Verify completeness before implementation (MANDATORY)
4. Collision Detection: Multiple solutions are features, not bugs (MANDATORY)
```

---

## Part 4: .claude/ Directory Refactoring

### New Agents

Create `.claude/agents/` additions:

1. **bb80-specification-validator.md**
   - Checks specification closure (no vague terms, all inputs/outputs characterized)
   - Returns pass/fail + list of incomplete areas

2. **bb80-collision-detector.md**
   - Analyzes outputs from 10 agents
   - Reports structural overlaps (same solution)
   - Reports semantic overlaps (different paths, same conclusions)
   - Reports divergences (requires reconciliation)

3. **bb80-convergence-orchestrator.md**
   - Takes collision report
   - Applies selection pressure (coverage, invariants, minimality, elegance)
   - Produces single synthesized artifact
   - Erases agent authorship

4. **bb80-parallel-task-coordinator.md**
   - Spawns 10 agents for context gathering
   - Monitors parallel execution
   - Triggers collision detection when all agents report
   - Invokes convergence orchestrator

### New Skills

Create `.claude/skills/` additions:

1. **bb80-specification-closure/SKILL.md**
   - Checklist for verifying specification completeness
   - Examples of complete vs. incomplete specifications
   - RDF ontology patterns for formal specifications

2. **bb80-parallel-agents/SKILL.md**
   - Framework for spawning 10 agents
   - Orchestration patterns
   - When to use vs. when to skip (trivial task whitelist)

3. **bb80-invariant-construction/SKILL.md**
   - Extracting minimal invariants from specification
   - Monoidal composition (single-pass, no rework)
   - Type-driven design in Rust

4. **bb80-deterministic-receipts/SKILL.md**
   - Evidence collection (benchmarks, guards, receipts)
   - Reproducibility checklist
   - Audit trail generation

### New Commands

Create `.claude/commands/` additions:

1. **speckit-verify.md** - Verify specification closure
2. **bb80-parallel.md** - Launch parallel agents for task
3. **collision-detect.md** - Analyze agent outputs for overlaps
4. **convergence.md** - Synthesize multiple solutions

### Updated settings.json

```json
{
  "execution_model": "parallel-first",
  "specification_required": true,
  "default_task": "non-trivial",
  "trivial_task_whitelist": [
    "single_file_read",
    "run_existing_script",
    "display_help",
    "factual_question"
  ],
  "epic9": {
    "enabled": true,
    "default_agents": 10,
    "collision_detection": "required",
    "convergence_method": "selection_pressure"
  }
}
```

### New Hooks

1. **pre-specification-check.sh** - Verify closure before implementation
2. **post-collision-detection.sh** - Validate collision analysis
3. **convergence-validation.sh** - Check synthesized artifact meets invariants

---

## Part 5: Integration with Existing ggen Patterns

### How EPIC 9 Plays with Cargo Make

**Atomic cycle phases map to cargo make targets**:

```
Fan-Out          → Create 10 Tasks() (each with subagent_type)
Independent      → Each agent runs cargo make check independently
Construction     → Each agent produces artifacts (code, tests, specs)
Collision        → Post-tool hook analyzes outputs
Convergence      → Separate reconciliation process merges best parts
Refactoring      → Final cargo make pre-commit validates
Closure          → All tests pass or no output
```

### How EPIC 9 Plays with Chicago TDD

**Collision detection is state-based verification**:

```
Agent 1 Test: assert_eq!(cache.len(), 0)
Agent 2 Test: assert_eq!(cache.len(), 0)
Agent 3 Test: assert_eq!(cache.len(), 0)

Collision detected: All agents converged on same invariant.
HIGH CONFIDENCE. Proceed.
```

### How EPIC 9 Plays with RDF-First Specs

**Specification closure = TTL completeness**:

```
.specify/specs/NNN-feature/feature.ttl must have:
- All input scenarios characterized (SPARQL queries)
- All output invariants specified (SHACL constraints)
- All edge cases listed (requirement entities)

/speckit-verify checks ✓ before EPIC 9 fan-out
```

---

## Part 6: Transition Strategy

### Phase 1: Documentation (Immediate)
- Update CLAUDE.md with new sections (EPIC 9, Big Bang 80/20, Deterministic Validation)
- Add .claude/ agents, skills, commands
- Create migration guide

### Phase 2: Proof of Concept (Next Task)
- Apply EPIC 9 to single feature
- Run 10 agents in parallel on same task
- Detect collisions, synthesize result
- Measure: Did collision detection find high-confidence solutions?

### Phase 3: Hardening (Ongoing)
- Add hooks for specification closure checks
- Implement convergence process as code (not manual)
- Create collision detection CLI tool (ggen collision-detect)

### Phase 4: Default Workflow (Long-term)
- All non-trivial tasks use EPIC 9 by default
- Specification closure verified before agent dispatch
- Collision detection automatic
- Convergence orchestrator runs deterministically

---

## Part 7: FAQ & Risk Mitigation

### Q: Won't 10 agents be slower than planning once?
**A**: No. Agents run in parallel (2.8-4.4x parallelism). Collision detection + convergence is faster than iterating.

### Q: What if agents disagree?
**A**: Disagreement is valuable (semantic divergence). Convergence process uses selection pressure to pick best, discard rest.

### Q: How do we ensure the converged solution is correct?
**A**: Deterministic validation (benchmarks, guards, receipts). No human narrative needed.

### Q: Can we skip specification closure?
**A**: No. If you do, you'll iterate. Iteration is the defect signal of incomplete spec.

### Q: Does this break existing ggen patterns?
**A**: No. Cargo Make, Chicago TDD, RDF-first, Andon Signals all still apply. EPIC 9 is an orchestration layer on top.

---

## Part 8: Success Criteria

Refactoring is complete when:

- [ ] CLAUDE.md rewritten with EPIC 9 as first-class concept
- [ ] Specification closure section added
- [ ] Deterministic validation section added
- [ ] 4+ new agents created (validator, collision-detector, convergence-orchestrator, coordinator)
- [ ] 4+ new skills created (closure, parallel-agents, invariant-construction, deterministic-receipts)
- [ ] 4+ new commands created (speckit-verify, bb80-parallel, collision-detect, convergence)
- [ ] settings.json updated with parallel-first configuration
- [ ] Proof-of-concept: One task run with full EPIC 9 cycle
- [ ] Migration guide written for team adoption

---

## Appendix: File Structure Changes

```
ggen/
├── CLAUDE.md (REFACTORED: 🎯→🚨→EPIC 9→LAWS→TOOLS)
├── .refactor/
│   ├── REFACTOR_STRATEGY.md (this file)
│   ├── MIGRATION_GUIDE.md (TBD)
│   └── PROOF_OF_CONCEPT.md (TBD)
└── .claude/
    ├── agents/
    │   ├── bb80-specification-validator.md (NEW)
    │   ├── bb80-collision-detector.md (NEW)
    │   ├── bb80-convergence-orchestrator.md (NEW)
    │   ├── bb80-parallel-task-coordinator.md (NEW)
    │   └── [existing agents]
    ├── skills/
    │   ├── bb80-specification-closure/ (NEW)
    │   ├── bb80-parallel-agents/ (NEW)
    │   ├── bb80-invariant-construction/ (NEW)
    │   ├── bb80-deterministic-receipts/ (NEW)
    │   └── [existing skills]
    ├── commands/
    │   ├── speckit-verify.md (NEW)
    │   ├── bb80-parallel.md (NEW)
    │   ├── collision-detect.md (NEW)
    │   ├── convergence.md (NEW)
    │   └── [existing commands]
    └── hooks/
        ├── pre-specification-check.sh (NEW)
        ├── post-collision-detection.sh (NEW)
        ├── convergence-validation.sh (NEW)
        └── [existing hooks]
```

---

## Next Steps

1. **User Review**: Confirm the three paradigm shifts align with your vision
2. **Design Phase**: Create detailed specs for new agents/skills/commands
3. **Implementation Phase**: Refactor CLAUDE.md, update .claude/
4. **Validation Phase**: Run proof-of-concept with full EPIC 9 cycle
5. **Adoption Phase**: Team migration and documentation

---

**Status**: Ready for your review. Shall we proceed with detailed implementation?
