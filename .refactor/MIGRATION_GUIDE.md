# ggen CLAUDE.md Migration Guide
## Bleeding-Edge Best Practices Adoption (2026)

**Date**: 2026-01-03
**Branch**: `claude/refactor-claude-docs-1zp8v`
**Status**: Ready for Team Adoption

---

## Overview

This guide helps teams transition from sequential (plan→code→test→review) workflow to **parallel-first, specification-driven, collision-aware architecture** powered by three paradigm shifts.

**What's changing**: The entire execution model, from sequential to atomic cognitive cycles.

**Why it matters**: 2.8-4.4x speedup, higher confidence, fewer iterations, better outcomes.

**Adoption timeline**: Phased, starting with understanding, then practice, then default workflow.

---

## Three Paradigm Shifts

### Shift 1: Big Bang 80/20 (Specification-First)

**OLD**: Vague requirement → Plan → Code → Test → [Iterate if broken]

**NEW**: Specification closure → Single-pass construction → Validation receipts

**What you do differently**:
- Before ANY implementation: verify specification is complete
- Vague terms (TBD, maybe, probably) → **STOP**, clarify first
- Use `/speckit-verify` to check closure before starting
- If iteration occurs: that's a signal of incomplete specification (not normal)

**Example**:
```
OLD approach:
  User: "Add validation"
  You: [Starts coding email validation]
  User: "Wait, which fields?"
  You: [Throws away work, restarts]

NEW approach:
  User: "Add validation"
  You: /speckit-verify
  → Report: "Incomplete - which fields? Which rules? Error format?"
  User: [Clarifies]
  You: [Code once, perfect first pass]
```

**Benefits**:
- Single-pass implementation (no rework)
- Clear success criteria upfront
- Fewer surprises during code review

**Skill to learn**: `bb80-specification-closure` (read it!)

**Command to use**: `/speckit-verify [feature]`

---

### Shift 2: EPIC 9 - Atomic Cognitive Cycle (Parallel-First)

**OLD**: Plan agent → Code agent → Test agent → Review agent [Sequential, 4-6 hours]

**NEW**: 10 agents in parallel → Collision detection → Convergence [Parallel, 2-3 hours]

**What you do differently**:

For non-trivial tasks (most work):
1. Verify spec closure (NEW)
2. Spawn 10 agents in parallel (INSTEAD of sequential planning)
3. Let them work independently (NO coordination)
4. Analyze what they converged on (collision detection)
5. Synthesize best solution (convergence)

**Example workflow**:

```bash
# OLD WORKFLOW (Sequential)
Plan agent: 1 hour (design)
  ↓
Code agent: 2 hours (implement)
  ↓
Test agent: 1 hour (test)
  ↓
Reviewer: 1 hour (review)
─────────────────────
Total: 5 hours

# NEW WORKFLOW (Parallel EPIC 9)
/speckit-verify [spec]
  ↓ (verify closure)
  ↓
/bb80-parallel "[specification]"
  ├─ 10 agents in parallel (2 hours)
  ├─ Collision detection (30 min)
  ├─ Convergence (30 min)
  └─ Ready for deployment
─────────────────────
Total: 3 hours (40% faster, 10 perspectives)
```

**Why it works**:
- Parallelism: All agents work simultaneously
- Diversity: Multiple perspectives prevent blind spots
- Confidence: When agents converge, you know you're right
- Coverage: Agents hit different parts of the solution space

**Trivial tasks** (still sequential):
- Reading one file
- Running one script
- Displaying help
- (Most tasks are NOT trivial)

**Skills to learn**:
- `bb80-parallel-agents` (when to use, why it works)
- `bb80-specification-closure` (prerequisite)

**Commands to use**:
- `/speckit-verify` (before EPIC 9)
- `/bb80-parallel` (orchestrate cycle)
- `/collision-detect` (analyze overlaps)
- `/convergence` (synthesize results)

---

### Shift 3: Deterministic Validation (Evidence-First)

**OLD**: "Code looks good. Tests should pass. Performance is fine." (Opinion)

**NEW**: "[Receipt] cargo make lint ✓ | [Receipt] All 347 tests pass | [Receipt] SLO met" (Evidence)

**What you do differently**:

Replace narrative review with reproducible receipts:

```
❌ BAD:
  "I reviewed the code. It looks good. Error handling seems correct.
   Performance should be fine."

✅ GOOD:
  [Receipt] cargo make check: ✓ (clean compilation)
  [Receipt] cargo make lint: ✓ (0 clippy violations)
  [Receipt] cargo make test: ✓ (347/347 tests pass)
  [Receipt] cargo make slo-check: ✓ (all SLOs met)
  [Receipt] Specification coverage: 95%
```

**Why it matters**:
- Reproducible (anyone can verify)
- Measurable (0 violations, not "probably fine")
- Auditable (timestamps, versions)
- Objective (no opinion involved)

**Skill to learn**: `bb80-deterministic-receipts` (what counts as evidence)

**How to produce receipts**:
```bash
# Before marking "done":
cargo make pre-commit  # Produces timestamped receipts
                        # (check + lint + test + format all pass)

# Reference the receipts in your communication:
"[Receipt] cargo make pre-commit: ✓ PASS (all phases clean)"
```

---

## Phased Adoption Plan

### Phase 0: Learning (1-2 days)

**For each person**:

1. Read the three skills:
   - `.claude/skills/bb80-specification-closure/SKILL.md` (15 min)
   - `.claude/skills/bb80-parallel-agents/SKILL.md` (15 min)
   - `.claude/skills/bb80-deterministic-receipts/SKILL.md` (15 min)

2. Read the new agents (reference only):
   - `.claude/agents/bb80-specification-validator.md` (5 min)
   - `.claude/agents/bb80-collision-detector.md` (5 min)
   - `.claude/agents/bb80-convergence-orchestrator.md` (5 min)
   - `.claude/agents/bb80-parallel-task-coordinator.md` (5 min)

3. Review updated CLAUDE.md:
   - Focus on EPIC 9 section (15 min)
   - Review Specification Closure section (10 min)
   - Check Deterministic Validation section (10 min)

**Total learning time**: ~90 minutes per person

**Success criteria**: Can explain the three paradigm shifts without notes

---

### Phase 1: Guided Practice (3-7 days)

**Pick ONE real task**, practice with guidance:

1. **Task selection**: Choose a feature or bug fix you were planning anyway

2. **Specification closure**:
   ```bash
   /speckit-verify [feature]
   ```
   - Does it say "100% closed" or "72% closed"?
   - If incomplete: clarify with user BEFORE starting code
   - This is the new normal - specification-first!

3. **Parallel agents**:
   ```bash
   /bb80-parallel "[full specification from closure check]"
   ```
   - Watch 10 agents work in parallel (cool!)
   - See collision detection identify where agents converged
   - Observe convergence synthesize best solution

4. **Validation**:
   ```bash
   cargo make pre-commit
   ```
   - Verify all receipts pass
   - Reference receipts instead of narratives

5. **Review**:
   - Notice how the synthesized result is cleaner than sequential planning
   - Compare time spent (new workflow faster? better?)
   - Discuss with team: what worked, what didn't

**Expected outcome**: One complete feature using EPIC 9 cycle

**If problems arise**:
- Specification incomplete? Run `/speckit-verify` again, clarify
- Agents diverged completely? Return to specification
- Convergence failed? Review selection criteria
- Ask for help from team leads

---

### Phase 2: Routine Use (Ongoing)

**Apply to ALL non-trivial tasks**:

1. **Default behavior**: Assume EPIC 9 applies
2. **Specification first**: Always `/speckit-verify` before implementation
3. **Parallel agents**: Use `/bb80-parallel` for all multi-step work
4. **Receipts only**: Never say "looks good", always show evidence

**Trivial tasks still sequential**:
- Single file reads
- Running existing scripts
- Displaying help
- (Keep the speedup for real work)

**Measure the impact**:
- Time to implementation (should decrease)
- Iteration rate (should decrease)
- Confidence in solutions (should increase)
- Team alignment (should improve)

---

## FAQ & Troubleshooting

### Q: "But I just want to code, not plan 10 different ways"

**A**: You're not planning 10 ways - 10 agents are exploring 10 ways in parallel while you might do other work. Then collision detection tells you which 3-4 are worth considering. Total time is LESS than sequential, not more.

### Q: "What if agents produce garbage?"

**A**: Collision detection will show you. If all 10 agents diverge (RED collision), that's a signal your specification is vague. Go back to `/speckit-verify`, clarify, retry. This is FASTER than iterating implementation.

### Q: "I'm already practicing specification closure intuitively"

**A**: Great! Now formalize it with `/speckit-verify` so it's reproducible. The checklist ensures you catch edge cases you might miss intuitively.

### Q: "Receipts seem verbose"

**A**: They're verbose on paper, but copy-paste is easy. And "0 clippy violations" is clearer than "I checked it". For important work, verbose evidence beats terse opinion.

### Q: "Our team is still sequential planning"

**A**: That's fine - EPIC 9 is opt-in per-task. Use it for complex features where parallelism helps most (architecture, multi-crate changes). Keep sequential for tiny fixes.

### Q: "When do we use convergence vs. just picking one agent's output?"

**A**: If only 1-2 agents produced good work, pick the best. If 5+ agents converged with minor variations, use convergence to synthesize best parts. Convergence is orchestrated synthesis, not consensus voting.

### Q: "Does this break existing CLAUDE.md rules?"

**A**: No. Cargo Make, Chicago TDD, Andon Signals, Error Handling, Path Protection - all still apply. EPIC 9 is an orchestration layer on top. Existing rules still enforce quality at the implementation level.

### Q: "How do we onboard new team members?"

**A**: Same as above - Phase 0 learning (90 min), then Phase 1 guided practice on one task with a mentor. They'll get it quickly.

---

## Checklist: Before Using EPIC 9

Before you run `/bb80-parallel`, verify:

- [ ] I understand the three paradigm shifts (read the skills)
- [ ] Specification is closed (ran `/speckit-verify`, score 100%)
- [ ] No vague terms in spec (TBD, maybe, probably all gone)
- [ ] Task is non-trivial (not a single file read or one-liner)
- [ ] I'm prepared to see 10 agents work in parallel (exciting!)
- [ ] I understand collision detection (overlap = good signal)
- [ ] I know selection pressure criteria (coverage, minimality, SLOs)
- [ ] I'll produce receipts, not narratives (cargo make pre-commit)

If ANY are unchecked: stop, review the skill / CLAUDE.md section, ask for help.

---

## Integration with Existing Workflows

### Cargo Make Protocol ✓
EPIC 9 agents each run `cargo make check`. Collision detection analyzes results.
Convergence selects agent with best SLO compliance.
NO CHANGE to "always use cargo make" rule.

### Chicago TDD ✓
Agents write tests independently. Collision detection finds where tests converge.
Convergence merges complementary test strategies.
NO CHANGE to "state-based testing" rule.

### RDF-First Specifications ✓
Specification closure works with .ttl files.
Agents read spec.ttl and design accordingly.
Convergence validates coverage against spec entities.
NO CHANGE to "TTL is source of truth" rule.

### Andon Signals ✓
Agents run `cargo make check`. Red signals stop individual agent progress.
Collision detection catches if ALL agents hit red (indicates bad spec).
NO CHANGE to "stop the line" rule.

### Error Handling ✓
Agents write Result<T, E>. Convergence selects error approach.
Collision detection reveals if agents diverge on error strategy.
NO CHANGE to "no unwrap in production" rule.

---

## Timeline

- **T+0**: RFC approved, documentation pushed
- **T+1 week**: Phase 0 learning (team reads skills)
- **T+2 weeks**: Phase 1 practice (first EPIC 9 task attempted)
- **T+4 weeks**: Phase 2 rollout (EPIC 9 becomes default)
- **T+8 weeks**: Metric review (measure speed/quality improvements)

---

## Metrics to Track

**Before EPIC 9**:
- Time per feature: 5 hours
- Iteration rate: 1-2 revisions per feature
- Agent disagreement rate: N/A (sequential)
- Specification incompleteness: TBD items in specs

**After EPIC 9** (target):
- Time per feature: 3 hours (40% speedup)
- Iteration rate: <1 revision (moved to spec phase)
- Agent agreement rate: 70%+ (collision-based confidence)
- Specification incompleteness: 0 TBD items (closure required)

---

## Resources

**Read in order**:

1. `.refactor/REFACTOR_STRATEGY.md` (this explains the WHY)
2. `CLAUDE.md` § EPIC 9 (this explains the WHAT)
3. `.claude/skills/bb80-*/SKILL.md` (deep dives on each shift)
4. `.claude/agents/bb80-*/md` (reference for agents)
5. `.claude/commands/` (usage examples)

**Commands to practice**:

```bash
/speckit-verify 004          # Check spec closure
/bb80-parallel "[spec]"      # Run full EPIC 9
/collision-detect            # Analyze overlaps
/convergence [report]        # Synthesize result
```

---

## Questions?

For questions:
- Review the relevant skill (`.claude/skills/`)
- Check CLAUDE.md § EPIC 9
- Look at refactor strategy (`.refactor/REFACTOR_STRATEGY.md`)
- Ask team leads (they've practiced Phase 1)

---

**Version**: 1.0.0
**Status**: Ready for adoption
**Effective**: 2026-01-03
**Next review**: 2026-02-03
