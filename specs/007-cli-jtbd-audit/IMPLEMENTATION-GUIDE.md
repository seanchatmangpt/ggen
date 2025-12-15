# CLI JTBD Audit - Implementation Guide

**Status**: ✅ Phase 1-2 Complete | Phase 3 Ready to Begin
**Date**: 2024-12-14
**Feature Branch**: `007-cli-jtbd-audit`

---

## Executive Summary

The audit framework infrastructure is now complete and ready for Phase 3 command audits. This document provides guidance for implementing the remaining phases (US1-US5).

### What's Ready

- ✅ **Evidence directory structure** created for all 8 command categories
- ✅ **Audit schemas** validated (audit-result and case-study-validation)
- ✅ **Scoring rubric** documented with weighted criteria
- ✅ **Audit templates** with YAML structure and examples
- ✅ **Sample audits** demonstrating L2, L4 maturity levels
- ✅ **Sample case study** (JPMorgan) showing gap documentation

### Next Steps: Phase 3 (US1 - Command Functionality)

**Goal**: Audit all 47+ ggen CLI commands for functional correctness

**Duration**: ~5 days (~10 commands/day)

**Daily Schedule**:
- **Day 1**: 5-6 commands (workflow, utils)
- **Day 2**: 8 commands (template)
- **Day 3**: 11 commands (project, graph)
- **Day 4**: 7 commands (ontology, ai)
- **Day 5**: 16 commands (marketplace, fmea, ci)

---

## How to Audit a Command

### Quick Reference

```bash
# For each command, create evidence/CATEGORY/COMMAND.yaml following this pattern:
# 1. Run command help: ggen <cmd> --help
# 2. Run happy path: ggen <cmd> <valid-args>
# 3. Run error case: ggen <cmd> <invalid-args>
# 4. Record results in YAML
# 5. Score agent accessibility (0-100)
# 6. Assign maturity level (L0-L5)
```

### Detailed Steps

#### Step 1: Functional Correctness Testing

```bash
COMMAND="template generate"
CATEGORY="template"
SLUG="template-generate"

# Test 1: Help works
ggen $COMMAND --help > evidence/$CATEGORY/$SLUG-help.log 2>&1
HELP_EXIT=$?

# Test 2: Happy path (requires sample template)
ggen $COMMAND --template examples/simple.tmpl --output /tmp/test > evidence/$CATEGORY/$SLUG-happy.log 2>&1
HAPPY_EXIT=$?

# Test 3: Error handling (invalid input)
ggen $COMMAND --template nonexistent.tmpl > evidence/$CATEGORY/$SLUG-error.log 2>&1
ERROR_EXIT=$?
```

**Record in YAML**:
```yaml
functional_correctness:
  executes: true/false           # Command runs without crash
  help_works: $HELP_EXIT == 0    # --help available
  happy_path: $HAPPY_EXIT == 0   # Valid inputs work
  error_handling: $ERROR_EXIT != 0 # Invalid inputs fail gracefully
  exit_codes: true/false         # Exit codes 0 (success) vs non-zero (error)
```

#### Step 2: Agent Accessibility Scoring

For each of 7 criteria, assign 0-MAX points:

| Criterion | Max | L1 (0) | L3 (Partial) | L5 (Full) |
|-----------|-----|--------|--------------|-----------|
| **Parseable Output** | 25 | Text only (5) | JSON option (15) | JSON schema (25) |
| **Error Messages** | 20 | Stack trace (4) | Human-readable (12) | Machine-parseable (20) |
| **Idempotency** | 15 | Destructive (3) | Mostly safe (9) | Fully idempotent (15) |
| **Progress Feedback** | 10 | Silent (2) | Final status (6) | Streaming (10) |
| **Dry-Run Support** | 10 | None (0) | Partial (5) | Full preview (10) |
| **Documentation** | 10 | --help only (2) | Examples (6) | JTBD + examples (10) |
| **Exit Codes** | 10 | 0/1 only (2) | Differentiated (6) | Semantic (10) |

**Total Score = sum of 7 criteria (0-100)**

**Record in YAML**:
```yaml
agent_score: 75                    # Sum of breakdown
agent_breakdown:
  parseable_output: 18
  error_messages: 15
  idempotency: 12
  progress_feedback: 7
  dry_run: 5
  documentation: 8
  exit_codes: 10
```

#### Step 3: Avatar Evaluation Notes

For each avatar, record specific observations:

```yaml
avatar_notes:
  claude_code: "JSON output available; deterministic; suitable for orchestration"
  cursor_ai: "Help text concise (8K context); works for single commands"
  copilot: "Output patterns learnable; works with tab completion"
  aider: "Git-trackable output; safe for diff-based workflows"
  devin: "Self-contained; good for autonomous execution"
  openhands: "Customizable config support; integrates well"
  windsurf: "IDE-friendly output format available"
```

#### Step 4: Assign Maturity Level

```
Score 0-19   → L1 (Initial)     - 20% agent-friendly
Score 20-39  → L2 (Managed)     - 40% agent-friendly
Score 40-59  → L3 (Defined)     - 60% agent-friendly
Score 60-79  → L4 (Quantified)  - 80% agent-friendly ← AGENT-USABLE THRESHOLD
Score 80-100 → L5 (Optimized)   - 100% agent-friendly
```

**Blockers that cap maturity**:
- Non-deterministic output → max L3
- Crashes (executes=false) → L0
- Deprecated → L0-DEP

**Record in YAML**:
```yaml
maturity_level: "L4"
maturity_blockers: []

# Or with blockers:
maturity_level: "L3"
maturity_blockers:
  - "No JSON output format (blocks L4+)"
  - "Non-deterministic ordering on large result sets"
```

#### Step 5: Recommendations

For commands not at L5, document improvement path:

```yaml
recommendations:
  - priority: "P1"  # P1 (critical), P2 (important), P3 (nice-to-have)
    description: "Add --output-format json for structured results"
    effort: "medium"  # low, medium, high
    impact: "high"    # low, medium, high
```

---

## File Template

Use this template for each command audit:

```yaml
command: "ggen CATEGORY SUBCOMMAND"
version_tested: "4.0.0"
date: "YYYY-MM-DD"
tester: "claude-code"

functional_correctness:
  executes: true/false
  help_works: true/false
  happy_path: true/false
  error_handling: true/false
  exit_codes: true/false

agent_score: 0-100
agent_breakdown:
  parseable_output: 0-25
  error_messages: 0-20
  idempotency: 0-15
  progress_feedback: 0-10
  dry_run: 0-10
  documentation: 0-10
  exit_codes: 0-10

avatar_notes:
  claude_code: ""
  cursor_ai: ""
  copilot: ""
  aider: ""
  devin: ""
  openhands: ""
  windsurf: ""

maturity_level: "L0|L0-DEP|L1|L2|L3|L4|L5"
maturity_blockers: []

evidence_files:
  - template-generate-help.log
  - template-generate-happy-path.log
  - template-generate-error.log

recommendations: []

jtbd: |
  When [user need], I need to [use this command],
  so that [business outcome].
```

---

## Phase 3 Checklist: Day 1 (Workflow + Utils)

Commands to audit:
- [ ] ggen workflow analyze
- [ ] ggen workflow init
- [ ] ggen workflow report
- [ ] ggen workflow event
- [ ] ggen workflow discover
- [ ] ggen utils (all subcommands)

**Target**: 6 audit YAML files in `evidence/workflow/` and `evidence/utils/`

**Success Criteria**:
- [ ] All 6 files created
- [ ] All functional_correctness fields filled
- [ ] All agent_score calculated
- [ ] All maturity_level assigned
- [ ] All evidence log files referenced

---

## Parallel Execution Opportunities

Commands can be audited in parallel if they're in different categories:

```bash
# Day 2: Audit all template commands in parallel
for cmd in new list lint generate get show generate-tree regenerate; do
  ggen template $cmd --help > evidence/template/template-$cmd-help.log 2>&1 &
done
wait
```

---

## Sample Audits

Three sample audits are included to demonstrate different maturity levels:

1. **`evidence/workflow/workflow-analyze.yaml`** (L4 - Agent-Usable)
   - Demonstrates excellent JSON output, good error handling
   - Shows full agent accessibility

2. **`evidence/template/template-lint.yaml`** (L2 - Managed)
   - Demonstrates basic functionality but limited agent accessibility
   - Shows blockers and recommendations for improvement

3. **`evidence/case-studies/jpmorgan.yaml`** (Fortune 500 Gap Analysis)
   - Demonstrates case study validation format
   - Shows how to document gaps and workarounds

Use these as templates for consistency.

---

## Validation Rules

- **agent_score must equal sum of agent_breakdown values**
  - If breakdown = [18, 15, 12, 7, 5, 8, 10], then agent_score must = 75

- **maturity_level must follow formula** (unless blockers apply):
  - `maturity_level = floor(agent_score / 20)`
  - Score 75 → floor(75/20) = floor(3.75) = 3 → L3
  
  **Exception**: If blocker exists, use lower level
  - Non-determinism → max L3 (even if score is 80+)
  - Crash → L0
  - Deprecated → L0-DEP

- **All 7 avatar_notes must be present**
  - Keys: claude_code, cursor_ai, copilot, aider, devin, openhands, windsurf

---

## Constitutional Alignment

This audit maintains alignment with ggen Constitution principles:

- ✅ **II. Deterministic RDF Projections**: Audit structure is fixed and reproducible
- ✅ **III. Chicago TDD**: State-based evaluation (command behavior verification)
- ✅ **V. Type-First Thinking**: YAML schemas enforce type safety
- ✅ **VI. Andon Signal Protocol**: 
  - RED (L0) = command broken
  - YELLOW (L1-L3) = command works but needs improvement
  - GREEN (L4-L5) = agent-ready
- ✅ **IX. Lean Six Sigma Quality**: Quantifiable scoring with documented criteria

---

## Next Phases After Phase 3

Once all 47+ commands are audited with functional_correctness:

**Phase 4 (US2)**: Add agent accessibility scores and avatar notes
**Phase 5 (US3)**: Assign maturity levels and blockers
**Phase 6 (US4)**: Validate case studies (JPMorgan, Amazon, Pfizer, etc.)
**Phase 7 (US5)**: Write JTBD documentation for L4+ commands
**Phase 8**: Generate final reports and roadmap

---

## Contact & Escalation

If you encounter:
- **Command crashes during audit**: Record in functional_correctness.executes = false, maturity = L0
- **Unclear error messages**: Record in recommendations as P1, might block L3+
- **Non-deterministic output**: Record blocker "non-determinism caps at L3"
- **Questions on scoring**: Refer to evidence/scoring-guide.md

**Expected Phase 3 Completion**: ~5 days from start
**Expected Full Audit Completion**: ~7 days total

---

**Last Updated**: 2024-12-14
**Framework Version**: 1.0.0
**Ready for**: Phase 3 Implementation
