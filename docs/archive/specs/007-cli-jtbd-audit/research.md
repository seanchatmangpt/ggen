# Research: CLI Jobs-to-be-Done Audit

**Feature Branch**: `007-cli-jtbd-audit`
**Date**: 2024-12-14

## Executive Summary

Research findings for implementing the CLI JTBD audit framework. All technical context was pre-clarified; this document consolidates best practices and patterns for CLI auditing, agent accessibility evaluation, and maturity assessment.

---

## Research Areas

### 1. CLI Audit Methodology

**Decision**: State-based functional testing with structured evidence collection

**Rationale**:
- Aligns with Chicago TDD (Constitution Principle III)
- Tests verify observable behavior: exit codes, stdout/stderr, file outputs
- Reproducible: same command + inputs → same results
- Evidence trail for compliance and trend analysis

**Alternatives Considered**:
| Alternative | Why Rejected |
|-------------|--------------|
| Manual inspection only | Not reproducible, no evidence trail |
| Mock-based testing | Violates Chicago TDD; doesn't test real behavior |
| Coverage-only metrics | Doesn't capture agent accessibility or UX quality |

**Implementation Pattern**:
```bash
# Audit execution pattern
ggen <command> <args> 2>&1 | tee evidence/<category>/<command>.log
echo "Exit code: $?" >> evidence/<category>/<command>.log
```

---

### 2. Agent Accessibility Scoring

**Decision**: Weighted rubric with 7 criteria (100-point scale)

**Rationale**:
- Quantifiable: enables objective comparison across commands
- Avatar-agnostic core criteria with avatar-specific notes
- L4+ (80 points) threshold provides clear "agent-usable" signal
- Weighted criteria reflect real-world agent limitations

**Criteria Weights** (from spec):
| Criterion | Weight | Justification |
|-----------|--------|---------------|
| Parseable Output | 25% | Agents MUST parse output to verify success |
| Error Messages | 20% | Agents need actionable errors for self-correction |
| Idempotency | 15% | Agents retry operations; destructive = dangerous |
| Progress Feedback | 10% | Long operations need visibility |
| Dry-Run Support | 10% | Preview before execution reduces risk |
| Documentation | 10% | Agents rely on --help and examples |
| Exit Codes | 10% | Semantic codes enable branching logic |

**Alternatives Considered**:
| Alternative | Why Rejected |
|-------------|--------------|
| Binary pass/fail per avatar | Loses nuance; can't prioritize improvements |
| Unweighted average | Parseable output more critical than docs |
| Avatar-specific scores only | Need cross-avatar comparable metric |

---

### 3. Maturity Level Assessment

**Decision**: 6-level scale (L0, L0-DEP, L1-L5) with explicit criteria checklists

**Rationale**:
- L0-DEP distinguishes deprecated from broken (clarification decision)
- Checklist-based: objective, verifiable criteria per level
- Non-determinism cap at L3 (clarification decision)
- L4+ required for "agent-usable" (clarification decision)

**Level Definitions** (from spec):
| Level | Agent-Friendly | Key Differentiator |
|-------|----------------|-------------------|
| L0 | 0% | Broken/absent |
| L0-DEP | 0% | Deprecated (intentionally unsupported) |
| L1 | 20% | Executes but unpredictable |
| L2 | 40% | Happy path works |
| L3 | 60% | Actionable errors |
| L4 | 80% | JSON output, semantic exit codes |
| L5 | 100% | Full agent optimization |

**Progression Blockers**:
- Non-deterministic output → capped at L3 max
- Missing JSON output → cannot reach L4
- No --dry-run → cannot reach L5

---

### 4. Evidence Collection Strategy

**Decision**: Structured YAML + execution logs per command in category subdirectories

**Rationale**:
- YAML enables programmatic report generation
- Logs provide reproducible evidence trail
- Category subdirectories (workflow/, template/, etc.) organize 47+ commands
- Git-tracked for version history and trend analysis

**Evidence Structure**:
```yaml
# evidence/<category>/<command>.yaml
command: ggen <subcommand>
version_tested: 4.0.0
date: 2024-12-14
tester: claude-code

functional_correctness:
  executes: true
  help_works: true
  happy_path: true
  error_handling: true
  exit_codes: true

agent_accessibility:
  score: 75  # 0-100
  breakdown:
    parseable_output: 15/25
    error_messages: 18/20
    idempotency: 12/15
    progress_feedback: 8/10
    dry_run: 5/10
    documentation: 10/10
    exit_codes: 7/10

  avatar_notes:
    claude_code: "JSON output available via --json flag"
    cursor_ai: "Help output exceeds 8K tokens"
    # ... other avatars

maturity_level: L4
evidence_files:
  - <command>.log
  - <command>-happy-path.log
  - <command>-error.log
```

---

### 5. Fortune 500 Case Study Validation

**Decision**: Command sequence mapping with gap analysis

**Rationale**:
- Each case study has specific JTBD → maps to command sequence
- Validation = execute sequence, record success/partial/blocked
- Gap analysis identifies missing capabilities for enterprise readiness

**Validation Pattern**:
```yaml
# evidence/case-studies/<company>.yaml
case_study: jpmorgan
commands_required:
  - ggen ontology validate: PASS
  - ggen template generate: PASS
  - ggen graph query: PARTIAL (missing --output-format json)

gaps:
  - command: ggen graph query
    issue: No JSON output option
    severity: P2
    workaround: Parse text output manually
```

---

### 6. Rapid Audit Execution (1 Week Target)

**Decision**: Parallel execution by category with daily batching

**Rationale**:
- 47+ commands ÷ 5 days = ~10 commands/day
- Categories enable parallel work streams
- Daily batching allows progress tracking

**Execution Schedule**:
| Day | Category | Commands |
|-----|----------|----------|
| 1 | workflow, utils | 5-6 |
| 2 | template | 8 |
| 3 | project, graph | 11 |
| 4 | ontology, ai | 7 |
| 5 | marketplace, fmea | 16 |
| 6-7 | Reports, gap analysis | N/A |

**Parallel Execution Pattern**:
```bash
# Execute category audits in parallel
./scripts/audit-workflow.sh &
./scripts/audit-template.sh &
./scripts/audit-project.sh &
wait
```

---

### 7. Reporting Strategy

**Decision**: Three output reports from aggregated evidence

**Reports**:
1. **Maturity Matrix Report** (`reports/maturity-matrix.md`)
   - All commands with L0-L5 levels
   - Distribution summary (how many at each level)
   - Commands needing improvement to reach L4

2. **Avatar Compatibility Matrix** (`reports/avatar-compatibility.md`)
   - 47 commands × 7 avatars
   - Score + notes per cell
   - Avatar-specific recommendations

3. **Fortune 500 Gap Analysis** (`reports/fortune500-gaps.md`)
   - 7 case studies with validation status
   - Command gaps per case study
   - Remediation roadmap

---

## Resolved Clarifications (from /speckit.clarify)

| Question | Answer | Impact |
|----------|--------|--------|
| Evidence storage location | `specs/007-cli-jtbd-audit/evidence/` | All YAML + logs co-located |
| Deprecated command handling | L0-DEP (counted, labeled) | Separate from broken commands |
| Agent-usable threshold | L4 (80%) | Higher bar than L3 default |
| Audit timeframe | 1 week (~10 commands/day) | Rapid assessment pace |
| Non-determinism handling | Cap at L3 max | Blocks L4+ agent-usable status |

---

## Dependencies & Prerequisites

### Required Tools
- ggen CLI v4.0.0 (installed)
- cargo-make (installed)
- jq (for JSON processing)
- yq (for YAML processing) - optional

### Existing Assets (from spec phase)
- `data/command-inventory.yaml` - 47+ commands listed
- `data/avatar-personas.yaml` - 7 avatars defined
- `data/case-studies.yaml` - 7 Fortune 500 scenarios
- `data/maturity-matrix.yaml` - L0-L5 criteria

### Evidence Directory Structure
```bash
mkdir -p evidence/{workflow,template,project,ontology,graph,marketplace,fmea,ai,case-studies}
mkdir -p reports
```

---

## Next Steps

1. **Phase 1**: Generate `data-model.md` with entity schemas
2. **Phase 1**: Create `contracts/` with audit templates
3. **Phase 1**: Write `quickstart.md` for audit execution
4. **Phase 2**: Generate `tasks.md` via `/speckit.tasks`
5. **Implementation**: Execute audits per schedule
