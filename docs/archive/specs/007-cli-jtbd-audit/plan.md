# Implementation Plan: CLI Jobs-to-be-Done Audit

**Branch**: `007-cli-jtbd-audit` | **Date**: 2024-12-14 | **Spec**: [spec.md](./spec.md)
**Input**: Feature specification from `/specs/007-cli-jtbd-audit/spec.md`

## Summary

Comprehensive JTBD audit framework evaluating 47+ ggen CLI commands for:
1. **Functional Correctness**: Command execution, error handling, output format
2. **Agent Accessibility**: AI coding agent usability (7 avatars)
3. **Maturity Classification**: L0-L5 maturity levels with L4+ required for "agent-usable"
4. **Enterprise Validation**: 7 Fortune 500 case study mappings

**Timeline**: 1 week rapid assessment (~10 commands/day)
**Evidence Storage**: `specs/007-cli-jtbd-audit/evidence/`

## Technical Context

**Language/Version**: Rust 1.74+ (edition 2021) - existing ggen toolchain
**Primary Dependencies**: ggen CLI (v4.0.0), cargo-make, existing workspace crates
**Storage**: Filesystem-based (YAML audit results, markdown reports) in feature evidence directory
**Testing**: cargo make test, shell script execution for CLI validation
**Target Platform**: macOS/Linux (CLI audit execution environment)
**Project Type**: Audit framework (no new crates - uses existing CLI)
**Performance Goals**: Complete 47+ command audits in ≤1 week (~10/day)
**Constraints**: L4+ maturity (80%) required for agent-usable; non-determinism caps at L3
**Scale/Scope**: 47+ commands × 7 avatars × 7 case studies = 329+ evaluation data points

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

Verify compliance with ggen Constitution v1.0.0 (`.specify/memory/constitution.md`):

- [x] **I. Crate-First Architecture**: N/A - This is an audit feature, not a new crate. Uses existing CLI infrastructure.
- [x] **II. Deterministic RDF Projections**: Audit results are deterministic (same command → same evaluation). Evidence files are version-controlled.
- [x] **III. Chicago TDD**: Audit execution will use state-based verification (run command → check exit code, output). Tests verify observable behavior.
- [x] **IV. cargo make Protocol**: All CLI testing uses `cargo make` targets. Respects SLOs.
- [x] **V. Type-First Thinking**: Audit templates use structured YAML schemas. Maturity levels are type-safe enums (L0-L5, L0-DEP).
- [x] **VI. Andon Signal Protocol**: Audit captures RED (L0-broken), YELLOW (L1-L3 warnings), GREEN (L4-L5 agent-ready) signals per command.
- [x] **VII. Error Handling**: Audit scripts will use proper error handling. Production code not being modified.
- [x] **VIII. Concurrent Execution**: Audit execution can be parallelized by command category. Evidence stored in proper subdirectories (not root).
- [x] **IX. Lean Six Sigma Quality**: Comprehensive audit with quantifiable metrics (47+ commands, 7 criteria weighted scoring).

**Quality Gates Pass?**: [x] YES / [ ] NO

## Project Structure

### Documentation (this feature)

```text
specs/007-cli-jtbd-audit/
├── plan.md              # This file
├── spec.md              # Feature specification (complete)
├── research.md          # Phase 0 output
├── data-model.md        # Phase 1 output
├── quickstart.md        # Phase 1 output
├── tasks.md             # Phase 2 output (/speckit.tasks)
├── checklists/
│   └── requirements.md  # Requirements tracking
├── data/
│   ├── command-inventory.yaml   # 47+ commands
│   ├── avatar-personas.yaml     # 7 AI agent avatars
│   ├── case-studies.yaml        # 7 Fortune 500 scenarios
│   └── maturity-matrix.yaml     # L0-L5 criteria
└── evidence/
    ├── workflow/        # Workflow command audits
    ├── template/        # Template command audits
    ├── project/         # Project command audits
    ├── ontology/        # Ontology command audits
    ├── graph/           # Graph command audits
    ├── marketplace/     # Marketplace command audits
    ├── fmea/            # FMEA command audits
    └── ai/              # AI command audits
```

### Source Code (repository root)

This feature does not create new source code. It audits existing CLI:

```text
crates/ggen-cli/src/cmds/    # Existing CLI commands being audited
├── workflow.rs
├── template.rs
├── project.rs
├── ontology.rs
├── graph.rs
├── marketplace.rs
├── fmea.rs
└── ai.rs
```

**Structure Decision**: Audit-only feature using existing ggen infrastructure. All outputs stored in feature spec directory (`specs/007-cli-jtbd-audit/`).

## Complexity Tracking

> No violations - Constitution Check passed with all gates GREEN.

| Violation | Why Needed | Simpler Alternative Rejected Because |
|-----------|------------|-------------------------------------|
| N/A | N/A | N/A |

---

## Post-Design Constitution Re-Check

*Re-evaluated after Phase 1 design artifacts completed.*

| Principle | Pre-Design | Post-Design | Notes |
|-----------|------------|-------------|-------|
| I. Crate-First | N/A | N/A | Audit feature, no new crates |
| II. Deterministic | ✅ | ✅ | YAML schemas enforce deterministic evidence format |
| III. Chicago TDD | ✅ | ✅ | State-based testing via exit codes, output verification |
| IV. cargo make | ✅ | ✅ | Quickstart uses cargo make targets |
| V. Type-First | ✅ | ✅ | JSON schemas in contracts/ define types |
| VI. Andon Protocol | ✅ | ✅ | Maturity levels map to RED/YELLOW/GREEN |
| VII. Error Handling | ✅ | ✅ | Scripts use proper exit code handling |
| VIII. Concurrent | ✅ | ✅ | Evidence in category subdirectories |
| IX. Lean Six Sigma | ✅ | ✅ | Quantifiable scoring rubric |

**Post-Design Status**: ✅ ALL GATES PASS

---

## Generated Artifacts

| Artifact | Path | Status |
|----------|------|--------|
| Implementation Plan | `specs/007-cli-jtbd-audit/plan.md` | ✅ Complete |
| Research | `specs/007-cli-jtbd-audit/research.md` | ✅ Complete |
| Data Model | `specs/007-cli-jtbd-audit/data-model.md` | ✅ Complete |
| Audit Schema | `specs/007-cli-jtbd-audit/contracts/audit-result.schema.yaml` | ✅ Complete |
| Case Study Schema | `specs/007-cli-jtbd-audit/contracts/case-study-validation.schema.yaml` | ✅ Complete |
| Quickstart Guide | `specs/007-cli-jtbd-audit/quickstart.md` | ✅ Complete |

---

## Next Steps

1. Run `/speckit.tasks` to generate actionable task breakdown
2. Execute audit per quickstart guide (~10 commands/day)
3. Generate reports after completion
