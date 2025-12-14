# Requirements Checklist: CLI Jobs-to-be-Done Audit

**Feature Branch**: `007-cli-jtbd-audit`
**Last Updated**: 2024-12-14

---

## Functional Requirements Status

### Audit Framework (FR-001 to FR-004)

| ID | Requirement | Status | Evidence |
|----|-------------|--------|----------|
| FR-001 | Structured evaluation template for each CLI command | [ ] Pending | |
| FR-002 | Capture functional correctness and agent accessibility scores | [ ] Pending | |
| FR-003 | Track maturity level (L0-L5) for each command | [ ] Pending | |
| FR-004 | Map commands to Fortune 500 case study requirements | [ ] Pending | |

### Agent Evaluation (FR-005 to FR-007)

| ID | Requirement | Status | Evidence |
|----|-------------|--------|----------|
| FR-005 | Evaluate each command against all 7 avatar personas | [ ] Pending | |
| FR-006 | Record specific failure modes per avatar | [ ] Pending | |
| FR-007 | Provide recommendations for improving agent accessibility | [ ] Pending | |

### Reporting (FR-008 to FR-010)

| ID | Requirement | Status | Evidence |
|----|-------------|--------|----------|
| FR-008 | Generate maturity matrix report showing all commands | [ ] Pending | |
| FR-009 | Generate command-by-avatar compatibility matrix | [ ] Pending | |
| FR-010 | Identify gaps between current state and Fortune 500 requirements | [ ] Pending | |

### Evidence Collection (FR-011 to FR-013)

| ID | Requirement | Status | Evidence |
|----|-------------|--------|----------|
| FR-011 | Capture command execution logs as audit evidence | [ ] Pending | |
| FR-012 | Store screenshots/recordings of agent interaction attempts | [ ] Pending | |
| FR-013 | Version audit results for trend analysis | [ ] Pending | |

---

## Success Criteria Status

| ID | Criterion | Target | Current | Status |
|----|-----------|--------|---------|--------|
| SC-001 | CLI commands evaluated with functional correctness | 100% (47+) | 0% | [ ] Pending |
| SC-002 | Commands evaluated against 7 avatar personas | 100% | 0% | [ ] Pending |
| SC-003 | Commands assigned maturity level with evidence | 100% | 0% | [ ] Pending |
| SC-004 | Fortune 500 case studies mapped to commands | 7/7 | 0/7 | [ ] Pending |
| SC-005 | Gaps documented with remediation plan | All | 0 | [ ] Pending |
| SC-006 | Agent accessibility score average | ≥60% (L3+) | TBD | [ ] Pending |
| SC-007 | Commands at L0 (broken) in production | 0 | TBD | [ ] Pending |
| SC-008 | JTBD documentation for L3+ commands | 100% | 0% | [ ] Pending |

---

## User Story Acceptance Status

### US-1: Command Functionality Audit (P1)

| Scenario | Status | Evidence |
|----------|--------|----------|
| `ggen --help` shows all subcommands | [ ] Pending | |
| `ggen ontology validate ./test.ttl` succeeds with valid file | [ ] Pending | |
| `ggen ontology validate ./invalid.ttl` fails with helpful message | [ ] Pending | |
| `ggen template generate` (no args) shows usage instructions | [ ] Pending | |

### US-2: Agent Accessibility Evaluation (P1)

| Scenario | Status | Evidence |
|----------|--------|----------|
| Claude Code can orchestrate `ggen project generate` | [ ] Pending | |
| Cursor AI can use `ggen template new` within 8K tokens | [ ] Pending | |
| Error output is parseable by all avatars | [ ] Pending | |

### US-3: Maturity Classification (P2)

| Scenario | Status | Evidence |
|----------|--------|----------|
| `ggen template generate` assigned maturity level | [ ] Pending | |
| Maturity distribution report generated | [ ] Pending | |
| L2 commands have clear L3 upgrade path | [ ] Pending | |

### US-4: Fortune 500 Scenario Validation (P2)

| Scenario | Status | Evidence |
|----------|--------|----------|
| JPMorgan compliance workflow executes | [ ] Pending | |
| Toyota `ggen project watch` meets SLO | [ ] Pending | |
| Gaps documented for incomplete scenarios | [ ] Pending | |

### US-5: JTBD Documentation (P3)

| Scenario | Status | Evidence |
|----------|--------|----------|
| Each command has JTBD documentation | [ ] Pending | |
| JTBD search discovers relevant commands | [ ] Pending | |

---

## Deliverables Checklist

### Specification Artifacts
- [x] spec.md - Feature specification
- [x] checklists/requirements.md - This checklist
- [ ] data/command-inventory.yaml - All 47+ commands with metadata
- [ ] data/avatar-personas.yaml - 7 coding agent definitions
- [ ] data/case-studies.yaml - 7 Fortune 500 scenarios
- [ ] data/maturity-matrix.yaml - Current state assessment

### Audit Evidence
- [ ] evidence/workflow/* - Workflow command audits
- [ ] evidence/template/* - Template command audits
- [ ] evidence/project/* - Project command audits
- [ ] evidence/ontology/* - Ontology command audits
- [ ] evidence/graph/* - Graph command audits
- [ ] evidence/marketplace/* - Marketplace command audits
- [ ] evidence/fmea/* - FMEA command audits
- [ ] evidence/ai/* - AI command audits

### Reports
- [ ] reports/maturity-matrix-report.md
- [ ] reports/avatar-compatibility-matrix.md
- [ ] reports/fortune500-gap-analysis.md
- [ ] reports/recommendations-by-priority.md

---

## Quality Gates

### Before Plan Phase
- [x] Spec.md complete with all sections filled
- [x] Requirements checklist created
- [ ] User stories prioritized and acceptance criteria defined
- [ ] Success criteria are measurable

### Before Implementation Phase
- [ ] Plan.md approved
- [ ] Tasks.md generated from plan
- [ ] Evidence directory structure created
- [ ] Evaluation templates validated

### Before Completion Phase
- [ ] All 47+ commands audited
- [ ] All 7 avatars evaluated per command
- [ ] All 7 case studies mapped
- [ ] Maturity matrix populated
- [ ] Gaps documented with recommendations
- [ ] Reports generated

---

## Notes

- Command inventory derived from `ggen --help` and subcommand exploration
- Avatar personas based on current AI coding assistant landscape (Dec 2024)
- Fortune 500 case studies are hypothetical but realistic scenarios
- Maturity levels aligned with CMMI-style assessment
