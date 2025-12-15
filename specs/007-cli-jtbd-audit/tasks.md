# Tasks: CLI Jobs-to-be-Done Audit

**Input**: Design documents from `/specs/007-cli-jtbd-audit/`
**Prerequisites**: plan.md ✅, spec.md ✅, research.md ✅, data-model.md ✅, contracts/ ✅, quickstart.md ✅

**Timeline**: 1 week (~10 commands/day)
**Total Commands**: 47+
**Evidence Path**: `specs/007-cli-jtbd-audit/evidence/`

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files/commands, no dependencies)
- **[Story]**: Which user story this task belongs to (US1-US5)
- All evidence files stored in `specs/007-cli-jtbd-audit/evidence/<category>/`

---

## Phase 1: Setup

**Purpose**: Prepare audit infrastructure and directory structure

- [X] T001 Create evidence subdirectories in `specs/007-cli-jtbd-audit/evidence/{workflow,template,project,ontology,graph,marketplace,fmea,ai,case-studies}`
- [X] T002 Create reports directory in `specs/007-cli-jtbd-audit/reports/`
- [X] T003 [P] Verify ggen CLI v4.0.0 is installed and accessible via `ggen --version`
- [X] T004 [P] Verify cargo-make is available via `cargo make --version`
- [X] T005 [P] Create audit execution script template in `specs/007-cli-jtbd-audit/scripts/audit-command.sh`

---

## Phase 2: Foundational (Blocking Prerequisites)

**Purpose**: Core audit infrastructure that MUST be complete before command audits can begin

**⚠️ CRITICAL**: No user story work can begin until this phase is complete

- [X] T006 Validate YAML schema in `specs/007-cli-jtbd-audit/contracts/audit-result.schema.yaml` against sample data
- [X] T007 Validate case study schema in `specs/007-cli-jtbd-audit/contracts/case-study-validation.schema.yaml` against sample data
- [X] T008 Create blank audit YAML template file in `specs/007-cli-jtbd-audit/evidence/_template.yaml`
- [X] T009 Test audit workflow with single command (`ggen --help`) as dry run
- [X] T010 Document scoring rubric reference card in `specs/007-cli-jtbd-audit/evidence/scoring-guide.md`

**Checkpoint**: Foundation ready - command audits can now begin in parallel by category

---

## Phase 3: User Story 1 - Command Functionality Audit (Priority: P1) 🎯 MVP

**Goal**: Verify every ggen CLI command executes correctly with expected inputs and produces predictable outputs

**Independent Test**: Run each command with valid inputs, verify exit codes, output format, and error handling. All 47+ commands have functional_correctness fields completed.

### Day 1: Workflow + Utils Commands (5-6 commands)

- [ ] T011 [P] [US1] Audit `ggen workflow analyze` → `evidence/workflow/workflow-analyze.yaml`
- [ ] T012 [P] [US1] Audit `ggen workflow init` → `evidence/workflow/workflow-init.yaml`
- [ ] T013 [P] [US1] Audit `ggen workflow report` → `evidence/workflow/workflow-report.yaml`
- [ ] T014 [P] [US1] Audit `ggen workflow event` → `evidence/workflow/workflow-event.yaml`
- [ ] T015 [P] [US1] Audit `ggen workflow discover` → `evidence/workflow/workflow-discover.yaml`
- [ ] T016 [P] [US1] Audit `ggen utils` (all subcommands) → `evidence/utils/utils.yaml`

### Day 2: Template Commands (8 commands)

- [ ] T017 [P] [US1] Audit `ggen template new` → `evidence/template/template-new.yaml`
- [ ] T018 [P] [US1] Audit `ggen template list` → `evidence/template/template-list.yaml`
- [ ] T019 [P] [US1] Audit `ggen template lint` → `evidence/template/template-lint.yaml`
- [ ] T020 [P] [US1] Audit `ggen template generate` → `evidence/template/template-generate.yaml`
- [ ] T021 [P] [US1] Audit `ggen template get` → `evidence/template/template-get.yaml`
- [ ] T022 [P] [US1] Audit `ggen template show` → `evidence/template/template-show.yaml`
- [ ] T023 [P] [US1] Audit `ggen template generate-tree` → `evidence/template/template-generate-tree.yaml`
- [ ] T024 [P] [US1] Audit `ggen template regenerate` → `evidence/template/template-regenerate.yaml`

### Day 3: Project + Graph Commands (11 commands)

- [ ] T025 [P] [US1] Audit `ggen project new` → `evidence/project/project-new.yaml`
- [ ] T026 [P] [US1] Audit `ggen project plan` → `evidence/project/project-plan.yaml`
- [ ] T027 [P] [US1] Audit `ggen project apply` → `evidence/project/project-apply.yaml`
- [ ] T028 [P] [US1] Audit `ggen project generate` → `evidence/project/project-generate.yaml`
- [ ] T029 [P] [US1] Audit `ggen project init` → `evidence/project/project-init.yaml`
- [ ] T030 [P] [US1] Audit `ggen project watch` → `evidence/project/project-watch.yaml`
- [ ] T031 [P] [US1] Audit `ggen project gen` → `evidence/project/project-gen.yaml`
- [ ] T032 [P] [US1] Audit `ggen graph query` → `evidence/graph/graph-query.yaml`
- [ ] T033 [P] [US1] Audit `ggen graph load` → `evidence/graph/graph-load.yaml`
- [ ] T034 [P] [US1] Audit `ggen graph visualize` → `evidence/graph/graph-visualize.yaml`
- [ ] T035 [P] [US1] Audit `ggen graph export` → `evidence/graph/graph-export.yaml`

### Day 4: Ontology + AI Commands (7 commands)

- [ ] T036 [P] [US1] Audit `ggen ontology generate` → `evidence/ontology/ontology-generate.yaml`
- [ ] T037 [P] [US1] Audit `ggen ontology extract` → `evidence/ontology/ontology-extract.yaml`
- [ ] T038 [P] [US1] Audit `ggen ontology validate` → `evidence/ontology/ontology-validate.yaml`
- [ ] T039 [P] [US1] Audit `ggen ontology init` → `evidence/ontology/ontology-init.yaml`
- [ ] T040 [P] [US1] Audit `ggen ai generate` → `evidence/ai/ai-generate.yaml`
- [ ] T041 [P] [US1] Audit `ggen ai chat` → `evidence/ai/ai-chat.yaml`
- [ ] T042 [P] [US1] Audit `ggen ai analyze` → `evidence/ai/ai-analyze.yaml`

### Day 5: Marketplace + FMEA Commands (16 commands)

- [ ] T043 [P] [US1] Audit `ggen marketplace sparql` → `evidence/marketplace/marketplace-sparql.yaml`
- [ ] T044 [P] [US1] Audit `ggen marketplace install` → `evidence/marketplace/marketplace-install.yaml`
- [ ] T045 [P] [US1] Audit `ggen marketplace metrics` → `evidence/marketplace/marketplace-metrics.yaml`
- [ ] T046 [P] [US1] Audit `ggen marketplace validate` → `evidence/marketplace/marketplace-validate.yaml`
- [ ] T047 [P] [US1] Audit `ggen marketplace info` → `evidence/marketplace/marketplace-info.yaml`
- [ ] T048 [P] [US1] Audit `ggen marketplace rdf_stats` → `evidence/marketplace/marketplace-rdf_stats.yaml`
- [ ] T049 [P] [US1] Audit `ggen marketplace versions` → `evidence/marketplace/marketplace-versions.yaml`
- [ ] T050 [P] [US1] Audit `ggen marketplace publish` → `evidence/marketplace/marketplace-publish.yaml`
- [ ] T051 [P] [US1] Audit `ggen marketplace validate_fmea` → `evidence/marketplace/marketplace-validate_fmea.yaml`
- [ ] T052 [P] [US1] Audit `ggen marketplace search` → `evidence/marketplace/marketplace-search.yaml`
- [ ] T053 [P] [US1] Audit `ggen fmea show` → `evidence/fmea/fmea-show.yaml`
- [ ] T054 [P] [US1] Audit `ggen fmea pareto` → `evidence/fmea/fmea-pareto.yaml`
- [ ] T055 [P] [US1] Audit `ggen fmea report` → `evidence/fmea/fmea-report.yaml`
- [ ] T056 [P] [US1] Audit `ggen fmea export` → `evidence/fmea/fmea-export.yaml`
- [ ] T057 [P] [US1] Audit `ggen fmea list` → `evidence/fmea/fmea-list.yaml`
- [ ] T058 [P] [US1] Audit `ggen ci` → `evidence/ci/ci.yaml`

**Checkpoint**: All 47+ commands have functional_correctness fields populated. US1 complete.

---

## Phase 4: User Story 2 - Agent Accessibility Evaluation (Priority: P1)

**Goal**: Evaluate each command for AI agent usability using 7 avatar personas

**Independent Test**: Each command has agent_score and avatar_notes populated. Agent compatibility matrix can be generated.

### Avatar Evaluation Pass (All Commands)

- [ ] T059 [US2] Update all workflow evidence files with agent_breakdown scores in `evidence/workflow/*.yaml`
- [ ] T060 [US2] Update all template evidence files with agent_breakdown scores in `evidence/template/*.yaml`
- [ ] T061 [US2] Update all project evidence files with agent_breakdown scores in `evidence/project/*.yaml`
- [ ] T062 [US2] Update all graph evidence files with agent_breakdown scores in `evidence/graph/*.yaml`
- [ ] T063 [US2] Update all ontology evidence files with agent_breakdown scores in `evidence/ontology/*.yaml`
- [ ] T064 [US2] Update all ai evidence files with agent_breakdown scores in `evidence/ai/*.yaml`
- [ ] T065 [US2] Update all marketplace evidence files with agent_breakdown scores in `evidence/marketplace/*.yaml`
- [ ] T066 [US2] Update all fmea evidence files with agent_breakdown scores in `evidence/fmea/*.yaml`

### Avatar-Specific Notes Pass

- [ ] T067 [P] [US2] Add Claude Code avatar notes to all evidence files
- [ ] T068 [P] [US2] Add Cursor AI avatar notes to all evidence files
- [ ] T069 [P] [US2] Add GitHub Copilot avatar notes to all evidence files
- [ ] T070 [P] [US2] Add Aider avatar notes to all evidence files
- [ ] T071 [P] [US2] Add Devin avatar notes to all evidence files
- [ ] T072 [P] [US2] Add OpenHands avatar notes to all evidence files
- [ ] T073 [P] [US2] Add Windsurf avatar notes to all evidence files

**Checkpoint**: All commands have agent_score calculated and all 7 avatar notes populated. US2 complete.

---

## Phase 5: User Story 3 - Maturity Classification (Priority: P2)

**Goal**: Classify each command by maturity level (L0-L5) with evidence

**Independent Test**: Each command has maturity_level assigned with maturity_blockers documented. Maturity matrix report can be generated.

### Maturity Level Assignment

- [ ] T074 [US3] Assign maturity levels to all workflow commands based on scores in `evidence/workflow/*.yaml`
- [ ] T075 [US3] Assign maturity levels to all template commands based on scores in `evidence/template/*.yaml`
- [ ] T076 [US3] Assign maturity levels to all project commands based on scores in `evidence/project/*.yaml`
- [ ] T077 [US3] Assign maturity levels to all graph commands based on scores in `evidence/graph/*.yaml`
- [ ] T078 [US3] Assign maturity levels to all ontology commands based on scores in `evidence/ontology/*.yaml`
- [ ] T079 [US3] Assign maturity levels to all ai commands based on scores in `evidence/ai/*.yaml`
- [ ] T080 [US3] Assign maturity levels to all marketplace commands based on scores in `evidence/marketplace/*.yaml`
- [ ] T081 [US3] Assign maturity levels to all fmea commands based on scores in `evidence/fmea/*.yaml`

### Blocker Documentation

- [ ] T082 [US3] Document maturity_blockers for commands capped at L3 (non-determinism)
- [ ] T083 [US3] Document maturity_blockers for commands capped at L2 (missing JSON output)
- [ ] T084 [US3] Identify and mark any L0-DEP deprecated commands

**Checkpoint**: All commands have maturity_level and blockers documented. US3 complete.

---

## Phase 6: User Story 4 - Fortune 500 Scenario Validation (Priority: P2)

**Goal**: Validate commands support Fortune 500 scale use cases

**Independent Test**: All 7 case studies have validation_status and gaps documented. Gap analysis report can be generated.

### Case Study Validation

- [ ] T085 [P] [US4] Validate JPMorgan case study commands → `evidence/case-studies/jpmorgan.yaml`
- [ ] T086 [P] [US4] Validate Amazon case study commands → `evidence/case-studies/amazon.yaml`
- [ ] T087 [P] [US4] Validate Pfizer case study commands → `evidence/case-studies/pfizer.yaml`
- [ ] T088 [P] [US4] Validate Boeing case study commands → `evidence/case-studies/boeing.yaml`
- [ ] T089 [P] [US4] Validate Netflix case study commands → `evidence/case-studies/netflix.yaml`
- [ ] T090 [P] [US4] Validate Toyota case study commands → `evidence/case-studies/toyota.yaml`
- [ ] T091 [P] [US4] Validate Goldman Sachs case study commands → `evidence/case-studies/goldman.yaml`

### Gap Analysis

- [ ] T092 [US4] Compile all gaps from case study validations
- [ ] T093 [US4] Prioritize gaps by severity (P1/P2/P3) and impact across case studies

**Checkpoint**: All 7 case studies validated with gaps documented. US4 complete.

---

## Phase 7: User Story 5 - JTBD Documentation (Priority: P3)

**Goal**: Document Jobs-to-be-Done for each command reaching L4+ maturity

**Independent Test**: All L4+ commands have JTBD documentation. Users can understand when/why to use each command.

### JTBD Documentation

- [ ] T094 [US5] Write JTBD documentation for L4+ workflow commands in `evidence/workflow/*.yaml` recommendations
- [ ] T095 [US5] Write JTBD documentation for L4+ template commands in `evidence/template/*.yaml` recommendations
- [ ] T096 [US5] Write JTBD documentation for L4+ project commands in `evidence/project/*.yaml` recommendations
- [ ] T097 [US5] Write JTBD documentation for L4+ graph commands in `evidence/graph/*.yaml` recommendations
- [ ] T098 [US5] Write JTBD documentation for L4+ ontology commands in `evidence/ontology/*.yaml` recommendations
- [ ] T099 [US5] Write JTBD documentation for L4+ ai commands in `evidence/ai/*.yaml` recommendations
- [ ] T100 [US5] Write JTBD documentation for L4+ marketplace commands in `evidence/marketplace/*.yaml` recommendations
- [ ] T101 [US5] Write JTBD documentation for L4+ fmea commands in `evidence/fmea/*.yaml` recommendations

**Checkpoint**: All L4+ commands have JTBD documentation. US5 complete.

---

## Phase 8: Polish & Reports

**Purpose**: Generate final reports and cross-cutting improvements

### Report Generation

- [ ] T102 [P] Generate maturity matrix report → `reports/maturity-matrix.md`
- [ ] T103 [P] Generate avatar compatibility matrix → `reports/avatar-compatibility.md`
- [ ] T104 [P] Generate Fortune 500 gap analysis → `reports/fortune500-gaps.md`
- [ ] T105 Generate recommendations by priority → `reports/recommendations.md`

### Final Validation

- [ ] T106 Verify SC-001: 100% commands evaluated (47+)
- [ ] T107 Verify SC-002: 100% commands have avatar notes (7 per command)
- [ ] T108 Verify SC-003: All commands have maturity level
- [ ] T109 Verify SC-004: All 7 case studies mapped
- [ ] T110 Verify SC-006: Calculate average agent score (target ≥80%)
- [ ] T111 Verify SC-007: Zero L0 broken commands
- [ ] T112 Update `checklists/requirements.md` with completion status

**Checkpoint**: All reports generated. All success criteria verified. Audit complete.

---

## Dependencies & Execution Order

### Phase Dependencies

```
Phase 1: Setup ──────────────────────┐
                                     ▼
Phase 2: Foundational ───────────────┼──► BLOCKS ALL USER STORIES
                                     │
        ┌────────────────────────────┘
        ▼
Phase 3: US1 (P1) ──► Phase 4: US2 (P1) ──► Phase 5: US3 (P2)
        │                    │                     │
        │                    │                     ▼
        │                    │              Phase 6: US4 (P2)
        │                    │                     │
        │                    │                     ▼
        │                    │              Phase 7: US5 (P3)
        │                    │                     │
        └────────────────────┴─────────────────────┘
                                     │
                                     ▼
                          Phase 8: Polish & Reports
```

### User Story Dependencies

| Story | Depends On | Can Start After |
|-------|------------|-----------------|
| US1 (Functional) | Phase 2 | Setup + Foundational |
| US2 (Agent) | US1 | Functional audit complete |
| US3 (Maturity) | US2 | Agent scores calculated |
| US4 (Fortune 500) | US1 | Commands audited |
| US5 (JTBD Docs) | US3 | Maturity levels assigned |

### Parallel Opportunities

**Within Phase 1 (Setup)**:
- T003, T004, T005 can run in parallel

**Within Phase 3 (US1 - Functional Audit)**:
- All commands within same day can be audited in parallel
- Different command categories can be audited in parallel

**Within Phase 4 (US2 - Agent Evaluation)**:
- Avatar notes (T067-T073) can be added in parallel

**Within Phase 6 (US4 - Case Studies)**:
- All 7 case studies (T085-T091) can be validated in parallel

**Within Phase 8 (Reports)**:
- T102, T103, T104 can be generated in parallel

---

## Parallel Example: Day 2 Template Audit

```bash
# Launch all template command audits in parallel:
Task: "Audit ggen template new"
Task: "Audit ggen template list"
Task: "Audit ggen template lint"
Task: "Audit ggen template generate"
Task: "Audit ggen template get"
Task: "Audit ggen template show"
Task: "Audit ggen template generate-tree"
Task: "Audit ggen template regenerate"
```

---

## Implementation Strategy

### MVP First (US1 Only)

1. Complete Phase 1: Setup
2. Complete Phase 2: Foundational
3. Complete Phase 3: US1 (All 47+ commands have functional_correctness)
4. **STOP and VALIDATE**: Can generate basic command inventory report
5. Proceed to US2 if time permits

### Incremental Delivery

1. **Day 1-5**: Complete US1 (Functional Audit) - delivers command inventory
2. **Day 5-6**: Complete US2 (Agent Scores) - delivers agent compatibility matrix
3. **Day 6**: Complete US3 (Maturity) - delivers maturity matrix
4. **Day 6-7**: Complete US4 (Case Studies) - delivers gap analysis
5. **Day 7**: Complete US5 (JTBD Docs) + Reports - delivers full audit package

### Daily Targets

| Day | Target Commands | User Story Progress |
|-----|-----------------|---------------------|
| 1 | 5-6 (workflow, utils) | US1 10% |
| 2 | 8 (template) | US1 25% |
| 3 | 11 (project, graph) | US1 50% |
| 4 | 7 (ontology, ai) | US1 65% |
| 5 | 16 (marketplace, fmea) | US1 100%, US2 start |
| 6 | Agent scores + maturity | US2 100%, US3 100%, US4 start |
| 7 | Case studies + reports | US4 100%, US5 100%, Reports |

---

## Notes

- All evidence files use schema from `contracts/audit-result.schema.yaml`
- Case study files use schema from `contracts/case-study-validation.schema.yaml`
- Non-deterministic commands are capped at L3 per clarification decision
- L4+ (80%) required for "agent-usable" classification per clarification decision
- Deprecated commands marked as L0-DEP per clarification decision
