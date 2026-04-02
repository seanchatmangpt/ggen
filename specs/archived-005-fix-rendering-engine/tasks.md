# Tasks: Fix Template Rendering Engine

**Input**: Design documents from `/specs/005-fix-rendering-engine/`
**Prerequisites**: plan.md (required), spec.md (required), research.md, data-model.md, quickstart.md

**Tests**: Integration tests included per Chicago TDD (Constitution III).

**Organization**: Tasks grouped by user story for independent implementation and testing.

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story this task belongs to (e.g., US1, US2, US3)
- Include exact file paths in descriptions

## Path Conventions

- **Workspace**: `crates/ggen-cli/` for CLI changes
- **Tests**: `crates/ggen-cli/tests/` for integration tests
- **Reference**: `crates/ggen-domain/src/template/` for domain logic

---

## Phase 1: Setup

**Purpose**: Verify environment and understand existing code

- [ ] T001 Run `cargo make check` to verify workspace compiles cleanly
- [ ] T002 [P] Read `crates/ggen-cli/src/cmds/template.rs` lines 228-272 (generate function)
- [ ] T003 [P] Read `crates/ggen-domain/src/template/render_with_rdf.rs` (RenderWithRdfOptions struct)

---

## Phase 2: Foundational (Blocking Prerequisites)

**Purpose**: Create test infrastructure and helper function

**⚠️ CRITICAL**: No user story implementation until tests exist (Chicago TDD)

- [ ] T004 Create test file `crates/ggen-cli/tests/template_rdf_test.rs` with module structure
- [ ] T005 Add helper function `has_rdf_frontmatter()` in `crates/ggen-cli/src/cmds/template.rs` to detect `rdf`, `rdf_inline`, or `sparql` fields
- [ ] T006 Run `cargo make check` to verify helper compiles

**Checkpoint**: Foundation ready - test infrastructure and detection helper in place

---

## Phase 3: User Story 1 - SPARQL Frontmatter Generates Code (Priority: P1) 🎯 MVP

**Goal**: Templates with SPARQL frontmatter execute queries and populate `sparql_results.*`

**Independent Test**: Create minimal template with one SPARQL query, verify `sparql_queries_executed > 0`

### Tests for User Story 1 (Chicago TDD - RED first)

- [ ] T007 [P] [US1] Write test `test_template_generate_with_sparql_executes_queries` in `crates/ggen-cli/tests/template_rdf_test.rs`
- [ ] T008 [P] [US1] Write test `test_template_generate_without_rdf_uses_fast_path` in `crates/ggen-cli/tests/template_rdf_test.rs`
- [ ] T009 [US1] Run `cargo make test-unit` to verify tests FAIL (RED)

### Implementation for User Story 1

- [ ] T010 [US1] Modify `generate()` function in `crates/ggen-cli/src/cmds/template.rs` to call `has_rdf_frontmatter()` before routing
- [ ] T011 [US1] When RDF detected, construct `RenderWithRdfOptions` and call `template::render_with_rdf()` in `crates/ggen-cli/src/cmds/template.rs`
- [ ] T012 [US1] When no RDF, continue using existing `GenerateFileOptions` path for backward compatibility
- [ ] T013 [US1] Run `cargo make test-unit` to verify tests PASS (GREEN)
- [ ] T014 [US1] Run `cargo make lint` to verify clippy clean

**Checkpoint**: `ggen template generate` with SPARQL frontmatter reports `sparql_queries_executed > 0`

---

## Phase 4: User Story 2 - Multi-File Output with FILE Markers (Priority: P2)

**Goal**: Templates with `{# FILE: path #}` markers generate multiple output files

**Independent Test**: Create template with two FILE markers, verify both files created

### Tests for User Story 2

- [ ] T015 [P] [US2] Write test `test_template_generate_multifile_output` in `crates/ggen-cli/tests/template_rdf_test.rs`
- [ ] T016 [US2] Run `cargo make test-unit` to verify test FAILS (RED)

### Implementation for User Story 2

- [ ] T017 [US2] Verify `render_with_rdf()` already handles FILE markers (read `crates/ggen-domain/src/template/render_with_rdf.rs` lines 300-350)
- [ ] T018 [US2] If needed, ensure output directory creation in `crates/ggen-cli/src/cmds/template.rs`
- [ ] T019 [US2] Run `cargo make test-unit` to verify test PASSES (GREEN)

**Checkpoint**: Multi-file templates produce correct number of output files

---

## Phase 5: User Story 3 - Project Generate Convention-Based Discovery (Priority: P3)

**Goal**: `ggen project generate` discovers and processes all templates in `templates/` directory

**Independent Test**: Create project with `templates/` containing one template+data pair, verify generation

### Tests for User Story 3

- [ ] T020 [P] [US3] Write test `test_project_generate_discovers_templates` in `crates/ggen-cli/tests/template_rdf_test.rs`
- [ ] T021 [US3] Run `cargo make test-unit` to verify test FAILS (RED)

### Implementation for User Story 3

- [ ] T022 [US3] Read `crates/ggen-cli/src/cmds/project.rs` lines 626-739 to understand existing behavior
- [ ] T023 [US3] Verify `project generate` already uses `render_with_rdf()` (should work out of box)
- [ ] T024 [US3] Run `cargo make test-unit` to verify test PASSES (GREEN)

**Checkpoint**: All user stories should now be independently functional

---

## Phase 6: Polish & Cross-Cutting Concerns

**Purpose**: End-to-end validation and documentation

- [ ] T025 [P] Test with clap-noun-verb calculator.ttl at `marketplace/packages/clap-noun-verb/examples/calculator.ttl`
- [ ] T026 [P] Verify generated Rust code compiles with `cargo check` in output directory
- [ ] T027 Run `cargo make test` for full test suite (1,168+ tests)
- [ ] T028 Run `cargo make lint` for final clippy validation
- [ ] T029 Update `specs/005-fix-rendering-engine/quickstart.md` with working examples if needed
- [ ] T030 Run quickstart.md validation manually

---

## Dependencies & Execution Order

### Phase Dependencies

- **Setup (Phase 1)**: No dependencies - can start immediately
- **Foundational (Phase 2)**: Depends on Setup - BLOCKS all user stories
- **User Stories (Phase 3-5)**: All depend on Foundational phase completion
  - US1 must complete first (core fix)
  - US2 depends on US1 (uses same render_with_rdf path)
  - US3 verifies existing behavior
- **Polish (Phase 6)**: Depends on all user stories complete

### User Story Dependencies

- **User Story 1 (P1)**: Core fix - no dependencies on other stories
- **User Story 2 (P2)**: Uses same `render_with_rdf()` path as US1 - depends on US1 implementation
- **User Story 3 (P3)**: Verifies `project generate` still works - can run after US1

### Within Each User Story

- Tests MUST be written and FAIL before implementation (RED)
- Implementation makes tests PASS (GREEN)
- Lint check after each story

### Parallel Opportunities

- T002 and T003 can run in parallel (reading different files)
- T007 and T008 can run in parallel (different tests)
- T015 can run in parallel with T007/T008 (independent test)
- T020 can run in parallel with T015 (independent test)
- T025 and T026 can run in parallel (different validation)

---

## Parallel Example: User Story 1 Tests

```bash
# Launch all US1 tests together:
Task: "Write test_template_generate_with_sparql_executes_queries" (crates/ggen-cli/tests/template_rdf_test.rs)
Task: "Write test_template_generate_without_rdf_uses_fast_path" (crates/ggen-cli/tests/template_rdf_test.rs)
```

---

## Implementation Strategy

### MVP First (User Story 1 Only)

1. Complete Phase 1: Setup (T001-T003)
2. Complete Phase 2: Foundational (T004-T006)
3. Complete Phase 3: User Story 1 (T007-T014)
4. **STOP and VALIDATE**: Test with real template containing SPARQL
5. Verify `sparql_queries_executed > 0` in output

### Incremental Delivery

1. Complete Setup + Foundational → Foundation ready
2. Add User Story 1 → Test with SPARQL template → Core fix works (MVP!)
3. Add User Story 2 → Test multi-file output → FILE markers work
4. Add User Story 3 → Test project generate → Convention discovery works
5. Each story adds confidence without breaking previous stories

---

## Notes

- [P] tasks = different files, no dependencies
- [Story] label maps task to specific user story for traceability
- Each user story is independently completable and testable
- Constitution III mandates Chicago TDD: tests FAIL before implementation
- Constitution IV mandates `cargo make` commands (not direct cargo)
- Stop at any checkpoint to validate story independently
- All changes in `crates/` directory (not root)
