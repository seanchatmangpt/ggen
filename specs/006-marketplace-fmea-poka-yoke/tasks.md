# Tasks: FMEA & Poka-Yoke Marketplace Framework

**Input**: Design documents from `/specs/006-marketplace-fmea-poka-yoke/`
**Prerequisites**: plan.md (complete), spec.md (complete), research.md (complete), data-model.md (complete), contracts/ (complete)

**Tests**: Chicago TDD required per constitution. Tests verify observable state.

**Organization**: Tasks grouped by user story for independent implementation and testing.

## Architecture Summary (DfLSS Aligned)

| Config File | Section | Contains | When Applied |
|-------------|---------|----------|--------------|
| **ggen.toml** | `[marketplace]` | FMEA validation settings | `ggen marketplace install` |
| **ggen.toml** | `[generation]` | Path protection, Poka-Yoke | `ggen generate` |
| **ggen.toml** | `[codeowners]` | Team ownership aggregation | `ggen generate` |
| **package.toml** | `[fmea]` | Package failure mode documentation | Validated during install |

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story this task belongs to (US1-US5)
- Include exact file paths in descriptions

## Path Conventions (ggen workspace)

- **Config parsing**: `crates/ggen-config/src/` (schema.rs - DONE)
- **Core types**: `crates/ggen-core/src/types/`
- **Domain logic**: `crates/ggen-domain/src/`
- **CLI commands**: `crates/ggen-cli/src/`
- **Integration tests**: `tests/integration/`
- **Test fixtures**: `tests/fixtures/`

---

## Phase 1: Setup (Shared Infrastructure)

**Purpose**: Project initialization and module structure

- [ ] T001 Create validation module directory at crates/ggen-cli/src/validation/
- [ ] T002 Create enterprise types module at crates/ggen-core/src/types/ (add enterprise.rs, fmea.rs)
- [ ] T003 [P] Add globset dependency to crates/ggen-domain/Cargo.toml for path pattern matching
- [ ] T004 [P] Create test fixtures directory at tests/fixtures/ with valid_fmea_package/, missing_fmea_package/, protected_domain/

---

## Phase 2: Foundational (Blocking Prerequisites)

**Purpose**: Core types and parsing that ALL user stories depend on

**STATUS**: Partially complete - config parsing DONE in ggen-config/src/schema.rs

### Already Completed (ggen-config)
- [x] T005-DONE MarketplaceConfig in schema.rs (fmea_validation, require_fmea, critical_threshold)
- [x] T006-DONE GenerationSafetyConfig in schema.rs (protected_paths, regenerate_paths)
- [x] T007-DONE PokaYokeSettings in schema.rs (warning_headers, gitignore, gitattributes)
- [x] T008-DONE CodeownersConfig in schema.rs (source_dirs, base_dirs, output_path)
- [x] T009-DONE FmeaControl struct in schema.rs (for package.toml parsing)
- [x] T010-DONE FmeaControl::rpn() and is_mitigated() methods

### Remaining Tasks (ggen-core newtypes)
- [ ] T011 [P] Implement ProtectedPath newtype with glob validation in crates/ggen-core/src/types/enterprise.rs
- [ ] T012 [P] Implement RegeneratePath newtype with glob validation in crates/ggen-core/src/types/enterprise.rs
- [ ] T013 [P] Implement RpnLevel enum (Critical, High, Medium) for display in crates/ggen-core/src/types/fmea.rs
- [ ] T014 Add re-exports to crates/ggen-core/src/types/mod.rs
- [ ] T015 Run cargo make check to verify compilation

**Checkpoint**: Foundation ready - config parsing done, core newtypes added

---

## Phase 3: User Story 1 - Protected Domain Logic (Priority: P1)

**Goal**: Domain files NEVER overwritten during regeneration. protected_paths vs regenerate_paths configuration.

**Independent Test**: Run `ggen generate --force` on a project with existing domain implementations and verify zero domain files are modified.

### Tests for User Story 1

- [ ] T021 [P] [US1] Create test fixture at tests/fixtures/protected_domain/ with package.toml and src/domain/user.rs
- [ ] T022 [P] [US1] Write test_protected_path_not_modified in tests/integration/path_protection_test.rs
- [ ] T023 [P] [US1] Write test_regenerate_path_updated in tests/integration/path_protection_test.rs
- [ ] T024 [P] [US1] Write test_protected_path_error_on_write_attempt in tests/integration/path_protection_test.rs
- [ ] T025 [P] [US1] Write test_first_generation_creates_stubs in tests/integration/path_protection_test.rs
- [ ] T026 [P] [US1] Write test_path_overlap_validation_error in tests/integration/path_protection_test.rs

### Implementation for User Story 1

- [ ] T027 [P] [US1] Implement PathMatcher struct using globset in crates/ggen-domain/src/generation/protection.rs
- [ ] T028 [US1] Implement is_protected() method in crates/ggen-domain/src/generation/protection.rs
- [ ] T029 [US1] Implement is_regeneratable() method in crates/ggen-domain/src/generation/protection.rs
- [ ] T030 [US1] Implement validate_no_overlap() in crates/ggen-domain/src/generation/protection.rs
- [ ] T031 [US1] Add check_protected_paths() call before file write in crates/ggen-domain/src/template.rs
- [ ] T032 [US1] Implement first_generation_detection() in crates/ggen-domain/src/generation/protection.rs
- [ ] T033 [US1] Implement generate_domain_stubs() for first generation in crates/ggen-domain/src/generation/protection.rs
- [ ] T034 [US1] Add ProtectedPathError to error types in crates/ggen-core/src/error.rs
- [ ] T035 [US1] Run cargo make test to verify all US1 tests pass

**Checkpoint**: User Story 1 complete - domain protection fully functional

---

## Phase 4: User Story 2 - Poka-Yoke Warning System (Priority: P1)

**Goal**: Clear visual warnings and IDE integration that prevent accidental edits to generated files.

**Independent Test**: Generate files and verify warning header present, .gitignore and .gitattributes configured.

### Tests for User Story 2

- [ ] T036 [P] [US2] Write test_warning_header_injected in tests/integration/poka_yoke_test.rs
- [ ] T037 [P] [US2] Write test_gitignore_generated in tests/integration/poka_yoke_test.rs
- [ ] T038 [P] [US2] Write test_gitattributes_generated in tests/integration/poka_yoke_test.rs
- [ ] T039 [P] [US2] Write test_regeneration_command_in_header in tests/integration/poka_yoke_test.rs
- [ ] T040 [P] [US2] Write test_header_format_per_language in tests/integration/poka_yoke_test.rs

### Implementation for User Story 2

- [ ] T041 [P] [US2] Implement HeaderInjector struct in crates/ggen-domain/src/generation/headers.rs
- [ ] T042 [US2] Implement get_comment_syntax() for language detection in crates/ggen-domain/src/generation/headers.rs
- [ ] T043 [US2] Implement format_header() with template variables in crates/ggen-domain/src/generation/headers.rs
- [ ] T044 [US2] Implement inject_header() that prepends header to content in crates/ggen-domain/src/generation/headers.rs
- [ ] T045 [P] [US2] Implement GitignoreGenerator in crates/ggen-domain/src/generation/headers.rs
- [ ] T046 [US2] Implement update_gitignore() with section markers in crates/ggen-domain/src/generation/headers.rs
- [ ] T047 [P] [US2] Implement GitattributesGenerator in crates/ggen-domain/src/generation/headers.rs
- [ ] T048 [US2] Implement update_gitattributes() with linguist-generated in crates/ggen-domain/src/generation/headers.rs
- [ ] T049 [US2] Integrate header injection into template render pipeline in crates/ggen-domain/src/template.rs
- [ ] T050 [US2] Run cargo make test to verify all US2 tests pass

**Checkpoint**: User Story 2 complete - Poka-Yoke controls fully functional

---

## Phase 5: User Story 3 - FMEA Validation Gate (Priority: P2)

**Goal**: FMEA validation integrated into `ggen marketplace install` that blocks packages without proper failure mode documentation.

**Key Architecture Decision**: FMEA validation is NOT a separate command. It's a quality gate during install.
- `[marketplace].fmea_validation = true` → Enables validation
- `[marketplace].require_fmea = true` → Requires [fmea] section in package.toml
- `[marketplace].critical_threshold = 200` → Rejects unmitigated modes above threshold

**Independent Test**: Run `ggen marketplace install` on packages with and without FMEA sections.

### Tests for User Story 3

- [ ] T051 [P] [US3] Create test fixture at tests/fixtures/valid_fmea_package/ with complete FMEA
- [ ] T052 [P] [US3] Create test fixture at tests/fixtures/missing_fmea_package/ without FMEA section
- [ ] T053 [P] [US3] Write test_install_missing_fmea_fails in tests/integration/fmea_validation_test.rs
- [ ] T054 [P] [US3] Write test_install_unmitigated_critical_fails in tests/integration/fmea_validation_test.rs
- [ ] T055 [P] [US3] Write test_install_valid_fmea_passes in tests/integration/fmea_validation_test.rs
- [ ] T056 [P] [US3] Write test_fmea_validation_respects_config in tests/integration/fmea_validation_test.rs
- [ ] T057 [P] [US3] Write test_rpn_calculation in tests/integration/fmea_validation_test.rs

### Implementation for User Story 3

- [ ] T058 [P] [US3] Implement FmeaValidator struct in crates/ggen-domain/src/marketplace/fmea_validator.rs
- [ ] T059 [US3] Implement validate_package_fmea() in crates/ggen-domain/src/marketplace/fmea_validator.rs
- [ ] T060 [US3] Implement check_rpn_thresholds() using FmeaControl::rpn() from schema.rs
- [ ] T061 [US3] Implement check_required_fmea() for require_fmea config
- [ ] T062 [US3] Implement check_unmitigated_critical() for critical_threshold config
- [ ] T063 [P] [US3] Implement FmeaValidationReport struct for install output
- [ ] T064 [US3] Integrate validate_package_fmea() into marketplace install flow
- [ ] T065 [US3] Add FMEA validation output to install success/failure messages
- [ ] T066 [US3] Run cargo make test to verify all US3 tests pass

**Checkpoint**: User Story 3 complete - FMEA validation integrated into marketplace install

---

## Phase 6: User Story 4 - Team Ownership Enforcement (Priority: P2)

**Goal**: CODEOWNERS-style ownership per noun directory with automatic CODEOWNERS aggregation.

**Independent Test**: Generate CODEOWNERS from noun OWNERS files and verify correct path mappings.

### Tests for User Story 4

- [ ] T069 [P] [US4] Create test fixture with ontology/user/OWNERS at tests/fixtures/owners_test/
- [ ] T070 [P] [US4] Write test_owners_file_parsed in tests/integration/owners_test.rs
- [ ] T071 [P] [US4] Write test_codeowners_aggregated in tests/integration/owners_test.rs
- [ ] T072 [P] [US4] Write test_breaking_pattern_requires_platform_team in tests/integration/owners_test.rs
- [ ] T073 [P] [US4] Write test_fallback_to_repo_codeowners in tests/integration/owners_test.rs

### Implementation for User Story 4

- [ ] T074 [P] [US4] Implement OwnersParser struct in crates/ggen-domain/src/owners.rs
- [ ] T075 [US4] Implement parse_owners_file() in crates/ggen-domain/src/owners.rs
- [ ] T076 [US4] Implement CodeownersGenerator struct in crates/ggen-domain/src/owners.rs
- [ ] T077 [US4] Implement aggregate_codeowners() in crates/ggen-domain/src/owners.rs
- [ ] T078 [US4] Implement generate_codeowners() output in crates/ggen-domain/src/owners.rs
- [ ] T079 [US4] Add --codeowners flag to ggen generate in crates/ggen-cli/src/cmds/template.rs
- [ ] T080 [US4] Run cargo make test to verify all US4 tests pass

**Checkpoint**: User Story 4 complete - Team ownership enforcement functional

---

## Phase 7: User Story 5 - Zero-Conflict Parallel Development (Priority: P3)

**Goal**: One-file-per-verb pattern that eliminates merge conflicts when 50+ developers add commands simultaneously.

**Independent Test**: Simulate two developers adding verbs to same noun and verify no merge conflicts.

### Tests for User Story 5

- [ ] T081 [P] [US5] Write test_one_file_per_verb_pattern in tests/integration/parallel_dev_test.rs
- [ ] T082 [P] [US5] Write test_new_verb_no_existing_file_modification in tests/integration/parallel_dev_test.rs
- [ ] T083 [P] [US5] Write test_domain_stub_created_for_new_verb in tests/integration/parallel_dev_test.rs

### Implementation for User Story 5

- [ ] T084 [US5] Implement VerbFileMapper in crates/ggen-domain/src/generation/verbs.rs
- [ ] T085 [US5] Implement detect_new_verbs() in crates/ggen-domain/src/generation/verbs.rs
- [ ] T086 [US5] Implement generate_verb_file() for individual verb output in crates/ggen-domain/src/generation/verbs.rs
- [ ] T087 [US5] Implement create_domain_stub_for_verb() in crates/ggen-domain/src/generation/verbs.rs
- [ ] T088 [US5] Integrate verb-per-file generation into template pipeline in crates/ggen-domain/src/template.rs
- [ ] T089 [US5] Run cargo make test to verify all US5 tests pass

**Checkpoint**: User Story 5 complete - Zero-conflict parallel development enabled

---

## Phase 8: Polish & Cross-Cutting Concerns

**Purpose**: Documentation, cleanup, and validation across all stories

- [ ] T090 [P] Update clap-noun-verb USAGE.md with enterprise sections in marketplace/packages/clap-noun-verb/USAGE.md
- [ ] T091 [P] Update clap-noun-verb README.md with FMEA/Poka-Yoke overview in marketplace/packages/clap-noun-verb/README.md
- [ ] T092 [P] Add FMEA validation to marketplace publish workflow in crates/ggen-cli/src/cmds/marketplace.rs
- [ ] T093 Verify all modules have proper re-exports in crates/ggen-cli/src/validation/mod.rs
- [ ] T094 Run cargo make lint to ensure no warnings
- [ ] T095 Run cargo make test to ensure all 1168+ tests pass
- [ ] T096 Validate enterprise example package at marketplace/packages/clap-noun-verb/examples/enterprise-ops/
- [ ] T097 Run quickstart.md validation against generated package
- [ ] T098 Update specs/006-marketplace-fmea-poka-yoke/checklists/requirements.md with completion status

---

## Dependencies & Execution Order

### Phase Dependencies

- **Setup (Phase 1)**: No dependencies - start immediately
- **Foundational (Phase 2)**: Depends on Setup - BLOCKS all user stories
- **US1 (Phase 3)**: Depends on Foundational
- **US2 (Phase 4)**: Depends on Foundational (can parallel with US1)
- **US3 (Phase 5)**: Depends on Foundational (can parallel with US1/US2)
- **US4 (Phase 6)**: Depends on Foundational (can parallel with US1/US2/US3)
- **US5 (Phase 7)**: Depends on US1 (needs path protection)
- **Polish (Phase 8)**: Depends on all user stories complete

### User Story Dependencies

| Story | Depends On | Can Parallel With |
|-------|------------|-------------------|
| US1 (Protected Domain) | Foundational | - |
| US2 (Poka-Yoke) | Foundational | US1, US3, US4 |
| US3 (FMEA Validation) | Foundational | US1, US2, US4 |
| US4 (Team Ownership) | Foundational | US1, US2, US3 |
| US5 (Zero-Conflict) | US1 | US3, US4 |

### Within Each User Story

1. Tests MUST be written first and FAIL before implementation
2. Foundational types/structs before methods
3. Core implementation before integration
4. Integration tests after implementation

---

## Parallel Opportunities

### Setup Phase (All Parallel)

```bash
# Can run all together:
T003 [P] Add globset dependency
T004 [P] Create test fixtures directory
```

### Foundational Phase (Multiple Parallel Groups)

```bash
# Group 1: Newtypes (parallel)
T005 Severity → T006 [P] Occurrence → T007 [P] Detection

# Group 2: Config structs (parallel after newtypes)
T011 [P] ProtectedPath
T012 [P] RegeneratePath
T014 [P] EnterpriseConfig
T015 [P] PokaYokeConfig
T016 [P] FmeaConfig
T017 [P] DomainProtectionStrategy
```

### User Story 1 Tests (All Parallel)

```bash
T021 [P] [US1] Create test fixture
T022 [P] [US1] test_protected_path_not_modified
T023 [P] [US1] test_regenerate_path_updated
T024 [P] [US1] test_protected_path_error_on_write_attempt
T025 [P] [US1] test_first_generation_creates_stubs
T026 [P] [US1] test_path_overlap_validation_error
```

### Cross-Story Parallelism

Once Foundational (Phase 2) completes:
- Developer A: US1 (Protected Domain Logic)
- Developer B: US2 (Poka-Yoke Warning System)
- Developer C: US3 (FMEA Validation Gate)
- Developer D: US4 (Team Ownership)

---

## Implementation Strategy

### MVP First (User Story 1 Only)

1. Complete Phase 1: Setup
2. Complete Phase 2: Foundational (CRITICAL)
3. Complete Phase 3: User Story 1 (Protected Domain Logic)
4. **STOP and VALIDATE**: Run all US1 tests
5. Deploy/demo domain protection alone

### Incremental Delivery

1. Setup + Foundational → Foundation ready
2. Add US1 → Test → Deploy (Domain Protection MVP!)
3. Add US2 → Test → Deploy (+ Poka-Yoke)
4. Add US3 → Test → Deploy (+ FMEA Validation)
5. Add US4 → Test → Deploy (+ Team Ownership)
6. Add US5 → Test → Deploy (+ Zero-Conflict)

### Suggested MVP Scope

**US1 + US2** (both P1 priority):
- Protected domain logic (core value proposition)
- Poka-Yoke warnings (developer experience)
- Delivers: "Safe regeneration with visual warnings"

---

## Task Summary

| Phase | Tasks | Parallel | Description |
|-------|-------|----------|-------------|
| Phase 1: Setup | 4 | 2 | Project structure |
| Phase 2: Foundational | 16 | 11 | Core types and parsing |
| Phase 3: US1 | 15 | 6 | Protected Domain Logic |
| Phase 4: US2 | 15 | 7 | Poka-Yoke Warning System |
| Phase 5: US3 | 18 | 8 | FMEA Validation Gate |
| Phase 6: US4 | 12 | 5 | Team Ownership Enforcement |
| Phase 7: US5 | 9 | 3 | Zero-Conflict Development |
| Phase 8: Polish | 9 | 3 | Documentation & cleanup |
| **Total** | **98** | **45** | 46% parallelizable |

---

## Notes

- All tasks follow Chicago TDD: tests verify observable state
- [P] = can run in parallel with other [P] tasks in same phase
- [USn] = maps to user story from spec.md
- Run `cargo make check` after each implementation task
- Run `cargo make test` after completing each user story
- Commit after each task or logical group
