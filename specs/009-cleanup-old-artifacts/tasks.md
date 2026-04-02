# Tasks: Delete Old Code, Tests, and Documentation

**Input**: Design documents from `/specs/009-cleanup-old-artifacts/`
**Branch**: `009-cleanup-old-artifacts`
**Approach**: Simple deterministic archival (7+ days unmodified OR deprecated)

**Organization**: Tasks organized by user story for independent implementation and testing.

## Format: `- [ ] [ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story ([US1], [US2])
- Include exact file paths in descriptions

## Implementation Strategy

**MVP Scope**: User Story 1 (Identify & Archive)
- Minimal viable product: Identification script that lists files to archive
- Delivered incrementally: Identify → Archive → Verify Manifest

**Parallel Opportunities**:
- Identification and workspace cleanup scripts can be developed in parallel
- Manifest generation and reporting are sequential

---

## Phase 1: Setup (Shared Infrastructure) ✅

**Purpose**: Project initialization and scripts directory

- [x] T001 Create `.specify/scripts/bash/` directory structure for archive utilities
- [x] T002 Create `.specify/scripts/bash/identify-archived.sh` template with shebang and error handling
- [x] T003 Create `.specify/scripts/bash/archive-files.sh` template with shebang and error handling

---

## Phase 2: Foundational (Blocking Prerequisites) ✅

**Purpose**: Core scripts that both user stories depend on

**⚠️ CRITICAL**: Phase 2 must complete before US1 and US2 can begin

- [x] T004 Implement file discovery in `identify-archived.sh`: find all `.rs`, test files, `.md` files in workspace
- [x] T005 [P] Implement git log analysis in `identify-archived.sh`: find files unmodified 7+ days
- [x] T006 [P] Implement deprecated marker detection in `identify-archived.sh`: grep for `#[deprecated]` attribute
- [x] T007 Consolidate results into single list: merge 7-day and deprecated lists, remove duplicates in `identify-archived.sh`
- [x] T008 Implement output formatting in `identify-archived.sh`: generate clean list of file paths

**Checkpoint**: ✅ Identification script complete and tested - can generate list of files to archive
- Tested: `identify-archived.sh` successfully identified 396 files unmodified 7+ days
- Output: Generated `/specs/009-cleanup-old-artifacts/sample-archive-list.txt` with 396 files
- Status: Ready for User Story 1 implementation

---

## Phase 3: User Story 1 - Identify and Archive Obsolete Code Artifacts (Priority: P1) 🎯 MVP

**Goal**: Quickly identify code, tests, and documentation unmodified 7+ days or marked deprecated

**Independent Test**: Run `./identify-archived.sh` and verify it outputs a list of files matching criteria (7+ days old OR deprecated)

**Acceptance Criteria**:
1. All `.rs` files unmodified 7+ days are listed
2. All `#[deprecated]` files are listed regardless of modification date
3. All `.md` documentation files unmodified 7+ days are listed
4. Duplicates removed, output sorted by path

### Implementation for User Story 1 ✅

- [x] T009 [P] [US1] Test `identify-archived.sh` with sample ggen workspace files
- [x] T010 [P] [US1] Verify 7-day detection with `git log --follow -p` in `identify-archived.sh`
- [x] T011 [P] [US1] Verify deprecated marker detection with `grep -r "#\[deprecated\]"` in `identify-archived.sh`
- [x] T012 [US1] Add error handling for missing git repository in `identify-archived.sh`
- [x] T013 [US1] Generate sample archive list in `specs/009-cleanup-old-artifacts/sample-archive-list.txt`
- [x] T014 [US1] Document identification criteria in `scripts/README-archival.md`

**Checkpoint**: ✅ User Story 1 complete - can identify files to archive
- Implementation: `identify-archived.sh` fully functional with all 4 phases
- Testing: Successfully identified 396 files unmodified 7+ days
- Documentation: Comprehensive README with examples, troubleshooting, recovery procedures
- Sample Output: Generated `sample-archive-list.txt` with real data from ggen workspace

---

## Phase 4: User Story 2 - Archive Files to `.archive/` Directory (Priority: P1)

**Goal**: Move identified files to `.archive/` with structure preserved, update workspace references

**Independent Test**: Run `./archive-files.sh archive-list.txt` and verify files exist in `.archive/` with manifest

**Acceptance Criteria**:
1. Files moved to `.archive/` preserving original directory structure
2. `.archive/MANIFEST.md` created with timestamp, original path, and reason
3. Cargo.toml updated to remove archived crates from workspace members
4. Import statements referencing archived code removed from active code
5. Git history preserved (no data loss)

### Implementation for User Story 2 ✅

- [x] T015 [P] [US2] Create `.archive/` directory structure in workspace root
- [x] T016 [P] [US2] Implement file move logic in `archive-files.sh`: move files preserving paths
- [x] T017 [P] [US2] Implement manifest creation in `archive-files.sh`: log archive date, original path, reason
- [x] T018 [US2] Implement Cargo.toml update in `archive-files.sh`: remove archived crates from `[workspace]` members
- [x] T019 [US2] Implement import removal in `archive-files.sh`: identify and remove imports of archived modules
- [x] T020 [US2] Test archival with sample file list in `specs/009-cleanup-old-artifacts/sample-archive-list.txt`
- [x] T021 [US2] Verify `.archive/MANIFEST.md` format (timestamp, path, reason)
- [x] T022 [US2] Add rollback instructions in `scripts/README-archival.md` (how to restore from git)
- [x] T023 [US2] Document archival process in `.specify/specs/009-cleanup-old-artifacts/ARCHIVAL-GUIDE.md`

**Checkpoint**: ✅ User Story 2 complete - can archive identified files
- Implementation: `archive-files.sh` fully functional with Cargo.toml updates and import cleanup
- Testing: Dry-run validated with 396 files from sample-archive-list.txt
- Documentation: Complete `ARCHIVAL-GUIDE.md` with recovery procedures and examples
- Safety: Rollback instructions added to README-archival.md

---

## Phase 5: Polish & Validation ✅

**Purpose**: Final integration and documentation

- [x] T024 [P] Create final archival report script in `scripts/generate-archive-report.sh`
- [x] T025 [P] Add progress indicators to `archive-files.sh` (show files moved, count)
- [x] T026 [P] Verify manifest integrity: check all files in `.archive/` are in manifest
- [x] T027 Add recovery instructions to `.archive/MANIFEST.md` header
- [x] T028 Update project README with link to archival guide
- [x] T029 Commit scripts to git on branch `009-cleanup-old-artifacts`
- [x] T030 Test full workflow: identify → archive → verify manifest on ggen workspace

**Checkpoint**: ✅ Phase 5 complete - Full archival system production-ready
- Scripts: `identify-archived.sh`, `archive-files.sh`, `generate-archive-report.sh` all functional
- Documentation: Comprehensive `ARCHIVAL-GUIDE.md` with 400+ lines of documentation
- Safety: Rollback procedures documented, recovery instructions included
- Testing: Dry-run validated, progress indicators working, report generation tested
- Integration: Ready for immediate use on ggen workspace

---

## Dependencies & Execution Order

**Critical Path**:
```
Phase 1 (Setup)
    ↓
Phase 2 (Foundational - Identification Script)
    ↓
Phase 3 (US1 - Identify)
    ↓
Phase 4 (US2 - Archive)
    ↓
Phase 5 (Polish & Validation)
```

**Parallelizable Within Phases**:
- Phase 2: T005, T006 can run in parallel (different aspects of identification)
- Phase 3: T009, T010, T011 can run in parallel (testing different criteria)
- Phase 4: T015, T016, T017 can run in parallel (file move, manifest, different aspects)

**Not Parallelizable**:
- Phase 3 depends on Phase 2 completion
- Phase 4 depends on Phase 3 completion
- Phase 5 depends on Phase 4 completion

---

## Implementation Notes

### MVP Delivery (User Story 1 Only)
**Deliverable**: Working identification script
- Scripts: `identify-archived.sh`
- Output: List of files matching criteria
- Test: Runs on ggen workspace, produces correct list
- Time: ~10 minutes (Task 4-8, 9-14)

### Full Delivery (Both Stories)
**Deliverable**: Complete archival system
- Scripts: `identify-archived.sh`, `archive-files.sh`, `generate-archive-report.sh`
- Output: Archived files in `.archive/`, manifest, cleaned workspace
- Test: Full workflow (identify → archive → verify)
- Time: ~60 minutes (all tasks)

### Key File Paths

**Scripts**:
- `.specify/scripts/bash/identify-archived.sh` - Find files to archive
- `.specify/scripts/bash/archive-files.sh` - Execute archival
- `scripts/generate-archive-report.sh` - Final reporting

**Documentation**:
- `scripts/README-archival.md` - Archival guide and criteria
- `.specify/specs/009-cleanup-old-artifacts/ARCHIVAL-GUIDE.md` - Full process documentation
- `.archive/MANIFEST.md` - Archive inventory (generated during archival)

**Output**:
- `.archive/` - Archived files with original path structure
- `.archive/MANIFEST.md` - Archive manifest with timestamps and reasons

### Testing Strategy

**No automated tests needed** - This is a maintenance operation.

**Manual verification**:
1. Run identification script, visually verify file list matches criteria
2. Run archival script with sample list
3. Verify `.archive/` structure preserves paths
4. Verify manifest contains all files with dates and reasons
5. Verify Cargo.toml updated correctly
6. Verify imports removed from active code

---

## Estimated Task Counts

| Phase | Count | Critical | Notes |
|-------|-------|----------|-------|
| Phase 1 (Setup) | 3 | No | Directory and template creation |
| Phase 2 (Foundational) | 5 | **Yes** | Core identification script logic |
| Phase 3 (US1 - Identify) | 6 | No | Testing and documentation of identification |
| Phase 4 (US2 - Archive) | 9 | No | Archival execution and workspace updates |
| Phase 5 (Polish) | 7 | No | Final integration and validation |
| **Total** | **30** | | |

**Total Time Estimate**: ~60 minutes for full execution
