# Feature Specification: Delete Old Code, Tests, and Documentation

**Feature Branch**: `009-cleanup-old-artifacts`
**Created**: 2025-12-14
**Status**: Draft
**Input**: User description: "delete old code tests docs, etc"

## User Scenarios & Testing *(mandatory)*

<!--
  IMPORTANT: User stories should be PRIORITIZED as user journeys ordered by importance.
  Each user story/journey must be INDEPENDENTLY TESTABLE - meaning if you implement just ONE of them,
  you should still have a viable MVP (Minimum Viable Product) that delivers value.
  
  Assign priorities (P1, P2, P3, etc.) to each story, where P1 is the most critical.
  Think of each story as a standalone slice of functionality that can be:
  - Developed independently
  - Tested independently
  - Deployed independently
  - Demonstrated to users independently
-->

### User Story 1 - Identify and Archive Obsolete Code Artifacts (Priority: P1)

As a repository maintainer, I need to quickly identify and archive code, tests, and documentation that are unmodified for 7 days or marked deprecated, so that the active codebase stays clean and focused.

**Why this priority**: This is the core requirement. Identifying based on simple criteria (last modified + deprecated marker) is fast and safe—only old/deprecated code gets archived.

**Independent Test**: Can be tested by running an archive operation that moves all unmodified (7+ days) or deprecated files to `.archive/`, completing in under 60 minutes.

**Acceptance Scenarios**:

1. **Given** the ggen repository, **When** running the archive command, **Then** all files unmodified for 7+ days are moved to `.archive/`
2. **Given** files marked with `#[deprecated]` attribute, **When** analyzing, **Then** deprecated files are moved to `.archive/` regardless of modification date
3. **Given** archived files, **When** examining `.archive/`, **Then** original directory structure is preserved with manifest showing archive date and reason

---

### User Story 2 - Archive Files to `.archive/` Directory (Priority: P1)

As a maintainer, I need to move identified obsolete files to `.archive/` with their directory structure preserved, so they're removed from active development but available for future reference.

**Why this priority**: Archival is the core action. Files are safely moved, not deleted, preserving all history and context.

**Independent Test**: Can be tested by running archive command and verifying files exist in `.archive/` with manifest.

**Acceptance Scenarios**:

1. **Given** files to archive, **When** running archive command, **Then** files moved to `.archive/` with original paths preserved
2. **Given** archived files, **When** examining manifest, **Then** archive date and reason (deprecated/7-day-old) recorded
3. **Given** archived crates, **When** checking workspace, **Then** Cargo.toml references removed from active workspace

---

### Edge Cases

- What if a file is 7 days old but has active references? (Archive anyway—active code will still work)
- How should we handle conditionally-compiled code (e.g., `#[cfg(test)]`)? (Archive with test code)
- Should we archive documentation even if it references kept code? (Only if deprecated or 7-day threshold met)
- What about git history? (All preserved—files moved in git, not deleted)

## Requirements *(mandatory)*

<!--
  ACTION REQUIRED: The content in this section represents placeholders.
  Fill them out with the right functional requirements.
-->

### Functional Requirements

- **FR-001**: System MUST identify all files unmodified for 7+ days in `.rs`, test files, and `.md` documentation
- **FR-002**: System MUST identify all files marked with `#[deprecated]` attribute regardless of modification date
- **FR-003**: System MUST move identified files to `.archive/` preserving original directory structure
- **FR-004**: System MUST create manifest in `.archive/MANIFEST.md` with archive date, original path, and reason
- **FR-005**: System MUST update `Cargo.toml` to remove archived crates from workspace members
- **FR-006**: System MUST remove import statements referencing archived code from remaining active code
- **FR-007**: System MUST handle nested directories, preserving full path hierarchy in archive
- **FR-008**: System MUST complete archival operation within 60 minutes for typical ggen workspace (12+ crates)

### Key Entities

- **Obsolete Artifact**: A code file, test file, or documentation file that is no longer needed in the current codebase
- **Reference**: An import, module declaration, or code usage of an artifact
- **Archive Manifest**: A document listing all archived artifacts with categorization, justification, archive location, and timestamp

## Success Criteria *(mandatory)*

<!--
  ACTION REQUIRED: Define measurable success criteria.
  These must be technology-agnostic and measurable.
-->

### Measurable Outcomes

- **SC-001**: 100% of unmodified (7+ days) files are archived to `.archive/`
- **SC-002**: 100% of `#[deprecated]` files are archived to `.archive/`
- **SC-003**: `.archive/MANIFEST.md` created with all archived files listed with timestamp and reason
- **SC-004**: Archived crates removed from `Cargo.toml` workspace members
- **SC-005**: Active code no longer imports archived modules (import statements updated/removed)
- **SC-006**: Archival operation completes within 60 minutes for entire ggen workspace
- **SC-007**: All archived files preserved with original directory paths intact in `.archive/`

## Assumptions

- **Simple Criteria**: Archival is deterministic—based only on modification date (7+ days) and deprecated marker, no judgment calls
- **Archival Preservation**: All files moved to `.archive/`, not deleted, preserving complete git history
- **Archive Structure**: Original directory paths preserved in `.archive/` subdirectories with manifest for recovery
- **Active Development**: Archival occurs on dedicated branch and doesn't interfere with active development
- **No Post-Archival Verification**: Archival is fire-and-forget; no build/test verification required
- **Workspace Cleanup**: Only Cargo.toml and import statements need updating; no extensive refactoring

## Out of Scope

- Performance optimization of remaining code
- Refactoring or modernization of active code
- Adding new features or functionality
- Updating dependencies (separate activity)

## Dependencies & Integration

- Requires read access to full codebase
- Requires ability to modify git repository
- Depends on ability to run build and test suite for verification
