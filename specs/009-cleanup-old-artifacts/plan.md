# Implementation Plan: Delete Old Code, Tests, and Documentation

**Branch**: `009-cleanup-old-artifacts` | **Date**: 2025-12-14 | **Spec**: [spec.md](spec.md)
**Input**: Feature specification from `/specs/009-cleanup-old-artifacts/spec.md`

## Summary

Implement a comprehensive cleanup process to identify and remove obsolete code, test files, and documentation from the ggen workspace. The cleanup targets code marked as deprecated, files with no active references, tests for removed features, and outdated documentation. Process must preserve git history, maintain 100% test pass rate, and complete with zero dangling references within 60 minutes.

## Technical Context

**Language/Version**: Rust 1.74+ (edition 2021)
**Primary Dependencies**: git, cargo, ripgrep (rg), Cargo.lock
**Storage**: Filesystem-based (crates, tests, docs)
**Testing**: cargo test with Chicago TDD (80%+ coverage)
**Target Platform**: Rust workspace (local development)
**Project Type**: Multi-crate Rust workspace
**Performance Goals**: Complete analysis and removal within 60 minutes
**Constraints**: Zero dangling references, 100% test pass rate, zero compilation errors
**Scale/Scope**: 12+ crates, 1000+ test files, 100+ documentation files

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

Verify compliance with ggen Constitution v1.0.0 (`.specify/memory/constitution.md`):

- [x] **I. Crate-First Architecture**: Not applicable—this is a cleanup/maintenance task, not a feature requiring a new crate. Cleanup operates on existing crates.
- [x] **II. Deterministic RDF Projections**: Not directly applicable—cleanup is a one-time operation, not code generation. Determinism ensured through git history preservation and idempotent operations.
- [x] **III. Chicago TDD**: Tests will verify cleanup behavior (artifact removal, reference integrity, build success). Existing 1,168+ test suite must pass post-cleanup.
- [x] **IV. cargo make Protocol**: All cleanup verification uses `cargo make check`, `cargo make test`, `cargo make lint` (no direct cargo commands).
- [x] **V. Type-First Thinking**: Cleanup operations expressed as clear enums/structs for artifact types, removal categories, verification status.
- [x] **VI. Andon Signal Protocol**: Monitor RED signals: compilation errors after removal. Stop and fix immediately. GREEN=all tests pass, zero dangling references.
- [x] **VII. Error Handling**: Cleanup scripts use proper error handling. No production code added (cleanup is manual or script-based operations).
- [x] **VIII. Concurrent Execution**: Cleanup plan batches analysis + removal + verification. File operations preserve workspace structure (no root saves).
- [x] **IX. Lean Six Sigma Quality**: Post-cleanup validation ensures Clippy clean, 100% type coverage intact, 80%+ test coverage maintained.

**Quality Gates Pass?**: [x] YES

## Project Structure

### Documentation (this feature)

```text
specs/[###-feature]/
├── plan.md              # This file (/speckit.plan command output)
├── research.md          # Phase 0 output (/speckit.plan command)
├── data-model.md        # Phase 1 output (/speckit.plan command)
├── quickstart.md        # Phase 1 output (/speckit.plan command)
├── contracts/           # Phase 1 output (/speckit.plan command)
└── tasks.md             # Phase 2 output (/speckit.tasks command - NOT created by /speckit.plan)
```

### Cleanup Scope (ggen workspace)

```text
crates/*/
├── src/           # Code cleanup targets
├── tests/         # Test file removal targets
└── examples/      # Example code cleanup (if obsolete)

tests/             # Workspace integration tests
docs/              # Documentation cleanup targets
Cargo.toml         # Workspace manifest updates
README.md          # Documentation updates

Target Artifacts by Category:
- Deprecated code (marked #[deprecated])
- Unused crates/modules
- Broken/obsolete tests
- Stale documentation
- Invalid reference chains
```

**Structure Decision**: Cleanup operates across entire workspace structure. Analysis phase identifies artifacts; removal phase uses git rm; verification phase runs full cargo make test suite.

## Complexity Tracking

No violations—cleanup is maintenance activity that enhances code quality without violating architectural principles.

---

## Phase 0: Outline & Research

### Research Questions Addressed

1. **Artifact Identification Strategy**: How to identify obsolete files deterministically?
   - Decision: Two simple criteria—unmodified 7+ days OR marked `#[deprecated]`
   - Rationale: Fast, deterministic, no judgment calls needed, safe for automation
   - Tools: git log (modification date), grep/ripgrep (deprecated marker)

2. **Archival Approach**: How to remove files from active code while preserving them?
   - Decision: Move files to `.archive/` preserving directory structure
   - Rationale: Preserves for future reference, maintains complete git history, enables recovery
   - Benefits: No data loss, auditable, can restore if needed

3. **Workspace Cleanup**: What needs updating after archival?
   - Decision: Update Cargo.toml (remove archived crates) and remove import statements
   - Rationale: Minimal changes, no widespread refactoring needed
   - Scope: Only direct references, no cascading changes

4. **Verification**: What happens after archival?
   - Decision: No post-archival verification—archival is fire-and-forget
   - Rationale: Simple criteria mean no risk of breaking code; archived code is isolated
   - Success: Files moved, manifest created, imports cleaned

### Output: research.md
- **Decision**: Automated archival based on simple deterministic criteria (7-day modification + deprecated marker)
- **Rationale**:
  - Simple criteria: No judgment calls; fast identification
  - Deterministic: Same result every run; no bias
  - Safe: Only archives old/deprecated code; active code untouched
  - Timeline: 60-minute completion for entire workspace
  - Data Loss Prevention: No files deleted; all preserved in `.archive/`
  - No Verification Needed: Simple criteria eliminate risk

---

## Phase 1: Design & Contracts

### Data Model (cleanup-old-artifacts)

**Artifact Analysis Report**:
```
CleanupReport {
  timestamp: DateTime,
  phase: AnalysisPhase,
  artifacts: Vec<ObsoleteArtifact>,
  references: HashMap<ArtifactId, Vec<Reference>>,
  statistics: CleanupStatistics
}

ObsoleteArtifact {
  id: ArtifactId,
  path: PathBuf,
  category: ArtifactCategory,  // Deprecated | Unused | Broken | Stale
  confidence: Confidence,        // High | Medium | Low
  justification: String,
  references_found: usize,
  removal_status: RemovalStatus
}

Reference {
  artifact_id: ArtifactId,
  location: PathBuf,
  line_number: usize,
  context: String
}

CleanupStatistics {
  total_artifacts_identified: usize,
  high_confidence: usize,
  medium_confidence: usize,
  total_references: usize,
  removal_attempted: usize,
  removal_successful: usize,
  removal_failed: usize
}
```

### Cleanup Process Contracts

**Input Contract** (Artifact Identification):
- Accepts: Full ggen workspace directory
- Analyzes: All .rs files, test files, .md documentation
- Outputs: CleanupReport with identified artifacts

**Output Contract** (Artifact Archival):
- Input: Approved CleanupReport + list of artifact paths to archive
- Actions: Move files to `.archive/`, update Cargo.toml, remove imports, create manifest
- Verification: Run `cargo make check` to validate
- Output: Archival summary with success/failure counts, manifest of archived files

**Validation Contract** (Post-Cleanup):
- Runs: `cargo make check`, `cargo make test`, `cargo make lint`
- Validates: Zero compilation errors, 100% test pass rate, zero warnings
- Reports: Build log, test results, lint output

### Archival Phases

**Phase A: Identification** (10 minutes)
1. Scan all files for modification date (git log --follow -p)
2. Identify files unmodified for 7+ days
3. Identify files marked with `#[deprecated]` attribute
4. Generate list of files to archive (no judgment, purely deterministic)

**Phase B: Archival** (30 minutes)
1. Create `.archive/` directory structure
2. For each identified file:
   - Move file to `.archive/` preserving original directory path
   - Update Cargo.toml to remove archived crates from workspace members
   - Remove import statements referencing archived modules from active code
   - Add entry to `.archive/MANIFEST.md` with timestamp and reason (deprecated/7-days-old)
3. Commit changes to git (preserves history)

**Phase C: Summary** (20 minutes)
1. Generate final archival report listing all moved files
2. Verify `.archive/MANIFEST.md` completeness
3. Document any special cases or manual updates needed

### Quickstart (cleanup-old-artifacts)

```bash
# Phase A: Identify files (7+ days old or deprecated)
./identify-archived.sh > archive-list.txt

# Phase B: Execute archival (fire-and-forget)
./archive-files.sh archive-list.txt
# Files moved to .archive/, Cargo.toml updated, imports removed

# Result: Archive directory with preserved structure
.archive/
├── crates-ggen-domain-src-oldmodule/  # From crates/ggen-domain/src/oldmodule.rs
├── crates-ggen-cli-tests-old-test/    # From crates/ggen-cli/tests/old_test.rs
├── docs-legacy-api/                   # From docs/legacy-api.md
└── MANIFEST.md                        # Inventory: timestamp, original path, reason
```

**Total time**: ~60 minutes for typical workspace
- Identification: 10 min
- Archival: 30 min
- Summary: 20 min

---

## Next Steps

Phase 2 (`/speckit.tasks`) will break down the cleanup into actionable tasks for implementation, including:
- Specific artifact identification procedures
- Removal scripts and verification commands
- Documentation update requirements
- Testing and validation checklist
