# Changes Report

This document records the exact changes made to implement the OCEL v2 self-audit log and requirement-to-evidence coverage verification.

## Added Files

### 1. `crates/ggen-graph/src/ocel/coverage.rs`
- Defines standard structures `RequirementEvidence` and `CoverageMatrix`.
- Implements `pub fn generate_coverage_matrix() -> CoverageMatrix` mapping all 9 project requirements (6 from the initial PRD, 3 from the follow-up request) to their respective implementation source files, test files, and verification commands.

### 2. `crates/ggen-graph/src/bin/emit_audit.rs`
- Workspace binary target that creates `crates/ggen-graph/audit/` if missing.
- Generates `vision2030.self_audit.ocel.json` and `vision2030.coverage.json` and writes them formatted in JSON.

### 3. `crates/ggen-graph/src/bin/verify_audit.rs`
- Workspace binary target that enforces the 5 Completeness Rules on the self-audit log and coverage files:
  1. Requirements have evidence (checks lists are non-empty and referenced in log).
  2. Checkpoints have Command evidence (evaluating checkpoints is linked to execution events of validation commands).
  3. Prior evaluations exist (checkpoint promotion/refusal is preceded chronologically by a checkpoint evaluation event).
  4. Anti-fake is audited (verifies the log contains both `AntiFakeScanned` and `ForbiddenSurfaceScanned` events).
  5. Unsupported capabilities are linked (any declared unsupported capability is linked back to its originating constraint/requirement).
- Exits with `0` on success and `1` on violation.

### 4. `scripts/gall/emit_ocel_self_audit.sh`
- Shell script wrapper for `cargo run -p ggen-graph --bin emit_audit` configured with `set -euo pipefail`.

### 5. `scripts/gall/verify_ocel_self_audit.sh`
- Shell script wrapper for `cargo run -p ggen-graph --bin verify_audit` configured with `set -euo pipefail`.

### 6. `crates/ggen-graph/tests/ocel_self_audit.rs`
- Projects the self-audit log into `DeterministicGraph`, extracts it back, and queries relationships.
- Uses SPARQL to verify relationship queries and event ordering (e.g. checkpoint evaluation precedes promotion chronologically).

### 7. `crates/ggen-graph/tests/vision2030_coverage.rs`
- Validates the coverage matrix schema, requirement count, and file existence.

## Modified Files

### 1. `crates/ggen-graph/src/ocel/mod.rs`
- Exposed the `coverage` module and its public items (`RequirementEvidence`, `CoverageMatrix`, `generate_coverage_matrix`).

### 2. `crates/ggen-graph/src/ocel/self_audit.rs`
- Updated the generator `generate_self_audit_log` to include all 9 requirement objects instead of placeholders.
- Linked the requirements in events, added command runs to the checkpoint evaluation events, and properly linked the checkpoint refusal events to the checkpoint object.
- Updated the unit test assertions to match the new counts of 25 objects and 17 events.
