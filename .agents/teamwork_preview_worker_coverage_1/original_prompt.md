## 2026-05-26T23:32:17Z
You are teamwork_preview_worker_coverage_1.
Your working directory is /Users/sac/ggen/.agents/teamwork_preview_worker_coverage_1/.
Your mission is:
1. Implement requirement-to-evidence coverage mapping in `crates/ggen-graph/src/ocel/coverage.rs`. Define standard structures and a generator function `pub fn generate_coverage_matrix() -> CoverageMatrix` mapping all 9 requirements (6 PRD requirements from the initial request, and 3 requirements from the follow-up request) to source files, tests, and evidence commands. Expose this module in `crates/ggen-graph/src/ocel/mod.rs`.
2. Implement the following workspace binary targets:
   - `crates/ggen-graph/src/bin/emit_audit.rs`: Creates the `crates/ggen-graph/audit/` directory if missing, generates both the self-audit log and the coverage matrix, and writes them to:
     - `crates/ggen-graph/audit/vision2030.self_audit.ocel.json`
     - `crates/ggen-graph/audit/vision2030.coverage.json`
   - `crates/ggen-graph/src/bin/verify_audit.rs`: Reads the self-audit log and coverage matrix, and enforces the 5 Completeness Rules:
     1. Requirements have evidence (check that every requirement in `vision2030.coverage.json` lists non-empty files/tests/commands, and that the events in the log link back to them).
     2. Checkpoints have Command evidence (evaluating checkpoints is linked to execution events of validation commands).
     3. Prior evaluations exist (any checkpoint promotion/refusal is preceded chronologically by a checkpoint evaluation event).
     4. Anti-fake is audited (the event stream contains both `AntiFakeScanned` and `ForbiddenSurfaceScanned` events).
     5. Unsupported capabilities are linked (any declared unsupported capability is linked back to its originating constraint/requirement).
     The binary must exit with 0 on success, and non-zero on failure.
3. Write the shell scripts:
   - `scripts/gall/emit_ocel_self_audit.sh`: Runs the emit binary target.
   - `scripts/gall/verify_ocel_self_audit.sh`: Runs the verify binary target.
   Make sure these scripts are executable and have proper shebangs and `set -euo pipefail`.
4. Add integration tests:
   - `crates/ggen-graph/tests/ocel_self_audit.rs`: Projects the self-audit log into `DeterministicGraph`, performs SPARQL queries (using the safe qualifiers) to verify relationship queries and event ordering, and asserts correctness.
   - `crates/ggen-graph/tests/vision2030_coverage.rs`: Validates the coverage matrix file schema and structure.
5. Verify that all tests pass and the crate compiles cleanly by running `cargo check -p ggen-graph --all-targets` and `cargo test -p ggen-graph`.
6. Write your changes in `/Users/sac/ggen/.agents/teamwork_preview_worker_coverage_1/changes.md` and handoff report in `/Users/sac/ggen/.agents/teamwork_preview_worker_coverage_1/handoff.md`.
7. Send a message to the orchestrator (f5c30d73-c213-4935-bd9d-5c2ce4c67f3e) with a link to your handoff.

MANDATORY INTEGRITY WARNING:
DO NOT CHEAT. All implementations must be genuine. DO NOT hardcode test results, create dummy/facade implementations, or circumvent the intended task. A Forensic Auditor will independently verify your work. Integrity violations WILL be detected and your work WILL be rejected.
