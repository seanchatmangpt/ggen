# Plan — GC003 Victory Audit

## Phase A: Timeline & Files Verification
1. [ ] Check current git branch using `git status` and verify if it's `feat/ggen-lsp-source-laws` or another appropriate branch.
2. [ ] Check git log to reconstruct the project timeline and check for anomalies (clusters of commits, file modification patterns).
3. [ ] Verify that the required directories exist: `.tmp_gc003/target`, `.tmp_gc003/staging`, `.tmp_gc003/receipts`.
4. [ ] List all files in the workspace and locate where the code resides (e.g. `ggen-projection`, etc.).

## Phase B: Cheating & Law Compliance Scan
1. [ ] Scan codebase for forbidden patterns defined in `AGENTS.md` (mocks, stubs, fake client constructs, `mockall`, test-only fake structures).
2. [ ] Verify that tests are Chicago-style and use real cryptographic derivations (BLAKE3) and real OpenTelemetry traces, without placeholder hashes like `"hash_placeholder"`, `"uuid_placeholder"`, `"TODO"`, etc.
3. [ ] Verify that no unauthorized mutations were made to `~/tower-lsp-max` (or the equivalent local/global repository if applicable) unless declared as an exported receipt artifact containing the 5 required fields.
4. [ ] Validate receipt structure in `.tmp_gc003/receipts`. Check that receipts contain authentic observations, valid BLAKE3 hashes, and actual boundary evidence.

## Phase C: Independent Test Execution
1. [ ] Run `cargo test -p ggen-projection --test dogfood_gc003` and record output.
2. [ ] Run `cargo test -p ggen-projection --test f8_equation_enforcement` and record output.
3. [ ] Run `cargo make check` and record output.
4. [ ] Run `cargo make test` and record output.
5. [ ] Compare results with any claimed scores or results in progress/completion reports.

## Phase D: Reporting & Verdict
1. [ ] Draft the comprehensive audit report `audit_report.md` in the working directory.
2. [ ] Write the `handoff.md` file following the Handoff Protocol.
3. [ ] Report the final verdict directly back to the Sentinel.
