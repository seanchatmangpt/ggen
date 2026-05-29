## 2026-05-26T23:47:43Z
You are teamwork_preview_worker. Your working directory for coordinating reports is /Users/sac/ggen/.agents/teamwork_preview_worker_m6_1.

Your objective is to implement the External Lifecycle Evaluation Doctrine for `ggen-graph`. You must perform the following tasks:

1. Update self-audit log generation in `crates/ggen-graph/src/ocel/self_audit.rs`:
   - Change the event order so that `ev_test_failed` occurs at `9:30:00` and `ev_test_passed` occurs at `9:35:00` (which remedies the test failure before evaluation at `10:05:00` and promotion at `10:10:00`).
   - Modify `generate_self_audit_log` to conditionally include either `CheckpointPromoted` or `CheckpointRefused` based on the environment variable `GALL_CHECKPOINT_STATUS`. If `GALL_CHECKPOINT_STATUS` is "Refused", include `CheckpointRefused` (and omit `CheckpointPromoted`). Otherwise (default), include `CheckpointPromoted` (and omit `CheckpointRefused`).
   - Update the unit tests in `self_audit.rs` (lines 635 and 648) to expect `16` events instead of `17`.

2. Update `crates/ggen-graph/audit/vision2030.self_audit.summary.md` line 11 to reference 16 process events instead of 17.

3. Create the External Observer Script Ring under `scripts/gall/external/`:
   - Implement scripts `00_capture_baseline.sh` through `12_detect_contradictions.sh` and `13_adjudicate_gall_promotion.sh`.
   - Make all scripts executable.
   - Use the JQ double-pass verification logic designed by Explorer 2 for `09_verify_ocel_self_audit.sh`.
   - Use the JQ contradiction checks designed by Explorer 3 for `12_detect_contradictions.sh`.
   - Use the master adjudication flow designed by Explorer 3 for `13_adjudicate_gall_promotion.sh` to run the scripts sequentially, verify script and file digests against a secure `manifest.sha256`, run contradiction checks, and output results to `crates/ggen-graph/audit/vision2030.external_adjudication.json`.
   - Generate `scripts/gall/external/manifest.sha256` containing the correct SHA-256 hashes for all 13 verifier scripts (00 to 12) and key source files.

4. Integrate `bash scripts/gall/external/13_adjudicate_gall_promotion.sh` into `Makefile.toml` (specifically under the `ci-gate` task) and into the GitHub Actions workflow `.github/workflows/ci.yml` (e.g. under the `comprehensive-test` job).

5. Rewrite `docs/VISION_2030_GALL_PROOF.md` such that the promotion decision is strictly declared as dependent on the external verifier scripts' adjudication results in `crates/ggen-graph/audit/vision2030.external_adjudication.json` (as designed by Explorer 3).

6. Verify that:
   - All unit and integration tests run successfully: `cargo test -p ggen-graph`.
   - `bash scripts/gall/external/13_adjudicate_gall_promotion.sh` runs successfully, producing the JSON file `crates/ggen-graph/audit/vision2030.external_adjudication.json`.
   - All verifier script scans return 0 indicating zero violations.

CRITICAL WARNING: DO NOT CHEAT. All implementations must be genuine. DO NOT hardcode test results, create dummy/facade implementations, or circumvent the intended task. A Forensic Auditor will independently verify your work. Integrity violations WILL be detected and your work WILL be rejected.

Write a detailed handoff report when complete and send it back to the caller.
