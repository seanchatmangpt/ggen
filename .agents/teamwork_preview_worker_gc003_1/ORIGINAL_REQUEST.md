## 2026-06-06T23:02:10Z

You are teamwork_preview_worker. Your working directory is /Users/sac/ggen/.agents/teamwork_preview_worker_gc003_1/.
Your task is to:
1. Run `cargo check --workspace` or `cargo make check` from /Users/sac/ggen to verify that compilation is fully successful and clean.
2. Run `cargo test --workspace --tests` or `cargo make test` from /Users/sac/ggen to verify that all workspace integration/unit tests pass cleanly.
3. Check the git status of the /Users/sac/tower-lsp-max repository to ensure it is clean and that no mutations have occurred unless declared as exported receipt artifacts containing:
   - `producing_workspace = ~/ggen`
   - `storing_workspace = ~/tower-lsp-max`
   - `export_reason = checkpoint_receipt_archive | downstream_playground_receipt`
   - `exported_artifact_digest`
   - `export_receipt_digest`
4. Confirm that the test output generated for target output, staging output, and receipt sink maps to the following sandboxed directory boundaries under /Users/sac/ggen:
   - `target = /Users/sac/ggen/.tmp_gc003/target`
   - `staging = /Users/sac/ggen/.tmp_gc003/staging`
   - `receipt_sink = /Users/sac/ggen/.tmp_gc003/receipts`
   - `proof_pack = /Users/sac/ggen/crates/ggen-pack-gall-checkpoint-proof`
5. Write your findings to changes.md and handoff.md in your working directory.

MANDATORY INTEGRITY WARNING:
DO NOT CHEAT. All implementations must be genuine. DO NOT hardcode test results, create dummy/facade implementations, or circumvent the intended task. A Forensic Auditor will independently verify your work. Integrity violations WILL be detected and your work WILL be rejected.
