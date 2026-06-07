## 2026-06-06T17:47:14-07:00
You are teamwork_preview_worker. Your working directory is /Users/sac/ggen/.agents/teamwork_preview_worker_gc006_1/.
Your tasks are:
1. The canonical proof pack path is `/Users/sac/ggen/crates/ggen-pack-proofs/`.
2. Edit `crates/ggen-pack-proofs/templates/dogfood_gc006.rs.tmpl` to remove all hardcoded `/Users/sac` paths and use environment variable or relative path resolution (e.g. workspace_root, path variables, or searching parent directories).
3. Verify if `dogfood_gc006.rs.tmpl` is registered in `crates/ggen-pack-proofs/manifest.toml` (or `manifest_gc006.toml`). If not, register it correctly.
4. Project `dogfood_gc006.rs` using `sync_target` projection engine.
5. Run the generated `dogfood_gc006` test.
6. Verify and prove:
   - No shadow wasm4pm crates exist in ggen or tower-lsp-max.
   - wasm4pm-lsp does not own conformance/replay logic.
   - Neutral adapter gc005-wasm4pm-adapter does not emit fake FIT from local logic.
   - Neutral adapter calls sealed wasm4pm authority.
   - Sealed workspaces ~/wasm4pm and ~/wasm4pm-compat remain 100% read-only.
7. Ensure all other cargo make check/test runs compile and pass cleanly on the `feat/ggen-lsp-source-laws` branch.
8. Write a detailed changes.md and handoff.md in your working directory.

MANDATORY INTEGRITY WARNING:
DO NOT CHEAT. All implementations must be genuine. DO NOT hardcode test results, create dummy/facade implementations, or circumvent the intended task. A Forensic Auditor will independently verify your work. Integrity violations WILL be detected and your work WILL be rejected.
