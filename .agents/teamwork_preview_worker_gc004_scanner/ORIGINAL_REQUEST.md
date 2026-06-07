## 2026-06-06T17:34:29-07:00

You are teamwork_preview_worker (Worker 3). Your working directory is /Users/sac/ggen/.agents/teamwork_preview_worker_gc004_scanner/.
Your task is to:
1. Implement the generated dogfood test to scan admission tests and enforce bypass-kills.
2. Anti-bypass rules to enforce:
   - `BYPASS-LSP-001`: Admission tests do not call `compute_observer_diagnostics` directly.
   - `BYPASS-LSP-002`: Admission tests do not call `validate_sync` directly.
   - `BYPASS-LSP-003`: Admission tests do not inspect private server state.
   - `BYPASS-LSP-004`: Admission tests do not write files except through LSP codeAction → PackPlan → MutationGate.
   - `BYPASS-LSP-005`: Diagnostics are asserted from `publishDiagnostics` payloads only.
3. Forbidden symbols to scan: `compute_observer_diagnostics`, `analyze_and_observe`, `validate_sync`, `observe_pack_domain`, `state.diagnostics`, `direct_write`, `std::fs::write`.
4. Compile and run check/tests. Write changes.md and handoff.md in your working directory.

MANDATORY INTEGRITY WARNING:
DO NOT CHEAT. All implementations must be genuine. DO NOT hardcode test results, create dummy/facade implementations, or circumvent the intended task. A Forensic Auditor will independently verify your work. Integrity violations WILL be detected and your work WILL be rejected.
