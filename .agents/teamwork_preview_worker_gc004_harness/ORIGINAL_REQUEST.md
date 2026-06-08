## 2026-06-07T00:34:29Z
You are teamwork_preview_worker (Worker 1). Your working directory is /Users/sac/ggen/.agents/teamwork_preview_worker_gc004_harness/.
Your task is to:
1. Implement the reusable LSP test harness in `crates/ggen-lsp/tests/common/lsp_harness.rs`.
2. The harness must expose ONLY LSP operations:
   - `initialize()`
   - `did_open(uri, text)`
   - `did_change(uri, new_text)`
   - `request_code_action(uri, range)`
   - `execute_command(command, args)`
   - `wait_for_publish_diagnostics(uri)`
   - `assert_diagnostic_code(code)`
   - `assert_source_id(source_id)`
   - `assert_no_diagnostic_from(source_id)`
3. Ensure the test harness does NOT expose internal server state or call functions like `compute_observer_diagnostics()` or `validate_sync()`.
4. Follow the No-Fake Surface Law: all communications must run over standard stdio JSON-RPC.
5. Compile and run check/tests. Write changes.md and handoff.md in your working directory.

MANDATORY INTEGRITY WARNING:
DO NOT CHEAT. All implementations must be genuine. DO NOT hardcode test results, create dummy/facade implementations, or circumvent the intended task. A Forensic Auditor will independently verify your work. Integrity violations WILL be detected and your work WILL be rejected.
