## 2026-06-06T17:34:29-07:00
You are teamwork_preview_worker (Worker 2). Your working directory is /Users/sac/ggen/.agents/teamwork_preview_worker_gc004_admission/.
Your task is to:
1. Implement the 5 admission test categories using the reusable harness in `crates/ggen-lsp/tests/common/lsp_harness.rs`.
2. Test categories:
   - Diagnostic protocol tests: initialize → didOpen generated file → wait publishDiagnostics → assert `GGEN-PROJECTED-001` and source_id `ggen_lsp_observer`.
   - Drift protocol tests: didOpen projected file → didChange modified text → wait publishDiagnostics → assert `GGEN-DRIFT-001`.
   - Pack-domain diagnostic tests:
     - open malformed clap command file → wait publishDiagnostics → assert `CLAP-PACK-*` and source_id `clap_noun_verb_pack_lsp`.
     - open malformed tower LSP composition file → wait publishDiagnostics → assert `TOWER-PACK-*` and source_id `tower_lsp_max_pack_lsp`.
   - Authority split tests: corrupt receipt → expect `GGEN-RECEIPT-*` (no `CLAP-*` or `TOWER-*` diagnostic owns it); corrupt clap domain shape → expect `CLAP-*` (ggen-lsp must not invent `CLAP-*` itself).
   - Code action routing tests: request codeAction → assert kind/intention → executeCommand → assert no direct write → assert PackPlan/Staging/MutationGate/Receipt path.
3. Ensure all tests go through the full stdio LSP pipeline (No-Fake Surface Law).
4. Compile and run tests. Write changes.md and handoff.md in your working directory.

MANDATORY INTEGRITY WARNING:
DO NOT CHEAT. All implementations must be genuine. DO NOT hardcode test results, create dummy/facade implementations, or circumvent the intended task. A Forensic Auditor will independently verify your work. Integrity violations WILL be detected and your work WILL be rejected.
