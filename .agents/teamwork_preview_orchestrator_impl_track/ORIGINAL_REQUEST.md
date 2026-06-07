# Original User Request

## 2026-06-06T20:33:00Z
You are the Implementation Track Orchestrator for the 'ggen Projection Intelligence' mission.
Your working directory is: /Users/sac/ggen/.agents/teamwork_preview_orchestrator_impl_track
Your parent is 4d62b35c-d650-46e1-a717-55a606bd5c2a (Project Orchestrator).
Your task is to:
1. Read /Users/sac/ggen/PROJECT.md and /Users/sac/ggen/TEST_INFRA.md.
2. Formulate a SCOPE.md in your working directory detailing the Implementation Track scope and milestones.
3. Decompose the implementation track into milestones:
   - Milestone 1: Implement Pack & Projection Core Model (PackDescriptor, PackPlan, maps, receipts, wasm4pm process-evidence export, and ggen sync stage μ₀).
   - Milestone 2: Implement Durable Packs (`ggen-pack-clap-noun-verb` and `ggen-pack-tower-lsp-max`) and target file generation sync inside `/Users/sac/tower-lsp-max/examples/clap-noun-verb-lsp/`.
   - Milestone 3: Implement LSP Meta-Observer (diagnostics GGEN-PROJECTED-001, GGEN-DRIFT-001, GGEN-EVIDENCE-001, GGEN-CUSTOMIZE-001, GGEN-OVERRIDE-001, and GGEN-PROJECT-OPPORTUNITY-001) in `ggen-lsp`.
   - Milestone 4: Implement Composite LSP diagnostics and inlay hints composition with source attribution (`source_id = "ggen-lsp"`) in `tower-lsp-max`.
4. Wait for the parent to signal that TEST_READY.md is published (or poll for its existence), then execute Phase 1 (passing all E2E tests) and Phase 2 (adversarial coverage hardening via Challengers and Reviewers).
5. Ensure a Forensic Auditor (`teamwork_preview_auditor`) is run to perform integrity checks, guaranteeing no stubs, mock/fake success patterns, or hardcoded expected outputs.
6. Once all verification criteria are met and tests pass 100%, send a message back to the parent Project Orchestrator (4d62b35c-d650-46e1-a717-55a606bd5c2a) with your handoff.

MANDATORY INTEGRITY WARNING:
DO NOT CHEAT. All implementations must be genuine. DO NOT hardcode test results, create dummy/facade implementations, or circumvent the intended task. A Forensic Auditor will independently verify your work. Integrity violations WILL be detected and your work WILL be rejected.
