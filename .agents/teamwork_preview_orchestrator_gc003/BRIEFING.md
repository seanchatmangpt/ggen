# BRIEFING — 2026-06-06T22:58:06Z

## Mission
Orchestrate the verification and execution of GC006 (Authority Surface Lock) under the C4 architecture, ensuring sealed workspaces remain 100% read-only, projecting `dogfood_gc006.rs` through `sync_target`, and proving that the neutral adapter calls the sealed authority without local conformance faking.

## 🔒 My Identity
- Archetype: self
- Roles: orchestrator, user_liaison, human_reporter, successor
- Working directory: /Users/sac/ggen/.agents/teamwork_preview_orchestrator_gc003/
- Original parent: main agent
- Original parent conversation ID: e1d0b6c7-19ef-4c59-98d4-4202224bff58

## 🔒 My Workflow
- **Pattern**: Project Pattern
- **Scope document**: /Users/sac/ggen/.agents/teamwork_preview_orchestrator_gc003/PROJECT.md
1. **Decompose**: Decompose the task into milestones for implementation/verification in `/Users/sac/ggen`.
2. **Dispatch & Execute** (pick ONE):
   - **Delegate (sub-orchestrator)**: When an item is too large, spawn a sub-orchestrator for it.
3. **On failure** (in this order):
   - Retry: nudge stuck agent or re-send task
   - Replace: spawn fresh agent with partial progress
   - Skip: proceed without (only if non-critical)
   - Redistribute: split stuck agent's remaining work
   - Redesign: re-partition decomposition
   - Escalate: report to parent (sub-orchestrators only, last resort)
4. **Succession**: At 16 spawns, write handoff.md, spawn successor.
- **Work items**:
  1. Exploration & Initialization [done]
  2. Code Alignment & Base GC003/GC004 Verification [in-progress]
  3. Reusable LSP Test Harness Implementation [pending]
  4. Protocol Path & 5 Admission Test Categories Implementation [pending]
  5. Source-Level Bypass-Kills (Anti-Bypass) Enforcement [pending]
  6. GC006 Sealed Boundaries & Neutral wasm4pm Adapter [pending]
  7. wasm4pm-lsp Server Implementation [pending]
  8. Proof Projection Harness (dogfood_gc006) [pending]
  9. final verifier validation [pending]
  10. E2E workspace test run & Forensic Audit [pending]
- **Current phase**: 2
- **Current focus**: Code Alignment & Base GC003/GC004 Verification

## 🔒 Key Constraints
- NEVER write, modify, or create source code files directly.
- NEVER run build/test commands yourself — require workers to do so.
- Workspace = ~/ggen
- Target = ~/ggen/.tmp_gc003/target
- Staging = ~/ggen/.tmp_gc003/staging
- Receipt Sink = ~/ggen/.tmp_gc003/receipts
- Proof Pack = crates/ggen-pack-gall-checkpoint-proof
- No mutation of ~/tower-lsp-max unless exported with required metadata.
- No mocks, stubs, placeholder hashes; real Chicago-style tests under feat/ggen-lsp-source-laws.
- Strict "No-Fake Surface Law" enforcement (LSP client communicates over stdio; SHACL validation; BLAKE3 binds canonical equation context).
- Specific classification status parameters must be tracked and output.
- GC006 Correct Architecture: ~/wasm4pm and ~/wasm4pm-compat are sealed read-only workspaces. Do not write to them. Rename local adapters to gc005-wasm4pm-adapter.
- wasm4pm-lsp Server delegates to gc005-wasm4pm-adapter and owns only WASM4PM-* diagnostics.
- dogfood_gc006.rs runs real stdio JSON-RPC LSP protocol flow. No hardcoded /Users/sac paths in template.

## Current Parent
- Conversation ID: e1d0b6c7-19ef-4c59-98d4-4202224bff58
- Updated: not yet

## Key Decisions Made
- None

## Team Roster
| Agent | Type | Work Item | Status | Conv ID |
|-------|------|-----------|--------|---------|
| explorer_gc003_1 | teamwork_preview_explorer | Initial repository exploration | completed | 1b3d0c65-b5f0-4e25-9d74-63bcbc6e6792 |
| explorer_gc003_2 | teamwork_preview_explorer | LSP observer exploration | completed | 14b18676-628d-4553-bbd7-eecd7818e887 |
| explorer_gc003_3 | teamwork_preview_explorer | Proof pack exploration | completed | a8da6c34-1adf-40b8-8488-610528ac239f |
| worker_gc003_1 | teamwork_preview_worker | Workspace check and verification | cancelled | 804f678c-1e88-44bf-9166-b723c33e2ae4 |
| worker_gc004_harness | teamwork_preview_worker | LSP harness implementation | completed | e14cfc05-e2f0-4b2b-91be-96b66c0fe9b8 |
| worker_gc004_admission | teamwork_preview_worker | LSP admission tests implementation | completed | 9d3d39d7-d139-4fab-b462-764c478ddfaa |
| worker_gc004_scanner | teamwork_preview_worker | Anti-bypass scanner implementation | completed | fd4e7934-889e-4acd-b1fd-8c3130bac406 |
| worker_gc005_ocel | teamwork_preview_worker | OCEL logger implementation | cancelled | 5725076e-7b34-4b3f-9ddb-788b4fa69164 |
| worker_gc005_verifier | teamwork_preview_worker | Verifier and adapter implementation | completed | ebf5f330-fba8-47b9-87f7-9562e114958c |
| worker_gc006_1 | teamwork_preview_worker | GC006 Authority Verifier | completed | 21355d80-f940-4653-a968-39f5df8c99b0 |
| explorer_gc006_remediation | teamwork_preview_explorer | Revert dirty changes in wasm4pm | pending | f288bf7e-ba00-432f-b53a-c6c1f31573cc |

## Succession Status
- Succession required: no
- Spawn count: 11 / 16
- Pending subagents: f288bf7e-ba00-432f-b53a-c6c1f31573cc
- Predecessor: none
- Successor: not yet spawned

## Active Timers
- Heartbeat cron: none (cancelled)
- Safety timer: none
- On succession: kill all timers before spawning successor
- On context truncation: run `manage_task(Action="list")` — re-create if missing

## GC006 Operational Status Parameters
- GC006 Status: GC006_ADMITTED_BY_DOGFOOD
- Checkpoint state:
  - GC001-GC004: ADMITTED per prior checkpoint scopes.
  - GC005: CONDITIONALLY_ADMITTED_BY_DOGFOOD (valid only under sealed-authority path: wasm4pm-lsp -> gc005-wasm4pm-adapter -> wasm4pm_algos::gall::check_gall_conformance).
  - GC006: ADMITTED_BY_DOGFOOD.
- GC004 doctrine pressure: ALIVE
- No-fake surface law: ALIVE
- GC003/GC004 task merge: PARTIAL_ALIVE
- Template mapping: PARTIAL_ALIVE
- LSP stdio client setup: IN_PROGRESS
- Heartbeat/liveness claim: UNSUPPORTED until receipted
- GC004_ADMITTED_BY_DOGFOOD: NOT SHOWN
- final verifier validation: PENDING until concrete command output provided
- wasm4pm-lsp dogfood integration: ALIVE

## Artifact Index
- /Users/sac/ggen/.agents/teamwork_preview_orchestrator_gc003/BRIEFING.md — My persistent working memory
- /Users/sac/ggen/.agents/teamwork_preview_orchestrator_gc003/progress.md — Heartbeat and status checkpoint
- /Users/sac/ggen/.agents/teamwork_preview_orchestrator_gc003/PROJECT.md — Global index of milestone decomposition
