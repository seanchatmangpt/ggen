# BRIEFING — 2026-06-06T14:20:30-07:00

## Mission
Orchestrate the design, implementation, and verification of the "80/20 Projection Core and Pack LSPs" in ~/ggen and ~/tower-lsp-max.

## 🔒 My Identity
- Archetype: Project Orchestrator
- Roles: orchestrator, user_liaison, human_reporter, successor
- Working directory: /Users/sac/ggen/.agents/teamwork_preview_orchestrator_projection_1
- Original parent: main agent
- Original parent conversation ID: 785da385-c679-460c-85ec-a33e193f9637

## 🔒 My Workflow
- **Pattern**: Project
- **Scope document**: /Users/sac/ggen/PROJECT.md
1. **Decompose**: Break down global mission into E2E testing track and implementation track. Implement implementation track in milestones, each delegated to a sub-orchestrator.
2. **Dispatch & Execute**:
   - **Delegate (sub-orchestrator)**: Spawn sub-orchestrators for milestones, which run their own Explorer -> Worker -> Reviewer loops.
3. **On failure**:
   - Retry: Ask subagent to continue or check status.
   - Replace: Kill and spawn fresh subagent.
   - Skip: Proceed without if non-critical.
   - Redistribute: Split work items.
   - Redesign: Re-partition milestones and interfaces.
4. **Succession**: Self-succeed at 16 spawns. Write handoff.md, spawn successor, cancel timers, exit.
- **Work items**:
  1. Decompose project scope and create PROJECT.md / TEST_INFRA.md [done]
  2. Spawn E2E Testing Track [done]
  3. Spawn Implementation Track [in-progress]
  4. Pass E2E tests and perform coverage hardening [pending]
- **Current phase**: 2
- **Current focus**: Implementation Track execution (currently in M2).

## 🔒 Key Constraints
- NEVER write, modify, or create source code files directly.
- NEVER run build/test commands yourself.
- Use file-editing tools only for metadata/state files in your own .agents/ folder.
- If Forensic Auditor reports integrity violation, milestone fails unconditionally.
- Integrity mode: benchmark.
- Never reuse a subagent after it has delivered its handoff — always spawn fresh.

## Current Parent
- Conversation ID: 785da385-c679-460c-85ec-a33e193f9637
- Updated: not yet

## Key Decisions Made
- Dispatched worker_setup_1 to create PROJECT.md and TEST_INFRA.md.
- Spawned E2E Testing Track Orchestrator and Implementation Track Orchestrator to run in parallel.
- Received E2E handoff showing E2E track is complete and published TEST_READY.md.
- Notified Implementation sub-orchestrator to pull/pass E2E tests when ready.

## Team Roster
| Agent | Type | Work Item | Status | Conv ID |
|-------|------|-----------|--------|---------|
| worker_setup_1 | teamwork_preview_worker | Write PROJECT.md & TEST_INFRA.md | completed | 3d782443-e217-4d65-b323-7e8737996ab1 |
| sub_orch_e2e | self | E2E Testing Track | completed | b1d36f34-8e2e-4d7a-a9da-9e476a670aec |
| sub_orch_impl | self | Implementation Track | in-progress | 4b5478bf-08ac-49b9-81dd-00793a75d992 |

## Succession Status
- Succession required: no
- Spawn count: 3 / 16
- Pending subagents: [4b5478bf-08ac-49b9-81dd-00793a75d992]
- Predecessor: none
- Successor: not yet spawned

## Active Timers
- Heartbeat cron: ddecf898-0231-41c8-9b72-4160f9465248/task-11
- Safety timer: none
- On succession: kill all timers before spawning successor
- On context truncation: run `manage_task(Action="list")` — re-create if missing

## Artifact Index
- /Users/sac/ggen/.agents/teamwork_preview_orchestrator_projection_1/ORIGINAL_REQUEST.md — Original request from parent
- /Users/sac/ggen/.agents/teamwork_preview_orchestrator_projection_1/BRIEFING.md — Persistent memory index
