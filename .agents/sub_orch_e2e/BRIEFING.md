# BRIEFING — 2026-06-06T13:28:00-07:00

## Mission
Orchestrate the design and implementation of the comprehensive opaque-box test suite for the '80/20 Projection Core and Pack LSPs' mission.

## 🔒 My Identity
- Archetype: Orchestrator
- Roles: orchestrator, user_liaison, human_reporter, successor
- Working directory: /Users/sac/ggen/.agents/sub_orch_e2e
- Original parent: main agent
- Original parent conversation ID: ddecf898-0231-41c8-9b72-4160f9465248

## 🔒 My Workflow
- **Pattern**: Project
- **Scope document**: /Users/sac/ggen/.agents/sub_orch_e2e/SCOPE.md
1. **Decompose**: The scope is divided into sequential milestones: Setup, Tier 1, Tier 2, Tier 3, Tier 4, and publishing TEST_READY.md.
2. **Dispatch & Execute**:
   - **Direct (iteration loop)**: For each milestone, we run the iteration loop: Explorer finds test strategy -> Worker implements and runs tests -> Reviewer verifies correctness and layout compliance -> Forensic Auditor verifies integrity -> Gate check.
3. **On failure** (in this order):
   - Retry: nudge stuck agent or re-send task
   - Replace: spawn fresh agent with partial progress
   - Skip: proceed without (only if non-critical)
   - Redistribute: split stuck agent's remaining work
   - Redesign: re-partition decomposition
   - Escalate: report to parent (sub-orchestrators only, last resort)
4. **Succession**: Self-succeed at 16 spawns. Write soft handoff, spawn successor, terminate timers, exit.
- **Work items**:
  1. Setup Test Layout [pending]
  2. Tier 1 Tests [pending]
  3. Tier 2 Tests [pending]
  4. Tier 3 Tests [pending]
  5. Tier 4 Tests [pending]
  6. Publish test ready [pending]
- Current phase: 1
- Current focus: Setup Test Layout

## 🔒 Key Constraints
- NEVER write, modify, or create source code files directly.
- NEVER run build/test commands yourself — require workers to do so.
- You MAY use file-editing tools ONLY for metadata/state files (.md) in your .agents/ folder.
- Never reuse a subagent after it has delivered its handoff — always spawn fresh.
- CODE_ONLY network mode: No external HTTP/web access.

## Current Parent
- Conversation ID: ddecf898-0231-41c8-9b72-4160f9465248
- Updated: not yet

## Key Decisions Made
- Established initial briefing and workflow parameters.

## Team Roster
| Agent | Type | Work Item | Status | Conv ID |
|-------|------|-----------|--------|---------|
| worker_setup_1 | teamwork_preview_worker | Setup compile and test verification | completed | 2daa8803-6d39-47e4-9c58-ae8db635282a |
| worker_setup_2 | teamwork_preview_worker | Crate compiler and test setup builder | completed | 8898341a-a261-48ea-9f5b-5b672a392018 |
| worker_impl_f3_1 | teamwork_preview_worker | Integration test builder for Feature 3 | completed | 98b62044-1ad5-48b4-a50b-2107f1dc5297 |
| worker_impl_f4_f5_1 | teamwork_preview_worker | LSP Diagnostics & Routing Test Builder | completed | 9533d48d-7462-4468-ba09-4475ff084b21 |
| worker_impl_t3_t4_1 | teamwork_preview_worker | Integration test builder for Tiers 3 & 4 | completed | a53c1f09-6199-41da-9165-abe0cce17f2b |
| worker_publish_1 | teamwork_preview_worker | TEST_READY publisher and verifier | completed | cb697cf3-85da-4114-a649-5e175086d0ad |

## Succession Status
- Succession required: yes
- Spawn count: 6 / 16
- Pending subagents: none
- Predecessor: none
- Successor: not yet spawned

## Active Timers
- Heartbeat cron: none
- Safety timer: none
- On succession: kill all timers before spawning successor
- On context truncation: run `manage_task(Action="list")` — re-create if missing

## Artifact Index
- /Users/sac/ggen/.agents/sub_orch_e2e/SCOPE.md — Milestone tracking
- /Users/sac/ggen/.agents/sub_orch_e2e/progress.md — Liveness and status heartbeat
- /Users/sac/ggen/.agents/sub_orch_e2e/ORIGINAL_REQUEST.md — Verbatim task request
