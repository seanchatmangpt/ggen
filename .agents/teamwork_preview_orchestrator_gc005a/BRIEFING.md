# BRIEFING — 2026-06-06T17:59:00-07:00

## Mission
Coordinate implementation of GC005A: Sealed wasm4pm Replay Surface Contract, LSP Integration Testing, and Sealed Workspace Sterility Baselines.

## 🔒 My Identity
- Archetype: teamwork_preview_orchestrator
- Roles: orchestrator, user_liaison, human_reporter, successor
- Working directory: /Users/sac/ggen/.agents/teamwork_preview_orchestrator_gc005a/
- Original parent: main agent (from subagent_reminder caller ID "00bdd97d-4363-483b-98f0-b8d243f489d6")
- Original parent conversation ID: 00bdd97d-4363-483b-98f0-b8d243f489d6

## 🔒 My Workflow
- **Pattern**: Project Pattern
- **Scope document**: /Users/sac/ggen/.agents/teamwork_preview_orchestrator_gc005a/PROJECT.md
1. **Decompose**: Decompose request into concrete milestones matching module boundaries and project requirements.
2. **Dispatch & Execute**:
   - **Delegate (sub-orchestrator)**: For large milestones, or run Explorer -> Worker -> Reviewer / Challenger loop for specific subtasks.
3. **On failure** (in this order):
   - Retry: nudge stuck agent or re-send task
   - Replace: spawn fresh agent with partial progress
   - Skip: proceed without (only if non-critical)
   - Redistribute: split stuck agent's remaining work
   - Redesign: re-partition decomposition
   - Escalate: report to parent (sub-orchestrators only, last resort)
4. **Succession**: Spawn successor after 16 subagent spawns are complete.
- **Work items**:
  1. Initialize project and baseline configurations [in-progress]
- **Current phase**: 1
- **Current focus**: Context recovery and initial decomposition

## 🔒 Key Constraints
- NEVER write, modify, or create source code files directly.
- NEVER run build/test commands yourself — require workers to do so.
- File-editing tools only for metadata/state files (.md) in .agents/ folder.
- Zero writes into sealed repos wasm4pm and wasm4pm-compat, assert sterility via baselines.
- Real stdio tower-lsp integration boundary, publishing diagnostics with WASM4PM-* error codes in dogfood_gc005.rs.

## Current Parent
- Conversation ID: 00bdd97d-4363-483b-98f0-b8d243f489d6
- Updated: not yet

## Key Decisions Made
- [TBD]

## Team Roster
| Agent | Type | Work Item | Status | Conv ID |
|-------|------|-----------|--------|---------|
| explorer_gc005a_1 | teamwork_preview_explorer | Investigate tests and codebase | in-progress | e8ce7272-f43e-44df-ac4b-03b97ebc64ff |

## Succession Status
- Succession required: no
- Spawn count: 1 / 16
- Pending subagents: e8ce7272-f43e-44df-ac4b-03b97ebc64ff
- Predecessor: none
- Successor: not yet spawned

## Active Timers
- Heartbeat cron: not started
- Safety timer: none

## Artifact Index
- /Users/sac/ggen/.agents/teamwork_preview_orchestrator_gc005a/ORIGINAL_REQUEST.md — Original User Request
- /Users/sac/ggen/.agents/teamwork_preview_orchestrator_gc005a/BRIEFING.md — BRIEFING index
- /Users/sac/ggen/.agents/teamwork_preview_orchestrator_gc005a/progress.md — Progress heartbeat
