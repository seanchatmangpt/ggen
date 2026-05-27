# BRIEFING — 2026-05-27T12:35:00-07:00

## Mission
Build and finish capability-map (cpmp) in /Users/sac/capability-map leveraging open-ontologies as the primary catalog store.

## 🔒 My Identity
- Archetype: teamwork_preview_orchestrator
- Roles: orchestrator, user_liaison, human_reporter, successor
- Working directory: /Users/sac/ggen/.agents/teamwork_preview_orchestrator_cpmp/
- Original parent: main agent
- Original parent conversation ID: 00b4cd88-55d7-4250-8159-016905f60bf9

## 🔒 My Workflow
- **Pattern**: Project
- **Scope document**: /Users/sac/ggen/.agents/teamwork_preview_orchestrator_cpmp/plan.md
1. **Decompose**: Decompose the task into milestones.
2. **Dispatch & Execute**:
   - **Direct (iteration loop)**: Explorer → Worker → Reviewer → test → gate
   - **Delegate (sub-orchestrator)**: For large milestones, spawn sub-orchestrator.
3. **On failure** (in this order):
   - Retry: nudge stuck agent or re-send task
   - Replace: spawn fresh agent with partial progress
   - Skip: proceed without (only if non-critical)
   - Redistribute: split stuck agent's remaining work
   - Redesign: re-partition decomposition
   - Escalate: report to parent (sub-orchestrators only, last resort)
4. **Succession**: Self-succeed at 16 spawns, write handoff.md, spawn successor.
- **Work items**:
  1. Project Assessment & Planning [pending]
  2. Implement R1 - R4 requirements [pending]
  3. Integration & Testing [pending]
- **Current phase**: 1
- **Current focus**: Project Assessment & Planning

## 🔒 Key Constraints
- Never write, modify, or create source code files directly.
- Never run build/test commands yourself — require workers to do so.
- Leverage open-ontologies as the primary catalog store.
- Never reuse a subagent after it has delivered its handoff — always spawn fresh.

## Current Parent
- Conversation ID: 00b4cd88-55d7-4250-8159-016905f60bf9
- Updated: not yet

## Key Decisions Made
- [TBD]

## Team Roster
| Agent | Type | Work Item | Status | Conv ID |
|-------|------|-----------|--------|---------|
| Explorer (Assessment) | teamwork_preview_explorer | Investigate codebase | in-progress | 1ebd7863-7f6b-4fe6-9e44-379089ef4ea1 |

## Succession Status
- Succession required: no
- Spawn count: 1 / 16
- Pending subagents: [1ebd7863-7f6b-4fe6-9e44-379089ef4ea1]
- Predecessor: none
- Successor: not yet spawned

## Active Timers
- Heartbeat cron: not started
- Safety timer: none
- On succession: kill all timers before spawning successor
- On context truncation: run manage_task(Action="list") — re-create if missing

## Artifact Index
- /Users/sac/ggen/.agents/teamwork_preview_orchestrator_cpmp/original_prompt.md — Verbatim original user request
- /Users/sac/ggen/.agents/teamwork_preview_orchestrator_cpmp/BRIEFING.md — Current working memory
- /Users/sac/ggen/.agents/teamwork_preview_orchestrator_cpmp/progress.md — Liveness and status heartbeat
- /Users/sac/ggen/.agents/teamwork_preview_orchestrator_cpmp/plan.md — Project and milestones plan
