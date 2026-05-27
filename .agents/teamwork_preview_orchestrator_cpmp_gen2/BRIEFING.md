# BRIEFING — 2026-05-27T15:25:00-07:00

## Mission
Build and finish `capability-map` (`cpmp`) in `/Users/sac/capability-map` leveraging `open-ontologies` as the primary catalog store, ensuring all code, capabilities, patterns, tests, docs, and symbols are discoverable by LLM coding agents.

## 🔒 My Identity
- Archetype: teamwork_preview_orchestrator
- Roles: orchestrator, user_liaison, human_reporter, successor
- Working directory: /Users/sac/ggen/.agents/teamwork_preview_orchestrator_cpmp_gen2
- Original parent: main agent
- Original parent conversation ID: 00b4cd88-55d7-4250-8159-016905f60bf9

## 🔒 My Workflow
- **Pattern**: Project / Canonical / Infinite
- **Scope document**: /Users/sac/ggen/ORIGINAL_REQUEST.md
1. **Decompose**: Check requirements, plan milestones, establish interfaces.
2. **Dispatch & Execute**:
   - **Direct (iteration loop)**: Explorer → Worker → Reviewer → test → gate
   - **Delegate (sub-orchestrator)**: Spawn a sub-orchestrator for compound milestones.
3. **On failure** (in this order):
   - Retry: nudge stuck agent or re-send task
   - Replace: spawn fresh agent with partial progress
   - Skip: proceed without (only if non-critical)
   - Redistribute: split stuck agent's remaining work
   - Redesign: re-partition decomposition
   - Escalate: report to parent (sub-orchestrators only, last resort)
4. **Succession**: Self-succeed at 16 spawns. Write handoff.md, spawn successor.
- **Work items**:
  1. Initial exploration and codebase check [in-progress]
  2. Implement/Stub CLI and Enterprise modules [pending]
  3. Validate & integrate with open-ontologies [pending]
  4. Write docs/enterprise/ documents [pending]
  5. Run sabotage, tests, and compliance validation [pending]
- **Current phase**: 1
- **Current focus**: Exploration and initial gap assessment

## 🔒 Key Constraints
- NEVER write, modify, or create source code files directly.
- NEVER run build/test commands yourself — require workers to do so.
- Banned: private RDF namespaces (e.g. `gall:`, `gg:`, `kh:`, etc.), namespace laundering.
- Use only public vocabularies.
- Strict verification using `open-ontologies`.

## Current Parent
- Conversation ID: 00b4cd88-55d7-4250-8159-016905f60bf9
- Updated: not yet

## Key Decisions Made
- None yet.

## Team Roster
| Agent | Type | Work Item | Status | Conv ID |
|-------|------|-----------|--------|---------|
| explorer_m1 | teamwork_preview_explorer | Audit codebase, tests, and CLI | completed | d32cf043-16fd-4dd7-8e79-08bd0357139c |
| worker_compile_check | teamwork_preview_worker | Verify compilation errors | completed | a4d21670-8dd9-407c-9f79-020bdad4779c |
| worker_git | teamwork_preview_worker | Inspect git status and diff | completed | 188b5c55-206a-4fee-9949-d9f3293908d7 |
| worker_verbose_check | teamwork_preview_worker | Run verbose cargo check | in-progress | 55309d50-64c8-4e69-8d15-2b69a400a215 |

## Succession Status
- Succession required: no
- Spawn count: 4 / 16
- Pending subagents: [55309d50-64c8-4e69-8d15-2b69a400a215]
- Predecessor: none
- Successor: not yet spawned

## Active Timers
- Heartbeat cron: task-29
- Safety timer: none

## Artifact Index
- /Users/sac/ggen/.agents/teamwork_preview_orchestrator_cpmp_gen2/plan.md — Project plan
- /Users/sac/ggen/.agents/teamwork_preview_orchestrator_cpmp_gen2/progress.md — Heartbeat and progress file
- /Users/sac/ggen/.agents/teamwork_preview_orchestrator_cpmp_gen2/handoff.md — Handoff metadata (to be created)
