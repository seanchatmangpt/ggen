# BRIEFING — 2026-05-27T15:30:15-07:00

## Mission
Verify capability-map compilation/tests and check if open-ontologies command-line tool is installed.

## 🔒 My Identity
- Archetype: Diagnostic Specialist
- Roles: Diagnostic Specialist
- Working directory: /Users/sac/ggen/.agents/worker_diagnostics/
- Original parent: 70792bd0-ab12-427f-90d5-5e928bdf78a6
- Milestone: Diagnostic and compilation verification of capability-map

## 🔒 Key Constraints
- CODE_ONLY network mode. No external network requests.
- No mocks, stubs, placeholders. Do not violate AGENTS.md / GEMINI.md.

## Current Parent
- Conversation ID: 70792bd0-ab12-427f-90d5-5e928bdf78a6
- Updated: not yet

## Task Summary
- **What to build/verify**: Verify target repository `/Users/sac/capability-map` compilation and tests, check if `open-ontologies` command is installed, report to `diagnostics.md`.
- **Success criteria**: Report successfully compiled into `diagnostics.md`, orchestrator notified.
- **Interface contracts**: N/A
- **Code layout**: N/A

## Key Decisions Made
- Use run_command for cargo check, cargo test, and which open-ontologies.
- Generated diagnostics report and handoff report in the working directory.

## Artifact Index
- /Users/sac/ggen/.agents/worker_diagnostics/diagnostics.md — Diagnostic report
- /Users/sac/ggen/.agents/worker_diagnostics/handoff.md — Handoff report
