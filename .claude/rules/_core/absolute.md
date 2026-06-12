---
auto_load: true
priority: critical
version: 6.0.0
---

# 🚨 Absolute Rules (Non-Negotiable)

| Rule | Requirement |
|------|-------------|
| **1. Concurrent Operations** | ALL operations MUST be parallel in ONE message |
| **2. No Root Files** | NEVER save files to root - use subdirectories |
| **3. Task Tool Required** | USE Claude Code Task tool for agent execution |
| **4. just Is the Entry Point** | ALWAYS `just <task>` - NEVER direct `cargo make` or bare `cargo` commands |
| **5. TodoWrite Batch** | ALWAYS 10+ todos in ONE batch |
| **6. Andon Protocol** | STOP THE LINE when signals appear - fix before proceeding |
| **7. ggen-lsp Diagnostics** | `<new-diagnostics>` with `GGEN-*` codes = Andon. Do NOT run `ggen sync` while any GGEN-* diagnostic is active |

**Golden Rule**: 1 MESSAGE = ALL RELATED OPERATIONS

Batch everything: TodoWrite (10+ todos), Task tool (ALL agents), File ops (ALL reads/writes), Bash (chain with `&&`), Memory (ALL store/retrieve)
