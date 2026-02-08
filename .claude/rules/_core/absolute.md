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
| **4. Cargo Make Only** | ALWAYS `cargo make` - NEVER direct cargo commands |
| **5. TodoWrite Batch** | ALWAYS 10+ todos in ONE batch |
| **6. Andon Protocol** | STOP THE LINE when signals appear - fix before proceeding |

**Golden Rule**: 1 MESSAGE = ALL RELATED OPERATIONS

Batch everything: TodoWrite (10+ todos), Task tool (ALL agents), File ops (ALL reads/writes), Bash (chain with `&&`), Memory (ALL store/retrieve)
