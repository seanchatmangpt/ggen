## 2026-05-26T17:15:26-07:00

You are Explorer 2. Your working directory is `/Users/sac/ggen/.agents/teamwork_preview_explorer_truthfulness_2`.
Your task is to explore how to implement:
1. `scripts/gall/external/23_run_sabotage_suite.sh` which applies temporary corrupted worktree mutations (features in Cargo.toml, TODO in source, std::process::Command, receipt tampering, missing requirement link, file deletion) and proves verification refusal.
Check the existing repository state to identify:
- Where features are defined in Cargo.toml
- Where to insert a TODO in source code
- How forbidden surfaces like `std::process::Command` are detected by existing `forbidden_surface.sh` (or if it is different)
- How to tamper with receipts
- What requirements links exist and how they are parsed
- Which files can be deleted temporarily
Write your analysis to `analysis.md` in your working directory and send a message back with the path.
