# Context — GC003 Continuation & wasm4pm Remediation

## Workspace Locations
- Primary Workspace: `/Users/sac/ggen`
- Sibling Sealed Workspaces:
  - `/Users/sac/wasm4pm`
  - `/Users/sac/wasm4pm-compat`
- Current working directory: `/Users/sac/ggen/.agents/teamwork_preview_orchestrator_gc003_gen2/`
- Previous working directory: `/Users/sac/ggen/.agents/teamwork_preview_orchestrator_gc003/`

## Context Retrieval Details
- The previous orchestrator was GC003/GC006. It went stale, leaving some subtasks incomplete.
- The `wasm4pm` workspace is currently dirty. We must restore it to be clean and conform to its `.gc-sealed-baseline` baseline.
- `Cargo.toml` in `wasm4pm` is modified (unpermitted).
- `crates/wasm4pm-lsp/` is untracked.
- The baseline `.gc-sealed-baseline` expects `Cargo.lock`, `crates/wasm4pm-algos/Cargo.toml`, and `crates/wasm4pm-algos/src/gall.rs` to have modifications matching their baseline state.
