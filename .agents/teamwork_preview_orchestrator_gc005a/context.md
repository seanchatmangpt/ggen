# Context: GC005A & Workspace Sterility Baselines

## Active Directories
- `/Users/sac/ggen`: mutable workspace
- `/Users/sac/tower-lsp-max`: mutable workspace containing wasm4pm-lsp and gc005-wasm4pm-adapter
- `/Users/sac/wasm4pm`: sealed read-only repository
- `/Users/sac/wasm4pm-compat`: sealed read-only repository

## Key Architectures
- **LSP Integration**: `wasm4pm-lsp` observes `.ocel.json` files and publishes diagnostics using standard JSON-RPC. It delegates conformance evaluation to `gc005-wasm4pm-adapter`'s `analyze_ocel` function.
- **Sealed Conformance API**: `gc005-wasm4pm-adapter` calls `wasm4pm_algos::gall::check_gall_conformance` from the sealed `wasm4pm` repo. It does not perform replay logic itself.
- **Sterility manifest**: `.gc-sealed-baseline` records baseline files, git status, ignored inventory, and is locally git-ignored via `.git/info/exclude` to prevent showing up as dirty.

## Active Subagents
- `worker_gc005a_1` (Conv ID: `3e013416-4449-4f5a-b402-21696bf71f2c`) is applying the fixes and writing baseline manifests.
