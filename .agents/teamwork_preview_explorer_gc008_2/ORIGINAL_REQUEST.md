## 2026-06-07T04:02:16Z
<USER_REQUEST>
You are teamwork_preview_explorer_gc008_2. Your working directory is /Users/sac/ggen/.agents/teamwork_preview_explorer_gc008_2/.
Your mission is to perform a detailed exploration of the new invariants, checkpoints, and dependencies.
Specifically:
1. Scan `/Users/sac/ggen`, `/Users/sac/tower-lsp-max`, `/Users/sac/wasm4pm`, and `/Users/sac/wasm4pm-compat` for any occurrences of plain `tower-lsp` or `tower_lsp` in Cargo.toml files, Rust source files (imports, etc.), and Cargo.lock files.
2. Determine if replacing plain `tower-lsp` with `tower-lsp-max` (or another mechanism) is feasible or if it is currently blocking.
3. Investigate the 15 LSP 3.18 features (LSP318-001 through LSP318-015) in the context of the workspaces: are they supported, refused by law, or blocked? Locate any tests, specifications, or code addressing these features.
4. Check if any forbidden patterns from the "Anti-Cheating Static Surveillance" list are present in the codebases:
   - wasm4pm.bind_receipt
   - bind_conformance_receipt
   - execute_command mutation path
   - WorkspaceEdit receipt binding
   - std::fs::write or tokio::fs::write in LSP mutation path
   - std::fs::read_to_string in receipt-binding path
   - ocel.events.push in adapter
   - manual FIT / ADMITTED returns, v1.0.0 or version = "1.0.0"
   - fake shadow crates (crates/wasm4pm, crates/wasm4pm-lsp, crates/wasm4pm-compat)
5. Document all your findings, command runs, outputs, and status determinations in /Users/sac/ggen/.agents/teamwork_preview_explorer_gc008_2/analysis.md, and write a self-contained handoff.md in your working directory. Send a message when complete.
</USER_REQUEST>
