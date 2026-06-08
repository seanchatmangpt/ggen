## 2026-06-07T04:53:37Z
You are teamwork_preview_explorer_gc008_3. Your working directory is /Users/sac/ggen/.agents/teamwork_preview_explorer_gc008_3/.
Your mission is to perform a detailed audit of the 4 workspaces: `/Users/sac/ggen`, `/Users/sac/tower-lsp-max`, `/Users/sac/wasm4pm`, and `/Users/sac/wasm4pm-compat`.

Specifically:
1. Scan all 4 workspaces for any occurrences of plain `tower-lsp` or `tower_lsp` in Cargo.toml files, Cargo.lock files, and Rust source imports/usages. Verify if the workspaces are completely clean of plain `tower-lsp` or if any linger.
2. Check the compilation of:
   - ggen-lsp (in `/Users/sac/ggen`)
   - clap-noun-verb-pack-lsp (in `/Users/sac/ggen`)
   - wasm4pm-lsp (in `/Users/sac/wasm4pm`)
   - wasm4pm-compat-lsp (in `/Users/sac/wasm4pm-compat`)
3. Run the full test suite in `/Users/sac/ggen` via:
   `cargo +nightly test -p clap-noun-verb-pack-lsp --all-targets --target-dir /tmp/cargo-target-gc008 -- --test-threads=1`
   Capture the complete list of tests that compile/run, their status (passed/failed), the exact command line used, and the exit code.
4. Scan the source code of `/Users/sac/ggen` for any of the R1/R5 suspicious or poor practices (e.g. diagnostics named DEBUG, prints of raw files/paths, substring checks for authority, fake log messages like "Routing to...", unwraps, etc.).
5. Check if any forbidden patterns from the "Anti-Cheating Static Surveillance" list are present in the active codebases:
   - wasm4pm.bind_receipt
   - bind_conformance_receipt
   - execute_command mutation path
   - WorkspaceEdit receipt binding
   - std::fs::write or tokio::fs::write in LSP mutation path
   - std::fs::read_to_string in receipt-binding path
   - ocel.events.push in adapter
   - manual FIT / ADMITTED returns, v1.0.0 or version = "1.0.0"
   - fake shadow crates (crates/wasm4pm, crates/wasm4pm-lsp, crates/wasm4pm-compat)
6. Verify the CalVer compliance (must not use 1.0.0 or v1.0.0).
7. Document all commands, code search results, file digests, and outputs in `/Users/sac/ggen/.agents/teamwork_preview_explorer_gc008_3/analysis.md`, and write a self-contained handoff.md in your working directory. Send a message when complete.
