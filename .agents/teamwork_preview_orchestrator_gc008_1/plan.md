# Verification Plan — GC008 (CLAP command grammar admission and lawful mutation route)

This document maps out the milestones and tasks to verify GC008B and GC008C.

## Goals
1. Prove `conformance-receipt.bind` is admitted by the actual CLAP noun/verb authority, while `wasm4pm.bind_receipt` and other malformed commands are refused.
2. Prove that a valid `conformance-receipt.bind` intent flows strictly through:
   `CodeAction → PackActionIntent → PackPlan → Staging → MutationGate → Receipt`
   with negative controls verifying that direct command executions or direct file writes are blocked/refused.
3. Perform anti-cheating static surveillance across the 4 workspaces: `/Users/sac/ggen`, `/Users/sac/tower-lsp-max`, `/Users/sac/wasm4pm`, `/Users/sac/wasm4pm-compat`.
4. Output the final report matching the requested YAML/markdown block schema.

## Milestones

### Milestone 1: Setup and Test Suite Fixes
- Add missing `[dev-dependencies]` to `crates/ggen-pack-clap-noun-verb/Cargo.toml` to allow test compilation.
- Verify test compilation and running.

### Milestone 2: Run Verification and Collect Logs/Transcripts
- Execute `cargo test -p ggen-pack-clap-noun-verb -- --nocapture` or target-specific tests.
- Capture the test execution command transcripts and digests.
- Verify that `conformance-receipt.bind` is validated, and malformed commands are rejected.
- Verify the mutation path trace in window/logMessage or other LSP log channels.

### Milestone 3: Anti-Cheating Scan
- Perform grep scanning for forbidden patterns:
  - `wasm4pm.bind_receipt`
  - `bind_conformance_receipt`
  - `execute_command` mutation path
  - `WorkspaceEdit` receipt binding
  - `std::fs::write` in LSP mutation path
  - `tokio::fs::write` in LSP mutation path
  - `std::fs::read_to_string` in receipt-binding path
  - `ocel.events.push` in adapter
  - manual `FIT` return
  - manual `ADMITTED` return
  - `v1.0.0`
  - `version = "1.0.0"`
  - fake shadow crates
- Document scan commands and results.

### Milestone 4: Final Synthesis and Reporting
- Compile the findings into the YAML structure format matching the required schema.
- Report results to the main agent.
