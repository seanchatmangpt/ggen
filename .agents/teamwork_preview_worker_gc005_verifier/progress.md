# Progress Tracker

Last visited: 2026-06-07T00:52:00Z

- [x] Task 1: Implement or align `wasm4pm-lsp` to act as a process-evidence observer only, owning only `WASM4PM-*` diagnostics. Delegates evaluation to `gc005-wasm4pm-adapter`.
- [x] Task 2: Implement `gc005-wasm4pm-adapter` to call sealed read-only authorities at `~/wasm4pm` and `~/wasm4pm-compat` and translate outputs into verdicts: `FIT | DEVIATION | BLOCKED | INCONCLUSIVE`. Do NOT perform replay/conformance itself inside local adapter logic. If sealed authority is unavailable, emit BLOCKED.
- [x] Task 3: Ensure the projection engine (`sync_target`) projects the protocol-only proof test `dogfood_gc005.rs` from `dogfood_gc005.rs.tmpl` template in `ggen-pack-gall-checkpoint-proof` using stdio LSP wire flow.
- [x] Task 4: Verify the required diagnostics: FIT fixture -> `WASM4PM-VERDICT-FIT`, Missing boundary fixture -> `WASM4PM-CONFORMANCE-BLOCKED`, Artifact-before-staging -> `WASM4PM-REPLAY-DEVIATION`, Broken receipt chain -> `WASM4PM-DIGEST-CHAIN-BROKEN` or `WASM4PM-CONFORMANCE-BLOCKED`.
- [x] Task 5: Compile and run check/tests. Write changes.md and handoff.md in your working directory.
