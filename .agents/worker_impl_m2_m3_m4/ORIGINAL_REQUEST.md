## 2026-06-06T21:01:27Z

You are the worker agent for Milestones 2, 3, and 4 of the Implementation Track.
Your tasks are:

1. Fix the `GgenManifest` compilation errors in `/Users/sac/ggen/crates/ggen-core` by adding `packs: vec![]` to the missing initializers in:
   - `crates/ggen-core/src/codegen/watch.rs`
   - `crates/ggen-core/src/codegen/watch_cache_integration.rs`
   - `crates/ggen-core/src/dflss.rs`
   - `crates/ggen-core/src/lean_six_sigma.rs`
   - `crates/ggen-core/src/manifest/validation.rs`
   - `crates/ggen-core/tests/conditional_execution_tests.rs`
   - `crates/ggen-core/tests/llm_generation_test.rs`
   - `crates/ggen-core/tests/pipeline_edge_cases_test.rs`
   - `crates/ggen-core/tests/values_inline_enforcement_test.rs`

2. Create durable pack directories and metadata files under `/Users/sac/ggen/crates/`:
   - `crates/ggen-pack-clap-noun-verb/pack.toml`
   - `crates/ggen-pack-clap-noun-verb/templates/cli.tmpl` (simple Tera CLI template, e.g. "fn main() { // cli template for {{ app_name }} }")
   - `crates/ggen-pack-tower-lsp-max/pack.toml` (declaring dependency on `ggen-pack-clap-noun-verb` = "^1.0.0")
   - `crates/ggen-pack-tower-lsp-max/templates/lsp.tmpl` (simple Tera LSP template, e.g. "fn start() { // lsp template on port {{ port }} }")

3. Run the generator sync using `sync` function in `ggen-projection` to populate `/Users/sac/tower-lsp-max/examples/clap-noun-verb-lsp/` with the sync marker and maps:
   - `projection-map.json` (mapping target files, e.g. `src/main.rs` to template)
   - `customization-map.json`
   - `receipts.json` (or `receipts.jsonl` containing Blake3 hashes of target files so they are in-sync and won't trigger drift unless modified)
   - `.sync_marker` (containing "sync_active")

4. Add `ggen-projection` dependency to `crates/ggen-lsp/Cargo.toml`:
   - `ggen-projection = { path = "../ggen-projection" }`

5. Implement boundary compliance observer diagnostics (`GGEN-PROJECTED-001`, `GGEN-DRIFT-001`, `GGEN-EVIDENCE-001`, `GGEN-CUSTOMIZE-001`, `GGEN-OVERRIDE-001`, and `GGEN-PROJECT-OPPORTUNITY-001`) in `crates/ggen-lsp/src/state.rs` (or inside `crates/ggen-lsp/src/handlers/diagnostics.rs` and call it from `state.rs` during `analyze_and_observe`).
   - The diagnostics must be published using `self.client.publish_diagnostics` and must contain the correct diagnostic code, message, and `source_id: "ggen_lsp_observer"` inside the `data` field of the diagnostic.

6. Edit `/Users/sac/tower-lsp-max/src/composition.rs`'s diagnostics merge logic (in `textDocument/publishDiagnostics` block, lines ~1600) to strictly enforce the `source_id` contract for upstreams:
   - If `src_id == "ggen-lsp"`, look up `source_id` inside the diagnostic's `data` field. If it is missing (None), reject the merge (i.e. `continue`).

7. Run `cargo test` in both `ggen` and `tower-lsp-max` workspaces and ensure all tests compile and pass 100%!
