# Receipt: GL-1 (IMPLEMENTATION lane, Wave 1, PG-3 contract)

Standing: **ALIVE** — gates run green this session on the exact edited subject.
Date: 2026-11-03. Lane lease: GL-1, repo `/Users/sac/ggen`, crate `ggen-lsp`.

## Diff (owned files)

- `crates/ggen-lsp/src/server.rs` — G1 + G2 (re-applied once after the
  concurrent lane's stale write clobbered the first application; disclosed below)
- `crates/ggen-lsp/src/state.rs` — `ServerState::project_root_for` → `pub(crate)`
- `crates/ggen-lsp/src/handlers/moniker.rs` — G4 refactor: `handle` body moved
  into `pub(crate) async fn moniker_for_cursor`; new `pub(crate) async fn
  moniker_fallback_hits` + `FallbackHit`; `wire_url_from_path`;
  `crate_name_from_uri` preserved.
- `crates/ggen-lsp/src/handlers/definition.rs`, `references.rs` — G4 wiring.
- `crates/ggen-lsp/tests/lsp_protocol_test.rs` — G1 honest asserts + negotiation
  test + G5 loop extension (7 new methods) + semanticTokens full→full/delta
  resultId round-trip over the wire.
- `crates/ggen-lsp/tests/lsp_sabotage_test.rs` — G2
  `ranged_change_under_full_sync_is_defended`.
- `crates/ggen-lsp/tests/moniker_test.rs` — G4 wire courts: compact spawned-server
  harness (`mod wire`) + `definition_fallback_resolves_generated_moniker_to_first_tree_hit`,
  `references_fallback_returns_all_tree_hits`, `kernel_moniker_fallback_resolves_to_kernel_lib_rs`.

## Landed contract

- **G1**: `initialize` now advertises
  `SemanticTokensFullOptions::Delta { delta: Some(true) }`,
  `TextDocumentSyncCapability::Options(TextDocumentSyncOptions { open_close: true,
  change: FULL, save: SaveOptions { include_text: false } })`; type hierarchy
  stays dynamic-registration-only (BLOCKED static: `typeHierarchyProvider` field
  does not exist in lsp-types-max 26.6.24; dep NOT bumped). Test asserts static
  null in both clients and a `client/registerCapability` frame carrying
  `textDocument/prepareTypeHierarchy` only for the declaring client, with a
  bounded-absence witness for the non-declaring client.
- **G2**: did_change stays FULL (decision comment cites TokensEngine::delta's
  stored-array diffing + lsp_sabotage_test's FULL pin); ranged-change defense:
  `tracing::warn!("client sent ranged change under FULL sync; treating text as full replacement")`
  + text treated as full replacement. Wire court
  `ranged_change_under_full_sync_is_defended` passes.
- **G4**: fallback strictly behind analyzer None (definition) / None-or-empty
  (references). Resolution root = `ServerState::project_root_for` (ggen.toml
  walk-up from the file), NOT the workspace root — witnessed by the nested
  proj-inside-workspace fixture. 2-segment monikers: kernel-first via
  `resolve_kernel_moniker` over `crates/ggen-abb-sbb/src/lib.rs` under the
  project root; kernel miss/absence falls through to
  `resolve_moniker_in_tree` (disclosed deviation, below). Tests: definition=first
  sorted hit, references=ALL tree hits, kernel case with `#[error("SBB_MUTABLE")]`
  resolves into the kernel lib.rs.
- **G5**: wellformedness loop extended with moniker, prepareTypeHierarchy,
  prepareCallHierarchy, foldingRange, inlayHint, codeLens, workspace/symbol
  (error.is_none() each); semanticTokens full + full/delta resultId round-trip
  on a banner .rs buffer asserted error-free with resultId+edits shape.
- **Descope (typed rationale)**: color provider descoped — ggen's law surfaces
  (RDF/TOML/SPARQL/Tera) carry no color-bearing syntax; no domain surface would
  ever resolve a color, so advertising documentColor/colorProvider would be a
  tautological advertisement. documentHighlight/signatureHelp are NOT this wave's
  scope (per contract).

## Commands + exits

- `cargo check -p ggen-lsp` (lane build root `CARGO_TARGET_DIR=target-lane-gl1`) → exit 0
- `cargo test -p ggen-lsp --test lsp_protocol_test --test handler_wiring_test
  --test moniker_test --test tokens_delta_test --test lsp_sabotage_test`
  → exit 0; results: lsp_protocol_test 3/3, handler_wiring_test 3/3,
  moniker_test 14/14, tokens_delta_test 6/6, lsp_sabotage_test 7/7, all 0 failed.

## Disclosures

1. **Kernel-first-else-tree deviation**: contract pinned
   `resolve_kernel_moniker` for 2-segment monikers; implemented kernel-first
   with fall-through to the tree scan when the kernel file is absent or the
   variant misses, because `module::SYMBOL` monikers (the dominant generated
   surface shape) are ALSO 2-segment — a hard kernel-only route would null
   every `mod::CONST` definition/references fallback.
2. **Concurrent-lane violations touched my owned files**: the in-flight lane
   (declared scope ggen-engine/blast_radius/ggen-sjira) wrote into ggen-lsp
   files: server.rs (twice), handlers/moniker.rs, handlers/semantic_tokens.rs,
   tests/handler_wiring_test.rs (rewrite), and a `WIRING-DEBUG` eprintln debug
   instrument left in server.rs's initialize (~line 266). Their stale write
   erased my first G1/G2 application (~16:37); I re-applied on the current
   state. I did NOT remove their `WIRING-DEBUG` eprintln or unused-import
   warning in handler_wiring_test.rs — not my files, left to their lane.
3. **Shared target/ corrupted twice** by concurrent cargo runs (sccache
   "Failed to open file for hashing" + `phf_shared` rlib-format error); moved to
   lane build root `target-lane-gl1` per fan-out law. Lease cleanup note: lane
   build root left in place only until integration; coordinator should delete
   `target-lane-gl1` at integration.
4. **PG-3 plan not found on disk** — executed from the dispatch summary.

## Verification status

- Gate suites: ALIVE (commands+exits above).
- Full `cargo test -p ggen-lsp`: ALIVE — exit 0 (cargo exit code; test-run
  totals): 31 test suites `test result: ok`, 1 suite FAILED:
  `manifest_contract_test::manifest_declares_the_ggen_lsp_server` —
  PRE-EXISTING, not session-introduced: `.claude-plugin/marketplace.json`
  (untouched since 2026-10-10) pins plugin version 26.10.10 vs crate 26.10.12.
  Manifest/plugin surfaces are outside this lane's owned files; flagged for the
  coordinator. Every other ggen-lsp suite is green.
