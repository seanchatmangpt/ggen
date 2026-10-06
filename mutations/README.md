# mutations/ — MU3 lane: replay/determinism mutation testing

Prove the TOCTOU and replay-idempotency laws in `crates/ggen-engine/src/sync.rs`
cannot silently weaken, by running cargo-mutants against the determinism/replay
functions and killing every mutant with test-side kills only (lib/ untouched).

## Recipe

```sh
just mutate-replay
```

Runs two chained cargo-mutants passes over `crates/ggen-engine/src/sync.rs`
(the law's home; see config `mutations/mutants-replay.toml`):

- **Pass A** oracle filter `determinism` —
  `determinism_query_reexecution_e2e`, `determinism_independent_reload_e2e`,
  `multi_template_determinism` (its `determinism`-named tests).
- **Pass B** oracle filter `sync` —
  `multi_template_determinism` (`second_sync_of_multi_template_project_is_fully_unchanged`,
  `receipt_payload_bytes_identical_across_fresh_syncs_of_identical_input`),
  `book_gap_closure_e2e::five_gap_packs_resolve_compose_receipt_and_replay_idempotently`,
  `sync_dry_run_no_mutation_e2e`, `cli_read_only_invariant_matrix`, and every other
  sync-named oracle.

Survivor classification baseline recorded below.

## Naming note (lane brief vs. reality)

The lane brief named `snapshot`/`snapshot_tree` in sync.rs. No functions by those
names exist in lib code; snapshot helpers live test-side only. The replay-idempotency
law in ggen-engine is realized by these sync.rs functions, all under mutation:

- `check_determinism` — second independent render must equal the first (path,
  cardinality, hook, ownership, and output bytes).
- `check_determinism_guard_agrees_false` / `check_determinism_rows_agree_empty` —
  second independent reload of the `when:` guard must agree.
- `hash_file_or_missing` — receipt file-hash binding.
- the sync receipt assembly and Stage-2 independent-recheck path.

## Bound

Each `just mutate-replay` run is wall-clock bounded at 40 minutes per pass
(`timeout 2400` in the recipe) and reports the bound when it trips. With
`--copy-target true` the per-pass fixed cost is ≈8 min target hardlink-copy +
≈11 min build; the residual ≈20 min samples ≈5–8 mutants per pass from the
243 sync.rs mutants. cargo-mutants RESUMES: rerunning the recipe adds to the
same outcome dirs, so repeated passes walk the 243 toward full coverage.
The recorded run tripped its bound after 6 outcomes (see below) — the
zero-survivor baseline is therefore SAMPLED, not exhaustive, until passes
accumulate.

## Baseline (v26.10.5 @ c1c4703a, 2026-10-05)

Recorded bounded sample (`mutations/outcome-replay-sync/`, filter `sync`):

- generated: 243 mutants total in `crates/ggen-engine/src/sync.rs`;
- tested: 6 (bounded sample, shuffled order);
- caught: 5 (`new_graph_engine`×1 unviable, `read_ontology_file`×4,
  `*`→`+` arithmetic mutant @ sync.rs:164);
- MISSED (survivors): **0**;
- replay/determinism function coverage in sample: partial (shuffled).

Classification: no survivors, so no equivalent/REAL-GAP split this pass and
no test strengthening was forced. The killing oracles exercised were the
`sync::tests::*` unit tests (`guard_agrees_false_*`, `rows_agree_empty_*`),
`multi_template_determinism` (receipt bytes/second-sync byte-identity),
`sync_dry_run_no_mutation_e2e`, and `cli_read_only_invariant_matrix`'s
byte+mtime fingerprint matrix.

## Known environment quirk (root-caused)

Under cargo-mutants' copied tree, running the FULL ggen-engine suite as baseline
failed `book_gap_closure_e2e` (byte-identity) and `cli_read_only_invariant_matrix`
(read-only invariant): the copied tree has no `target/debug/ggen` (cargo-mutants
builds only `-p ggen-engine`), so `chicago_tdd_tools`' binary fallback walked up
to PATH and spawned the installed `ggen` **26.9.28** (`~/.local/bin/ggen` →
homebrew), which predates the telemetry redirect in
`crates/ggen-cli/src/lib.rs:116-136` and writes `.clap-noun-verb/{ocel.json,
receipts.jsonl}` into the consumer project cwd — breaking byte-identity.
Both binaries pass on the real tree with the tree's own build. Fix applied:
`--copy-target true` so the copy carries the current `target/debug/ggen`.
Do not drop it. (A stale installed ggen on PATH silently poisons any
ggen-engine CLI-spawning test run in a copied tree.)
