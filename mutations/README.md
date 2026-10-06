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

Each `just mutate-replay` run is wall-clock bounded at 40 minutes
(`timeout 2400` in the recipe) and reports the bound when it trips.

## Baseline (v26.10.5 @ c1c4703a, 2026-10-05)

Recorded from the recorded outcome in `mutations/outcome-*.log`:

<!-- MU3-RESULTS -->

## Known environment quirk

Under cargo-mutants' copied tree, running the FULL ggen-engine suite as baseline
fails `book_gap_closure_e2e::five_gap_packs_resolve_compose_receipt_and_replay_idempotently`
(`.clap-noun-verb/ocel.json` / `receipts.jsonl` byte-identity) via cross-binary
interference in the copy; it passes on the real tree (54s). The scoped name filters
above avoid that interference; do not remove them without re-verifying the baseline.
