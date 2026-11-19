# Reconcile default authority index (hands-free)
Standing: VERIFIED

Close census GAP: Rust `ggen-sjira` reconcile had no default authority index
(Elixir loads canonical semantic-jira-pack). Embedded canonical index now
loads when `ReconcileOpts.authority` is `None`.

## Identity

- Workstream: GAP2 hands-free Semantic Jira — reconcile default authority
- Kernel: `/Users/sac/ggen/crates/ggen-sjira/src/reconcile.rs` + `authority.rs`
- CLI: `/Users/sac/ggen/crates/ggen-cli/src/cmds/sjira.rs` (`sjira reconcile`)
- Recipe: `just sjira-reconcile` in `/Users/sac/ggen/justfile`
- Date: 2026-11-19
- Authority: fnox-planner-engineer lane (fixture fix + gate re-run)

## Consequence

- `canonical_authority_graph` / `canonical_index` mirror the three pack
  StrategicObjectives + trust roots (IRIs + pin law; digests stamped under
  the Rust JSON digest law — TTL N-Triples digests diverge by design).
- `reconcile` with `authority: None` loads that index (or
  `<pack_dir>/authority.json` when set; corrupt JSON refuses
  `AuthorityIndexUnavailable`).
- CLI `ggen sjira reconcile` works hands-free without `--authority-path`.
- Prior "None → AuthorityIndexUnavailable" court replaced by
  origin-law courts against the canonical index.
- CLI fixture: `valid_order` now carries `path_scope: ["lib/"]` (B3
  non-empty; mirrors `reconcile_test` / frontier sibling) so
  `reconcile_receipt` → `admit_work_order` no longer panics
  `InvalidPathScope`.

## Replay (commands — VERIFIED 2026-11-19)

```sh
cd /Users/sac/ggen
cargo test -p ggen-sjira --test reconcile_test
# EXIT=0 (16 passed)
cargo test -p ggen-cli-lib --test cli_sjira_test reconcile_
# EXIT=0 (2 passed: reconcile_hands_free_with_canonical_pack_origin,
#         reconcile_hands_free_refuses_unknown_origin)
```

## Disclosures

- TTL parse of pack `ontology.ttl` remains deferred; embedded JSON-shaped
  mirror is the Rust default (honest parity on IRIs/pins, not byte digests).
- Frontier still fails closed without `--authority` (out of this gap's scope).

## L03 follow-on (2026-11-19)

CLI `frontier` now loads the same canonical index when `--authority` is absent
(`fd4f99bbd` on preserve/v26.11.16-main). Reconcile courts still EXIT=0 (16+2).
