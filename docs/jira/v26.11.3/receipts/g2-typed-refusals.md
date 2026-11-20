# G2 — Typed kernel refusals (WO-01 item b, RESOLUTIONS R2)

Status: ALIVE (see verification ladder below)
Subject: `/Users/sac/ggen` — crates/ggen-engine, 2026-11-03

## Scope (contract)

- OWNED: `crates/ggen-engine/src/error.rs` (new variant/constructor only),
  `crates/ggen-engine/src/sync.rs` (only the two Stage 2c refusal sites at
  ~1037/~1087).
- NOT touched: `crates/ggen-abb-sbb/**`, main.rs, other error.rs regions, any
  other lane's owned files except as disclosed SLA/collision repairs below.

## Diff

### crates/ggen-engine/src/error.rs

- New variant `AppError::AbbRefusal { code: &'static str, op: String, refusal:
  ggen_abb_sbb::Refusal }`. Display renders `[FM-PACK-015]/[FM-LAW-015]`-shaped
  prefix + kernel Display, so existing `[FM-XXX-NNN]` log scrapers and the
  human text are preserved byte-comparable to the old fm_pack(15)/fm_law(15)
  output shape.
- Constructor `AppError::abb_refusal(code, op, refusal)`; accessors
  `AppError::kernel_refusal()` (typed refusal) and `AppError::abb_refusal_tag()`
  (kernel's serde SCREAMING_SNAKE tag via the `"refusal"` key of the
  internally-tagged serde representation; minimal machine surface — full
  structured fields via serializing `kernel_refusal()`).
- `ggen-engine` Cargo.toml already had `ggen-abb-sbb` as a non-dev dep
  (path+version edge, Cargo.toml:118) — no Cargo change, nothing to disclose.

### crates/ggen-engine/src/sync.rs — the two contracted sites

- Line ~1037: `resolve_sync_order` refusal now
  `AppError::abb_refusal("FM-PACK-015", "Cross-pack dependency graph admission", refusal)`
  (was `fm_pack(15, format!(...))`).
- Line ~1087: `admit` refusal now `AppError::abb_refusal("FM-LAW-015",
  format!("EA SBB admission for `{id}`"), refusal)` (was `fm_law(15, ...)`.
  All other fm_law/fm_pack sites unchanged (parse_graph/validate EA sites left
  as-is per contract scope).

## Tests (new, in error.rs `fm_code_extraction_tests`)

- `abb_refusal_surfaces_kernel_serde_tag`: typed access via `kernel_refusal()`,
  tag == `INSUFFICIENT_AUTHORITY`, full structured serde via
  `serde_json::to_value(kernel_refusal())`.
- `abb_refusal_text_still_human_readable_with_fm_code`: `[FM-PACK-015]` prefix
  + `CYCLIC_PACK_DEPENDENCY: a -> b -> a` kernel Display in the rendered text;
  non-refusal errors (fm_pack) expose `None` for both accessors.

## Verification ladder (real commands, this session)

- `cargo test -p ggen-engine --lib abb_refusal` → 2/2 pass.
- `cargo test -p ggen-engine --lib` → 232 passed; 0 failed (2 ignored).
- `cargo test -p ggen-engine` full-suite: reached 22 consecutive all-green
  test binaries (365+ tests) mid-session on the concurrently-mutated tree.
  The final wide census (`--no-fail-fast`, run while ~40 lanes share one
  target dir) shows only concurrency artifacts: every flake-suspect file
  passes standalone immediately after (frontmatter_fields 14/14,
  gall_checkpoint 11/11, sync_dry_run 1/1, book_gap 2/2, receipt_chain 25/25,
  cli_boundary 29/29, fortune5 4/4, doctor 5/5, cross_pack_matrix 7/7,
  custom_behavior 2/2 — each cited with its command in-session). The one
  census failure that reproduces standalone,
  `product_mirror_conformance::fm_diagnostic_codes_ttl_matches_real_source`,
  flags `FM-PACK-022/023` call sites at `sync.rs:1161-1175` — the concurrent
  composition-plan lane's undeclared codes, NOT this lane's surfaces (this
  lane's `FM-PACK-015`/`FM-LAW-015` strings are pre-declared and unflagged).
  Sccache/target-dir hash races (`libhttp_body-*.rmeta`) caused 4 spurious
  file-level compile aborts under load; each compiled clean on retry.
- `cargo build -p ggen-cli-lib --bin ggen` → Finished (needed by the CLI e2e
  tests; was broken mid-session by another lane's cmds/ea edits, since fixed
  by that lane).

## Disclosed cross-lane collision/SLA repairs (all same class)

The concurrent WO-01 lane landed the fail-closed EA admission gate
(`FM-LAW-015` refusal when a sync root lacks `ea.graph.json`) without updating
test fixtures. Every repair below carries a `Collision repair (lane G2,
disclosed...)` comment in place:

1. `src/sync.rs`: deleted the orphaned duplicate OLD EA block (former lines
   1149-1196) left by the WO-01 lane's new block — it was the compile break
   (unbalanced braces) that froze the shared compile; the new block
   (admit_with_caps, digest pinning) supersedes it.
2. `src/pack.rs`, `src/watch.rs` (lib tests): added minimal valid-but-empty
   `ea.graph.json` to the two test fixtures (pack git-subdir e2e, watch loop
   test).
3. `tests/abb_sbb_datalog_admission_e2e.rs`: `scaffold()` writes the empty
   graph (tests 1-2 overwrite with populated graphs; depgraph-refusal tests
   need the gate satisfied without contributing SBBs). Fixture port
   canonicalization was done by the owning lane, not me.
4. `tests/support/mod.rs` `scaffold_multi_pack`: empty graph (single point for
   composed/autofde/etc.). `scaffold_pack` was fixed identically by lane K2.
5. Same minimal `ea.graph.json` fixture repair, each with the disclosure
   comment: `tests/cli_boundary.rs`, `tests/cli_read_only_invariant_matrix.rs`,
   `tests/config_schema_dispatch_e2e.rs` (x2 fixtures),
   `tests/constitution_obligation_e2e.rs`, `tests/consumer_mode_fixture_only_e2e.rs`,
   `tests/cross_pack_macros_e2e.rs`, `tests/combinatorial_matrix.rs` (x2),
   `tests/doctor_e2e.rs` (scaffold + scaffold_with_pack),
   `tests/receipt_chain_e2e.rs`, `tests/book_gap_closure_e2e.rs`
   (mfw-pcp pack root), `tests/custom_behavior_e2e.rs`,
   `tests/fortune5_architecture_pack_e2e.rs`.
6. `src/envelope.rs` ~253: SLA unblock (compile frozen >10 min by owner's
   E0308s): match on `var.as_str()` instead of `&String` vs `&str` literal —
   disclosed in place.
7. `crates/ggen-marketplace/src/packs_registry/composer.rs` E0716: I prepared
   the owned-stack fix but the owning lane landed it themselves; my edit was a
   no-op. Not my diff.
8. A transient E0277 in `tests/blast_radius_gate_test.rs:124` was another
   lane's in-flight edit, fixed by them mid-session.

## Pre-existing vs session-introduced

- All failures I encountered were session-introduced by CONCURRENT LANES (the
  WO-01 gate rollout) or environmental (missing `ggen` binary), never by my
  diff. My two contracted sites changed only error construction; the refusal
  paths fire identically.
- Kernel canonical-port rule change (colon now non-canonical) broke the
  `unbound_pack_port_refuses_sync` fixture mid-session; root-caused via a
  throwaway kernel probe (deleted), fixed by the owning lane.

## Standing

- Typed-refusal surface: ALIVE (cargo test receipts above, this session).
- The two contracted refusal sites render the kernel Display with the FM code
  prefix and carry the typed refusal + serde tag for machine access.
- Falsifier: serialize any Stage 2c refusal from the two sites and find no
  `refusal` tag under SCREAMING_SNAKE_CASE — would falsify "kernel Refusal
  survives as a typed code, not prose".
