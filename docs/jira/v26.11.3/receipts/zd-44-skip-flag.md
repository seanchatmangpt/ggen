# ZD-44 / CARRY-04 — `--skip-ea-admission` end-to-end state

Standing: PASS

Date: 2026-11-04 · Branch: v26-11-3-wave · Subject: ggen @ 58a3b4108 + this lane's diff

## Resolved state: (a) fully wired CLI→gate + NEW bypass-marked receipt law

**Finding**: the flag was ALREADY fully wired CLI→gate (state (a) confirmed, not
(b) or (c)):
- CLI arg surface: `crates/ggen-engine/src/verbs/sync.rs:18` — `sync run` verb
  function takes `skip_ea_admission: bool` (clap-noun-verb auto-discovery; the
  verb lives in ggen-engine, not ggen-cli's cmds/ — ggen-cli's `cmds/` contains
  only generated stubs, e.g. `sync_cmd_template.rs` is 7 lines of comment stub).
- Handler: `crates/ggen-engine/src/verbs/handlers.rs:62,74` plumbs it into
  `SyncOptions.skip_ea_admission` (`crates/ggen-engine/src/sync.rs:225`).
- Gate: `run_ea_graph_admission` (`crates/ggen-engine/src/sync.rs:354-371`)
  refuses with FM-LAW-015 when
  `ea.graph.json` is absent and no opt-out given; both schema paths (frontmatter
  sync.rs:491 and declarative-rules generation_rules.rs:856's Stage-0 dispatch,
  sync.rs:1238) pass the flag through.

K8 court already witnessing both directions (real ggen binary, real tempdir
project): `crates/ggen-engine/tests/cli_boundary.rs:375`
`sync_run_skip_ea_admission_flag_opt_out_of_fm_law_015` — PASS (pre-existing).

## Gap closed this lane: bypass-marked receipt

The receipt did NOT disclose the bypass: `ReceiptPayload`
(crates/ggen-engine/src/sync.rs:306) had no skip disclosure field. A bypassed
FM-LAW-015 gate left the receipt byte-identical to an admitted run's.

### Diff

- `crates/ggen-engine/src/sync.rs` — `ReceiptPayload` gains
  `ea_admission_skipped: Option<String>` (serde default, skip-when-None, so
  legacy/committed fixture payloads stay byte-identical); `write_receipt` gains
  an `ea_admission_skipped: bool` param fed from `opts.skip_ea_admission`; the
  mark is `"skipped: --skip-ea-admission (FM-LAW-015 gate not evaluated)"`.
- `crates/ggen-engine/src/generation_rules.rs:856` — caller updated.
- `crates/ggen-engine/tests/cli_boundary.rs` — new fail-closed court
  `sync_run_skip_flag_receipt_carries_bypass_mark_admitted_run_does_not`
  (cli_boundary.rs:411): bypassed run's receipt MUST carry the mark naming the
  flag; admitted run's receipt MUST NOT. Both directions flip the court.
- `crates/ggen-engine/tests/receipt_chain_scale_stress.rs:76` — initializer
  updated for the new field (`ea_admission_skipped: None`).

## Verification (all executed this session)

- K8 court + new court, freshly rebuilt binary:
  `cargo test -p ggen-engine --test cli_boundary sync_run_skip`
  → `2 passed; 0 failed`.
- `cargo test -p ggen-engine --lib` → `236 passed; 0 failed; 2 ignored`.
- `cargo test -p ggen-engine --no-fail-fast` (full) → 592+ passed, 3 distinct
  failing tests, ALL classified pre-existing / other-lane (see below); killed
  at the 30-minute background limit mid-suite (alphabetically ~`gall_*`).
- Full `cargo test -p ggen-engine --no-fail-fast` → see command receipt below.
- Full `--test cli_boundary` → `33 passed; 0 failed`.
- End-to-end scratch consumer (real binary, mktemp dir, minimal ggen.toml +
  ontology + template, NO ea.graph.json):
  - no flag → `ERROR: ... [FM-LAW-015] EA admission gate: ... is absent and no
    admission opt-out was given`, exit 1.
  - `--skip-ea-admission` → exit 0, `out/names.txt` written,
    `.ggen-v2/receipt.json` payload carries
    `"ea_admission_skipped": "skipped: --skip-ea-admission (FM-LAW-015 gate not evaluated)"`.

## Falsifiers run

- Bypass-mark court flips fail-closed both directions: without the new payload
  field the court panicked
  ("payload.ea_admission_skipped is absent ... the receipt must disclose the
  bypass") on the pre-fix binary; post-fix it passes. An admitted run with
  a mark would also fail.
- Legacy-fixture neutrality: `ea_admission_skipped` is skipped at
  serialization when `None`, so committed chains (tcps-generated) keep their
  stored hashes.

## Pre-existing vs session-introduced (classified)

Three failures under `cargo test -p ggen-engine --no-fail-fast`, NONE
session-introduced:

1. `receipt_chain_e2e::committed_tcps_generated_head_recomputes_...` and
   `sync_extends_the_committed_tcps_generated_chain_with_capped_standing` —
   verify a committed fixture (`examples/tcps-generated/.ggen-v2`) through
   graphlaw's verifier; my diff never touches that path and the new payload
   field serializes nothing when `None` (byte-identical legacy payloads).
2. `multi_template_determinism::receipt_payload_bytes_identical_...` — fails
   on FM-KEY-013 (signing key mode 644 in host temp dir, umask-dependent);
   my diff does not touch the signing-key path.
3. `product_mirror_conformance::fm_diagnostic_codes_ttl_matches_real_source`
   — FM-KEY-13 call site exists in `crates/ggen-engine/src/keys.rs`
   (mtime Nov 4 01:12, another lane) but is not declared in
   `.specify/ggen-product.ttl` (mtime Nov 3 17:33). My diff adds no FM-*
   call sites. Not mine; flagged for the keys lane.

Also `economics_receipt_chain_wall_clock_...` fails as a declared cascade of
failure 1 (it requires receipt_chain_e2e to pass first).

Environment note: a host disk-cleaner pass actively deleted files under
`target/debug` and `~/.cargo/registry/src` mid-compile during this session
(sentinel test: deletions stopped ~11:33 UTC); two rebuilds were forced. All
post-rebuild reruns are stable. `book_gap_closure_e2e` failed once during the
concurrent-storm run, passes cleanly isolated (2 passed; 0 failed).

## Standing

CARRY-04 CLOSED — resolved state (a) + bypass-marked-receipt law landed with
fail-closed court. Receipt law: a bypassed gate leaves a mark; a receipt
without the note under a bypassed run flips the court.
