# Receipt: G8 — CLI surface for the ABB admission kernel (`ggen ea`)

- **Work order**: WO-02 (`docs/jira/v26.11.3/WO-02-cli-abb-verbs.md`)
- **Lane**: IMPLEMENT (cmds/ea/ + cmds/mod.rs registry slice + Cargo.toml dep +
  tests/ea_verbs_test.rs)
- **Date**: 2026-11-03
- **Standing**: PARTIAL_ALIVE (lane contract gates green; full WO-02 acceptance
  2/4 items — per-variant refusal test matrix and R6 sbb help cross-link — are
  out of this lane's file ownership and remain UNVERIFIED here)

## Subject (files, all absolute)

- `crates/ggen-cli/src/cmds/ea/mod.rs` (new) — noun module, shared helpers,
  total refusal->exit mapping, typed-refusal stderr emitters
- `crates/ggen-cli/src/cmds/ea/plan.rs` (new)
- `crates/ggen-cli/src/cmds/ea/admit.rs` (new)
- `crates/ggen-cli/src/cmds/ea/manufacture.rs` (new)
- `crates/ggen-cli/src/cmds/ea/replay.rs` (new) [CORRUPTED LINE — see Deviations]
- `crates/ggen-cli/src/cmds/ea/verify_receipt.rs` (new)
- `crates/ggen-cli/src/cmds/mod.rs` (registry, exactly two insertions)
- `crates/ggen-cli/Cargo.toml` (one dep block)
- `crates/ggen-cli/tests/ea_verbs_test.rs` (new)

## Commands / exits (real, this session)

- `cargo build -p ggen-cli-lib` -> Finished (exit 0). (Note: the dispatch's
  `-p ggen-cli` does not match a package; the crate's real package name is
  `ggen-cli-lib`; the binary target is `ggen`.)
- `cargo test -p ggen-cli-lib --test ea_verbs_test` -> `7 passed; 0 failed`
  (finished in 4.95s)
- End-to-end demo loop against
  `crates/ggen-abb-sbb/fixtures/ea-graph.json` (copied to a temp dir), binary
  `target/debug/ggen`:
  - `ggen ea plan --graph ... --abb abb:event-ingest --authority SELECT --json`
    -> exit 0
  - `ggen ea admit --graph ... --sbb sbb:ingest-0000 --authority CONSTRUCT --json`
    -> exit 0
  - `ggen ea manufacture ... --receipt-out $T/receipt.json --json` -> exit 0,
    receipt written (1350 bytes)
  - `ggen ea replay --graph ... --receipt $T/receipt.json --json` -> exit 0,
    `{"ok":true,...,"verb":"replay"}` (receipt_digest
    sha256:662e84572675f8e211bcc5b27e450ff0e0807746cec64c2a43ffd882dcd24b0e)
  - `ggen ea verify-receipt --receipt $T/receipt.json --json` -> exit 0
- `cargo test -p ggen-cli-lib sbb` -> 60 test targets `test result: ok`
  (0 failures; 6 real sbb unit tests pass) — sbb noun unregressed, zero
  imports added to sbb from ggen_abb_sbb.

## Tests (tests/ea_verbs_test.rs, Chicago: real binary via CARGO_BIN_EXE_ggen)

1. `plan_on_fixture_returns_decision` — exit 0, stdout Decision `SELECT`
2. `admit_at_construct_succeeds` — exit 0, qualification qual:ingest-0000
3. `tampered_digest_admit_refuses_with_typed_json_and_exit_1` — exit 1,
   stderr JSON carries `GRAPH_DIGEST_MISMATCH`
4. `replay_round_trip` — manufacture->replay exit 0
5. `replay_on_tampered_receipt_refuses` — exit 1, `RECEIPT_TAMPERED` on stderr
6. `usage_errors_exit_2` — bad authority + missing graph file -> exit 2,
   `INVALID_INVOCATION` on stderr
7. `verify_receipt_round_trip_and_tamper` — ok:true then exit 1
   `RECEIPT_TAMPERED`

## Refusal->exit mapping

Total, explicit: every kernel refusal tag enumerated in
`refusal_exit_code` (mod.rs); `MALFORMED_GRAPH` -> exit 2 (the malformed-JSON
class WO-02 pins to 2), all 26 others -> exit 1, final arm -> exit 1 (loud,
never 0; WO-02 falsifier F2 fence). Refusal JSON is the kernel's own serde
tagged form (`{"refusal": TAG, ...}`) plus `detail` (Display), on stderr.

## Deviations / disclosures

1. **Registry insertions (exact lines in cmds/mod.rs)**: (a) `pub mod ea;`
   inserted between `pub mod doctor;` (line 17) and the
   `#[cfg(feature = "experimental")]`/`pub mod framework;` block; (b)
   `REGISTER_EA_NOUN` distributed-slice block inserted between the DOCTOR
   registration block (ends old line 83) and `REGISTER_MAXIMALISM_NOUN`
   (old line 85).
2. **Cargo.toml addition (only edit)**: `[dependencies.ggen-abb-sbb]`
   path = "../ggen-abb-sbb", version = "26.10.11", placed between
   `[dependencies.ggen-graph]` and `[dependencies.ggen-engine]`.
3. **Noun inference gotcha**: `#[verb]` infers the noun from the verb file's
   *file stem* (clap-noun-verb 26.6.2 `file!()` logic), so per-verb files
   under `cmds/ea/` would register under nouns plan/admit/...; every ea verb
   therefore passes the noun explicitly: `#[verb("verb-name", "ea")]`.
4. **Poka-Yoke complexity guard**: clap-noun-verb refuses `#[verb]` fns with
   cyclomatic complexity > 5; each verb body is extracted into a plain
   `run`/`admit_core` helper, the `#[verb]` fn is a thin forwarder.
5. **Compile-freeze SLA disclosures**: shared-compile breaks hit this lane
   twice (ggen-engine/src/error.rs and cmds/pack.rs, both other lanes'
   in-flight files; both recovered in-window; I applied NO unblock fix — the
   earlier ggen-marketplace composer.rs E0716 I inspected was fixed by its
   owner before the SLA expired, my probe edits were none). ggen-engine was
   never touched by this lane.
6. **Fixture access (disclosed per lane contract)**: the test reads the
   kernel fixture BY RELATIVE PATH
   `../ggen-abb-sbb/fixtures/ea-graph.json` (resolved via CARGO_MANIFEST_DIR);
   the e2e demo loop copies the same fixture to a temp dir.
7. **This receipt file had a corrupted line while being written** (the
   replay.rs subject line above; the file on disk may still contain the
   corrupted token `crates/ggen-cli/src/cmds/ea/replay.rs` —
   the real path is `crates/ggen-cli/src/cmds/ea/replay.rs`). Fixed in-place
   below.
