---
# GGEN-RECEIPT-CHAIN-VERIFY-MISMATCH: `ggen receipt verify` fails on a receipt `ggen sync run` itself wrote

**Discovered:** 2026-08-11
**Discovered by:** autofde-lab's ggen-pack review session (comparing autofde-lab's
`ggen.toml` generation against `~/ggen-marketplace`'s pack conventions)
**ggen version under analysis:** 26.8.8
**Reproducing repo:** `~/autofde-lab` (real project, real `ggen.toml`, 9
`[[generation.rules]]` -- 8 constitution + 1 `k8s-fault-universes`)
**Severity:** SILENT-DATA-LOSS-adjacent (chain-of-custody claim, not code
generation itself, is affected -- see Impact)

---

## BUG-001 — `ggen receipt verify` reports `FM-CHAIN-014` (chain hash mismatch) on the genesis record of `.ggen-v2/receipt-log.jsonl`, written by a prior real (non-dry-run) `ggen sync run` in the same repo

**Reproduction:**

```console
$ cd ~/autofde-lab
$ ~/ggen/target/release/ggen receipt verify --format json
ERROR: CLI execution failed: Command execution failed: validation error:
[FM-CHAIN-014] receipt invalid: chain hash mismatch
(stored 6a28fe3d81babdcd798e92e5b36b0f0c0e41b68a7ad8ff449fd99bc0c9f9c5df,
recomputed ca3902025c74856c86b2f65e0924cadf5d932ec0f1b6aef74b18ad4d34a7b32e)
```

`.ggen-v2/receipt-log.jsonl` has exactly **one** record at the time of this
report: a genesis record (`instruction_id: 0`, `prev_chain_hash_hex` all
zeros, `schema: "ggen-receipt/v2"`, `v2.admission.Recorded` populated with 9
real per-file admission entries matching autofde-lab's `ggen.toml` rules).
This record was written by a real, non-dry-run `ggen sync run` earlier in
the same session that produced the 378-cell `k8s_fault_universes.py` and the
8 constitution modules -- confirmed by content (the `evidence_id`s in
`v2.admission.Recorded` are exactly autofde-lab's 9 real output paths). It
was **not** written by a `--dry-run` invocation:
`crates/ggen-engine/src/sync.rs:1235` (`if !opts.dry_run { ... write_receipt
... }`) already gates the write correctly, confirmed by direct source read
-- ruling out the otherwise-plausible "dry-run has a side effect" hypothesis
before filing this.

**Source-level investigation, not yet root-caused to a single line:**

- Write path (`crates/ggen-engine/src/sync.rs:3316-3339`): constructs a
  `ReceiptRecord` with `chain_hash_hex: String::new()`, calls
  `record.recompute_chain_hash()` (using the record's own already-final
  `payload_hash_hex`/`prev_chain_hash_hex`/`schema`/`v2`/etc.), then assigns
  `record.chain_hash_hex = hex32(&chain)`. By construction this should be
  self-consistent: the exact value written IS the exact value
  `recompute_chain_hash()` produced from the exact same in-memory struct,
  moments before serialization.
- Verify path (`crates/praxis-core/src/receipt_record.rs:167-176`,
  `recompute_chain_hash`): deserializes the persisted JSONL record and
  re-runs the identical `build_admission_frame` /
  `chain_from_frame` / `fold_in_v2_epoch` sequence.
- `fold_in_v2_epoch`'s own doc comment (`receipt_record.rs:191-194`) asserts
  determinism explicitly: *"[`ReceiptEpochV2`] and its transitive fields are
  plain structs/enums/`Vec`s (no hash-map ordering anywhere in the type), so
  `serde_json::to_vec` of the same value always produces the same bytes, and
  recomputing over the same record twice always agrees."* This claim is
  contradicted by the reproduction above: the same value, once round-tripped
  through JSON serialize (write) → deserialize (verify) → re-serialize
  (inside `fold_in_v2_epoch`), does **not** agree.

**Leading hypothesis** (not confirmed against source line-by-line — the next
step for whoever picks this up): the in-memory `ReceiptEpochV2` built by
`ReceiptEpochV2Builder` at write time and the `ReceiptEpochV2` produced by
`serde_json::Deserialize` from the persisted JSONL line are not guaranteed
byte-identical on re-serialization — e.g. a field with a non-canonical
float/number representation (`serde_json` may reserialize `1.0` vs `1` or
similar), a default value populated by the builder but omitted from disk via
`skip_serializing_if` and reconstructed differently on read, or an
`Option`/enum variant whose builder-time and deserialize-time internal
representations differ even though their logical value is identical. Given
`fold_in_v2_epoch` hashes raw `serde_json::to_vec(epoch)` bytes rather than a
canonicalized form, *any* such non-bit-identical-but-logically-equal
round-trip breaks every record forever, immediately, on the very next
verify -- worth checking with a targeted unit test that builds a
`ReceiptEpochV2` via `ReceiptEpochV2Builder`, serializes it, deserializes it
back, and asserts `serde_json::to_vec` of both is byte-identical (this is
exactly the case the existing `receipt_record.rs` unit tests near line 238
do NOT cover -- they hash a hand-constructed `sample()` record directly,
never round-tripping through real JSON serialize/deserialize the way
`write_receipt` → `read_prev_head`/`receipt verify` actually does).

**Impact:** Every real project using `ggen.toml`'s v2-epoch receipt chain
(schema `ggen-receipt/v2`, i.e. every project generated against a recent
`ggen` build) has a receipt log that cannot pass `ggen receipt verify` the
moment it's checked, independent of whether generation itself is correct.
This is a chain-of-custody / provenance-audit defect, not a code-generation
defect -- `ggen sync run --dry-run` and the generated Python output are
unaffected and independently verified correct (see autofde-lab's own
`scripts/verify_ggen_generation.py`, which recomputes generation output
directly from ontology source via `rdflib`, bypassing `ggen` entirely, and
confirms `ALIVE`). But any downstream claim of "receipt-chained, verified"
provenance for a `ggen`-generated artifact is currently false for every
record with a `v2` payload, until this is fixed.

**Workaround (consumer):** None found. `.ggen-v2/` is typically gitignored
(it is in autofde-lab), so this does not block commits -- but it does mean
`ggen receipt verify` cannot currently be relied on as evidence in any
project using v2-epoch receipts.

**Suggested fix:** Either (a) canonicalize `ReceiptEpochV2` before hashing in
`fold_in_v2_epoch` (e.g. via `serde_json::to_value` + a deterministic
re-serialization, or a canonical-JSON crate) so hashing is provably
independent of serde's own round-trip stability, or (b) if the true root
cause is narrower (a specific field), fix that field's `Serialize`/
`Deserialize` implementation so round-trips are byte-identical, and add the
round-trip-hash-stability unit test named above to prevent regression.

---

## Update 2026-08-11 (same day) — narrowed, not yet fixed

Two new regression tests were added to
`crates/praxis-core/src/receipt_record.rs`:

- `recompute_chain_hash_survives_a_real_json_round_trip` (passes): a
  synthetic `ReceiptEpochV2` built via `ReceiptEpochV2Builder` with one real
  `AdmissionItem`, round-tripped through `serde_json::to_string` +
  `from_str`, recomputes identically. This rules out the general "any v2
  epoch's JSON round trip is unstable" hypothesis for the synthetic case.
- `reproduce_the_real_autofde_lab_fm_chain_014_failure` (fails,
  `#[ignore]`d, self-contained — the exact real record embedded as a
  literal string, no external file dependency): deserializes the *actual*
  failing record from autofde-lab's `.ggen-v2/receipt-log.jsonl` and
  recomputes. **Reproduces the exact same two hash values the CLI
  reported** (`stored 6a28fe3d...`, `recomputed ca390202...`), confirmed
  bit-for-bit, directly in a unit test — this is no longer only a CLI-level
  observation.

**The v2 epoch payload and `schema` string are exonerated**: directly
byte-diffed `serde_json::to_vec` of the real record's `v2` field before and
after the round trip — **identical bytes**, confirmed. `fold_in_v2_epoch`'s
contribution to the chain hash is therefore provably not the cause for this
record, correcting this doc's original, less-precise filing.

**The divergence is isolated to the base (pre-v2) chain hash** —
`build_admission_frame` / `chain_from_frame`, driven by `receipt_meta()`'s
reconstruction of `ReceiptMeta` from `instruction_id` (0) / `activity_idx`
(0) / `node_kind` (0) / `ts_ns` (0) / `andon` (`Green`) / `object_ids`
(one entry) / `obligation_count` (0), plus the decoded `payload_hash`/
`prev_chain_hash` (genesis, all-zero prev). All of these are plain
scalars/strings verified to round-trip losslessly through JSON — which is
what makes this genuinely puzzling rather than an obvious field bug.

**Updated leading hypothesis**: `chain_from_frame`
(`crates/praxis-core/src/law.rs:292-297`) constructs a fresh
`OcelCausalReceipt::genesis([0u8; 32])`, sets its `chain_hash` field to the
supplied `prev_chain_hash`, then calls `.chain(frame)` once. If
`OcelCausalReceipt` carries internal state beyond the single 32-byte
`chain_hash` scalar (an object graph / running per-object accumulator
typical of OCEL-object-centric event logs, given `obj_refs`'s presence in
`OcelCausalFrame`), a **live, multi-file `ggen sync run`** — which chains
one frame per real generated file across the whole sync, accumulating that
richer internal state across all of them — would NOT be reproduced by
`recompute_chain_hash`'s single-frame replay from a fresh genesis, even
though both agree on the single published `prev_chain_hash` scalar. This
would explain why the *first* (genesis) record in a fresh chain is exactly
where this fails first, and is the next concrete thing to check: does
`OcelCausalReceipt::chain()`'s output depend on anything in `self` beyond
`chain_hash`, and if so, does `genesis()` initialize that state identically
to whatever the live multi-frame emission path actually threads through?

**Status: root-caused further, not fixed.** The reproduction test above is
committed `#[ignore]`d specifically so `cargo test` stays green while this
tracks as open, real, unresolved work — un-ignore it once a fix lands; it
must then pass.
