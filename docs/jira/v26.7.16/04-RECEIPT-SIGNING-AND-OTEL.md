# Receipt Signing and OTEL

Part of [00-OVERVIEW](00-OVERVIEW.md) — Phase 2, depends on
[01-PUBLISH-SAFETY-AND-CRATE-RENAME](01-PUBLISH-SAFETY-AND-CRATE-RENAME.md).

## File reference table

| Path | LOC | Relevant lines |
|---|---:|---|
| `/Users/sac/ggen/crates/ggen-core/src/agent/receipt.rs` | 300 | `emit_install_receipt` 76-171; `PackInstallClosure` struct 44-58; `read_artifact_bytes` 178-195; `save_keypair`/`load_keypair` 197-238 (`load_keypair` 198-222, `save_keypair` 224-238; `hex::encode` calls at 229, 233) |
| `/Users/sac/ggen/crates/ggen-core/src/pipeline_engine/pipeline.rs` | 777 | `pipeline.load` span 421-426, `.record()` 439; `pipeline.extract` span 449-454, `.record()` 467; `pipeline.generate` span 482-487, `.record()` 501; `pipeline.validate` span 511-516, `.record()` 529; `pipeline.emit` span 545-550, `.record()` 664; hardcoded-empty `files_generated: vec![],` at line 604 |
| `/Users/sac/praxis/crates/praxis-core/src/receipt_record.rs` | 186 | `pub struct ReceiptRecord {` at line 27; `payload_hash_hex` at 55; `prev_chain_hash_hex` at 57; `chain_hash_hex` at 59; `recompute_chain_hash()` at line 121 |
| `/Users/sac/praxis/crates/praxis-core/src/law.rs` | 698 | `LawObject.signature: Option<Vec<u8>>` field at line 122; `#[cfg(feature = "signed")]` signature-set block 369-372 (in `receipt()`); `receipt_with_record()` 394-453 (record literal built ~423-437, no `signature` field threaded through) |
| `/Users/sac/praxis/crates/praxis-core/src/signing.rs` | 218 | `sign_chain_hash_with_key` fn at line 43 (doc comment from 38), closes line 51; `verify_chain_hash_with_key` fn at line 73 (doc comment from 66), closes line 81 |
| `/Users/sac/praxis/crates/praxis-core/Cargo.toml` | — | `signed = ["chatman-common/signed-receipts"]` feature at line 11 |
| `/Users/sac/ggen/crates/ggen-core/src/receipt/receipt_impl.rs` | 362 | local `Receipt` copy `agent/receipt.rs` actually resolves to (not the `ggen_config` re-export) |
| `/Users/sac/ggen/crates/ggen-config/src/receipt/receipt_impl.rs` | 352 | the copy the ported logic should point at instead |

## Gaps this ticket closes

- **Cryptographic receipt *signing*** — praxis's chain has zero signature (pure hash chain;
  anyone who can write to disk can forge a self-consistent chain from a fresh genesis). This
  ticket layers ggen-core's one genuinely rigorous signed path onto praxis's fail-closed
  chain/log discipline — best-of-both, not a straight swap either way.
- **OTEL pipeline spans** — praxis has zero tracing instrumentation, and ggen-core's own
  spans have a real, confirmed bug (below).
- **Receipt-shape migration**: `ggen-config`'s `Receipt`/`ReceiptChain`/`ProvenanceEnvelope`
  (what `ggen-cli`'s `receipt_manager.rs`, `cmds/{sync,receipt,packs_receipt,inverse_sync}.rs`
  actually call) has no drop-in equivalent shape in `praxis-core`'s `ReceiptRecord` —
  different fields, different key-management model. The two receipt-signing designs (below)
  must be reconciled as part of this work, not treated as independent. Budget this as real
  migration work, not a rename.

## Ed25519 signature layer over the BLAKE3 chain — concrete design

`/Users/sac/praxis/crates/praxis-core/src/law.rs`'s chain is unsigned by default today, and
even with its `signed` feature on (`/Users/sac/praxis/crates/praxis-core/Cargo.toml:11`,
`signed = ["chatman-common/signed-receipts"]`), the signature set on the in-memory
`LawObject` (`law.rs:369-372`, field defined `law.rs:122`) is **never copied into the
persisted `ReceiptRecord`** that the JSONL ledger actually stores and replays
(`law.rs:394-453` builds the record without threading the signature through — the struct
literal at approximately lines 423-437 has no `signature` field) — a real, pre-existing gap
this design closes, not a hypothetical one.

`praxis-core` already ships the needed primitive in the right shape:
`praxis_core::signing::sign_chain_hash_with_key` (`signing.rs:43-51`) /
`verify_chain_hash_with_key` (`signing.rs:73-81`) take a raw 32-byte hash plus a hex-encoded
signing/verifying key string — exactly the format ggen-core's `save_keypair`/`load_keypair`
already produce (`/Users/sac/ggen/crates/ggen-core/src/agent/receipt.rs:229,233`,
`hex::encode(signing_key.to_bytes())`). The two designs slot together directly:

```rust
// /Users/sac/praxis/crates/praxis-core/src/receipt_record.rs -- add one field
// to the existing struct (defined at line 27; fields at 55/57/59 unchanged)
pub struct ReceiptRecord {
    // ...version, instruction_id, activity_idx, activity, node_kind, ts_ns,
    //    duration_ms, payload_hash_hex, prev_chain_hash_hex, chain_hash_hex,
    //    andon, obligation_count, object_ids -- all unchanged.

    /// Ed25519 signature over the raw 32 bytes of `chain_hash_hex`, hex-encoded.
    /// `None` for a record written before signing was enabled -- an honest
    /// "unsigned" state, never conflated with "verified".
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub signature_hex: Option<String>,
}

impl ReceiptRecord {
    // recompute_chain_hash() (line 121) unchanged -- stays the pure hash-chain
    // integrity check, no key material involved.

    /// Step 2: authenticity. Callers MUST call this only after
    /// recompute_chain_hash() confirms the record wasn't tampered with -- a
    /// valid signature over a hash that doesn't match the record's own
    /// fields proves nothing.
    pub fn verify_signature(&self, verifying_key_hex: &str) -> Result<(), CoreError> {
        let sig_hex = self.signature_hex.as_deref().ok_or(CoreError::SignatureInvalid)?;
        let sig_bytes = hex::decode(sig_hex)
            .map_err(|e| CoreError::HexDecodeFailed(e.to_string()))?;
        crate::signing::verify_chain_hash_with_key(&self.chain_hash()?, &sig_bytes, verifying_key_hex)
    }
}
```

```rust
// <engine-crate>/src/receipt_signing.rs -- ports
// /Users/sac/ggen/crates/ggen-core/src/agent/receipt.rs's load_keypair (198-222)
// and save_keypair (224-238) verbatim, keeping the same on-disk convention:
// <root>/.ggen/keys/{private,public}.pem (note: ".pem" is a misnomer
// inherited from ggen-core -- it is hex, not PEM/ASN.1; fix or at least
// document this, don't silently perpetuate it).

pub enum VerifyOutcome { Valid, ChainBroken, SignatureInvalid, Unsigned }

/// `ggen receipt verify` entry point -- two-step, fail-closed.
pub fn verify_receipt_record(root: &Path, record: &ReceiptRecord) -> Result<VerifyOutcome, CoreError> {
    // Step 1 -- hash-chain integrity (existing praxis-core logic, unmodified).
    let recomputed = record.recompute_chain_hash()?;
    if hex::encode(recomputed) != record.chain_hash_hex {
        return Ok(VerifyOutcome::ChainBroken);
    }
    // Step 2 -- signature authenticity, against the project's known public key.
    let verifying_key_hex = read_hex_keyfile(&root.join(".ggen/keys/public.pem"))?;
    match record.verify_signature(&verifying_key_hex) {
        Ok(()) => Ok(VerifyOutcome::Valid),
        Err(CoreError::SignatureInvalid) if record.signature_hex.is_none() => Ok(VerifyOutcome::Unsigned),
        Err(_) => Ok(VerifyOutcome::SignatureInvalid),
    }
}
```

## Receipt signing — what to lift from ggen-core

`emit_install_receipt()` (`/Users/sac/ggen/crates/ggen-core/src/agent/receipt.rs:76-171`)
refuses empty digests; loads/generates a persisted Ed25519 keypair under
`<root>/.ggen/keys/{private,public}.pem`; builds `input_hashes`/`output_hashes` from a
`PackInstallClosure` (struct, lines 44-58) — actuator identity, `pack:id@version:digest`,
artifact bytes hashed via SHA-256 with an explicit `MISSING` sentinel for unreadable paths
(`read_artifact_bytes`, lines 178-195). Do **not** lift `Receipt::sign`/`verify`/
`generate_keypair` wholesale — that duplicates signing logic praxis-core already owns in a
stronger, chain-hash-anchored form (above).

**Concrete action:**
- Lift only the domain-specific closure-construction logic:
  `/Users/sac/ggen/crates/ggen-core/src/agent/receipt.rs`'s `PackInstallClosure` (lines
  44-58) and the `input_hashes`/`output_hashes` construction including the
  `MISSING`-sentinel helper (lines 178-195) — ggen-specific knowledge praxis-core has no
  equivalent for. (This logic moves alongside the install code in
  [06-MARKETPLACE-PACK-REGISTRY-MERGE](06-MARKETPLACE-PACK-REGISTRY-MERGE.md).)
- Replace the hash-then-sign step: hash the joined input/output hashes into a
  `payload_hash_hex`, build a `ReceiptRecord` (per `receipt_record.rs:27`) to get
  `chain_hash_hex` (field at line 59), then call
  `praxis_core::signing::sign_chain_hash_with_key(&chain_hash_bytes, &signing_key_hex)`
  (`signing.rs:43-51`) in place of `Receipt::new(...).sign(&signing_key)`.
- **Decide and document explicitly** (this is a behavior change, not a detail): keep
  ggen-core's zero-config auto-generate-and-persist file keypair
  (`agent/receipt.rs:198-238`) feeding `sign_chain_hash_with_key`, versus adopting
  praxis-core's fail-closed `PRAXIS_SIGNING_KEY` env-var model, which would newly require
  operators to set an env var for `ggen pack install` to succeed.
- Enable `praxis-core`'s `signed` feature (`praxis-core/Cargo.toml:11`, and
  `chatman-common/signed-receipts`) in whatever crate hosts the ported logic — it is off by
  default.
- The `Receipt`/`ReceiptChain` implementation is itself already forked between
  `/Users/sac/ggen/crates/ggen-core/src/receipt/receipt_impl.rs` (362 lines) and
  `/Users/sac/ggen/crates/ggen-config/src/receipt/receipt_impl.rs` (352 lines) —
  near-identical but diverged copies (confirmed via `diff` in prior research) —
  `agent/receipt.rs`'s `use crate::receipt::{hash_data, Receipt}` resolves to the **local
  ggen-core copy**, not the `ggen_config` re-export at `ggen-core/src/lib.rs:257-262`. Point
  the ported logic at `ggen_config::receipt::Receipt`
  (`/Users/sac/ggen/crates/ggen-config/src/receipt/receipt_impl.rs`) explicitly, not the
  copy being retired.

## OTEL span bug — confirmed, exact fix

`/Users/sac/ggen/crates/ggen-core/src/pipeline_engine/pipeline.rs` (777 lines total) creates
all five `pipeline.*` spans with `tracing::info_span!` field lists declaring only
`operation.name`, `operation.type`, `pipeline.stage`. Example (`pipeline.load`, lines
421-426):

```rust
let span = tracing::info_span!(
    "pipeline.load",
    "operation.name" = "pipeline.load",
    "operation.type" = "pipeline",
    "pipeline.stage" = "mu1",
);
```

Line 439: `tracing::Span::current().record("pipeline.duration_ms", elapsed.as_millis() as
u64);` — `pipeline.duration_ms` was never declared in the macro's field list (not even as
`tracing::field::Empty`), so per `tracing`'s documented contract, `record()` on an undeclared
field is a silent no-op. The same pattern repeats identically for all five stages: `extract`
span 449-454, record 467; `generate` span 482-487, record 501; `validate` span 511-516,
record 529; `emit` span 545-550, record 664. `pipeline.files_generated` (required by
`/Users/sac/ggen/.claude/rules/otel-validation.md`) does not exist as a span field anywhere
in this file — the one `files_generated` hit is at line 604, an unrelated hardcoded-empty
`PackProvenance` struct field (`files_generated: vec![],`).

**Concrete action:**
- Add `"pipeline.duration_ms" = tracing::field::Empty,` to each of the 5 `info_span!` field
  lists (lines 421-426, 449-454, 482-487, 511-516, 545-550) when porting — the one-line fix
  that makes every existing `.record()` call actually take effect.
- Add `"pipeline.files_generated" = tracing::field::Empty` to at least the `pipeline.emit`
  span (line 545) and record it from `self.generated_files.len()`, which is already
  populated nearby — this attribute is currently emitted nowhere, not just broken.
- Add a regression test (or `RUST_LOG=trace` capture) asserting the recorded value actually
  appears in emitted trace output — the existing test suite would not have caught this bug,
  since `record()` on an undeclared field never errors.

## Definition of done for this ticket

- `ReceiptRecord.signature_hex` field added to
  `/Users/sac/praxis/crates/praxis-core/src/receipt_record.rs`, `verify_receipt_record`
  two-step verification implemented and tested (chain-broken vs. unsigned vs.
  signature-invalid vs. valid, all distinguishable).
- Key-management policy (zero-config file keypair vs. `PRAXIS_SIGNING_KEY` env var)
  decided and documented, not left implicit.
- All 5 OTEL spans in the ported pipeline carry a working `pipeline.duration_ms`,
  `pipeline.emit` also carries a working `pipeline.files_generated`; a `RUST_LOG=trace`
  capture demonstrates both firing on a real `ggen sync` run.
- No lingering `use crate::receipt::{..}` pointing at the ggen-core copy of `Receipt`
  (`/Users/sac/ggen/crates/ggen-core/src/receipt/receipt_impl.rs`).
