# Deletion and Definition of Done

Part of [00-OVERVIEW](00-OVERVIEW.md) — Phases 6–7, depends on
[08](08-GGEN-CLI-MIGRATION.md), [09](09-GGEN-LSP-MIGRATION.md), and
[10](10-ROOT-PACKAGE-TEST-MIGRATION.md) all completing.

## File reference table — confirmed dead code, do not port

| Path | LOC | Status |
|---|---:|---|
| `/Users/sac/ggen/crates/ggen-core/src/receipt/mod.rs` | 119 | dead — unreachable duplicate |
| `/Users/sac/ggen/crates/ggen-core/src/receipt/chain.rs` | 351 | dead — unreachable duplicate |
| `/Users/sac/ggen/crates/ggen-core/src/receipt/envelope.rs` | 454 | dead — unreachable duplicate |
| `/Users/sac/ggen/crates/ggen-core/src/receipt/error.rs` | 39 | dead — unreachable duplicate |
| `/Users/sac/ggen/crates/ggen-core/src/receipt/receipt_impl.rs` | 362 | dead — unreachable duplicate |
| `/Users/sac/ggen/crates/ggen-core/src/receipt/chain_linking.rs` | 188 | **unclassified** — see correction below |
| `/Users/sac/ggen/crates/ggen-core/src/receipt/provenance_envelope.rs` | 754 | **unclassified** — see correction below |
| **`receipt/` directory total (7 files)** | **2,267** | |
| `/Users/sac/ggen/crates/ggen-core/src/naming.rs` | 360 | dead — confirmed via `grep -rn "mod naming" src/` returning zero hits, genuinely not declared as a module anywhere |
| `/Users/sac/ggen/crates/ggen-core/src/manufacturing/gates.rs` | 99 | dead in release builds |
| `/Users/sac/ggen/crates/ggen-core/src/manufacturing/mod.rs` | 10 | dead in release builds |
| `/Users/sac/ggen/crates/ggen-core/src/manufacturing/operator.rs` | 54 | dead in release builds |
| **`manufacturing/` directory total (3 files)** | **163** | `#[cfg(test)]` gate confirmed at `ggen-core/src/lib.rs:212`, immediately preceding `pub mod manufacturing;` at line 213 |
| `/Users/sac/ggen/crates/ggen-core/src/packs/install.rs:33-39` | 39 (whole file) | dead stub — see [06-MARKETPLACE-PACK-REGISTRY-MERGE](06-MARKETPLACE-PACK-REGISTRY-MERGE.md) |
| `/Users/sac/praxis/crates/ggen/src/types.rs` | 1,241 | largest file in praxis's `crates/ggen` — see correction below |
| `/Users/sac/praxis/crates/ggen/src/bin/mcp_server.rs` | 173 | shells out to unrelated binary `affi` at lines 78, 109, 136 — non-functional stub, untested |

**Correction — `receipt/` directory is incomplete in earlier drafts of this ticket set**: two
files exist beyond the originally-named five —
`/Users/sac/ggen/crates/ggen-core/src/receipt/chain_linking.rs` (188 lines) and
`/Users/sac/ggen/crates/ggen-core/src/receipt/provenance_envelope.rs` (754 lines). All 7
files are wired into the module tree (`receipt/mod.rs:78-83` declares `pub mod chain; pub
mod chain_linking; pub mod envelope; pub mod error; pub mod provenance_envelope; pub mod
receipt_impl;`). The "1,325 dead lines" figure quoted elsewhere in this ticket set covers
only the originally-named 5 files (`mod.rs` 119 + `chain.rs` 351 + `envelope.rs` 454 +
`error.rs` 39 + `receipt_impl.rs` 362 = 1,325); `chain_linking.rs` and
`provenance_envelope.rs` have **not** been re-audited for reachability and must not be
silently folded into "confirmed dead" without that check. **Action before Phase 6**: confirm
via `grep -rn "provenance_envelope\|chain_linking" crates/ggen-core/src/` whether anything
outside `receipt/mod.rs`'s own module declarations references these two files (prior
research found `provenance_envelope::{CoherenceReport, ProvenanceEnvelope}` re-exported and
used by `ggen-cli/src/cmds/inverse_sync.rs` — likely load-bearing, not dead; verify before
deleting).

**Correction — `praxis/crates/ggen/src/types.rs`'s "zero callers" claim is too strong**:
`CicdPolicy`, `PolicyVerdict`, `HashAdmit`, `StageOutcome` genuinely appear only inside
`types.rs` itself. But `ObjectRef`, `AdmittedReceipt`, and `Evidence` (bare, not
`Evidence<...>`) **are** re-exported at the crate root via
`/Users/sac/praxis/crates/ggen/src/lib.rs:22-23`:
```
22: canonical_bytes, Admit, Admitted, AdmittedEvidence, AdmittedReceipt, Blake3Hash, Evidence,
23: ObjectRef, ProfileId, Raw, RawEvidence, Validated, ValidatedEvidence,
```
So `types.rs` is not entirely dead weight — only `CicdPolicy`/`PolicyVerdict`/`HashAdmit`/
`StageOutcome` (and whatever internal-only logic depends solely on them) are confirmed
unused; the rest of the file backs the crate's public API surface even if unused
internally. Treat this as "mostly dead, not entirely" when deciding what (if anything) to
carry forward.

## Phase 6: delete `ggen-core`

Once [08](08-GGEN-CLI-MIGRATION.md), [09](09-GGEN-LSP-MIGRATION.md), and
[10](10-ROOT-PACKAGE-TEST-MIGRATION.md) are complete and `just check`/`just test` are green
with `ggen-core` unreferenced, delete `/Users/sac/ggen/crates/ggen-core/` in full, plus the
confirmed-dead code listed above (wherever it was carried over into the new engine rather
than left behind — re-verify `chain_linking.rs`/`provenance_envelope.rs` per the correction
above before deleting them). A `ggen-core` compatibility shim crate left "for compatibility"
is Legacy Path Contamination and defeats the purpose of this ticket set — see the 6-Question
Patch Contract below, question 5.

## Phase 7: Verification / Definition of Done

Done only when all five Andon gates (`/Users/sac/ggen/.claude/rules/andon/signals.md:36-42`)
are green, OTEL pipeline spans are captured (not just tests passing), and the 6-Question
Patch Contract (`/Users/sac/ggen/.claude/rules/coding-agent-mistakes.md`) is answered
concretely for this migration.

```bash
just timeout-check   # unaffected -- verifies the `timeout` binary exists
just check           # cargo check --workspace, ggen-core removed from members = [...]
just test            # cargo test --workspace --tests, ggen-core test suite deleted, not skipped
just lint            # cargo clippy --all-targets -- -D warnings, zero warnings from the new engine crate(s)
just slo-check       # see retargeting below -- the gate confirmed broken by fresh research
```

### `slo-check` retargeting

Exact current body (`/Users/sac/ggen/justfile:153-167`) runs three lines with three
different dispositions: `cargo bench --bench cli_startup_performance -- --test` is
**unaffected** (shells out to the compiled `ggen` binary, engine-agnostic — bench source
`/Users/sac/ggen/benches/cli_startup_performance.rs`); `cargo test -p ggen-graph --test
coherence_hash_expectations_test` is **unaffected** (this ticket set retires `ggen-core`,
not `ggen-graph`); `cargo test -p ggen-core --test inverse_receipt_chain_test -- --nocapture`
**must be retargeted** — once `ggen-core` is deleted this hard-fails ("package ID
specification did not match any packages"), a loud and correct failure, but the recipe still
needs a real replacement line. Candidate, confirmed to exist today: `cargo test -p ggen
--test receipt_chain_e2e -- --nocapture`
(`/Users/sac/praxis/crates/ggen/tests/receipt_chain_e2e.rs`, a direct correctness analog).

**Gap that must be closed, not carried over silently**: neither `inverse_receipt_chain_test.rs`
nor `receipt_chain_e2e.rs` contains any timing assertion (`Duration|Instant|elapsed`, zero
matches in either, 552 and 479 lines respectively), despite the justfile's own comment above
the old recipe claiming "`InversePipeline::run_signed()` must complete in <5s... measured via
integration tests that include timing assertions." That claim is false today — a pre-existing
Decorative Completion (mistake class 1). The migration should either add a real
`Instant`/`Duration` assertion enforcing the claimed bound, or rewrite the comment to stop
claiming a guarantee the test doesn't check — a straight rename perpetuates the mislabeling.
Two more `-p ggen-core` targets outside `slo-check` itself
(`/Users/sac/ggen/justfile:100,106`, `ast_extractor_70pct_test` and
`provenance_envelope_test` under `test-phase2`) go stale the same way and should be triaged
in the same pass.

### OTEL verification

Per this repo's own rule, tests passing is explicitly insufficient. Required proof: a
captured `ggen sync` run showing all five `pipeline.*` spans and their attributes (see
[04-RECEIPT-SIGNING-AND-OTEL](04-RECEIPT-SIGNING-AND-OTEL.md) for the span-fix design).

```bash
export RUST_LOG=trace,ggen_core=trace,ggen=trace
cargo run --bin ggen -- sync --audit true 2>&1 | tee otel_sync_output.txt
grep -E "pipeline\.(load|extract|generate|validate|emit)" otel_sync_output.txt
grep -E "pipeline\.stage=|pipeline\.duration_ms=|pipeline\.files_generated=" otel_sync_output.txt
```

`pipeline.files_generated` must be cross-checked against the actual file list bound into
`.ggen/receipts/latest.json` — a plausible-looking count with no matching receipt entry is
"Partial — OBSERVED," not "Real — PROVEN," and does not clear this gate. No spans at all is
"Missing — UNVERIFIED" regardless of how many `just test` assertions pass.

### The 6-Question Patch Contract, answered for this migration

1. **What real state changed?** `/Users/sac/ggen/crates/ggen-core/` (142,908 lines) removed;
   `/Users/sac/ggen/Cargo.toml`'s `members` drops `ggen-core`, gains the new engine crate(s);
   every crate depending on `ggen-core` (`ggen-cli`, `ggen-lsp`, root `ggen`) is re-pointed.
2. **What authoritative path did this patch touch?** Every `ggen_core::` call site in
   `ggen-cli/src` (164 across 31 files) and `ggen-lsp/src` (17 across 6 files); most
   consequentially, `ggen-cli`'s own public API surface (`lib.rs`'s `pub use
   ggen_core::utils::error::Result` and `cli_match()`'s return type) changes shape.
3. **What negative path now fails correctly?** A malformed `ggen.toml`, an unbound SPARQL
   variable, or an `output_file` escaping the project root must still surface as
   `GGEN-TPL-001`/`GGEN-OUT-001`/`GGEN-YIELD-001`/`GGEN-RULE-001` through the new engine's
   diagnostic surface. Any remaining `-p ggen-core` reference must hard-fail loudly once the
   crate is gone, not silently skip.
4. **What invariant protects this patch from drift?** Every `ggen sync` still produces a
   signed BLAKE3 transition receipt, verifiable via `ggen receipt verify`, with input/output
   hash bindings reflecting the new engine's actual render path — not stale hashes carried
   over by an un-removed shim.
5. **What legacy path was removed?** `/Users/sac/ggen/crates/ggen-core/` itself, in full, in
   Phase 6. A `ggen-core` compatibility shim crate left "for compatibility" is exactly the
   Legacy Path Contamination class this contract exists to catch.
6. **What proof object shows it worked?** `just pre-commit` green; `just test` green; `just
   slo-check` green against the retargeted line (with the timing-assertion gap closed, not
   just renamed); `.ggen/receipts/latest.json` regenerated and `ggen receipt verify` passing;
   the OTEL span capture above, pasted into the completion message.

## Definition of done for this ticket

- `/Users/sac/ggen/crates/ggen-core/` deleted in full.
- All confirmed-dead code from above either deleted or explicitly ported with a note on why
  (with `chain_linking.rs`/`provenance_envelope.rs` reachability re-verified first).
- `slo-check` retargeted, including the closed timing-assertion gap.
- All five Andon gates green, OTEL span capture attached, 6-Question Patch Contract answered
  in the closing PR/commit message.
