# v26.10.20 RESOLUTIONS (wave-2 implementation seams)

Canonical checkout: /Users/sac/ggen. Coordinator owns git + justfile.

## Shared-file ownership (sole writers)
- `crates/ggen-engine/src/sync.rs` → A05 ONLY (envelope CapTable extraction + Stage-2c epoch gate + receipt generation field + refusal envelope emission). A02's envelope.rs must land first; A05 applies minimal unblock with disclosure if structs lag.
- `crates/ggen-engine/src/write.rs` → A06 ONLY (lease slots, FM-WRITE-012, gate after check_freeze).
- `crates/ggen-abb-sbb/src/lib.rs` → A07 ONLY (Refusal::StaleLease, Request.generation, DEFAULT_MAX_ARTIFACTS/CapTable param).
- `crates/ggen-engine/src/error.rs` → A02 ONLY (EnvelopeUndeclared variant + fm_ constructor).
- `crates/ggen-graph/**` → A08 ONLY (receipt v2 domain separation, v1 legacy_verify golden).
- `crates/ggen-engine/src/portable_receipt.rs` + `receipt_chain_seam.rs` → A18 ONLY (generation field, AdmissionItem default 0 migration).
- `crates/ggen-engine/src/shell_safety.rs` → A16 ONLY (check_generated_surface).
- `crates/ggen-engine/src/law_engine.rs` → A05 ONLY (envelope SHACL hook placement).
- `scripts/ci/guard_fail_open_subprocess.py` → A16 ONLY.
- `justfile` → coordinator only (recipes added at integration; lanes must NOT edit).

## Vocabulary decisions
- Refusal registry home: `ontologies/shapes/boundary-refusal.shacl.ttl` (L13). Codes: `E-ENVELOPE-UNDECLARED` (surface=envelope), `REFUSED_STALE_LEASE` (surface=lease, abb-sbb variant `StaleLease{evidence_id,receipt,baseline}`), `VACUOUS_COURT`, `CONTROL_REFUSED`, `RESTORE_TAMPER`, `WITNESS_STALE`, `CENSUS_STALE`.
- ABB-sbb `Refusal` enum is the canonical typed home for admission refusals; ggen-engine mirrors codes as `REFUSED:<CODE>` strings via PortableStanding (both registered in L13 registry).
- Lease slots: `<root>/.ggen-v2/lease/<blake3-hex(rel_to)>`, decimal u64; missing=0 admits once then bumps; corrupt slot = FM-WRITE-012 fail-closed.
- Receipt `generation: Option<u64>` serde-default additive (ReceiptPayload deny_unknown_fields-safe); monotonic from `.ggen-v2/receipt-log.jsonl` tail.
- GraphReceipt v2: version-gated dual verify (v1 legacy raw concat pinned by golden vector, deprecated; v2 BLAKE3 derive_key("ggen.graph.receipt.v2")).
- Court compiler seam (L11): `crates/ggen-engine/src/court/{mod,catalog,compile}.rs`; `compile_court(catalog: &MutantCatalog, out_dir: &Path) -> Result<CourtManifest>`; verdict vocabulary Pass/Refused/Unknown (replay_court.rs convention). Templates `crates/ggen-engine/templates/court/{runner.sh,register_row,fixture.rs}.tera`. CLI verb `crates/ggen-cli/src/cmds/court.rs` (A12 sole owner of that file + its mod.rs/generated_commands.rs registration edits). Exit 1=VACUOUS GATE, 2=COURT_BLOCKED, 3=SKIP-loud. Mutation sourcing: cargo-mutants where possible, shim_patch diffs otherwise; ggen-cheat-scanner as output gate (T01–T04 on generated fixtures). A13's tests consume this seam (RED-by-compile until A12 lands — correct TDD).
- Vacuity schema ns `vc:` per L10; mutation-op/refusal-code enums exactly as L10 defined; receipts graph `.specify/artifacts/vacuity-receipts-<gate-id>.ttl`.
- Canonical vocab `canon:` per L04; frontmatter gains optional `canonical_identity` key; schema TTL + mirror test updated in same commit (A09).
- Shim-court lib vendored to `scripts/shim_court_lib.sh` (no smalt sourcing), self-check first.
- Generated refusal enums: `crates/ggen-engine/src/generated/refusals.rs`, `crates/ggen-abb-sbb/src/generated/refusals.rs`; trybuild compile_fail if trybuild already a dep, else doc-test compile_fail (abb-sbb pattern).
- Non-ABB stale write refusal: `AppError::fm_write(12, ...)` = `FM-WRITE-012`, refusal not skip.
- Tests-first order per pattern: courts land RED, engine GREEN, vacuity court validates.

## Integration decision (coordinator, wave-2 mid-flight)
- A05 pre-landed envelope.rs with JSON-sidecar caps (`<root>/.ggen-v2/envelope.json`). Ruled NON-canonical: the reticle is `.specify/envelope-shapes.ttl` (RDF sole source of truth). At integration (after A05 lands), `extract_caps` is retargeted to SPARQL over `res:Envelope` (A02's plan contract); the JSON sidecar path is removed, not left as a legacy bypass. Error remediation text already names the res: fields.
A02/A07/A06/A18 structs first (no cross-deps); A05 wires sync.rs after A02+A07; A13 depends on A12 seam declaration only (compiles RED until A12 lands — acceptable, TDD); A10/A03/A17 courts independent of engine land (RED-first is correct).
