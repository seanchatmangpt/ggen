# Phase 1 Data Model: Retire ggen-core in favor of a first-principles engine

Entities are grounded in the actual Rust types found during research (file:line citations in
the referenced tickets), presented here at the specification-appropriate level (fields and
relationships, not full struct definitions — those live in the tickets and will be lifted
directly during implementation).

## Engine

The deterministic code-generation pipeline that replaces `ggen-core`'s role.

- **Owns**: RDF graph load, law/SHACL/N3 evaluation (via the `LawEngine` trait bridge, see
  [03-RDF-ENGINE-BRIDGE-DESIGN](../../docs/jira/v26.7.16/03-RDF-ENGINE-BRIDGE-DESIGN.md)),
  Tera template rendering, hygen-semantics file writes, the receipt chain, and operational
  telemetry emission.
- **Depends on**: `ggen-config` (manifest/config parsing), `ggen-marketplace` (resolving
  `QuerySource::Pack` — a new, deliberate edge), `ggen-graph`/`ggen-marketplace` only via the
  `LawEngine` trait seam (N-Triples strings, never shared model types).
- **Relationships**: One Engine instance drives one `sync` run, producing exactly one new
  `Receipt` chained to the previous one.

## Receipt

A record produced by every sync run, binding consumed inputs to produced outputs.

- **Fields** (per `ReceiptRecord`, `/Users/sac/praxis/crates/praxis-core/src/receipt_record.rs:27`):
  `payload_hash_hex` (line 55), `prev_chain_hash_hex` (line 57), `chain_hash_hex` (line 59),
  plus this migration's addition: `signature_hex: Option<String>` (see
  [04-RECEIPT-SIGNING-AND-OTEL](../../docs/jira/v26.7.16/04-RECEIPT-SIGNING-AND-OTEL.md)).
- **Validation rules**: `recompute_chain_hash()` must match `chain_hash_hex` (integrity);
  when `signature_hex` is present, it must verify against the project's known public key
  (authenticity) — the two checks are independently composable, never conflated. A record
  with `signature_hex: None` is a distinct, honestly-labeled "unsigned" state, not an error.
- **State transitions**: `Unsigned` (chain-valid, no signature) → `Valid` (chain-valid,
  signature-valid) is the only legitimate transition; a record can never move from `Valid`
  back to `Unsigned` without the chain integrity check itself failing (`ChainBroken`).
- **Relationships**: Each Receipt links to exactly one predecessor via `prev_chain_hash_hex`
  (or is the chain's genesis); the full ordered sequence is the append-only receipt log,
  where the log tail — not any single snapshot file — is the source of truth.

## Pack

An installable, versioned unit of templates/queries/rules a project can depend on.

- **Fields** (per `PackLockfile`/`LockedPack`/`PackSource`,
  `/Users/sac/ggen/crates/ggen-core/src/packs/lockfile.rs`, 788 lines, ported to
  `ggen-marketplace` per
  [06-MARKETPLACE-PACK-REGISTRY-MERGE](../../docs/jira/v26.7.16/06-MARKETPLACE-PACK-REGISTRY-MERGE.md)):
  name, source (path or git), pinned content hash, lock state.
- **Validation rules**: lockfile writes are idempotent (same inputs → same lock content);
  drift between the lockfile's recorded hash and the pack's actual on-disk content must be
  detected, not silently accepted.
- **Relationships**: A project's manifest (`GgenManifest`) references zero or more Packs by
  name; a `GenerationRule`'s `QuerySource::Pack`/`TemplateSource::Pack` variant resolves a
  specific file from within a named Pack's installed output — this is the "new edge" from
  the Engine to `ggen-marketplace` documented in the marketplace-merge decision.

## Diagnostic

A named, coded finding surfaced by the editor-integration (LSP) layer.

- **Fields**: code (e.g. `GGEN-TPL-001`, `E0011`), severity, message, source location.
- **Validation rules**: every diagnostic code that fires today must continue to fire with
  the same code after migration (spec FR-006); `E0015` in particular is documented in
  `ggen-core`'s own test suite as reserved/inactive, not a live diagnostic — this migration
  must not accidentally activate it, since CLAUDE.md currently (incorrectly) describes it as
  live (tracked in
  [13-CLAUDE-MD-REFACTOR](../../docs/jira/v26.7.16/13-CLAUDE-MD-REFACTOR.md)).
- **Relationships**: Diagnostics are computed against a project's manifest + templates
  independent of whether a `sync` has ever run — they depend on the Configuration Schema
  entity below, not on the Engine or Receipt.

## Configuration Schema

The authoritative definition of what a project's `ggen.toml` may contain.

- **Fields**: today split across two independently-evolved definitions —
  `config_lib::GgenConfig` (`/Users/sac/ggen/crates/ggen-config/src/config_lib/schema.rs:14`)
  and `GgenManifest`
  (`/Users/sac/ggen/crates/ggen-core/src/manifest/types.rs`, ported to `ggen-config` per
  [05-MANIFEST-CONFIG-PORT](../../docs/jira/v26.7.16/05-MANIFEST-CONFIG-PORT.md)) — this
  migration's FR-009 requires resolving overlapping fields (`sync`, `rdf`, `templates`,
  `output`, `ai`, `sparql`, `lifecycle`, `security`, `performance`, `logging`, `telemetry`,
  `features`, `env`, currently silently ignored by one parser) into one authoritative
  definition per top-level key.
- **Validation rules**: `deny_unknown_fields` semantics must not regress — a config typo
  that's caught today must still be caught after reconciliation.
- **Relationships**: One Configuration Schema instance governs one project's `ggen.toml`;
  both the Engine (for `sync`) and the Diagnostic layer (for editor-time validation) read
  from the same reconciled schema after this migration, versus two different, drifting
  schemas today.
