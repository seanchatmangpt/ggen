# ggen-graph — Generation-First Audit

**Purpose:** Deterministic RDF graph substrate — Oxigraph wrapper with canonical N-Quads sorting, BLAKE3 deterministic hashing, deltas, SHACL/dialect checking, GraphReceipt transition receipts, three-pole coherence (O ≅ A ≅ L), OCEL/PROV evidence projection, and a suite of `gall_*` audit binaries. This is the RDF/SPARQL/BLAKE3-receipt substrate for the GALL security boundary.

**This crate is predominantly E-ledger (engine core).** Under METHODOLOGY.md's three-ledger model, the graph algorithms, deterministic hashing, coherence checker, receipt logic, and SHACL/SPARQL internals belong on the reviewed E allowlist and are excluded from the C/(G+C) ratio. Its IRREDUCIBLY-CUSTOM mass is expected and defended, not a coverage failure.

## LOC

`tokei crates/ggen-graph/src --output json | jq '.Rust.code'` → **7,921** code LOC (tokei 14.0.0).

| Module | Code LOC |
|---|---|
| `bin/` (11 gall_*/audit binaries) | 3,229 |
| `ocel/` | 2,271 |
| `graph/` | 639 |
| `coherence.rs` | 416 |
| `dialect.rs` | 270 |
| `vocab/` | 238 |
| `receipt/` | 194 |
| `shacl.rs` | 181 |
| `interchangeable.rs` | 113 |
| `lib.rs` / `prelude.rs` / `sparql.rs` / `delta/` / `doctor/` / `diagnostics/` | 370 |

(`src/lib.rs.clean` is a stray non-Rust text artifact, 0 code LOC — DEAD-DELETE.)

## Class breakdown

| Class | LOC | % |
|---|---|---|
| IRREDUCIBLY-CUSTOM | 6,492 | 82.0% |
| GENERATABLE-WITH-SPEC | 1,429 | 18.0% |
| DEAD-DELETE | 0 (lib.rs.clean, 0 code) | — |
| GENERATED / GENERATABLE-NOW | 0 | 0% |

**Verdict on prior estimate (~65% custom): direction confirmed, magnitude higher — 82% custom at file level.** The delta is the `bin/` directory (3,229 LOC, 41% of the crate) of bespoke GALL audit/sabotage harness binaries. Excluding `bin/`, the library proper is 3,263/4,692 ≈ **69.5% custom**, matching the prior ~65% estimate. Sampling basis: full sweep — all 74 .rs files enumerated with per-file tokei, 14 representative files read at 20+ lines.

## Per-module rationale

### IRREDUCIBLY-CUSTOM (6,492 LOC — proposed E-ledger except `bin/`)
- `graph/` (639): canonical quad sorting (`canonical.rs`), blank-node handling, BLAKE3 quad hashing (`hash.rs`), `DeterministicGraph` dataset wrapper (`dataset.rs`), N-Quads parse/serialize/locate/introspect. The determinism kernel every receipt depends on.
- `coherence.rs` (416): three-pole O ≅ A ≅ L BLAKE3 fingerprint/drift checker — the post-Chatman-equation auditor.
- `receipt/mod.rs` (194) + `delta/mod.rs` (79): GraphReceipt binding pre/post/delta BLAKE3 hashes; replay verification. Crypto/receipts allowlist.
- `shacl.rs` (181), `dialect.rs` (270), `sparql.rs` (74): SHACL minCount evaluation, SPARQL/Turtle dialect conformance over live Oxigraph stores — RDF/SPARQL internals.
- `ocel/projection.rs` (728): bidirectional OCEL/PROV ↔ RDF projection engine. `ocel/lifecycle.rs` (119): SPARQL-derived lifecycle ordering (the emission-side boundary CLAUDE.md mandates stays in ggen). `ocel/dfg.rs` (112) + `ocel/coverage.rs` (175) + `ocel/gall_projection.rs` (67): thin delegation glue to `wasm4pm-compat` authority functions.
- `interchangeable.rs` (113), `doctor/` (52), `diagnostics/` (44): membrane/adapter architecture glue and health checks.
- `bin/` (3,229): `gall_actuate_code_evaluation` (859), `gall_observe_sabotage` (591), `gall_materialize_evidence_graph` (421), `gall_adjudicate_witnessed_truthfulness` (296), plus 7 more. Bespoke one-off audit/observation/sabotage harnesses (URI whitelisting, mutation refusal testing, evidence-graph materialization). Not spec-shaped; however they are audit tooling, **not** E-ledger engine core — flagged for a follow-up DEAD-DELETE/consolidation review since several appear to be single-campaign artifacts.

### GENERATABLE-WITH-SPEC (1,429 LOC)
- `vocab/` (238): RDF/RDFS/OWL/XSD/SHACL/PROV/SKOS/OCEL/POWL/DFG `NamedNodeRef` constants — the single most ggen-shaped module in the workspace (vocabulary URIs ARE ontology terms; a CONSTRUCT over the source ontologies emits these files directly).
- `ocel/self_audit.rs` (562): declarative construction of a fixed OcelLog fixture (objects/events/attributes as literal data) — expressible as TTL + template.
- `ocel/pack_events.rs` (342): "thin, lawful builders" (its own words) — constructor boilerplate over OcelEvent/OcelObject for five activities; registry-shaped.
- `ocel/ocel_types.rs` (93), `ocel/prov_types.rs` (48), `ocel/mod.rs` (25): serde working types converting to/from wasm4pm-compat authority types.
- `lib.rs` (104), `prelude.rs` (17): module wiring/re-exports.
