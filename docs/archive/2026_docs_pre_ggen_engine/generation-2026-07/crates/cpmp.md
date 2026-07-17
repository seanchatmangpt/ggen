# cpmp — audit (generation-first, 2026-07)

**Purpose:** "Computer Project Mapping Protocol — scanner, capability classification, projection, receipts." (`crates/cpmp/Cargo.toml`); also hosts the Open Ontologies Catalog registry (embedded tier-0 TTLs).

## LOC

`tokei crates/cpmp/src --output json` (tokei Rust code lines):

| File | Code LOC | Class |
|---|---|---|
| registry.rs | 706 | GENERATABLE-NOW |
| projection.rs | 215 | GENERATABLE-WITH-SPEC |
| ocel.rs | 204 | GENERATABLE-WITH-SPEC |
| classification.rs | 175 | IRREDUCIBLY-CUSTOM |
| scanner.rs | 110 | IRREDUCIBLY-CUSTOM |
| receipt.rs | 106 | IRREDUCIBLY-CUSTOM |
| models.rs | 91 | GENERATABLE-WITH-SPEC |
| entry.rs | 77 | GENERATABLE-WITH-SPEC |
| capability.rs | 63 | IRREDUCIBLY-CUSTOM |
| db.rs | 52 | GENERATABLE-WITH-SPEC |
| tier.rs | 46 | GENERATABLE-WITH-SPEC |
| error.rs | 45 | GENERATABLE-WITH-SPEC |
| symbol.rs | 43 | IRREDUCIBLY-CUSTOM |
| catalog.rs | 29 | GENERATABLE-WITH-SPEC |
| lib.rs | 17 | GENERATABLE-WITH-SPEC |
| main.rs | 6 | GENERATABLE-WITH-SPEC |
| **Total** | **1,985** | |

## Consolidation status

KEEP per `CRATE_CONSOLIDATION_ANALYSIS_2026-07-01.md` §5 (line 56): "Self-contained leaf CLI tool… no domain overlap with any other crate." Nothing here is DEAD.

## Class breakdown

| Class | LOC | Share of crate |
|---|---|---|
| GENERATABLE-NOW | 706 | 35.6% |
| GENERATABLE-WITH-SPEC | 782 | 39.4% |
| IRREDUCIBLY-CUSTOM | 497 | 25.0% |

## Per-file rationale

- **registry.rs (706, GENERATABLE-NOW):** `build_entries()` (lines 157–770) is 63 `tier0(`/`cached(`/`referenced(` builder calls (`grep -c "tier0(\|cached(\|referenced(" crates/cpmp/src/registry.rs` → 63) hand-duplicating the DCAT catalog that already exists as **`crates/cpmp/ontologies/catalog.ttl`** — the crate even `include_str!`s that TTL (line 12) without deriving the entries from it. Spec exists; a Tera template over the catalog graph is the missing wiring. The ~90 LOC of `OntologyRegistry` wrapper/`load_ttl_into_store` would ride along or stay as thin custom glue; dominant class assigned per file.
- **projection.rs (215, GENERATABLE-WITH-SPEC):** `generate_audit_dashboard`/`generate_reports`/`generate_rdf_fallback` — markdown/RDF report string assembly, i.e. template-shaped output currently written as `format!` calls.
- **ocel.rs (204, GENERATABLE-WITH-SPEC):** OCEL 2.0 serde structs (`OcelEvent`, `OcelObject`, type specs, `OcelEventLog`) mirror a published standard — spec-emittable; the small DFG helpers (`directly_follows_graph`, `variants_per_object_type`, ~45 LOC) are algorithmic, noted as the file's custom remainder. (Aside for the boundary rule in CLAUDE.md: these are local DFG helpers inside a leaf tool; flagging for the wasm4pm-compat owners, out of audit scope.)
- **models.rs (91) / entry.rs (77) / tier.rs (46) / error.rs (45), GENERATABLE-WITH-SPEC:** result/receipt/capability type defs, enums with Display impls, thiserror-style error enum + From impls — pure boilerplate shape.
- **db.rs (52, GENERATABLE-WITH-SPEC):** three CREATE TABLE strings + insert wiring; schema-derivable.
- **catalog.rs (29) / lib.rs (17) / main.rs (6), GENERATABLE-WITH-SPEC:** clap-noun-verb `#[verb]` CLI wiring (its own doc comment says the verbs are "derived… from each function signature"); nearest spec family `.specify/cli-schema.ttl`, `.specify/clap-noun-verb-cli-shapes.ttl`.
- **classification.rs (175, IRREDUCIBLY-CUSTOM):** `classify_file` signal heuristics + `dormant_crate_dirs` workspace-parsing logic — judgment-encoding heuristics, per the prompt's prior-exploration finding, confirmed by read.
- **capability.rs (63) / symbol.rs (43) / scanner.rs (110), IRREDUCIBLY-CUSTOM:** keyword/regex capability detection, regex symbol extraction, walkdir scan orchestration — heuristic/algorithmic.
- **receipt.rs (106, IRREDUCIBLY-CUSTOM):** order-independent BLAKE3 `aggregate_hash` + `verify_no_deletion` diff logic — crypto/verification core; E-ledger candidate.

## Matching template/spec

- `crates/cpmp/ontologies/catalog.ttl` — existing spec for registry.rs entries (GENERATABLE-NOW evidence).
- `.specify/cli-schema.ttl` / `.specify/clap-noun-verb-cli-shapes.ttl` — CLI-wiring spec family for catalog.rs/main.rs.
