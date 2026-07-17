# ggen-marketplace — Generation Audit

**Purpose:** Marketplace / package management system for ggen (Cargo.toml). Package models,
metadata, install/dependency resolution, RDF-backed registry (oxigraph), SPARQL search,
trust/PKI/policy enforcement, and an FMEA/poka-yoke safety layer.

## LOC baseline

Command: `tokei crates/ggen-marketplace/src --output json | jq '.Rust.code'` → **18,942** code LOC
(tokei 14.0.0; per-file via `tokei crates/ggen-marketplace/src --files`). 42 .rs files, all under
`src/marketplace/` plus `lib.rs`.

## Class breakdown

| Class | LOC | % of crate |
|---|---|---|
| GENERATED | 0 | 0% |
| GENERATABLE-NOW | 0 | 0% |
| GENERATABLE-WITH-SPEC | 7,545 | ~40% |
| IRREDUCIBLY-CUSTOM | 11,397 | ~60% |
| DEAD-DELETE | 0 | 0% (no consolidation-analysis citation available for this crate) |

`potential_coverage ≈ 40%` — **estimate**, sampled from module headers + representative file
reads; whole-file classification, no intra-file splitting. This is somewhat below the prior
~45% circulated estimate because policy/profile/control enforcement engines are classified
custom whole-file even though their rule *definitions* are declarative.

## Trust blocker: P0-03 namespace conflicts

`docs/crate-audits/AUDIT_DASHBOARD.md` P0-03 flags: "Namespace conflicts —
ggen-marketplace/ontology.rs — Blocks: SPARQL queries (silent data loss)" ("Three competing
URIs", called "most insidious"). Silent SPARQL data loss in the registry backend is a
**trust-blocker for any generated output** flowing through this crate: generated code derived
from marketplace RDF queries could silently omit packages.

Current-state evidence: `src/marketplace/rdf/ontology.rs` header now reads
"## Single Canonical Namespace (P0-03 fix) — The marketplace namespace is owned by
`crate::marketplace::ontology` (`MARKETPLACE_NS = "https://ggen.io/marketplace/"`) … re-exports
that constant rather than declaring a second source of truth". The fix is *claimed in code*;
the audit dashboard entry is carried forward until a conformance test proves no third
namespace remains (`grep -rn 'http.*ggen' crates/ggen-marketplace/src` recommended as the
verification step).

## Per-module rationale

### GENERATABLE-WITH-SPEC (7,545 LOC, ~40%)

Boilerplate-shaped per taxonomy (type defs, serde structs, error enums, registries, vocab
tables); needs a new TTL spec, no novel algorithm. Nearest template family: the a2a_generated
type/handler templates.

| File(s) | LOC | Rationale |
|---|---|---|
| `models.rs` | 436 | serde data models, newtypes (`PackageId`), poka-yoke enums |
| `metadata.rs` | 329 | package.toml / metadata.json field mapping |
| `error.rs` | 220 | thiserror enum |
| `builders.rs` | 129 | builder-pattern boilerplate over models |
| `traits.rs` | 141 | trait signature definitions (GAT/HRTB) — interface spec, no logic |
| `ontology.rs` | 347 | canonical namespace + vocabulary constants — quintessentially TTL-derivable |
| `rdf/ontology.rs` | 614 | enum-based Class/Property URI vocabulary re-exporting `MARKETPLACE_NS` |
| `rdf/sparql_queries.rs` | 609 | named SPARQL query registry — queries belong in `.rq`/TTL specs |
| `rdf/turtle_config.rs` | 342 | Turtle serialization config tables |
| `rdf/state_machine.rs` | 307 | package lifecycle transition table (TTL-expressible) |
| `rdf_mapper.rs` | 750 | struct↔RDF triple mapping — exactly what μ-pipeline emits elsewhere |
| `registry_rdf.rs` | 711 | oxigraph-backed CRUD over the vocabulary; query-template-shaped |
| `registry.rs` | 220 | registry facade |
| `search_sparql.rs` | 139 | SPARQL search query builders |
| `trust.rs` | 152 | TrustTier/RegistryType enums + tables |
| `fmea_mitigations.rs` + `rdf/fmea_mitigations.rs` | 455 + 447 | FMEA mitigation tables — declarative risk registry |
| `metrics.rs` | 245 | metric name/label definitions |
| `bundle.rs` | 272 | bundle manifest types |
| `ownership.rs` | 585 | OwnershipClass classification tables |
| `mod.rs`, `rdf/mod.rs`, `lib.rs` | 72+18+5 | re-exports |

### IRREDUCIBLY-CUSTOM (11,397 LOC, ~60%)

Algorithmic/IO cores TTL cannot express (each entry is a claim per methodology):

| File | LOC | Justification |
|---|---|---|
| `compatibility.rs` | 1,757 | 10-dimension conflict-detection algorithm |
| `install.rs` | 1,315 | dependency resolution with cycle detection, atomic install, rollback, parallel batching |
| `rdf/control.rs` | 893 | profile enforcement engine (authoritative path per coding-agent-mistakes.md) |
| `rdf/poka_yoke.rs` | 824 | phantom-type/typestate SPARQL safety system — type-level programming, not data |
| `composition_receipt.rs` | 734 | receipt hashing/chaining (crypto-adjacent; E-ledger candidate) |
| `migration.rs` | 699 | stateful schema/data migration logic |
| `profile.rs` | 621 | profile constraint evaluation |
| `policy.rs` | 614 | policy evaluation engine (rule *definitions* are declarative; evaluator is not) |
| `v3.rs` | 581 | LRU query caching, rayon parallel search, index optimization |
| `rdf/sparql.rs` | 538 | SPARQL execution/binding-extraction layer |
| `cache.rs` | 496 | cache eviction + fs lifecycle |
| `validation.rs` | 431 | validation rule execution |
| `pki.rs` | 382 | Ed25519 trusted-key + revocation management (E-ledger candidate: crypto) |
| `rdf/rdf_control.rs` | 376 | RDF control-flow enforcement |
| `atomic.rs` | 307 | atomic filesystem transaction primitives |
| `network.rs` | 281 | reqwest HTTP client, download progress, offline fallback |
| `security.rs` | 277 | checksum/signature verification paths |
| `search.rs` | 271 | fuzzy matching + relevance ranking |

### Notes

- `pki.rs`, `composition_receipt.rs`, `security.rs` (≈1,393 LOC) are crypto/receipt code —
  strong candidates for the reviewed **E (engine core) allowlist**, which would lower this
  crate's effective custom ratio; not self-declared here per anti-gaming rules.
- Possible redundancy worth a DEAD-DELETE pass by the consolidation owner: parallel
  `fmea_mitigations.rs` vs `rdf/fmea_mitigations.rs`, and `rdf/control.rs` vs
  `rdf/rdf_control.rs` — no consolidation-analysis citation exists, so both kept classified.
