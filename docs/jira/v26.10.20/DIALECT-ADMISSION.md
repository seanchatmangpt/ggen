# RDF Dialect Admission & Scoping (ADMITTED 2026-10-20)

Grounded in W/R lane audits (graphlaw prep wave) + ggen-engine constraints.
Reference lanes: R01 (dialect inventory), R02 (parser backend), R03 (cross-dialect
canonicalization), R04 (tier split), R05 (pack declarations), R06 (W3C corpus),
R07 (error taxonomy), R08 (API surface), W04 (size court), W06 (resource bounds).

## Admission matrix

| Capability | Status | Guard |
|---|---|---|
| Dialect tagging + Chomsky layering | Admit, Slice 1 | FM-DIALECT-001 typed refusal; envelope per dialect |
| Universal RDFC-1.0 hashing | Admit, Slice 1 | Cross-dialect canonical court (R03); purrdf C14N already works, needs pinning |
| Pack-manifest dialect gates | Admit, Slice 1 | R05 plan: pack.toml `dialects` globs → `load_declared`; closed schema |
| TriG/N-Quads named graphs | Admit, Slice 2 | Engine graph→dataset seam first; graph-separation invariants |
| JSON-LD | Admit reduced (Slice 3) | Expanded-form only; remote context fetch FAIL_CLOSED; no framing |
| N3 quoted-graph meta-programming | Admit constrained | N3 = admission/policy only; Tera sole projection engine. Boundary permanent |

## Slice 1 (immediate)
1. pack.toml `dialects` schema (R05: PackCapabilities closed field, default `*.ttl→turtle`).
2. FM-DIALECT-001/002 refusal codes → boundary-refusal registry (L13 surface `dialect`).
3. ggen-engine `load_declared` dispatch (R05); retire the raw-string assumption at sync.rs:381-451.
4. Cross-dialect canonical identity court (R03 plan: one graph in 6 serializations → one digest; dataset semantics negative case; N3 Unsupported positive refusal).
5. Resolve R01 vs R02 conflict empirically BEFORE any new dep: probe purrdf 2.0.2
   `parse_dataset` with TriG/RDF-XML/JSON-LD media types. oxrdfio only if purrdf refuses.
   → RESOLVED (V04, tests/parser_capability_probe.rs 7/7): purrdf parses ALL probed
   formats natively, named-graph scoping preserved; remote @context fail-closed offline
   (jsonld-context-invalid, zero network). oxrdfio REJECTED — no new parser deps.
6. W06 wasm bounds fixes (op_shacl depth guard, MAX_LAW_STEPS, datalog budgets) land
   BEFORE dialect ingestion widens the wasm attack surface.

## Slice 2
Dataset seam in ggen-engine (graph→Quad store); named-graph intent segregation
(state/mutations/receipt-proof); cross-graph leak refusal.

## Slice 3
Expanded-form JSON-LD ingestion (context-free, no network, no framing).

## Guardrails (permanent)
- WASM size court: 25 MiB ceiling; 6.34 MiB baseline defended; new parser deps
  must re-measure under the court before admission.
- Lithographic boundary: Φ_lease owns permission; Tera owns syntax projection.
  N3 never generates code paths.
- All dialect parse errors route through ParseRefusal taxonomy (R07) → E-PARSE-*
  registry codes; no lossy `.to_string()` (fixes law_engine.rs:52,76,110,116).
