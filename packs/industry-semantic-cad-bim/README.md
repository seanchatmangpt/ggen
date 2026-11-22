# industry-semantic-cad-bim

Semantic CAD/BIM pack: event-sourced temporal knowledge graph over building
designs, built on REAL vocabularies.

## Identity

- Semantic source: `ontology.ttl`
  (namespace `https://seanchatmangpt.github.io/packs/industry-semantic-cad-bim#`).
- Real vocabularies: **BOT** (Building Topology Ontology, `https://w3id.org/bot#`)
  carries site/building/storey/space/element; **PROV-O** (`http://www.w3.org/ns/prov#`)
  carries the design-revision chain (`prov:wasRevisionOf`) — the time-travel axis.
- Faces: Blob = design-artifact WORM store; Cosmos = temporal KG documents
  (valid-time bounded, ETag CAS); Purview Atlas = KG face; KV PS256 = design-authority
  signing; AppConfig snapshots = schema/policy revision pinning (azure-industry-matrix.md,
  "Semantic CAD/BIM (temporal KG)" row).

## Gates (SPARQL SELECT; each row is a refusal)

1. `gates/010_element_storey_containment.rq` — every `bot:Element` MUST have
   `bot:hasElement` from a `bot:Storey`.
2. `gates/020_revision_predecessor.rq` — every `cad:DesignRevision` MUST have
   `prov:wasRevisionOf`, EXCEPT `cad:RootRevision`.
3. `gates/030_orphan_geometry.rq` — every `cad:Geometry` MUST be the
   `cad:hasGeometry` object of some element.

Witnesses under `witnesses/pass` and `witnesses/fail` share the gates' exact stems.
Court: `python3 qualification/verify.py` (real rdflib SPARQL engine, fail-closed)
plus the marketplace structural checker `scripts/check_gate_witness_courts.py`.

## Templates

- `templates/cad-shacl.ttl.tera` — SHACL shapes mirroring the three refusal laws.
- `templates/walkthrough.md.tera` — client walkthrough: browser console ->
  `smaltFetch` -> design-decode events -> Blob artifact store -> Cosmos KG docs ->
  temporal query walkthrough (revision chain = time travel).

## Evidence boundary

Marketplace admission and ggen qualification only. No execution authority:
gates SELECT, templates CONSTRUCT projections; generated artifacts claim NONE
and stand UNKNOWN until a consumer court admits them.
