# WO-04 Receipt: g7 — Canonical Schema + Parity Court (ggen.ea.graph.v1)

Standing: ALIVE

Lane: IMPLEMENTATION (WO-04 deliverables 1+2; the projection rule is a later WO — not authored here).
Date: 2026-11-03

## Subject

- `/Users/sac/ggen/ontologies/shapes/ea-graph.shacl.ttl` (NEW — canonical declarative schema)
- `/Users/sac/ggen/crates/ggen-config/tests/ea_graph_schema_parity_test.rs` (NEW — parity court)

## Diff

Two new files only. Pinned target `crates/ggen-abb-sbb/src/lib.rs` and
`sync.rs` untouched; no git commands run (lane law).

## What the shapes pin

SHACL (geag: namespace) mirroring the pinned serde structs in
`crates/ggen-abb-sbb/src/lib.rs` exactly:

- NodeShapes for Strategy, Capability, Abb, ArchitectureContract, ArtifactSpec,
  CandidateSbb, Qualification, Pack (+ Authority / Verdict value shapes), all
  `sh:closed true` (JSON projection uses `deny_unknown_fields`).
- `sh:path geag:<serde_field_name>` (serde default snake_case; no renames on
  these structs).
- Required fields `sh:minCount 1`; Option fields and collection fields
  (Vec/BTreeSet — always serialize, but an empty collection has no value
  triples in the RDF projection) `sh:minCount 0`.
- Patterns where serde constrains: `^[0-9a-f]{40}$` (digest, sbb_digest,
  contract_digest — 40-hex), `^(NONE|SELECT|CONSTRUCT|DO)$` (Authority,
  serde `rename_all = "UPPERCASE"`), `^(QUALIFIED|REFUTED)$` (Verdict).
- `sh:sparql` referential checks: abb.capability → Capability exists;
  abb.contract ↔ contract.abb bidirectional; sbb.abb → Abb; sbb.pack → Pack;
  qualification.sbb/contract → CandidateSbb/ArchitectureContract.
- `geag:PackConflationDisjoint`: pack ids disjoint from all element ids as a
  SPARQL constraint (UNION over six element classes).

## Verification ladder (commands + exits)

- `cargo test -p ggen-config --test ea_graph_schema_parity_test` — exit 0,
  **4/4 passed**, 0 warnings:
  1. `ea_graph_shapes_mirror_pinned_serde_structs_exactly` — real-file parity
     (two-way set equality of serde fields vs sh:path sets, per struct,
     closed-flag, and minCount pinning incl. collection-field rule).
  2. `ea_graph_shapes_carry_referential_and_conflation_constraints` — six
     named SPARQL constraint shapes present; bidirectional abb↔contract
     SPARQL fragments pinned by string.
  3. `deleting_one_shape_property_from_a_fixture_fails_the_court` —
     anti-vacuity: three mutations of a fixture copy (title property deleted,
     phantom field added, shapes un-closed) each fail the same court function
     that passes on the real file. Real shapes file never edited by the test.
  4. `extraction_lens_covers_all_pinned_structs` — lens guard: all 8 pinned
     structs still present in lib.rs (renames fail loudly, never shorten the
     sweep silently).
- Iterations: 3 red→green cycles (trailing-`;` in path extraction; minCount
  rule needed the collection-field modeling; phantom mutation inserted before
  `sh:targetClass` outside the extraction window — fixed by inserting after
  it). The minCount and phantom findings were the court catching real defects
  in my own artifacts — evidence the court is not vacuous.

## Extraction method (disclosed)

Text-level field extraction (precedent: `system_crate_map_parity_test.rs`),
not `syn` — the pinned structs use a uniform `pub name: Type,` line form with
no inter-field attributes, so line scanning is exact for this surface; `syn`
would add a dev-dep for zero extraction gain. Disclosed in the test header.

## Standing / falsifiers

- ALIVE: 4/4 courts GREEN this session, exact commands above.
- Pre-existing state (not session-introduced): none touched; the two new
  files had no prior state.
- UNVERIFIED by this lane: full-Turtle validity of the shapes file against a
  real RDF parser and SPARQLConstraint execution (e.g. `ggen graph validate
  --files ontologies/shapes/ea-graph.shacl.ttl`) — deferred to the projection
  WO that will actually load these shapes.
- Falsifier inherited: any future edit to the pinned structs that does not
  update the shapes fails court 1; any shapes edit that drops a property,
  adds a phantom, or un-closes a shape fails court 3's live checks.
