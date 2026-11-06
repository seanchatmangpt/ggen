# WO-04: Make the EA graph schema declarative (SHACL canonical + ggen projection)

- **id**: GGEN-26113-04
- **milestone**: v26.11.3
- **standing**: UNVERIFIED
- **type**: specification / work order
- **priority**: high
- **constraints**: kernel structs unchanged (they become the projection target); reuse enterprise-architecture-pack's `ea:` namespace

## Ground truth (measured, 2026-11-03)

- `ggen.ea.graph.v1` is serde-structs-only: `crates/ggen-abb-sbb/src/lib.rs:78-194`, with
  `deny_unknown_fields` — the only closed struct in the crate.
- No `.ttl` / `.shacl` anywhere defines `Strategy`, `Capability`, `ABB`,
  `ArchitectureContract`, `CandidateSBB`, `Qualification`.
- The only graph producers today are the kernel's own `synthetic_graph` and hand-written
  test JSON.
- Two parallel vocabularies already exist and are NOT the kernel schema:
  - `fortune5-enterprise-architecture-pack`: Turtle-native EA surface for a parallel
    vocabulary (`f5ea:SolutionGroup` etc.) plus a
    `00-metamodel-hygiene.shacl.ttl` disjointness fence.
  - `enterprise-architecture-pack`: ships `ea:` classes
    (`ea:ArchitectureBuildingBlock`, `ea:SolutionBuildingBlock`,
    `ea:ArchitectureContract`) as a hand-synced parallel projection.

## Define

The EA graph schema is currently implicit in Rust serde structs. Authoring a graph requires
hand-writing JSON against a closed struct; there is no machine-checkable schema, and two
packs have already drifted into parallel vocabularies. Target end state:

1. **SHACL is the canonical schema** for the six EA node classes.
2. **ggen projects Turtle authoring → `ea.graph.json`**, byte-identical to the serde
   canonical form, via ONE `[[generation.rules]]` entry.
3. **A parity court** pins shapes ↔ serde field sets so any struct drift fails CI.
4. **The two existing parallel vocabularies are reconciled** into the `ea:` namespace.

## Measure (baseline)

- Schema definition for the six node classes in Turtle: **0 files** (grep-verified claim
  above; re-verify with `grep -r "Strategy\|CandidateSBB\|ArchitectureContract" --include="*.ttl"` at implement time).
- Authoring path: hand-written JSON only; `parse_graph` + `validate` consume it.
- Parallel vocabulary surfaces: 2 packs, 0 shared definitions with the kernel.

## Explore / Develop

### 1. Ontology: `shapes/ea-graph.shacl.ttl`

Define closed shapes for the six node classes — `Strategy`, `Capability`, `ABB`
(ArchitectureBuildingBlock), `ArchitectureContract`, `CandidateSBB`, `Qualification` —
mirroring the serde requirements exactly:

- `sh:closed true` on every node shape (mirrors `deny_unknown_fields`).
- `sh:property` with `sh:minCount 1` for every non-Option serde field (required-field
  sets mirror serde exactly; Option fields stay `minCount 0`).
- `Authority` and `Verdict` as UPPERCASE `sh:in` enums, values matching the serde
  representations exactly.
- `sh:sparql` referential checks for id links (e.g. a contract's referenced capability id
  must resolve to an existing `Capability` node).
- PackConflation disjointness fence, modeled on fortune5's
  `00-metamodel-hygiene.shacl.ttl`.

### 2. Parity court: shapes ↔ serde field sets

Schema-mirror pattern, like ggen-engine's `frontmatter_schema_match.rs`: a test that
mechanically compares each SHACL node shape's required-property set against the
corresponding serde struct's fields and fails on any drift — a struct field added or
removed without a shape edit (or vice versa) must fail the court, not silently diverge.

**Acceptance for the court**: adding a required field to a struct (or removing a shape
property) makes the parity test fail.

### 3. ONE ggen projection: Turtle → `ea.graph.json`

A single `[[generation.rules]]` entry:

- SPARQL SELECT per node type over the authored Turtle graph.
- Tera template emitting `ea.graph.json` (JSON, key-sorted) in the serde canonical form.
- Authoring becomes Turtle; the JSON is generated, never hand-edited.

**Acceptance (end to end)**: author a graph in Turtle, run `ggen sync`, and the projected
`ea.graph.json` is byte-identical to the serde canonical form; `parse_graph` +
`validate` pass on the projected file.

### 4. Vocabulary reconciliation

- **Adopt**: enterprise-architecture-pack's `ea:` namespace becomes THE `ea:` namespace
  (`ea:ArchitectureBuildingBlock`, `ea:SolutionBuildingBlock`, `ea:ArchitectureContract`
  are the canonical classes; the pack's hand-synced projection retires into the
  shape-defined model).
- **Map**: fortune5's parallel vocabulary is mapped in its pack — `f5ea:groupsSBB` →
  ea graph edges; `f5ea:SolutionGroup` etc. documented as mappings to the canonical
  `ea:` classes, not independent definitions.

## Implement (ordered)

1. Write `shapes/ea-graph.shacl.ttl` (six closed shapes + enums + sparql referential
   checks + disjointness fence).
2. Write the parity court test pinning shapes ↔ serde field sets; make it fail on a
   deliberate drift mutation, then restore (shim-court discipline).
3. Add the `[[generation.rules]]` projection (SELECTs + Tera JSON emission, key-sorted).
4. Author one example graph in Turtle; verify projected JSON is byte-identical to the
   serde canonical form and passes `parse_graph` + `validate`.
5. Reconcile the two packs: adopt `ea:` as canonical in enterprise-architecture-pack;
   add the `f5ea:` → ea-edge mapping in fortune5-enterprise-architecture-pack.
6. Kernel structs are NOT edited — they are the projection target.

## Falsifiers

- F1: projected `ea.graph.json` differs in bytes from the serde canonical form for an
  authored Turtle graph → work order fails.
- F2: `parse_graph` or `validate` rejects the projected JSON → fails.
- F3: a deliberate struct/shape drift passes the parity court → court is vacuous, fails.
- F4: a Turtle graph violating a closed shape (unknown field, missing required field,
  bad enum case, dangling id link, conflated pack classes) validates → SHACL fence
  vacuous, fails.

## Verification ladder

1. SHACL validation of example Turtle (valid + one mutation per falsifier class).
2. Byte-compare projected JSON vs serde canonical form.
3. `parse_graph` + `validate` on projected output.
4. Parity court: pass on current structs, fail on injected drift, pass on restore.

## See Also

`crates/ggen-abb-sbb/src/lib.rs:78-194` · ggen-engine `frontmatter_schema_match.rs`
(schema-mirror pattern) · fortune5-enterprise-architecture-pack
`00-metamodel-hygiene.shacl.ttl` · enterprise-architecture-pack `ea:` classes
