# x3-projection: Turtle authoring -> ggen.ea.graph.v1 JSON (WO-04 deliverable 3)

Date: 2026-11-03. Lane: IMPLEMENTATION (WO-04 deliverable 3, deferred by G7).
Standing: ALIVE — every claim below was executed this session with real exits.

## Subject

Complete WO-04 deliverable 3: author EA graphs as Turtle and project them to
the serde canonical `ggen.ea.graph.v1` JSON, byte-identical to the checked-in
kernel fixture, admitted by the kernel and by the sync pipeline's own Stage 2c
EA admission gate.

## Files landed (all new; src/** and ea-graph.shacl.ttl untouched)

- crates/ggen-abb-sbb/examples/ea_projection/ggen.toml
- crates/ggen-abb-sbb/examples/ea_projection/schema/demo-graph.ttl
- crates/ggen-abb-sbb/examples/ea_projection/queries/ea_graph/*.rq
  (7 files: strategy, capability, abb, contract, candidate_sbb,
  qualification, pack)
- crates/ggen-abb-sbb/examples/ea_projection/templates/ea_graph.json.tera
- crates/ggen-abb-sbb/examples/ea_projection/ea.graph.json
  (GENERATED output; byte-identical to the golden fixture)
- crates/ggen-abb-sbb/examples/ea_projection/.ggen/ + .ggen-v2/
  (sync receipt state from the verified runs)
- crates/ggen-abb-sbb/tests/ea_projection_test.rs (court)
- docs/jira/v26.11.3/receipts/x3-projection.md (this file)

## Design, with disclosures

- **Manifest shape**: consumer-style manifest in
  crates/ggen-abb-sbb/examples/ea_projection/ (its own ggen.toml + queries/ +
  templates/). Picked over extending an existing example manifest: keeps the
  projection self-contained and consumable as a pack-style surface.
- **One rule, not one rule per node type.** The declarative-rules engine
  refuses duplicate output_file across rules, so the whole-graph JSON is
  emitted by ONE [[generation.rules]] entry. The rule-level query binds
  queries/ea_graph/strategy.rq (the Strategy section's source); the six other
  per-node-type SELECTs reach the template through the sparql() Tera function.
  Deviation from the contract's "[[generation.rules]] per node type" wording,
  forced by the engine; the per-node .rq files remain the single source.
- **Inline query copies, pinned by court.** The engine's sparql() Tera
  function accepts query text, not file paths, so the six SELECTs are inlined
  in templates/ea_graph.json.tera. Drift between the .rq files and the inline
  copies is caught by a text court in ea_projection_test.rs
  (`inline_template_queries_match_rq_files`, whitespace-normalized equality,
  plus `ORDER BY ?id` presence on every file). A second clause keeps the
  manifest's rule→strategy.rq binding, output subject, and SHACL import
  pinned (`manifest_rule_binds_strategy_query_and_kernel_output`).
- **Bootstrap disclosure**: the Stage 2c EA admission gate requires
  ea.graph.json to exist BEFORE it can admit it, so the first sync after
  deleting the output runs with the documented `--skip-ea-admission` opt-out;
  the admitted run that follows exercises the gate for real and exits 0.
- **Digest-pattern drift (pre-existing, not fixed here)**: G7's
  ea-graph.shacl.ttl pins digests to `^[0-9a-f]{40}$`, but the kernel's own
  checked-in fixtures carry `sha256:<64 hex>` digests — the shapes and the
  serde/fixture surface disagree. The demo TTL transcribes the fixture
  faithfully, so it would fail the shapes' digest patterns if SHACL-validated
  as-is. Filed here for the schema owner; not touched (lane boundary).

## Verification ladder (real commands, real exits)

1. Generation + admission end to end:
   `cd crates/ggen-abb-sbb/examples/ea_projection && ggen sync run` (binary
   26.10.12, built from this checkout) → exit 0, Stage 2c gate admits the
   projected graph, `.ggen-v2/receipt.json` records
   `ea.graph.json → Pass / Admitted`; `ggen receipt verify` → "valid": true.
2. Byte-law: `diff ea.graph.json ../../..../fixtures/marketplace-ea-graph.json`
   → byte-identical (also asserted by
   `projection_is_byte_identical_to_kernel_fixture`).
3. Kernel court: `ggen_abb_sbb::parse_graph` + `validate` on the projected
   JSON → pass (`projection_passes_kernel_parse_and_validate`).
4. Determinism: two admitted syncs byte-identical
   (`projection_is_deterministic_across_syncs`); output sha256
   1e7167ad397343d34e3aa8e657b8fbd73efec018d052b200e87ed0c265b3a0a0.
5. Court: `cargo test -p ggen-abb-sbb --test ea_projection_test`
   → 5 passed / 0 failed.
6. Non-vacuity (shim-court discipline): TTL title mutated
   "Real-time operations" → "Mutated operations" →
   projection_is_byte_identical_to_kernel_fixture FAILED (court flips);
   restore byte-exact (sha256 4d5897ad... matches both sides) → 5/5 PASS.
7. No regressions: `cargo test -p ggen-abb-sbb` → all suites ok
   (unit 11, bench_bound 4, depgraph 15, ea_projection 5, envelope 4,
   falsifiers 43, kernel_highs 8, marketplace_fixture 1, property 16,
   refusal_crosswalk 3+3; 0 failures).

## Falsifiers status

- F1 (projected JSON differs in bytes from serde canonical form): closed —
  byte-identical, witnessed in ladder step 2.
- F2 (parse_graph/validate rejects): closed — step 3.
- F3 (deliberate drift passes the court): closed — step 6 mutation flipped the
  court to FAILED, restore passed.
- F4 (SHACL-invalid Turtle validates): NOT RUN here — SHACL validation of the
  demo TTL against ea-graph.shacl.ttl would fail on the digest-pattern drift
  above (fixture uses sha256:64-hex; shapes pin bare 40-hex). Deferred to the
  schema owner with the drift finding; the projection itself is byte-lawful.

## Standing

WO-04 deliverable 3 complete; WO-04 as a whole is complete per its four
deliverables (schema G7, parity court G7, projection this receipt, example
authoring = the demo TTL itself). Standing: ALIVE (all commands above ran this
session with the exits shown).
