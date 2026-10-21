# Typed-Code Crosswalk: ggen Registry ↔ graphlaw Taxonomy

Lane H11, v26.10.20. Court: `crates/ggen-engine/tests/refusal_crosswalk_court.rs`
(gate: `cargo test -p ggen-engine --test refusal_crosswalk_court`, 10/10 pass).

## Inventories

- **ggen WIRE census** (`crates/ggen-engine/src/typed_boundary_census.rs::WIRE_CODES`,
  14 codes): 11 typed registry codes with sealed constructors in
  `crates/ggen-engine/src/generated/refusals.rs` (generated from
  `ontologies/shapes/boundary-refusal.shacl.ttl`, L13) plus 3 `E-PARSE-{DIALECT}`
  string-surface codes emitted by `law_engine.rs` format helpers (no typed
  constructor — message prefixes, not `REFUSED:` wire forms).
- **graphlaw** (`/Users/sac/graphlaw/src/dialect.rs`): `RefusalKind` — 6 variants
  (`NotSemanticContent`, `Ambiguous`, `EngineRejected`, `Unsupported`,
  `ResourceLimit`, `TierMismatch`); `ReasonClass` (`#[non_exhaustive]`) — 8 current
  variants (`Syntax`, `UndefinedPrefix`, `BadIri`, `FormulaInRdfLayer`,
  `NotSemanticContent`, `AmbiguousDialect`, `RemoteContext`, `Limit{name,observed,max}`).

## Table 1 — ggen registry code → producer

| Code | Typed constructor | Producer surface | Test |
|---|---|---|---|
| E-ENVELOPE-UNDECLARED | `Refusal::envelope_undeclared` | `GraphLawEngine::validate_shacl` envelope pre-check (`envelope::undeclared_envelope`) | `undeclared_envelope_refusal_carries_registry_code` |
| REFUSED_STALE_LEASE | `Refusal::refused_stale_lease` | sync write-path lease gate (`portable_receipt.rs` `PortableStanding::Refused`) | `every_registry_code_emit_wire_form_round_trips` |
| VACUOUS_COURT | `Refusal::vacuous_court` | court admission gates (`court.rs`, `replay_court.rs`) | doctest in `generated/refusals.rs` |
| CONTROL_REFUSED | `Refusal::control_refused` | positive-control courts | `every_registry_code_emit_wire_form_round_trips` |
| RESTORE_TAMPER | `Refusal::restore_tamper` | shim-court restore check | `every_registry_code_emit_wire_form_round_trips` |
| WITNESS_STALE | `Refusal::witness_stale` | witness-receipt freshness gate | `every_registry_code_emit_wire_form_round_trips` |
| CENSUS_STALE | `Refusal::census_stale` | `scripts/ci/guard_wire_code_census.py` conflation gate | `every_registry_code_emit_wire_form_round_trips` |
| FD-UNDECLARED_INPUT_DIALECT | `Refusal::undeclared_input_dialect` | `graph.rs::load_declared` undeclared-dialect arm | `dialect_routing_refusals_carry_registry_codes` |
| FD-DIALECT_FEATURE_DISALLOWED | `Refusal::dialect_feature_disallowed` | `graph.rs::load_declared` n3 arm; `receipt_dialect.rs` blank-node refusal | `dialect_routing_refusals_carry_registry_codes` |
| FD-MALFORMED_SYNTAX | `Refusal::malformed_syntax` | `graph.rs::insert_quad_format` parse failure; `receipt_dialect.rs::verify_stream` | `dialect_routing_refusals_carry_registry_codes` (ntriples) |
| FD-REMOTE_CONTEXT_FORBIDDEN | `Refusal::remote_context_forbidden` | `graph.rs::load_declared` json-ld remote `@context` arm (`jsonld_has_remote_context`) | `dialect_routing_refusals_carry_registry_codes` |

Dead-code check (ggen → producer): **no orphans** — all 11 typed codes have a named
producer surface; all constructors are reachable.

## Table 2 — graphlaw → ggen mapping

| graphlaw item | ggen-side code | Crossing surface | Test |
|---|---|---|---|
| `RefusalKind::NotSemanticContent` | `[E-PARSE-{DIALECT}]` kind-named prefix | `law_engine.rs::format_law_refusal` | `every_graphlaw_refusal_kind_has_a_ggen_mapping` |
| `RefusalKind::Ambiguous` | `[E-PARSE-{DIALECT}]` kind-named prefix | `format_law_refusal` | same |
| `RefusalKind::EngineRejected` | `[E-PARSE-{DIALECT}] Kind: message` | `format_law_refusal` (witnessed live: `EngineRejected: bad IRI in NTriples`) | `malformed_shapes_through_shacl_gate_carry_eparse_prefix_with_kind` |
| `RefusalKind::Unsupported` | `[E-PARSE-{DIALECT}]` kind-named prefix | `format_law_refusal` | `every_graphlaw_refusal_kind_has_a_ggen_mapping` |
| `RefusalKind::ResourceLimit` | `[E-PARSE-{DIALECT}]`; typed `LIMIT:<name> observed/max` | `format_law_refusal`; `receipt_dialect::Limit` | `limit_wire_form_round_trips` |
| `RefusalKind::TierMismatch` | `[E-PARSE-{DIALECT}]` kind-named prefix (no FD- tier code exists) | `format_law_refusal` | `every_graphlaw_refusal_kind_has_a_ggen_mapping` |
| `ReasonClass::Syntax` | `E-PARSE-{DIALECT} ... at l:c` (positioned) / `FD-MALFORMED_SYNTAX` (ggen-side paths) | `format_eyeron_parse`, `format_rdf_diagnostic`, `insert_quad_format` | `malformed_facts_through_law_engine_carry_eparse_prefix` |
| `ReasonClass::UndefinedPrefix` | `E-PARSE-{DIALECT}` via PurRDF diagnostic code | `format_rdf_diagnostic` | `every_graphlaw_reason_class_has_a_ggen_mapping` |
| `ReasonClass::BadIri` | `E-PARSE-{DIALECT}` via PurRDF diagnostic code | `format_rdf_diagnostic` | same |
| `ReasonClass::FormulaInRdfLayer` | `FD-DIALECT_FEATURE_DISALLOWED` | `graph.rs::load_declared` n3 arm (refused pre-engine) | `dialect_routing_refusals_carry_registry_codes` |
| `ReasonClass::NotSemanticContent` | `[E-PARSE-{DIALECT}]` kind pass-through | `format_law_refusal` | `every_graphlaw_reason_class_has_a_ggen_mapping` |
| `ReasonClass::AmbiguousDialect` | `[E-PARSE-{DIALECT}]` kind pass-through | `format_law_refusal` | same |
| `ReasonClass::RemoteContext` | `FD-REMOTE_CONTEXT_FORBIDDEN` | `graph.rs::jsonld_has_remote_context` (pre-engine, offline fail-closed) | `dialect_routing_refusals_carry_registry_codes` |
| `ReasonClass::Limit{..}` | typed `LIMIT:<name> observed/max` (code `LIMIT_EXCEEDED`) | `receipt_dialect.rs` | `limit_wire_form_round_trips` |

Silent-coercion check (graphlaw → ggen): **no unmapped kinds** — all 6
`RefusalKind` and all 8 current `ReasonClass` variants have a named mapping row.
The court enumerates every variant by construction, so a graphlaw rename or a
new `#[non_exhaustive]` variant breaks the test and forces a table update.

## Flagged gaps

1. **Turtle malformed-syntax gap (pinned)**: `load_declared(.., "turtle")` routes
   through `GraphEngine::insert_turtle`, which reports parse failures as
   `[FM-GRAPH-002]` WITHOUT the `FD-MALFORMED_SYNTAX` registry prefix — unlike
   every quad-format path (`ntriples`/`nquads`/`trig`/`rdf-xml`/`json-ld`), which
   wraps via `insert_quad_format`. Pinned by `turtle_malformed_syntax_gap_is_pinned`;
   fix would be routing turtle through the same wrapping.
2. **E-PARSE codes are string-surface only**: `E-PARSE-{N3,TURTLE,NTRIPLES}` are in
   the WIRE census but have no typed constructor in `generated/refusals.rs` — they
   exist only as bracketed message prefixes. A `REFUSED:E-PARSE-N3` wire form is
   unconstructible by design, but the census/registry guard must keep treating them
   as prefix-codes, not `REFUSED:` codes.
3. **`ReasonClass` is `#[non_exhaustive]`**: future graphlaw variants arrive as
   `format!("{other:?}")` Debug-uppercased dialect names in `format_law_refusal`'s
   dialect match — typed but not registry-shaped until a mapping row is added. The
   court's mapping-table assertions are the tripwire.

## Gate receipt

`CARGO_TARGET_DIR=target-laneH11 cargo test -p ggen-engine --test refusal_crosswalk_court`
→ `test result: ok. 10 passed; 0 failed` (2026-10-20, lane H11).

## See Also

`crates/ggen-engine/src/generated/refusals.rs` ·
`ontologies/shapes/boundary-refusal.shacl.ttl` ·
`crates/ggen-engine/src/law_engine.rs` · `/Users/sac/graphlaw/src/dialect.rs` ·
`RECEIPTS.md`
