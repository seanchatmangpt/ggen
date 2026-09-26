# RFC v26.9.26 — ABB/SBB manufacture seed

## Ownership
ggen is the manufacturing function `A = μ(O*)`. It does not choose strategy, invent authority, or silently select an SBB.

## Definition of done
1. Accept an admitted EA graph containing Strategy/Capability/ABB/ArchitectureContract/CandidateSBB/Qualification.
2. Refuse manufacture if the selected SBB is UNKNOWN, mutable, unqualified, or exceeds contract/authority ceilings.
3. Compile at least one qualified SBB into deterministic artifacts.
4. Preserve exact architecture provenance in every generated artifact.
5. Emit a machine-readable manufacture receipt binding input graph digest, ABB, SBB, templates/generators and output digests.
6. Prove second-run deterministic identity.
7. Implement `SELECT existing SBB || MANUFACTURE missing realization` as separate from DO.
8. Demonstrate one end-to-end fixture driven by the marketplace EA pack.
9. Never make Pack == ABB or Pack == SBB.
10. Add falsifiers for stale qualification and changed architecture contract.

The follow-on agent should maximize generated implementation and minimize handwritten projection residue.

## Kernel and falsifier map (harden pass)
Kernel: `crates/ggen-abb-sbb` (IO-free, authority NONE, ceiling CONSTRUCT). Court:
`.github/workflows/abb-sbb-manufacture.yml`. Tests: `crates/ggen-abb-sbb/tests/falsifiers.rs`.

| DoD | standing | falsifier (test name) |
|---|---|---|
| 1 | PARTIAL_ALIVE | `malformed_inputs_are_refused_with_typed_refusals`, `dangling_references_are_refused` (JSON projection of the EA graph; no RDF ingestion yet) |
| 2 | ALIVE | `unknown_sbb_is_refused`, `sbb_with_unknown_identity_is_refused`, `mutable_sbb_is_refused`, `unqualified_sbb_is_refused`, `sbb_exceeding_artifact_ceiling_is_refused`, `sbb_authority_above_contract_ceiling_is_refused`, `unauthorized_do_request_is_refused` |
| 3, 4 | ALIVE | `qualified_sbb_manufactures_with_provenance_and_receipt` |
| 5 | ALIVE | same; `tampered_receipt_is_refused` |
| 6 | ALIVE | `second_run_is_byte_identical`, `element_reordering_does_not_change_digest_or_receipt` |
| 7 | ALIVE | `plan_selects_lowest_admissible_candidate`, `plan_falls_back_to_manufacture_with_every_refusal_recorded` |
| 8 | UNSUPPORTED | fixture is `synthetic_graph(2, 3)`, not the marketplace EA pack |
| 9 | ALIVE | `pack_named_as_abb_or_sbb_is_refused` |
| 10 | ALIVE | `stale_qualification_is_refused_after_sbb_changes`, `changed_architecture_contract_invalidates_qualification` |

Benchmark: `cargo bench --manifest-path crates/ggen-abb-sbb/Cargo.toml`; recorded numbers
in `crates/ggen-abb-sbb/bench/receipt.json`; regression bounds enforced by
`crates/ggen-abb-sbb/tests/bench_bound.rs` (admission digests the graph once).
