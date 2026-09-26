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
| 1 | PARTIAL_ALIVE | `malformed_inputs_are_refused_with_typed_refusals`, `dangling_references_are_refused`, `sbb_realizing_a_nonexistent_abb_is_a_dangling_reference`, `contract_binding_a_nonexistent_abb_is_a_dangling_reference` (JSON projection of the EA graph; no RDF ingestion yet) |
| 2 | ALIVE | `unknown_sbb_is_refused`, `sbb_with_unknown_identity_is_refused`, `mutable_sbb_is_refused`, `unqualified_sbb_is_refused`, `sbb_exceeding_artifact_ceiling_is_refused`, `sbb_authority_above_contract_ceiling_is_refused`, `contract_ceiling_of_do_is_clamped_to_construct`, `unauthorized_do_request_is_refused`, `admission_below_construct_is_refused` (admit requires CONSTRUCT; plan requires SELECT and refuses NONE); `Admitted` is sealed (private fields, only `admit` constructs it; `compile_fail` doctests on `Admitted`), so `manufacture` cannot be applied to a hand-built or edited value |
| 3, 4 | ALIVE | `qualified_sbb_manufactures_with_provenance_and_receipt` |
| 5 | ALIVE | same; `tampered_receipt_is_refused` |
| 6 | ALIVE | `second_run_is_byte_identical`, `element_reordering_does_not_change_digest_or_receipt` |
| 7 | ALIVE | `plan_selects_lowest_admissible_candidate`, `plan_falls_back_to_manufacture_with_every_refusal_recorded`, `manufacture_decision_reports_only_ports_no_candidate_provides`; SELECT implies manufacturable (artifact paths and placeholders are checked at admission: `path_escape_is_refused`, `duplicate_artifact_path_is_refused`, `unbound_or_unterminated_placeholder_is_refused`) |
| 8 | ALIVE | `marketplace_fixture_is_the_exact_admitted_projection`; vendored bytes are bound to ggen-marketplace PR #506 fixture blob `dce7518e8a1e22f3864a5f957b3fe6809f27df02` by `fixtures/marketplace-source.json` |
| 9 | ALIVE | `pack_named_as_abb_or_sbb_is_refused`, `pack_colliding_with_any_element_id_is_refused_without_a_pack_list` (a pack id may not equal any element id, with or without a `packs` list) |
| 10 | ALIVE | `stale_qualification_is_refused_after_sbb_changes`, `changed_architecture_contract_invalidates_qualification` |

Standing of the whole seed: PARTIAL_ALIVE. The kernel has no consumer yet (nothing in
ggen-cli, ggen-engine or sync calls it); it is an independent workspace root listed in the root
`Cargo.toml` `exclude`, and remains PARTIAL_ALIVE only because the kernel is not yet wired into ggen-cli/ggen-engine/sync and the EA graph is still a JSON projection rather than native RDF ingestion.

Benchmark: `cargo bench --manifest-path crates/ggen-abb-sbb/Cargo.toml`; recorded numbers
in `crates/ggen-abb-sbb/bench/receipt.json`; regression bounds enforced by
`crates/ggen-abb-sbb/tests/bench_bound.rs`. "Admission digests the graph once" is witnessed
structurally (`graph_digests_computed()` counter deltas for admit, plan and replay, load
independent); the admit/digest timing ratio is its twin (minimum of interleaved rounds, best of
three attempts). The receipt names the measured commit and src tree (`subject.measured_commit`).
