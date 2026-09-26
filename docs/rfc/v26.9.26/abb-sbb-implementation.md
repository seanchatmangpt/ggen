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
