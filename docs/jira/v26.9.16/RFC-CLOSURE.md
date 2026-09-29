# ggen v26.9.16 — RFC Closure Contract

Status: DRAFT IMPLEMENTATION PR. This branch is a work container for closing the semantic-law/manufacture portions of the v26.9.16 RFC set.

## Canonical Jira tickets

- A2A-2605 — CMCA bounded resource allocation
- A2A-2606 — recursive MFW / DME closure
- A2A-2607 — Blue River Dam cross-repo closure
- A2A-2608 — projected-ephemeral software invariant
- A2A-2609 — portable content-addressed `graphlaw.wasm`
- A2A-2612 — machine-experience compile-back

## RFC ownership

This repo owns the semantic/manufacturing core:

- admitted graph-state and least-expressive routing;
- GraphLaw SHACL/ShEx/SPARQL/N3/Datalog machinery;
- bounded MFW/PDDL/POWL planning and witness algebra;
- deterministic RDF-driven manufacture;
- projected ephemeral software from admitted semantic source;
- Blue River upstream admission/manufacture law;
- reusable-machine construction after successful UNKNOWN discovery;
- portable GraphLaw artifact production and exact semantic identity.

## Existing machinery to reuse

- `ggen-engine`
- `ggen-graph`
- `praxis-graphlaw`
- `bcinr-pddl`
- `bcinr-mfw-ir`
- existing Blue River Dam ontology/generation ledgers and courts
- existing Chatman least-expressive router and Chatman Constant guards

## Required closure

1. Make CMCA-visible cost/capability metadata projectable from the formal planning substrate.
2. Compose MFW residualization with DME work classes rather than inventing a parallel orchestrator.
3. Enforce `C_t = pi(O*,G,T,E_t)` as a source-authority invariant: generated code may be regenerated/discarded but may not silently become the semantic source of truth.
4. Produce a content-addressed GraphLaw WASM artifact with deterministic profile identity and host-independent admission fixtures.
5. Manufacture successful admitted UNKNOWN solutions into reusable semantic machinery consumable by future KNOWN routing.
6. Bind all output to exact semantic/manufacturer/projection digests suitable for SA2A receipts.

## Chicago falsifiers

The implementation must fail if:

- a more expressive GraphLaw dialect is chosen when a permitted lower one suffices;
- N3 directly actuates;
- a generated projection is edited and silently treated as canonical semantic truth;
- two hosts execute the same admitted GraphLaw subject and produce divergent canonical verdict/post-state within the declared deterministic profile;
- MFW consumes an unbounded epoch or silently expands resource mass;
- an LLM-produced planning/world artifact becomes admitted without the existing admission/witness path;
- an UNKNOWN solution is promoted to KNOWN without content-addressed qualification evidence.

## Definition of done

All listed Jira tickets have executable code and exact-head courts; `graphlaw.wasm` (or the final canonical portable artifact name) is content-addressed; native and WASM fixtures demonstrate equivalent admission semantics; projected-ephemeral invariants have negative controls; and the outputs expose exact identities consumed by the ash_a2a and qualification PRs.
