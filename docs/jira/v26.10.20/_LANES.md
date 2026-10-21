# v26.10.20 Wave-1 Lane Partition (read-only planning)

Canonical checkout: /Users/sac/ggen. Agents never run git commands. Coordinator owns transitions/commits.

| Lane | Pattern | Surface | Scope |
|---|---|---|---|
| L01 | BoundedResourceEnvelope | reticle/RDF | .specify TTL shapes, ggen.toml schema surface |
| L02 | BoundedResourceEnvelope | Rust engine | ggen-engine sync caps + generated const tables |
| L03 | BoundedResourceEnvelope | courts | cap-refusal falsifiers, envelope undeclared court |
| L04 | CanonicalIdentityLattice | reticle/RDF | canonical-form ontology rules |
| L05 | CanonicalIdentityLattice | Rust engine | canonical serializer generation, digests |
| L06 | CanonicalIdentityLattice | courts | field-mutation exhaustiveness, two-pass determinism |
| L07 | FreshEpochLease | reticle/RDF | lease admission Datalog/N3 rule |
| L08 | FreshEpochLease | Rust engine | Stage-2c lease gate, receipt generation field |
| L09 | FreshEpochLease | courts | stale-lease shim court |
| L10 | VacuityCourtCompiler | reticle/RDF | mutant-catalog schema |
| L11 | VacuityCourtCompiler | engine | court compiler (mutant catalog → runner) |
| L12 | VacuityCourtCompiler | courts | self-referential non-vacuity court |
| L13 | TypedBoundaryRefusal | reticle/RDF | refusal registry shapes |
| L14 | TypedBoundaryRefusal | engine | enum generation + compile_fail projection |
| L15 | TypedBoundaryRefusal | courts | census staleness + fail-open guard court |

Integration seam: `RESOLUTIONS.md` (same dir) records cross-lane decisions. Conflict rule: reticle lanes own TTL/schema; engine lanes own crates/ggen-engine + ggen-abb-sbb; court lanes own tests/. No lane touches justfile except via coordinator.
