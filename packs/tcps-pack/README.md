# tcps-pack

`tcps-pack` 0.2.0 is the unified production-system pack extracted from
`examples/tcps-lifecycle`. Its atomic boundary is the complete TCPS control law,
not one language, tool, or deployment surface.

The pack projects canonical Rust, Lean, Aeneas, cargo-cicd, Praxis, autonomics,
release, deployment, verification, and lifecycle artifacts directly into normal
project paths. It never creates a `generated/` directory.

Its 1000x autonomic policy is an exact, fail-closed phase boundary. Below 1000x,
standard work remains continuous. At or above 1000x, the production topology is
recomposed through five bounded actions: demand partitioning, pull shards,
independent judgment, proof-carrying receipts, and canary rollback.

The autonomic worker never judges its own evidence. cargo-cicd observes and
executes, Praxis judges, TCPS owns the phase law and atomic receipt, and ggen
projects the admitted system.

The example is the conformance oracle. A pack change has standing only when a
clean consumer can be reprojected and passes the example's Rust, Lean, Aeneas,
Praxis, cargo-cicd, projection-equivalence, and autonomic falsification lifecycle.
