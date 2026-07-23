# tcps-pack

`tcps-pack` is the unified production-system pack extracted from
`examples/tcps-lifecycle`. Its atomic boundary is the complete TCPS control law,
not one language or deployment surface.

The pack projects canonical Rust, Lean, cargo-cicd, Praxis, release, deployment,
verification, and lifecycle artifacts directly into normal project paths. It
never creates a `generated/` directory.

The example is the conformance oracle. A pack change has standing only when a
clean consumer can be reprojected and passes the example's Rust, Lean, Praxis,
and cargo-cicd verification lifecycle.
