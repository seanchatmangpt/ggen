<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Explanation: Control Plane Security in the Marketplace](#explanation-control-plane-security-in-the-marketplace)
  - [The Purpose of the Control Plane](#the-purpose-of-the-control-plane)
  - [POKA YOKE: Compile-Time Type Safety](#poka-yoke-compile-time-type-safety)
  - [FMEA Mitigations: Active Injection Defense](#fmea-mitigations-active-injection-defense)
  - [State Machine Transitions](#state-machine-transitions)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Explanation: Control Plane Security in the Marketplace

This document explains the security architecture of the `RdfControlPlane`, the centralized gateway for all semantic data operations within the `ggen-marketplace`. 

## The Purpose of the Control Plane

Because the marketplace stores all configurations, dependencies, and metadata in an open, highly-connected RDF graph, a single malformed triple could poison the integrity of package resolutions or dependency trees. The `RdfControlPlane` exists to provide rigorous boundaries around what data is allowed into the graph and how it transitions over time.

## POKA YOKE: Compile-Time Type Safety

The marketplace implements the **Poka-Yoke (mistake-proofing)** pattern by wrapping raw RDF structures in a strictly typed Rust state machine (`typestate`).

Instead of writing strings directly:
```rust
// Unsafe, error-prone string concatenation
let query = format!("SELECT * WHERE {{ ?s <{}> ?o }}", user_input);
```

The engine forces developers to use strongly-typed builders:
```rust
let query = Triple::builder()
    .subject(package_id.clone())
    .predicate_from_property(Property::PackageName)
    .object_literal(Literal::String(name.to_string()))
    .build();
```
This ensures invalid resource IDs or mismatched domain/range relationships fail at compile-time rather than corrupting the data store at runtime.

## FMEA Mitigations: Active Injection Defense

Semantic persistence layers are vulnerable to SPARQL injections (e.g., an attacker embedding `DROP GRAPH` in a search string).

The control plane incorporates an **FMEA (Failure Mode and Effects Analysis)** mitigation manager that intercepts all raw queries. If a suspicious pattern like `INSERT DATA {`, `DELETE WHERE {`, or `; DROP` is detected, the mitigation manager:
1. Immediately aborts the execution.
2. Returns a `ControlPlaneError::SecurityViolation`.
3. Records metrics on the attack attempt for the dashboard telemetry.

## State Machine Transitions

Lifecycle events in the marketplace (e.g., publishing a package) are strictly managed via deterministic state transitions rather than ad-hoc property updates. 

A package exists on a spectrum:
`draft` → `published` → `deprecated`

When the control plane receives a `publish` event for a package ID, it:
1. Queries the current state node (`http://ggen.dev/vocab/state`).
2. Validates if the transition is structurally legal.
3. Only if the package passes SHACL structural validations (e.g., ensuring it possesses an `rdf:type` and a `PackageName`), the transition is allowed to execute.

This strict governance guarantees that the marketplace graph remains a highly trusted, tamper-resistant foundation for dependency resolution.
