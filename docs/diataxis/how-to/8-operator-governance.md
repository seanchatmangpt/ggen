# How-To Guide: Implementing 8-Operator Governance

This guide explains how to move a package through the mandatory 8-operator lifecycle enforced by the `RdfControlPlane`. Following this sequence is a core requirement of the **Vision 2030** security pillar.

## Problem
You need to publish a new package to the marketplace, but it must pass all security, audit, and quality gates before being visible to other users.

## Solution: The Operator Sequence

The `RdfControlPlane` enforces a deterministic state machine. You cannot skip steps (e.g., you cannot `release` a package that has not been `audited`).

### 1. Admission (Admit)
Moves a package from `draft` to `admitted`. This signals that the package intent is recognized.

```rust
control_plane.transition_state(&package_id, "admit")?;
```

### 2. Dependency Breeding (Breed)
The system analyzes and "breeds" the internal dependency graph.
```rust
control_plane.transition_state(&package_id, "breed")?;
```

### 3. Quality Validation (Validate)
Executes SHACL structural checks and Chicago TDD verification.
```rust
control_plane.transition_state(&package_id, "validate")?;
```

### 4. Canonicalization (Canonicalize)
Normalizes URIs and identifier schemes.
```rust
control_plane.transition_state(&package_id, "canonicalize")?;
```

### 5. Provenance Receipt (Receipt)
Generates an Ed25519-signed receipt confirming the current state.
```rust
control_plane.transition_state(&package_id, "receipt")?;
```

### 6. Security Audit (Audit)
Final cryptographic check of the receipt chain.
```rust
control_plane.transition_state(&package_id, "audit")?;
```

### 7. Public Release (Release)
Marks the package as `released` and visible in the public registry index.
```rust
control_plane.transition_state(&package_id, "release")?;
```

### 8. Final Archival (Archive)
When a package is no longer maintained, archive it to ensure the historical graph remains immutable.
```rust
control_plane.transition_state(&package_id, "archive")?;
```

## Best Practices
- **Atomic Operations:** Always wrap multiple state transitions in a single logical transaction if your use case allows.
- **Fail Fast:** If a `validate` operator fails, don't attempt to `receipt` the artifact. The `RdfControlPlane` will block this, but catching it early saves compute resources.
- **Telemetry:** Monitor the transition latency. According to Vision 2030 SLOs, each operator should complete in under **1μs**.
