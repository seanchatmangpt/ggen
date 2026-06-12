<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Reference: 8-Operator Lifecycle States](#reference-8-operator-lifecycle-states)
  - [Lifecycle Operators & State Map](#lifecycle-operators--state-map)
  - [Property Mappings](#property-mappings)
  - [Error Codes](#error-codes)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Reference: 8-Operator Lifecycle States

This document provides a technical reference for the states and operators used in the `ggen` marketplace governance model.

## Lifecycle Operators & State Map

The following table defines the legal transitions in the `RdfControlPlane` state machine.

| Operator | Input State | Output State | Description |
| :--- | :--- | :--- | :--- |
| `admit` | `draft` | `admitted` | Registers intent and admits the resource to the system. |
| `breed` | `admitted` | `bred` | Expands internal dependencies and related semantic nodes. |
| `validate` | `bred` | `validated` | Enforces SHACL constraints and structural integrity. |
| `canonicalize`| `validated` | `canonicalized`| Normalizes identifiers and ensures deterministic sorting. |
| `receipt` | `canonicalized`| `receipted` | Signs the artifact state with an Ed25519 key. |
| `audit` | `receipted` | `audited` | Verifies the full provenance chain and cryptographic proofs. |
| `release` | `audited` | `released` | Makes the resource visible in the global registry. |
| `archive` | `released` | `archived` | Marks the resource as immutable and deprecated. |

## Property Mappings

The state is stored in the RDF graph using the following property:
- **Property:** `https://ggen.io/marketplace/currentLifecycleState`
- **Range:** `xsd:string`

## Error Codes
If a transition is attempted out of order, the `RdfControlPlane` returns `ControlPlaneError::StateTransitionError`.

```rust
// Example Error Result
Err(StateTransitionError { 
    reason: "Invalid lifecycle transition: bred via receipt" 
})
```
Vision 2030 requires that every state transition violation is logged as an **FMEA Failure Mode (FM-008)** for security monitoring.
