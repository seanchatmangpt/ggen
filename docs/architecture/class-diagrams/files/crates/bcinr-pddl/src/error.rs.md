# `crates/bcinr-pddl/src/error.rs`

Source SHA-256: `6dace69587fcc4d53bea67c1c74d77427349f52b01be3d17083cf64a8f656556`

```mermaid
classDiagram
    class enum_Pddl8Error {
      <<enum>>
    }
    class mod_tests {
      <<mod>>
    }
    note "From~bcinr_mfw_ir::PlannerFailure~ for Pddl8Error"
    note "std::error::Error for Pddl8Error"
    note "std::fmt::Display for Pddl8Error"
```

## Dependencies

- `bcinr_mfw_ir::PlannerOutcome`
- `bcinr_mfw_ir::{ BoundHit, BoundKind, Digest, ExhaustionWitness, PlannerFailure, SearchProfileId, }`
- `super::*`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
