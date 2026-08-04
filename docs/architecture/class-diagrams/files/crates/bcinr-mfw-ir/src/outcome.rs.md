# `crates/bcinr-mfw-ir/src/outcome.rs`

Source SHA-256: `f5bf00303e3ec38452d2f8a34c615f0e6f0fdec043804624024e01cfdbd79618`

```mermaid
classDiagram
    class enum_PlannerOutcome {
      <<enum>>
    }
    class enum_PlannerFailure {
      <<enum>>
    }
    class struct_ExhaustionWitness {
      <<struct>>
      +"search_profile: SearchProfileId"
      +"explored_states: u64"
      +"frontier_empty: bool"
      +"digest: Digest"
    }
    class enum_BoundKind {
      <<enum>>
    }
    class struct_BoundHit {
      <<struct>>
      +"kind: BoundKind"
      +"limit: u64"
      +"observed: u64"
    }
    class struct_UnsupportedFeature {
      <<struct>>
      +"feature_name: String"
      +"context: String"
    }
    class struct_InconsistencyWitness {
      <<struct>>
      +"kind_name: String"
      +"context: String"
      +"digest: Digest"
    }
    class mod_tests {
      <<mod>>
    }
    note "PlannerOutcome~T~"
    note "std::error::Error for PlannerFailure"
    note "std::fmt::Display for PlannerFailure"
```

## Dependencies

- `crate::digest::Digest`
- `crate::ids::SearchProfileId`
- `super::*`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
