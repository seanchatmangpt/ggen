# `crates/bcinr-mfw-ir/src/contracts.rs`

Source SHA-256: `efc1144f85d03707e6b93e573940ce7ffbaf0bfbafbaa2554e37186f940f58f0`

```mermaid
classDiagram
    class enum_FormalStanding {
      <<enum>>
    }
    class struct_FormalLawRef {
      <<struct>>
      +"module: &'static str"
      +"declaration: &'static str"
      +"source_commit: Option~&'static str~"
      +"certification_digest: Option~Digest~"
      +"standing: FormalStanding"
    }
    class enum_ContractError {
      <<enum>>
    }
    class struct_SemanticOptimizationContract {
      <<struct>>
      +"law: FormalLawRef"
      +"consequence_horizon: ConsequenceHorizonId"
      +"transformation: TransformationProfileId"
      +"assumptions: Vec~String~"
    }
    class mod_tests {
      <<mod>>
    }
    note "FormalStanding"
    note "SemanticOptimizationContract"
    note "std::error::Error for ContractError"
    note "std::fmt::Display for ContractError"
```

## Dependencies

- `crate::digest::Digest`
- `crate::ids::{ConsequenceHorizonId, TransformationProfileId}`
- `super::*`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
