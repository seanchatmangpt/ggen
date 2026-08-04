# `crates/ggen-architecture/src/certification/standards.rs`

Source SHA-256: `4ff2815a1d97367076d1ccac35122f6f5aff7312fb37b302a3410465590bc5e0`

```mermaid
classDiagram
    class enum_StandardAuthority {
      <<enum>>
    }
    class enum_StandardStatus {
      <<enum>>
    }
    class struct_StandardCheckpoint {
      <<struct>>
      +"id: String"
      +"authority: StandardAuthority"
      +"status: StandardStatus"
      +"law: String"
      +"falsifier: String"
    }
    class struct_StandardsProfile {
      <<struct>>
      +"id: String"
      +"version: String"
      +"observation_window: String"
      +"governing_equation: String"
      +"root_broker: String"
      +"standing_lattice: Vec~String~"
      +"testing_bblock: TestingBblockProtocol"
      +"checkpoints: Vec~StandardCheckpoint~"
      +"exclusions: Vec~String~"
    }
    class fn_seven_day_standards_profile {
      <<fn>>
    }
    class enum_StandardsProfileRefusal {
      <<enum>>
    }
    note "StandardsProfile"
```

## Dependencies

- `serde::{Deserialize, Serialize}`
- `std::collections::BTreeSet`
- `super::{digest, testing_bblock_protocol, TestingBblockProtocol, TestingBblockRefusal}`
- `thiserror::Error`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
