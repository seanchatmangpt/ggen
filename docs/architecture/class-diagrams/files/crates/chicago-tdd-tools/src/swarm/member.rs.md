# `crates/chicago-tdd-tools/src/swarm/member.rs`

Source SHA-256: `07ec3aef7d020a4710b77b86edf08c43eb820318946a41167d82880e11e39257`

```mermaid
classDiagram
    class enum_MemberState {
      <<enum>>
    }
    class struct_SwarmMember {
      <<struct>>
      +"id: String"
      +"name: String"
      +"sectors: Vec~String~"
      +"state: MemberState"
      +"capacity: u32"
      +"current_tasks: u32"
      +"ontologies: HashMap~String"
      +"last_heartbeat: String"
      +"reputation: u32"
    }
    class mod_tests {
      <<mod>>
    }
    note "SwarmMember"
    note "std::fmt::Display for MemberState"
```

## Dependencies

- `serde::{Deserialize, Serialize}`
- `std::collections::HashMap`
- `super::*`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
