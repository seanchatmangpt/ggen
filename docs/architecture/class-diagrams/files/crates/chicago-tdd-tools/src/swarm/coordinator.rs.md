# `crates/chicago-tdd-tools/src/swarm/coordinator.rs`

Source SHA-256: `b477f8aa58ac166126c73e5034f478b1294d51dfd8ac5bd505b755cd2db61993`

```mermaid
classDiagram
    class struct_SwarmMembership {
      <<struct>>
      +"swarm_id: String"
      +"members: HashMap~String"
    }
    class struct_SwarmCoordinator {
      <<struct>>
      +"membership: SwarmMembership"
      +"task_queue: TaskQueue"
      +"task_assignments: HashMap~String"
      +"consensus_threshold: f32"
    }
    class struct_SwarmStatus {
      <<struct>>
      +"swarm_id: String"
      +"total_members: usize"
      +"active_members: usize"
      +"total_capacity: u32"
      +"current_tasks: u32"
      +"queued_tasks: usize"
      +"completed_tasks: usize"
    }
    class mod_tests {
      <<mod>>
    }
    note "Default for SwarmCoordinator"
    note "SwarmCoordinator"
    note "SwarmMembership"
```

## Dependencies

- `serde::{Deserialize, Serialize}`
- `std::collections::HashMap`
- `super::*`
- `super::member::SwarmMember`
- `super::task::{TaskQueue, TaskReceipt, TaskRequest}`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
