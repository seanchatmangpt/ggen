# `crates/chicago-tdd-tools/src/swarm/task.rs`

Source SHA-256: `a67c1bd3f58ec42eab046add2a52b91627270b21ec71350273643d3132b4d435`

```mermaid
classDiagram
    class enum_TaskStatus {
      <<enum>>
    }
    class struct_TaskRequest {
      <<struct>>
      +"id: String"
      +"sectors: Vec~String~"
      +"operation: String"
      +"input: String"
      +"priority: u32"
      +"deadline: String"
    }
    class struct_TaskReceipt {
      <<struct>>
      +"task_id: String"
      +"agent_id: String"
      +"sectors: Vec~String~"
      +"status: TaskStatus"
      +"result: String"
      +"execution_time_ms: u64"
      +"timestamp: String"
      +"result_merkle: String"
      +"metadata: HashMap~String"
    }
    class struct_TaskQueue {
      <<struct>>
      +"tasks: Vec~TaskRequest~"
      +"receipts: Vec~TaskReceipt~"
    }
    class mod_tests {
      <<mod>>
    }
    note "Default for TaskQueue"
    note "TaskQueue"
    note "TaskReceipt"
    note "TaskRequest"
    note "std::fmt::Display for TaskStatus"
```

## Dependencies

- `serde::{Deserialize, Serialize}`
- `std::collections::HashMap`
- `super::*`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
