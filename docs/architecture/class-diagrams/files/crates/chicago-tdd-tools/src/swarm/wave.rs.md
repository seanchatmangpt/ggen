# `crates/chicago-tdd-tools/src/swarm/wave.rs`

Source SHA-256: `3e33c9b98c5b5967bae74dcbd4bef2e77a83099d40e67a7c07e34d7250522dbb`

```mermaid
classDiagram
    class enum_WaveStatus {
      <<enum>>
    }
    class struct_ResidualClass {
      <<struct>>
      +"code: String"
      +"description: String"
      +"severity: Severity"
    }
    class struct_WavePhase {
      <<struct>>
      +"name: String"
      +"tasks: Vec~TaskRequest~"
    }
    class struct_Wave {
      <<struct>>
      +"id: String"
      +"phases: Vec~WavePhase~"
      +"status: WaveStatus"
      +"metadata: HashMap~String"
    }
    class struct_WaveReceipt {
      <<struct>>
      +"wave_id: String"
      +"phase_receipts: Vec~PhaseReceipt~"
      +"status: WaveStatus"
      +"residual_classes: Vec~ResidualClass~"
      +"total_execution_time_ms: u64"
    }
    class struct_PhaseReceipt {
      <<struct>>
      +"phase_name: String"
      +"task_receipts: Vec~TaskReceipt~"
    }
    note "Wave"
    note "WaveReceipt"
```

## Dependencies

- `crate::core::governance::Severity`
- `crate::swarm::task::{TaskReceipt, TaskRequest}`
- `serde::{Deserialize, Serialize}`
- `std::collections::HashMap`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
