# `crates/ggen-graph/src/rwr/autonomic.rs`

Source SHA-256: `d52e5a6dd128b26e2cc63e9c2e1cc123acd3a487acb0b1b3b6b55ea5441ba36a`

```mermaid
classDiagram
    class fn_put_len_prefixed {
      <<fn>>
    }
    class struct_ManagedCell {
      <<struct>>
      +"desired_state: Vec~u8~"
      +"current_action_id: String"
    }
    class struct_AutonomicCycleReceipt {
      <<struct>>
      +"schema: String"
      +"initial_state_digest: [u8; 32]"
      +"final_state_digest: [u8; 32]"
      +"cycles: u8"
      +"actuation_receipts: Vec~ActuationReceipt~"
      +"converged: bool"
      +"receipt_digest: [u8; 32]"
    }
    class fn_cycle_digest {
      <<fn>>
    }
    class struct_AutonomicController {
      <<struct>>
      +"max_cycles: u8"
    }
    class fn_read_committed_state {
      <<fn>>
    }
    class enum_AutonomicError {
      <<enum>>
    }
    note "AutonomicController"
    note "AutonomicCycleReceipt"
    note "ManagedCell"
```

## Dependencies

- `crate::rwr::execution::{ Action, ActuationReceipt, ExecutionError, FilesystemActuator, FoundationMachine, }`
- `crate::rwr::matrix::Dimension`
- `serde::{Deserialize, Serialize}`
- `std::fs::File`
- `std::io::Read`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
