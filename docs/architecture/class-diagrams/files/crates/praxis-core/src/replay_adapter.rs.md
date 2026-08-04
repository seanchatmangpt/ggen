# `crates/praxis-core/src/replay_adapter.rs`

Source SHA-256: `1521577dbd3fecbfa60f18f5e92f7dd70008c8f13e72e39037db24b7dcdef04c`

```mermaid
classDiagram
    class enum_LifecycleStep {
      <<enum>>
    }
    class fn_lifecycle_frame {
      <<fn>>
    }
    class fn_replay_lifecycle {
      <<fn>>
    }
    class fn_replay_receipt_lifecycle {
      <<fn>>
    }
    class mod_tests {
      <<mod>>
    }
    note "LifecycleStep"
```

## Dependencies

- `bcinr_powl_receipt::{ conformance::ConformanceMetrics, replay::{PowlReplayFrame, PowlReplayVerifier, ReplayViolation}, }`
- `crate::receipt_record::ReceiptRecord`
- `super::*`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
