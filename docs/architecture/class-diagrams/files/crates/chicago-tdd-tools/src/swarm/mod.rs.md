# `crates/chicago-tdd-tools/src/swarm/mod.rs`

Source SHA-256: `dd6d01d19f7c75f5f283f9dfb454915e679eb455c236e62b41122a41f68a49ed`

```mermaid
classDiagram
    class mod_composition {
      <<mod>>
    }
    class mod_coordinator {
      <<mod>>
    }
    class mod_member {
      <<mod>>
    }
    class mod_task {
      <<mod>>
    }
    class mod_test_orchestrator {
      <<mod>>
    }
    class mod_wave {
      <<mod>>
    }
    class mod_tests {
      <<mod>>
    }
```

## Dependencies

- `composition::{ComposedOperation, OperationChain}`
- `coordinator::{SwarmCoordinator, SwarmMembership}`
- `member::SwarmMember`
- `super::*`
- `task::{TaskReceipt, TaskRequest, TaskStatus}`
- `test_orchestrator::{ QoSClass, ResourceBudget, TestOrchestrator, TestPlan, TestPlanningAPI, }`
- `wave::{ResidualClass, Wave, WavePhase, WaveReceipt, WaveStatus}`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
