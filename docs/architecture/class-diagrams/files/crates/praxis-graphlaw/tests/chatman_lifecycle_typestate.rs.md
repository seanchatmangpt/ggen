# `crates/praxis-graphlaw/tests/chatman_lifecycle_typestate.rs`

Source SHA-256: `b1fc27bc23b52761c4f85591348246ef7cbd3a99dec77d5fb3f598da34024b70`

```mermaid
classDiagram
    class fn_build_profile {
      <<fn>>
    }
    class fn_envelope {
      <<fn>>
    }
```

## Dependencies

- `chicago_tdd_tools::assert_matches`
- `chicago_tdd_tools::prelude::*`
- `praxis_graphlaw::chatman::abi::{ GraphSnapshotId, InputHandles, InvocationEnvelope, InvocationId, OperatorId, ProfileId, Refusal, }`
- `praxis_graphlaw::chatman::engine::{AdmissionSpec, ChatmanEngine, EngineProfile}`
- `praxis_graphlaw::chatman::router::ProfileGates`
- `praxis_graphlaw::chatman::triple8::ProfileSymbolTable`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
