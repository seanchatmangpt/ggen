# `crates/praxis-graphlaw/tests/chatman_e2e_pipeline.rs`

Source SHA-256: `449659533502b515ced68a3f148b1c78599d1fe65c273fc399330ae5403fd874`

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

- `chicago_tdd_tools::prelude::*`
- `praxis_graphlaw::chatman::abi::{ GraphSnapshotId, InputHandles, InvocationEnvelope, InvocationId, OperatorId, ProfileId, Refusal, }`
- `praxis_graphlaw::chatman::engine::{AdmissionSpec, ChatmanEngine, EngineProfile}`
- `praxis_graphlaw::chatman::router::ProfileGates`
- `praxis_graphlaw::chatman::triple8::ProfileSymbolTable`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
