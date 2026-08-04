# `crates/praxis-graphlaw/src/chatman/abi.rs`

Source SHA-256: `3eff06e7df948271b0f920216a5cfd651bd869fca173bb0c2530232198c4b171`

```mermaid
classDiagram
    class struct_StageSeal {
      <<struct>>
    }
    class struct_InputHandles {
      <<struct>>
      +"nodes: Vec~String~"
      +"events: Vec~String~"
      +"plan_steps: Vec~String~"
    }
    class struct_InvocationEnvelope {
      <<struct>>
      +"invocation_id: InvocationId"
      +"snapshot_id: GraphSnapshotId"
      +"profile_id: ProfileId"
      +"operator_id: OperatorId"
      +"input_handles: InputHandles"
    }
    class fn_sorted_handles_digest {
      <<fn>>
    }
    class struct_Receipt {
      <<struct>>
      +"envelope: ReceiptEnvelope"
      +"canon_nquads: String"
    }
    class enum_Refusal {
      <<enum>>
    }
    class mod_refusal_variant_tests {
      <<mod>>
    }
    note "InvocationEnvelope"
    note "Receipt"
    note "Refusal"
    note "StageSeal"
```

## Dependencies

- `serde::{Deserialize, Serialize}`
- `wasm4pm_compat::hash::{blake3_combined, blake3_hex}`
- `wasm4pm_compat::receipt::Digest`
- `wasm4pm_compat::receipt::{ReceiptEnvelope, ReplayHint}`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
