# `crates/ggen-lsp/src/intel/receipt.rs`

Source SHA-256: `d62e450011fcf419f8073d8bd426f8a58663ed01a439c805c3f58471b0a0047a`

```mermaid
classDiagram
    class struct_RepairReceipt {
      <<struct>>
      +"version: u8"
      +"timestamp: DateTime~Utc~"
      +"diagnostic_event_id: String"
      +"diagnostic_code: String"
      +"pre_state_hash: [u8; 32]"
      +"post_state_hash: [u8; 32]"
      +"gate_pass: bool"
      +"signature_or_hash: [u8; 32]"
    }
    class fn_hash_content {
      <<fn>>
    }
    class mod_tests {
      <<mod>>
    }
    note "RepairReceipt"
```

## Dependencies

- `chrono::{DateTime, Utc}`
- `serde::{Deserialize, Serialize}`
- `std::fmt::Write as _`
- `super::*`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
