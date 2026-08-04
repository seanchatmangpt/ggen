# `crates/praxis-core/src/receipt_record.rs`

Source SHA-256: `8083626c0b268b060534c320912526587107f30f221cd3be7feb98012df005e8`

```mermaid
classDiagram
    class struct_ReceiptRecord {
      <<struct>>
      +"version: u32"
      +"instruction_id: u64"
      +"activity_idx: u16"
      +"activity: Option~String~"
      +"node_kind: u8"
      +"ts_ns: u64"
      +"duration_ms: Option~u64~"
      +"payload_hash_hex: String"
      +"prev_chain_hash_hex: String"
      +"chain_hash_hex: String"
      +"andon: Andon"
      +"obligation_count: u32"
      +"object_ids: Vec~String~"
      +"signature_hex: Option~String~"
      +"schema: String"
      +"v2: Option~crate::receipt_epoch::ReceiptEpochV2~"
    }
    class fn_decode_hex32 {
      <<fn>>
    }
    class mod_tests {
      <<mod>>
    }
    note "ReceiptRecord"
```

## Dependencies

- `crate::{ error::CoreError, law::{build_admission_frame, chain_from_frame, Andon, ReceiptMeta}, }`
- `serde::{Deserialize, Serialize}`
- `super::*`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
