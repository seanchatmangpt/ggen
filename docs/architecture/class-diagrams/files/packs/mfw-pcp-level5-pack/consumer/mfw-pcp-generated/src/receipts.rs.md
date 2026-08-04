# `packs/mfw-pcp-level5-pack/consumer/mfw-pcp-generated/src/receipts.rs`

Source SHA-256: `9e4b0512877c687a19ee2a68ea88e808775defa9fe4ffd96b1a2cfa7cce9bf3e`

```mermaid
classDiagram
    class struct_OpenReceipt {
      <<struct>>
      +"sequence: u64"
    }
    class struct_CloseReceipt {
      <<struct>>
      +"sequence: u64"
    }
    class struct_Level5GenerationReceipt {
      <<struct>>
      +"source_graph_digest: String"
      +"shapes_digest: String"
      +"query_inventory_digest: String"
      +"template_inventory_digest: String"
      +"ggen_binary_digest: String"
      +"output_inventory_digest: String"
      +"falsifier_report_digest: String"
      +"replay_report_digest: String"
      +"verifier_report_digest: String"
    }
    class fn_canonical_digest {
      <<fn>>
    }
```

## Dependencies

- `serde::{Deserialize, Serialize}`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
