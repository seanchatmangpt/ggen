# `crates/ggen-graph/src/bin/gall_adjudicate_witnessed_truthfulness.rs`

Source SHA-256: `2e73471a8254d9b08567d0ca464923c5a5c98517b75e0a504a071e6da5f7db47`

```mermaid
classDiagram
    class struct_AdjudicationPayload {
      <<struct>>
      +"timestamp: String"
      +"verdict: String"
      +"reason: String"
      +"integrity_status: String"
    }
    class struct_SignedAdjudicationPayload {
      <<struct>>
      +"timestamp: String"
      +"verdict: String"
      +"reason: String"
      +"integrity_status: String"
      +"witness_adjudication_blake3_receipt: String"
    }
    class fn_check_validation_report_conforms {
      <<fn>>
    }
    class fn_main {
      <<fn>>
    }
```

## Dependencies

- `chrono::Utc`
- `oxigraph::io::{RdfFormat, RdfParser}`
- `oxigraph::model::Term`
- `oxigraph::store::Store`
- `serde::{Deserialize, Serialize}`
- `std::fs::File`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
