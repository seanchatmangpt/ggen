# `crates/ggen-graph/src/bin/gall_observe_doctests.rs`

Source SHA-256: `29320eed2c3b89b50754e999710c319650e5235f5779d6fa8accc49b89a53aeb`

```mermaid
classDiagram
    class struct_DoctestPayload {
      <<struct>>
      +"timestamp: String"
      +"status: String"
      +"exit_code: i32"
      +"passed_count: u32"
      +"failed_count: u32"
    }
    class fn_main {
      <<fn>>
    }
```

## Dependencies

- `chrono::Utc`
- `oxigraph::io::{RdfFormat, RdfParser}`
- `oxigraph::model::Term`
- `oxigraph::sparql::QueryResults`
- `oxigraph::store::Store`
- `serde::{Deserialize, Serialize}`
- `std::fs::File`
- `std::process::Command`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
