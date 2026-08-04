# `crates/cpmp/src/error.rs`

Source SHA-256: `28ad3f2c691dbdf244a1b7d07c795862867c67c6a3f551b39d37ab1349bbb6e1`

```mermaid
classDiagram
    class type_Result {
      <<type>>
    }
    class enum_MapError {
      <<enum>>
    }
    note "From~rusqlite::Error~ for MapError"
    note "From~serde_json::error::Error~ for MapError"
    note "fmt::Display for MapError"
    note "std::error::Error for MapError"
```

## Dependencies

- `std::fmt`
- `std::io`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
