# `crates/ggen-marketplace/src/marketplace/error.rs`

Source SHA-256: `15b4f6c1e5f19556daad1c7ab9852840230acc30f717a20ef76e6a231335fc2f`

```mermaid
classDiagram
    class type_Result {
      <<type>>
    }
    class enum_Error {
      <<enum>>
    }
    class struct_ErrorContext {
      <<struct>>
      +"operation: String"
      +"context: String"
    }
    class mod_tests {
      <<mod>>
    }
    note "Error"
    note "ErrorContext"
    note "From~ggen_config::receipt::ReceiptError~ for Error"
    note "From~oxigraph::sparql::SparqlSyntaxError~ for Error"
    note "fmt::Display for ErrorContext"
```

## Dependencies

- `std::fmt`
- `super::*`
- `thiserror::Error`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
