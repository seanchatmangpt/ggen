# `examples/rmcp-verify/src/rmcp_handler_catalog.rs`

Source SHA-256: `54ba45ab7823ae545175ea860ac6f1184386bcf326ebf79c5b3239a261fb44a5`

```mermaid
classDiagram
    class struct_RmcpHandlerMethod {
      <<struct>>
      +"role: &'static str"
      +"method_name: &'static str"
      +"params_type: &'static str"
      +"return_type: &'static str"
      +"default_behavior: &'static str"
      +"source_file: &'static str"
    }
```

## Dependencies

- None observed.

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
