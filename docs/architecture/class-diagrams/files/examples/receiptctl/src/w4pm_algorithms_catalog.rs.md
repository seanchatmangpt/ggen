# `examples/receiptctl/src/w4pm_algorithms_catalog.rs`

Source SHA-256: `de5d61367a85323e4890662af080d057f92709734568408188a4a05cfbb1d39d`

```mermaid
classDiagram
    class enum_AlgorithmId {
      <<enum>>
    }
    class struct_AlgorithmInfo {
      <<struct>>
      +"id: AlgorithmId"
      +"algorithm_id: &'static str"
      +"label: &'static str"
      +"category: &'static str"
      +"wasm_export: &'static str"
      +"cli_alias: &'static str"
    }
    class fn_by_wasm_export {
      <<fn>>
    }
    note "AlgorithmId"
```

## Dependencies

- None observed.

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
