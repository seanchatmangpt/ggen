# `examples/receiptctl/src/wasm4pm_facts_registry.rs`

Source SHA-256: `9b47642a7a37528bff37025db3e9e0a2b09e3df7197fbf6e278f63c195c030f6`

```mermaid
classDiagram
    class struct_Breed {
      <<generated>>
      +"id: &'static str"
      +"label: &'static str"
      +"doc: &'static str"
      +"citation: &'static str"
    }
    class struct_Algorithm {
      <<generated>>
      +"id: &'static str"
      +"label: &'static str"
      +"doc: &'static str"
      +"citation: &'static str"
      +"category: &'static str"
      +"output_type: &'static str"
      +"speed_tier: u32"
      +"quality_tier: u32"
      +"wasm_export: &'static str"
      +"input_format: &'static str"
      +"standing: &'static str"
    }
    class fn_lookup_breed {
      <<generated>>
    }
    class fn_lookup_algorithm {
      <<generated>>
    }
    class fn_algorithms_by_category {
      <<generated>>
    }
    class fn_breed_count {
      <<generated>>
    }
    class fn_algorithm_count {
      <<generated>>
    }
```

## Dependencies

- None observed.

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
