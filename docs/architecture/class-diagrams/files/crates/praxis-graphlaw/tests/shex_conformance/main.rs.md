# `crates/praxis-graphlaw/tests/shex_conformance/main.rs`

Source SHA-256: `45a34a01b46b0367911c24b790e0ceabecf8655139a00819e3e288ac84952ac0`

```mermaid
classDiagram
    class struct_ShapeMapEntry {
      <<struct>>
      +"node: String"
      +"shape: String"
    }
    class struct_ExpectedResult {
      <<struct>>
      +"status: String"
      +"details: String"
    }
    class struct_W3cCaseMeta {
      <<struct>>
      +"shape: String"
      +"focus: String"
      +"expected: String"
      +"source_schema: String"
      +"source_shexc: String"
      +"source_data: String"
      +"comment: String"
    }
    class struct_TestOutcome {
      <<struct>>
      +"name: String"
      +"passed: bool"
      +"expected: String"
      +"actual: String"
      +"detail: String"
    }
    class fn_build_data_index {
      <<fn>>
    }
    class fn_test_shex_conformance {
      <<fn>>
    }
    class fn_test_w3c_shex_conformance_suite {
      <<fn>>
    }
```

## Dependencies

- `praxis_graphlaw::parser::{Parser, Syntax}`
- `praxis_graphlaw::shex::validate_shex`
- `praxis_graphlaw::tripleindex::TripleIndex`
- `std::fs`
- `std::path::Path`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
