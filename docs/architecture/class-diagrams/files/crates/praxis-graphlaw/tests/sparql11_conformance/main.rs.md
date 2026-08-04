# `crates/praxis-graphlaw/tests/sparql11_conformance/main.rs`

Source SHA-256: `9ceef62a61ee86f5055507def2f66dc8f13f50c5a8fe1f434cc7ed1ad669b96b`

```mermaid
classDiagram
    class struct_Manifest {
      <<struct>>
      +"provenance: Provenance"
      +"conformance_suites: Vec~Suite~"
    }
    class struct_Provenance {
      <<struct>>
      +"source: String"
      +"description: String"
      +"last_synced: String"
    }
    class struct_Suite {
      <<struct>>
      +"suite_name: String"
      +"test_cases: Vec~TestCase~"
    }
    class struct_TestCase {
      <<struct>>
      +"id: String"
      +"name: String"
      +"features: Vec~String~"
      +"data: String"
      +"query: String"
      +"expected: String"
      +"status: String"
    }
    class fn_test_w3c_sparql11_conformance {
      <<fn>>
    }
    class fn_compare_results {
      <<fn>>
    }
    class fn_clean_val {
      <<fn>>
    }
```

## Dependencies

- `praxis_graphlaw::parser::{Parser, Syntax}`
- `praxis_graphlaw::sparql::{eval_query, evaluate_plan_and_debug, Binding}`
- `praxis_graphlaw::tripleindex::TripleIndex`
- `spargebra::Query`
- `std::collections::HashMap`
- `std::fs`
- `std::path::Path`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
