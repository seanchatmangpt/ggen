# `crates/praxis-graphlaw/tests/shacl_conformance/w3c_runner.rs`

Source SHA-256: `eefa0af5f8702e250d30c2a3afdeebff2c10d1c3a1484aaf77a631e0e792f8c0`

```mermaid
classDiagram
    class struct_Case {
      <<struct>>
      +"name: String"
      +"meta: String"
      +"data: String"
      +"shapes: String"
    }
    class fn_discover_cases {
      <<fn>>
    }
    class fn_suite_dir {
      <<fn>>
    }
    class fn_resolve_relative_self_refs {
      <<fn>>
    }
    class fn_build_index {
      <<fn>>
    }
    class fn_expected_from_manifest {
      <<fn>>
    }
    class struct_CaseOutcome {
      <<struct>>
      +"name: String"
      +"passed: bool"
      +"detail: String"
    }
    class fn_test_w3c_core_constraint_component_suite {
      <<fn>>
    }
    class fn_write_manifest_report {
      <<fn>>
    }
```

## Dependencies

- `praxis_graphlaw::parser::{Parser, Syntax}`
- `praxis_graphlaw::shacl::{ShapesGraph, Validator}`
- `praxis_graphlaw::tripleindex::TripleIndex`
- `std::fs`
- `std::path::{Path, PathBuf}`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
