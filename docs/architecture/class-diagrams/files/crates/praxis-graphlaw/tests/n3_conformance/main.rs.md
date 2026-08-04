# `crates/praxis-graphlaw/tests/n3_conformance/main.rs`

Source SHA-256: `103389a815b5d6edf4e99999e3fa0caf14d8b8a4512d4ba18986c7db4e5ef1ba`

```mermaid
classDiagram
    class struct_Manifest {
      <<struct>>
      +"conformance_suites: Vec~Suite~"
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
      +"input: String"
      +"expected: String"
      +"status: String"
      +"blocked_reason: Option~String~"
      +"prove_goal: Option~[String; 3]~"
      +"prove_goal_n3: Option~String~"
      +"solve_goal: Option~String~"
      +"solve_expected_bindings: Option~Vec~std::collections::HashMap~String"
      +"expect_no_denial_violations: bool"
    }
    class struct_CaseResult {
      <<struct>>
      +"suite_name: String"
      +"id: String"
      +"name: String"
      +"passed: bool"
      +"detail: String"
    }
    class fn_decoded_fact_set {
      <<fn>>
    }
    class fn_run_case {
      <<fn>>
    }
    class fn_test_n3_conformance {
      <<fn>>
    }
    class fn_write_manifest_report {
      <<fn>>
    }
```

## Dependencies

- `praxis_graphlaw::TripleStore`
- `serde::Deserialize`
- `std::collections::HashSet`
- `std::fs`
- `std::path::{Path, PathBuf}`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
