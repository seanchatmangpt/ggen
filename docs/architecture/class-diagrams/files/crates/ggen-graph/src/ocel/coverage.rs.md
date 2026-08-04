# `crates/ggen-graph/src/ocel/coverage.rs`

Source SHA-256: `11a556ea89eb4ce345c8394ee267868b0ccf3ffc15086bc7ca0c819799ff8460`

```mermaid
classDiagram
    class struct_RequirementEvidence {
      <<struct>>
      +"id: String"
      +"title: String"
      +"description: String"
      +"source_files: Vec~String~"
      +"test_files: Vec~String~"
      +"commands: Vec~String~"
    }
    class struct_CoverageMatrix {
      <<struct>>
      +"requirements: Vec~RequirementEvidence~"
    }
    class fn_generate_coverage_matrix {
      <<fn>>
    }
```

## Dependencies

- `serde::{Deserialize, Serialize}`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
