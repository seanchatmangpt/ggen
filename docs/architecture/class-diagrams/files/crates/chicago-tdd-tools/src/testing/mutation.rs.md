# `crates/chicago-tdd-tools/src/testing/mutation.rs`

Source SHA-256: `51102f7ce1c0a6760a639d71a294503344dc611e67cb6e190d09f5264e260190`

```mermaid
classDiagram
    class enum_MutationOperator {
      <<enum>>
    }
    class enum_CaseMode {
      <<enum>>
    }
    class struct_MutationTester {
      <<struct>>
      +"original: HashMap~String"
      +"mutations: Vec~MutationOperator~"
    }
    class struct_MutationScore {
      <<struct>>
      +"total: usize"
      +"caught: usize"
      +"score: f64"
    }
    class mod_tests {
      <<mod>>
    }
    note "MutationScore"
    note "MutationTester"
```

## Dependencies

- `std::collections::HashMap`
- `super::*`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
