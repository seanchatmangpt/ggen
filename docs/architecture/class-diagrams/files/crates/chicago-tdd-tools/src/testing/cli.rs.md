# `crates/chicago-tdd-tools/src/testing/cli.rs`

Source SHA-256: `fbdefde0c8619375816e43788dc37e6503010bb1f72e25b44a41cedff0530cbc`

```mermaid
classDiagram
    class struct_CliTest {
      <<struct>>
    }
    class struct_CliCommandBuilder {
      <<struct>>
      +"binary: String"
      +"args: Vec~String~"
      +"env: HashMap~String"
    }
    class struct_CliAssertions {
      <<struct>>
    }
    class struct_CliEnvironment {
      <<struct>>
      +"vars: HashMap~String"
      +"original_vars: HashMap~String"
    }
    class mod_tests {
      <<mod>>
    }
    note "CliAssertions"
    note "CliCommandBuilder"
    note "CliEnvironment"
    note "CliTest"
    note "Default for CliEnvironment"
    note "Drop for CliEnvironment"
```

## Dependencies

- `std::collections::HashMap`
- `super::{CliAssertions, CliCommandBuilder, CliEnvironment, CliTest}`
- `trycmd::TestCases`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
