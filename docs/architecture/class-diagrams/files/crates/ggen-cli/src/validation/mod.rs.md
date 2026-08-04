# `crates/ggen-cli/src/validation/mod.rs`

Source SHA-256: `f3882f3e4b73f2d3225577cae7e5e7ca17aa301b373827cca6b116ffbe624a7d`

```mermaid
classDiagram
    class struct_CommandValidator {
      <<struct>>
    }
    class struct_TestConfigValidator {
      <<struct>>
    }
    class enum_ValidationError {
      <<enum>>
    }
    class mod_tests {
      <<mod>>
    }
    note "CommandValidator"
    note "TestConfigValidator"
    note "std::error::Error for ValidationError"
    note "std::fmt::Display for ValidationError"
```

## Dependencies

- `$crate::validation::CommandValidator`
- `std::collections::HashMap`
- `std::path::Path`
- `super::*`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
