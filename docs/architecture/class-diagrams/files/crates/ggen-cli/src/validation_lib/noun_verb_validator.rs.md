# `crates/ggen-cli/src/validation_lib/noun_verb_validator.rs`

Source SHA-256: `4dc3ef79bd9aea19ea44036dde5a83e34fcc5a16e656caedd09353e1ceb8f787`

```mermaid
classDiagram
    class struct_NounVerbValidator {
      <<struct>>
      +"dependencies: HashMap~String"
      +"audit_trail: Vec~AuditEntry~"
    }
    class struct_AuditEntry {
      <<struct>>
      +"command: String"
      +"timestamp: String"
      +"dependencies: Vec~String~"
      +"success: bool"
    }
    class mod_tests {
      <<mod>>
    }
    note "NounVerbValidator"
```

## Dependencies

- `crate::validation_lib::error::{Result, ValidationError}`
- `std::collections::{HashMap, HashSet}`
- `super::*`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
