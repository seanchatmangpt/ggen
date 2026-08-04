# `boilerplate/crates/domain/src/value_objects.rs`

Source SHA-256: `5e3c122831d4fd5239bc96426de0de116d1bed3af8d98629233bcef640a02088`

```mermaid
classDiagram
    class struct_Name {
      <<struct>>
    }
    class struct_Email {
      <<struct>>
    }
    class struct_Slug {
      <<struct>>
    }
    class mod_tests {
      <<mod>>
    }
    note "Email"
    note "Name"
    note "Slug"
    note "std::fmt::Display for Email"
    note "std::fmt::Display for Name"
    note "std::fmt::Display for Slug"
```

## Dependencies

- `bp_core::{error::CoreError, Result}`
- `proptest::prelude::*`
- `super::*`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
