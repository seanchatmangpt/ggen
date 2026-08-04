# `marketplace/packages/hello-world/src/lib.rs`

Source SHA-256: `03ac0eaea5ca4eb23a681c29d5be44bc6e588c04d436dd5a14bf2c2416b45095`

```mermaid
classDiagram
    class struct_HelloConfig {
      <<struct>>
      +"greeting: String"
      +"name: String"
      +"repeat_count: usize"
    }
    class struct_HelloWorld {
      <<struct>>
      +"config: HelloConfig"
    }
    class mod_utils {
      <<mod>>
    }
    class mod_examples {
      <<mod>>
    }
    class mod_tests {
      <<mod>>
    }
    note "Default for HelloConfig"
    note "Default for HelloWorld"
    note "HelloWorld"
```

## Dependencies

- `anyhow::Result`
- `serde::{Deserialize, Serialize}`
- `super::*`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
