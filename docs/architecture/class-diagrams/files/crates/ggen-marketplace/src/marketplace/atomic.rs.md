# `crates/ggen-marketplace/src/marketplace/atomic.rs`

Source SHA-256: `c62d31753ee73762db50b7e2f8d983d834c749dd1a9594b64e438d9d06ccfb00`

```mermaid
classDiagram
    class enum_AtomicPackClass {
      <<enum>>
    }
    class enum_AtomicPackCategory {
      <<enum>>
    }
    class struct_AtomicPackId {
      <<struct>>
      +"class: AtomicPackClass"
      +"name: String"
    }
    class fn_foundation_packs {
      <<fn>>
    }
    class mod_tests {
      <<mod>>
    }
    note "AtomicPackClass"
    note "AtomicPackId"
    note "fmt::Display for AtomicPackId"
    note "std::str::FromStr for AtomicPackId"
```

## Dependencies

- `serde::{Deserialize, Serialize}`
- `std::fmt`
- `super::*`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
