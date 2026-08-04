# `crates/powl2-decompose/src/external_cut.rs`

Source SHA-256: `1ffb96b121ee54a77df3ef7efa04ec826d45624b81956115b4d2a854110a45b2`

```mermaid
classDiagram
    class enum_ExternalCutRefusal {
      <<enum>>
    }
    class fn_validate_external_cut {
      <<fn>>
    }
    class fn_validate_region {
      <<fn>>
    }
    class mod_tests {
      <<mod>>
    }
    note "std::error::Error for ExternalCutRefusal"
    note "std::fmt::Display for ExternalCutRefusal"
```

## Dependencies

- `crate::powl::Powl`
- `serde::{Deserialize, Serialize}`
- `super::*`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
