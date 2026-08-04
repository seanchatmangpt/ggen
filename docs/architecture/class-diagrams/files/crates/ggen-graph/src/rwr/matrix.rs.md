# `crates/ggen-graph/src/rwr/matrix.rs`

Source SHA-256: `fdfc769038a5c700492683ec0b6b605bb0c0e6db69f7d0efd46309842a3fd8d5`

```mermaid
classDiagram
    class enum_MaturityLevel {
      <<enum>>
    }
    class enum_RwrDomain {
      <<enum>>
    }
    class enum_EvidenceSurface {
      <<enum>>
    }
    class enum_Dimension {
      <<enum>>
    }
    class struct_DimensionContract {
      <<struct>>
      +"dimension: Dimension"
      +"domain: RwrDomain"
      +"level5_outcome: &'static str"
      +"required_surfaces: &'static [EvidenceSurface]"
    }
    class fn_all_contracts {
      <<fn>>
    }
    note "MaturityLevel"
```

## Dependencies

- `Dimension as D`
- `EvidenceSurface as S`
- `RwrDomain as R`
- `serde::{Deserialize, Serialize}`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
