# `tools/ggen-architecture/src/model.rs`

Source SHA-256: `17c75a4006c4aa92d2ad3698c053110f15fc7174b816869cdee59e6cea698241`

```mermaid
classDiagram
    class enum_AssetKind {
      <<enum>>
    }
    class enum_LifecycleState {
      <<enum>>
    }
    class enum_Standing {
      <<enum>>
    }
    class enum_Severity {
      <<enum>>
    }
    class struct_ArchitectureAsset {
      <<struct>>
      +"id: String"
      +"name: String"
      +"kind: AssetKind"
      +"version: Option~String~"
      +"owner: Option~String~"
      +"lifecycle: LifecycleState"
      +"standing: Standing"
      +"dependencies: BTreeSet~String~"
      +"replaces: BTreeSet~String~"
      +"attributes: BTreeMap~String"
    }
    class struct_TransitionStep {
      <<struct>>
      +"asset_id: String"
      +"from: LifecycleState"
      +"to: LifecycleState"
      +"prerequisites: Vec~String~"
    }
    note "ArchitectureAsset"
    note "LifecycleState"
    note "fmt::Display for LifecycleState"
```

## Dependencies

- `serde::{Deserialize, Serialize}`
- `std::{ collections::{BTreeMap, BTreeSet}, fmt, }`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
