# `crates/ggen-marketplace/src/marketplace/bundle.rs`

Source SHA-256: `b225d1157a7e15efbacc3c24d62d392b5e30375a787a8dbb67abdf7689e6ae50`

```mermaid
classDiagram
    class type_BundleId {
      <<type>>
    }
    class struct_Bundle {
      <<struct>>
      +"id: BundleId"
      +"name: String"
      +"atomic_packs: Vec~AtomicPackId~"
      +"description: String"
      +"explicit_runtime: Option~String~"
      +"version_requirement: Option~String~"
    }
    class struct_Bundles {
      <<struct>>
    }
    class mod_tests {
      <<mod>>
    }
    note "Bundle"
    note "Bundles"
```

## Dependencies

- `crate::marketplace::atomic::AtomicPackId`
- `serde::{Deserialize, Serialize}`
- `std::collections::HashSet`
- `super::*`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
