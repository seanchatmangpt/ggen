# `crates/ggen-config/src/config/lock_manager.rs`

Source SHA-256: `db348c4711651e3e52b1a8015e3dc03791da461d0faffb566452cf877f1c7c62`

```mermaid
classDiagram
    class struct_OntologyLockfile {
      <<struct>>
      +"version: u32"
      +"generated_at: String"
      +"generator_version: String"
      +"packages: BTreeMap~String"
      +"composition: CompositionMetadata"
      +"hashes: BTreeMap~String"
    }
    class struct_LockedPackage {
      <<struct>>
      +"version: String"
      +"resolved: String"
      +"integrity: String"
      +"location: String"
      +"namespace: Option~String~"
      +"classes_count: usize"
      +"properties_count: usize"
      +"dependencies: BTreeMap~String"
      +"installed_at: String"
    }
    class struct_CompositionMetadata {
      <<struct>>
      +"strategy: String"
      +"total_classes: usize"
      +"total_properties: usize"
      +"conflicts_resolved: usize"
      +"validation_status: String"
    }
    class struct_LockfileManager {
      <<struct>>
    }
    class mod_tests {
      <<mod>>
    }
    note "LockfileManager"
    note "OntologyLockfile"
```

## Dependencies

- `crate::config_lib::error::Result`
- `serde::{Deserialize, Serialize}`
- `std::collections::BTreeMap`
- `std::path::Path`
- `super::*`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
