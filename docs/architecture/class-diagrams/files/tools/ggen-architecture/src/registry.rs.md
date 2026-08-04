# `tools/ggen-architecture/src/registry.rs`

Source SHA-256: `330c0459f79d4523bdd4b676cdff0df5e894152c44c00cc5ebd8309935879ba1`

```mermaid
classDiagram
    class struct_RegistryViolation {
      <<struct>>
      +"code: String"
      +"severity: Severity"
      +"asset_id: String"
      +"message: String"
    }
    class struct_ImpactReport {
      <<struct>>
      +"root: String"
      +"affected: Vec~String~"
      +"ordered_revalidation: Vec~String~"
      +"paths: BTreeMap~String"
    }
    class struct_ArchitectureRegistry {
      <<struct>>
      +"schema_version: u32"
      +"assets: BTreeMap~String"
    }
    note "ArchitectureRegistry"
    note "Default for ArchitectureRegistry"
```

## Dependencies

- `crate::{ error::{ArchitectureError, Result}, model::{ArchitectureAsset, LifecycleState, Severity, TransitionStep}, }`
- `serde::{Deserialize, Serialize}`
- `std::collections::{BTreeMap, BTreeSet, VecDeque}`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
