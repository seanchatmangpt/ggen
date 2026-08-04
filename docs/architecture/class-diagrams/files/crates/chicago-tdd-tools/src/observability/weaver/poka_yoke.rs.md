# `crates/chicago-tdd-tools/src/observability/weaver/poka_yoke.rs`

Source SHA-256: `80977a536f727a0b7b26a30df7cbf3530d552508a355d0227674ba051533b9bc`

```mermaid
classDiagram
    class struct_ValidRegistryPath {
      <<struct>>
      +"path: PathBuf"
    }
    class struct_RegistryVersion {
      <<struct>>
      +"version: String"
    }
    class enum_RegistryState {
      <<enum>>
    }
    class mod_tests {
      <<mod>>
    }
    note "AsRef~Path~ for ValidRegistryPath"
    note "AsRef~str~ for RegistryVersion"
    note "From~RegistryVersion~ for String"
    note "From~ValidRegistryPath~ for PathBuf"
    note "RegistryState"
    note "RegistryVersion"
    note "ValidRegistryPath"
```

## Dependencies

- `std::fs`
- `std::path::{Path, PathBuf}`
- `super::*`
- `tempfile::TempDir`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
