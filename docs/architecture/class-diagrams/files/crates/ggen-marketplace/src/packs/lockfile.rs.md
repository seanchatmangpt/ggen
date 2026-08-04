# `crates/ggen-marketplace/src/packs/lockfile.rs`

Source SHA-256: `152460e852a8eeed4194c2e4c48aec4d44c70a4eb65d5b5fa8496a993fcd5dd9`

```mermaid
classDiagram
    class struct_PackLockfile {
      <<struct>>
      +"packs: BTreeMap~String"
      +"updated_at: DateTime~Utc~"
      +"ggen_version: String"
      +"profile: Option~String~"
    }
    class struct_LockedPack {
      <<struct>>
      +"version: String"
      +"source: PackSource"
      +"integrity: Option~String~"
      +"installed_at: DateTime~Utc~"
      +"dependencies: Vec~String~"
    }
    class enum_PackSource {
      <<enum>>
    }
    class fn_is_valid_sha256_integrity {
      <<fn>>
    }
    class mod_tests {
      <<mod>>
    }
    note "PackLockfile"
    note "fmt::Display for PackLockfile"
    note "fmt::Display for PackSource"
```

## Dependencies

- `chrono::{DateTime, Utc}`
- `crate::marketplace::error::{Error, Result}`
- `serde::{Deserialize, Serialize}`
- `std::collections::BTreeMap`
- `std::fmt`
- `std::fs`
- `std::path::{Path, PathBuf}`
- `super::*`
- `tempfile::TempDir`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
