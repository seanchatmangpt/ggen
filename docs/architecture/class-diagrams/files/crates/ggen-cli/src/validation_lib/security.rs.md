# `crates/ggen-cli/src/validation_lib/security.rs`

Source SHA-256: `86e99a59a6dd81ea61dd86b8bb94830de3a7f4e4539cb153336c57ca5ef7c8b7`

```mermaid
classDiagram
    class enum_Permission {
      <<enum>>
    }
    class struct_PermissionModel {
      <<struct>>
      +"allowed_read_paths: Vec~PathBuf~"
      +"allowed_write_paths: Vec~PathBuf~"
      +"sandbox_root: Option~PathBuf~"
      +"restricted_env_vars: Vec~String~"
    }
    class mod_tests {
      <<mod>>
    }
    note "Default for PermissionModel"
    note "PermissionModel"
```

## Dependencies

- `crate::validation_lib::error::{Result, ValidationError}`
- `std::env`
- `std::path::{Path, PathBuf}`
- `super::*`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
