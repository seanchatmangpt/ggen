# `crates/ggen-marketplace/src/packs_registry/installer.rs`

Source SHA-256: `1ca946db1bf9a6f92b1c8bc5e483bdb2799d537a73df09a7280c8d8bff1c8d3e`

```mermaid
classDiagram
    class struct_PackInstaller {
      <<struct>>
      +"repository: Box~dyn PackRepository~"
    }
    class struct_InstallOptions {
      <<struct>>
      +"target_dir: Option~PathBuf~"
      +"force: bool"
      +"dry_run: bool"
      +"skip_dependencies: bool"
    }
    class struct_InstallReport {
      <<struct>>
      +"pack_id: String"
      +"pack_name: String"
      +"pack_version: String"
      +"packages_installed: Vec~String~"
      +"templates_available: Vec~String~"
      +"install_path: PathBuf"
      +"dependencies_resolved: Vec~String~"
      +"install_order: Vec~String~"
      +"conflicts: Vec~String~"
      +"duration: std::time::Duration"
      +"success: bool"
    }
    class mod_tests {
      <<mod>>
    }
    note "Default for InstallOptions"
    note "InstallReport"
    note "PackInstaller"
```

## Dependencies

- `crate::marketplace::error::{Error, Result}`
- `crate::packs::lockfile::{LockedPack, PackLockfile, PackSource}`
- `crate::packs_registry::dependency_graph::DependencyGraph`
- `crate::packs_registry::repository::{FileSystemRepository, PackRepository}`
- `crate::packs_registry::types::Pack`
- `std::collections::HashMap`
- `std::path::PathBuf`
- `std::time::Instant`
- `super::*`
- `tracing::{error, info, warn}`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
