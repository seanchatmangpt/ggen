# `crates/ggen-marketplace/src/packs_registry/composer.rs`

Source SHA-256: `361e56046fd7bc48f3b13f3f5fc75e4045c1a1733b1598c5b913c4c113a053cd`

```mermaid
classDiagram
    class struct_PackComposer {
      <<struct>>
      +"repository: Box~dyn PackRepository~"
    }
    class struct_CompositionOptions {
      <<struct>>
      +"strategy: CompositionStrategy"
      +"output_dir: Option~PathBuf~"
      +"force_composition: bool"
      +"dry_run: bool"
    }
    class struct_CompositionResult {
      <<struct>>
      +"project_name: String"
      +"packs_composed: Vec~String~"
      +"composed_pack: Pack"
      +"composition_order: Vec~String~"
      +"conflicts: Vec~String~"
      +"plan: CompositionPlan"
      +"output_path: PathBuf"
      +"duration: std::time::Duration"
    }
    class struct_CompositionPlan {
      <<struct>>
      +"total_packs: usize"
      +"total_packages: usize"
      +"total_templates: usize"
      +"composition_order: Vec~String~"
      +"steps: Vec~CompositionStep~"
    }
    class struct_CompositionStep {
      <<struct>>
      +"pack_id: String"
      +"pack_name: String"
      +"packages_to_add: usize"
      +"templates_to_add: usize"
    }
    class mod_tests {
      <<mod>>
    }
    note "Default for CompositionOptions"
    note "PackComposer"
```

## Dependencies

- `crate::marketplace::error::{Error, Result}`
- `crate::packs_registry::dependency_graph::DependencyGraph`
- `crate::packs_registry::installer::{InstallOptions, PackInstaller}`
- `crate::packs_registry::repository::{FileSystemRepository, PackRepository}`
- `crate::packs_registry::types::{CompositionStrategy, Pack}`
- `crate::packs_registry::types::{PackDependency, PackMetadata, PackTemplate}`
- `std::collections::{HashMap, HashSet}`
- `std::path::PathBuf`
- `std::time::Instant`
- `super::*`
- `tracing::{info, warn}`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
