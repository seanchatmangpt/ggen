# `crates/ggen-marketplace/src/packs_registry/compose.rs`

Source SHA-256: `267ddec36342ab34971110fcffad40ae92c614a44eed109ebe590145fdaa423c`

```mermaid
classDiagram
    class struct_ComposePacksInput {
      <<struct>>
      +"pack_ids: Vec~String~"
      +"project_name: String"
      +"output_dir: Option~PathBuf~"
      +"strategy: CompositionStrategy"
    }
    class struct_ComposePacksOutput {
      <<struct>>
      +"project_name: String"
      +"packs_composed: Vec~String~"
      +"total_packages: usize"
      +"total_templates: usize"
      +"total_sparql_queries: usize"
      +"output_path: PathBuf"
      +"composition_strategy: String"
    }
    class fn_detect_circular_dependencies {
      <<fn>>
    }
    class fn_dfs_cycle_check {
      <<fn>>
    }
    class fn_resolve_dependencies {
      <<fn>>
    }
    class fn_merge_packs {
      <<fn>>
    }
    class fn_layer_packs {
      <<fn>>
    }
    class mod_tests {
      <<mod>>
    }
```

## Dependencies

- `crate::marketplace::error::Result`
- `crate::packs_registry::metadata::load_pack_metadata`
- `crate::packs_registry::types::{CompositionStrategy, Pack}`
- `crate::packs_registry::types::{PackDependency, PackMetadata}`
- `serde::{Deserialize, Serialize}`
- `std::collections::{HashMap, HashSet}`
- `std::path::PathBuf`
- `super::*`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
