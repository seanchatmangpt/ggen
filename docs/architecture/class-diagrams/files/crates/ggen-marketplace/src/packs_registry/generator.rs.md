# `crates/ggen-marketplace/src/packs_registry/generator.rs`

Source SHA-256: `f0348d5840d86f741f45c98518140cb7b932d513c50c0381653763a172d1afb4`

```mermaid
classDiagram
    class struct_GenerateInput {
      <<struct>>
      +"pack_id: String"
      +"project_name: String"
      +"template_name: Option~String~"
      +"output_dir: Option~PathBuf~"
      +"variables: BTreeMap~String"
    }
    class struct_GenerateOutput {
      <<struct>>
      +"pack_id: String"
      +"project_name: String"
      +"templates_generated: Vec~String~"
      +"files_created: usize"
      +"output_path: PathBuf"
    }
    class mod_tests {
      <<mod>>
    }
```

## Dependencies

- `crate::marketplace::error::Result`
- `crate::packs_registry::metadata::load_pack_metadata`
- `serde::{Deserialize, Serialize}`
- `std::collections::BTreeMap`
- `std::path::PathBuf`
- `super::*`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
