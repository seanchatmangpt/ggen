# `crates/ggen-marketplace/src/packs_registry/template_generator.rs`

Source SHA-256: `dd875f1968e39c6c7604d55c70d34f6bd2744167359d8273a02499c83c8ce08f`

```mermaid
classDiagram
    class struct_TemplateGenerator {
      <<struct>>
      +"tera: Tera"
    }
    class struct_GenerationReport {
      <<struct>>
      +"files_created: Vec~PathBuf~"
      +"total_size: u64"
      +"variables_used: HashMap~String"
      +"duration: Duration"
      +"hooks_executed: Vec~String~"
      +"success: bool"
    }
    class struct_VariableDefinition {
      <<struct>>
      +"name: String"
      +"var_type: VariableType"
      +"description: String"
      +"default: Option~String~"
      +"required: bool"
      +"pattern: Option~String~"
    }
    class enum_VariableType {
      <<enum>>
    }
    class fn_collect_files_recursive {
      <<fn>>
    }
    class mod_tests {
      <<mod>>
    }
    note "Default for TemplateGenerator"
    note "TemplateGenerator"
```

## Dependencies

- `crate::marketplace::error::{Error, Result}`
- `crate::packs_registry::types::{Pack, PackTemplate}`
- `serde::{Deserialize, Serialize}`
- `std::collections::HashMap`
- `std::path::{Path, PathBuf}`
- `std::time::{Duration, Instant}`
- `super::*`
- `tera::{Context, Tera}`
- `tracing::{debug, info, warn}`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
