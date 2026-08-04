# `crates/ggen-config/src/config/template_config.rs`

Source SHA-256: `8cd70642c53d5867bb61c4d2070b74be59c0e2935c18c33728a638ed6c78ce6b`

```mermaid
classDiagram
    class struct_TemplateConfig {
      <<struct>>
      +"search_paths: Vec~PathBuf~"
      +"default_variables: HashMap~String"
      +"metadata_store: PathBuf"
      +"cache_dir: Option~PathBuf~"
      +"generation: GenerationOptions"
      +"marketplace: MarketplaceSettings"
    }
    class struct_GenerationOptions {
      <<struct>>
      +"default_output_dir: PathBuf"
      +"auto_format: bool"
      +"run_hooks: bool"
      +"interactive: bool"
      +"force_overwrite: bool"
      +"validate_before_gen: bool"
    }
    class struct_MarketplaceSettings {
      <<struct>>
      +"enabled: bool"
      +"package_cache: PathBuf"
      +"auto_update: bool"
      +"trusted_sources: Vec~String~"
    }
    class mod_tests {
      <<mod>>
    }
    note "Default for GenerationOptions"
    note "Default for MarketplaceSettings"
    note "Default for TemplateConfig"
    note "TemplateConfig"
```

## Dependencies

- `serde::{Deserialize, Serialize}`
- `std::collections::HashMap`
- `std::path::PathBuf`
- `super::*`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
