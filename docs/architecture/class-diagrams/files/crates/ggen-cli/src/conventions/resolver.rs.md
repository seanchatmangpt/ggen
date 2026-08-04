# `crates/ggen-cli/src/conventions/resolver.rs`

Source SHA-256: `09b504573e154088a4cd3d8950f1433837b37be076beb8bcd34010585af146bc`

```mermaid
classDiagram
    class struct_ProjectConventions {
      <<struct>>
      +"rdf_files: Vec~PathBuf~"
      +"rdf_dir: PathBuf"
      +"templates: HashMap~String"
      +"templates_dir: PathBuf"
      +"queries: HashMap~String"
      +"output_dir: PathBuf"
      +"preset: String"
    }
    class struct_ConventionOverrides {
      <<struct>>
      +"rdf: RdfOverrides"
      +"templates: TemplatesOverrides"
      +"queries: QueriesOverrides"
      +"output: OutputOverrides"
    }
    class struct_RdfOverrides {
      <<struct>>
      +"patterns: Vec~String~"
    }
    class struct_TemplatesOverrides {
      <<struct>>
      +"patterns: Vec~String~"
    }
    class struct_QueriesOverrides {
      <<struct>>
      +"patterns: Vec~String~"
    }
    class struct_OutputOverrides {
      <<struct>>
      +"dir: Option~String~"
    }
    class struct_ConventionResolver {
      <<struct>>
      +"project_root: PathBuf"
    }
    class mod_tests {
      <<mod>>
    }
    note "ConventionResolver"
```

## Dependencies

- `crate::utils::error::{Context, Result}`
- `glob::glob`
- `serde::{Deserialize, Serialize}`
- `std::collections::HashMap`
- `std::fs`
- `std::path::PathBuf`
- `super::*`
- `tempfile::TempDir`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
