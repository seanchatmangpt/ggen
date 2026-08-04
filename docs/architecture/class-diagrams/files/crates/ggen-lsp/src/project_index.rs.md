# `crates/ggen-lsp/src/project_index.rs`

Source SHA-256: `0596d1e31a73b68dcb68d96c08847b531d6fcf66da8a2761e10cae5062fab8d1`

```mermaid
classDiagram
    class type_BufferOverlay {
      <<type>>
    }
    class enum_IndexError {
      <<enum>>
    }
    class struct_ProjectIndex {
      <<struct>>
      +"root: PathBuf"
      +"rule_entries: Vec~RuleIndexEntry~"
    }
    class mod_tests {
      <<mod>>
    }
    note "ProjectIndex"
    note "std::error::Error for IndexError"
    note "std::fmt::Display for IndexError"
```

## Dependencies

- `crate::rule_index::RuleIndexEntry`
- `ggen_config::{manifest::ManifestParser, ConfigSchemaClassification}`
- `std::collections::BTreeSet`
- `std::path::{Path, PathBuf}`
- `super::*`
- `tempfile::TempDir`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
