# `crates/ggen-lsp/src/rule_index.rs`

Source SHA-256: `00dd4e24b526c154538ff0847754889587b2587c55860b2c812145c8666ca2fd`

```mermaid
classDiagram
    class struct_RuleIndexEntry {
      <<struct>>
      +"rule_id: String"
      +"manifest_path: PathBuf"
      +"query_inline: bool"
      +"query_content: String"
      +"template_path: Option~PathBuf~"
      +"template_content: Option~String~"
      +"output_file: String"
      +"selected_vars: BTreeSet~String~"
      +"issues: Vec~String~"
    }
    class fn_read_overlay_or_disk {
      <<fn>>
    }
    class mod_tests {
      <<mod>>
    }
    note "RuleIndexEntry"
```

## Dependencies

- `crate::project_index::BufferOverlay`
- `ggen_config::manifest::{GenerationRule, QuerySource, TemplateSource}`
- `std::collections::BTreeSet`
- `std::path::{Path, PathBuf}`
- `super::*`
- `tempfile::TempDir`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
