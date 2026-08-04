# `crates/ggen-cli/src/conventions/planner.rs`

Source SHA-256: `b7c77bc32e8837d0c3184506f7cdfad0d939cb5b3b2e8cbbd86263dfe3e8d253`

```mermaid
classDiagram
    class struct_TemplateMetadata {
      <<struct>>
      +"output: String"
      +"when: Vec~String~"
      +"query: String"
      +"foreach: Option~String~"
    }
    class struct_GenerationTask {
      <<struct>>
      +"template: String"
      +"output_pattern: String"
      +"trigger_files: Vec~PathBuf~"
      +"query: Option~String~"
      +"foreach: Option~String~"
    }
    class struct_GenerationPlan {
      <<struct>>
      +"tasks: Vec~GenerationTask~"
    }
    class struct_GenerationPlanner {
      <<struct>>
      +"conventions: ProjectConventions"
    }
    class mod_tests {
      <<mod>>
    }
    note "GenerationPlanner"
    note "TemplateMetadata"
```

## Dependencies

- `crate::utils::error::Result`
- `std::collections::{HashMap, HashSet}`
- `std::fs`
- `std::path::{Path, PathBuf}`
- `super::*`
- `super::ProjectConventions`
- `tempfile::TempDir`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
