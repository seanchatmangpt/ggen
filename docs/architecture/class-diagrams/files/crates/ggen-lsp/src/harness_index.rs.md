# `crates/ggen-lsp/src/harness_index.rs`

Source SHA-256: `e04c6695a0fd1996cebe45c8d34924b40818ed59fdb0b7ab4aa200aea847a956`

```mermaid
classDiagram
    class enum_HarnessIndexError {
      <<enum>>
    }
    class struct_HarnessIndex {
      <<struct>>
      +"root: PathBuf"
      +"targets: Vec~DeclaredTarget~"
      +"existing_files: BTreeSet~PathBuf~"
    }
    class fn_collect_targets {
      <<fn>>
    }
    class fn_locate_path_line {
      <<fn>>
    }
    class fn_enumerate_proof_files {
      <<fn>>
    }
    class mod_tests {
      <<mod>>
    }
    note "HarnessIndex"
    note "std::error::Error for HarnessIndexError"
    note "std::fmt::Display for HarnessIndexError"
```

## Dependencies

- `crate::analyzers::DeclaredTarget`
- `crate::project_index::BufferOverlay`
- `std::collections::BTreeSet`
- `std::path::{Path, PathBuf}`
- `super::*`
- `tempfile::TempDir`
- `walkdir::WalkDir`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
