# `crates/ggen-engine/tests/composed_packs_e2e.rs`

Source SHA-256: `d50e984e6afbbc7beb18fca0008a500a81ec2fd721c039a1585b2353fa27f99f`

```mermaid
classDiagram
    class fn_packs_dir {
      <<fn>>
    }
    class fn_copy_tree {
      <<fn>>
    }
    class fn_scaffold_composed_project {
      <<fn>>
    }
    class fn_tree_digest {
      <<fn>>
    }
    class fn_thirty_packs_compose_into_one_consumer {
      <<fn>>
    }
    class fn_make_executable {
      <<fn>>
    }
    class fn_run_script {
      <<fn>>
    }
    class fn_thirty_one_packs_compose_with_verify_gates_active {
      <<fn>>
    }
```

## Dependencies

- `chicago_tdd_tools::cli_proof::CliHarness`
- `std::os::unix::fs::PermissionsExt`
- `std::path::{Path, PathBuf}`
- `std::process::Command`
- `tempfile::TempDir`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
