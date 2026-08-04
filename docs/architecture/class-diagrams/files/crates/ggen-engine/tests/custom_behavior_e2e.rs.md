# `crates/ggen-engine/tests/custom_behavior_e2e.rs`

Source SHA-256: `d795f9a438e58ec9a9fa18c352d4e8feb7ba0c1f7442e443983fe5092c0fe339`

```mermaid
classDiagram
    class fn_packs_dir {
      <<fn>>
    }
    class fn_copy_tree {
      <<fn>>
    }
    class fn_assert_cli_success {
      <<fn>>
    }
    class fn_run_cargo {
      <<fn>>
    }
    class fn_scaffold_consumer {
      <<fn>>
    }
    class fn_custom_behavior_scaffolds_once_and_survives_hand_completion {
      <<fn>>
    }
    class fn_custom_behavior_wiring_proof_test_catches_misdirected_routing {
      <<fn>>
    }
```

## Dependencies

- `chicago_tdd_tools::cli_proof::CliHarness`
- `std::path::{Path, PathBuf}`
- `std::process::Command`
- `tempfile::TempDir`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
