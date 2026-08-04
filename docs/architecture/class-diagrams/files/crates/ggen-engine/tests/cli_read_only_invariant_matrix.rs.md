# `crates/ggen-engine/tests/cli_read_only_invariant_matrix.rs`

Source SHA-256: `81f80135271cc83e1fd1a4fc84588a28315474229809f184abb6ebec11fb1a40`

```mermaid
classDiagram
    class fn_snapshot {
      <<fn>>
    }
    class fn_assert_disk_unchanged {
      <<fn>>
    }
    class fn_state_never_synced {
      <<fn>>
    }
    class fn_state_freshly_synced {
      <<fn>>
    }
    class fn_state_no_project {
      <<fn>>
    }
    class fn_assert_command_is_read_only {
      <<fn>>
    }
    class fn_receipt_verify_is_read_only_even_when_it_finds_tampering {
      <<fn>>
    }
```

## Dependencies

- `chicago_tdd_tools::cli_proof::CliHarness`
- `std::collections::BTreeMap`
- `std::path::{Path, PathBuf}`
- `std::time::SystemTime`
- `tempfile::TempDir`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
