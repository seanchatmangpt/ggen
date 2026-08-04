# `crates/ggen-cli/tests/sabotage_tests.rs`

Source SHA-256: `82eac0728a99235996a838152c8e0f25cf8594854c35ec4fec43dca5c7c58179`

```mermaid
classDiagram
    class fn_ggen {
      <<fn>>
    }
    class fn_test_sabotage_remove_pack_toml_sync_locked_exits_nonzero {
      <<fn>>
    }
    class fn_test_sabotage_corrupt_lockfile_sync_locked_exits_nonzero {
      <<fn>>
    }
    class fn_test_sabotage_delete_verifying_key_receipt_verify_returns_invalid {
      <<fn>>
    }
    class fn_test_sabotage_empty_packs_dir_install_exits_nonzero {
      <<fn>>
    }
```

## Dependencies

- `assert_cmd::Command`
- `predicates::prelude::*`
- `std::fs`
- `tempfile::TempDir`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
