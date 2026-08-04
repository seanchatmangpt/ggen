# `tests/e2e_marketplace.rs`

Source SHA-256: `6eeee4e0f8841033a4e2726f36f85ff5c02f20daf7b0020b0ba3ab934b814a6d`

```mermaid
classDiagram
    class struct_EnvVarGuard {
      <<struct>>
      +"key: &'static str"
      +"previous: Option~std::ffi::OsString~"
    }
    class fn_test_marketplace_e2e_workflow {
      <<fn>>
    }
    class fn_simulate_search {
      <<fn>>
    }
    class fn_copy_dir_all {
      <<fn>>
    }
    class fn_resolve_template {
      <<fn>>
    }
    class fn_test_registry_validation {
      <<fn>>
    }
    class fn_test_hash_generation {
      <<fn>>
    }
    note "Drop for EnvVarGuard"
    note "EnvVarGuard"
```

## Dependencies

- `anyhow::Result`
- `serial_test::serial`
- `std::env`
- `std::fs`
- `std::path::PathBuf`
- `tempfile::TempDir`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
