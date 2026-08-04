# `tests/v3-refactor/lib.rs`

Source SHA-256: `720271a0bc8f25bf9d750445d41bfd1978f131237bd9d8a7cadebb1ced756d19`

```mermaid
classDiagram
    class struct_TestEnvironment {
      <<struct>>
      +"temp_dir: TempDir"
    }
    class fn_save_baseline {
      <<fn>>
    }
    class fn_load_baseline {
      <<fn>>
    }
    class fn_compare_with_baseline {
      <<fn>>
    }
    note "TestEnvironment"
```

## Dependencies

- `std::fs`
- `std::path::{Path, PathBuf}`
- `tempfile::TempDir`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
