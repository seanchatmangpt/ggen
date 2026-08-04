# `crates/chicago-tdd-tools/build.rs`

Source SHA-256: `a5c5ccf2b3fa59d3f016328c634ff657e4b720536e5a02356423a95ecfc18c14`

```mermaid
classDiagram
    class fn_main {
      <<fn>>
    }
    class fn_detect_platform {
      <<fn>>
    }
    class fn_download_weaver {
      <<fn>>
    }
    class fn_extract_tar_xz {
      <<fn>>
    }
    class fn_extract_tar_gz {
      <<fn>>
    }
    class fn_extract_zip {
      <<fn>>
    }
    class fn_clone_registry {
      <<fn>>
    }
```

## Dependencies

- `std::env`
- `std::fs`
- `std::os::unix::fs::PermissionsExt`
- `std::path::{Path, PathBuf}`
- `std::process::Command`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
