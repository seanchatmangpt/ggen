# `crates/ggen-engine/src/keys.rs`

Source SHA-256: `7c89e834c62b4e839adea107e78db13d9afb3304982cf9932155d9e61658a1a5`

```mermaid
classDiagram
    class fn_keys_dir {
      <<fn>>
    }
    class fn_decode_key_hex {
      <<fn>>
    }
    class fn_resolve_signing_key {
      <<fn>>
    }
    class fn_resolve_verifying_key {
      <<fn>>
    }
    class fn_generate_and_persist_keypair {
      <<fn>>
    }
    class fn_write_new_file {
      <<fn>>
    }
    class fn_restrict_to_owner {
      <<fn>>
    }
    class fn_restrict_to_owner {
      <<fn>>
    }
    class mod_tests {
      <<mod>>
    }
```

## Dependencies

- `crate::error::{AppError, Result}`
- `ed25519_dalek::SigningKey`
- `std::io::Write as _`
- `std::os::unix::fs::PermissionsExt as _`
- `std::path::{Path, PathBuf}`
- `super::*`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
