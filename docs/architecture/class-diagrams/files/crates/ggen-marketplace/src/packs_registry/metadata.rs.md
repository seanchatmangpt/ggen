# `crates/ggen-marketplace/src/packs_registry/metadata.rs`

Source SHA-256: `1b13ae351952403acd7355b871e9890f2b26fd76ca15e34e3eb14e14c804b129`

```mermaid
classDiagram
    class fn_try_get_packs_dir {
      <<fn>>
    }
    class fn_get_packs_dir {
      <<fn>>
    }
    class fn_load_pack_metadata {
      <<fn>>
    }
    class fn_list_packs {
      <<fn>>
    }
    class fn_show_pack {
      <<fn>>
    }
    class mod_tests {
      <<mod>>
    }
```

## Dependencies

- `crate::marketplace::error::Result`
- `crate::packs_registry::types::{Pack, PackFile}`
- `serial_test::serial`
- `std::fs`
- `std::path::PathBuf`
- `super::*`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
