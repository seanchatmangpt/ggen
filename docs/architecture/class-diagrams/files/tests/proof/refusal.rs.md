# `tests/proof/refusal.rs`

Source SHA-256: `bc589762338baa5f5bda06206043b403b20ea72a987c63f48dab6f7a5c384857`

```mermaid
classDiagram
    class fn_ggen {
      <<fn>>
    }
    class fn_sab_01_corrupt_packs_lock_garbage_json {
      <<fn>>
    }
    class fn_sab_01b_truncate_packs_lock_mid_json {
      <<fn>>
    }
    class fn_sab_02_empty_receipt_signature {
      <<fn>>
    }
    class fn_sab_03_pack_add_nonexistent {
      <<fn>>
    }
    class fn_sab_04_missing_ggen_toml_doctor_fails {
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
