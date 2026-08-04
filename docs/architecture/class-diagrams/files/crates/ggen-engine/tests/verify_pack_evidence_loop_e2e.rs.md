# `crates/ggen-engine/tests/verify_pack_evidence_loop_e2e.rs`

Source SHA-256: `f7ad86936fbd830c8dea75986e869fe8ddd196b21c8c71d0439a987a386c726f`

```mermaid
classDiagram
    class fn_packs_dir {
      <<fn>>
    }
    class fn_copy_tree {
      <<fn>>
    }
    class fn_make_executable {
      <<fn>>
    }
    class fn_scaffold {
      <<fn>>
    }
    class fn_write_stub_cargo {
      <<fn>>
    }
    class fn_run_script {
      <<fn>>
    }
    class fn_read {
      <<fn>>
    }
    class fn_run_sync {
      <<fn>>
    }
    class fn_verify_pack_evidence_bootstrap_loop_green_then_sabotage_refusal {
      <<fn>>
    }
```

## Dependencies

- `ggen_engine::sync::{sync, SyncOptions}`
- `std::os::unix::fs::PermissionsExt`
- `std::path::{Path, PathBuf}`
- `std::process::Command`
- `tempfile::TempDir`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
