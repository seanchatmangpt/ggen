# `crates/ggen-engine/tests/reasoner_independence_e2e.rs`

Source SHA-256: `6debfc3f79830d5b85352c388650c93184286f7da6bda1f3117e7a13636a8e99`

```mermaid
classDiagram
    class fn_write_synthetic_pack {
      <<fn>>
    }
    class fn_scaffold_consumer {
      <<fn>>
    }
    class fn_sync_conforming_under {
      <<fn>>
    }
    class fn_both_engines_sync_a_gated_pack_to_identical_outputs {
      <<fn>>
    }
    class fn_violating_fixture_is_refused_under_both_engines {
      <<fn>>
    }
```

## Dependencies

- `ggen_engine::sync::{sync, EngineKind, SyncOptions}`
- `std::path::{Path, PathBuf}`
- `tempfile::TempDir`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
