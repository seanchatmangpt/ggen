# `crates/ggen-engine/tests/projection_determinism_test.rs`

Source SHA-256: `52067587a5f03d83a24987b93e1e11d8d10da76a86afbce83eb8c546cd79e643`

```mermaid
classDiagram
    class fn_scaffold {
      <<fn>>
    }
    class fn_identical_fixtures_replay_to_byte_identical_output_trees {
      <<fn>>
    }
    class fn_unless_exists_frontmatter_preserves_hand_edited_scaffold_file {
      <<fn>>
    }
    class fn_unless_exists_frontmatter_writes_when_target_absent {
      <<fn>>
    }
```

## Dependencies

- `ggen_engine::sync::{sync, SyncOptions}`
- `std::path::Path`
- `tempfile::TempDir`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
