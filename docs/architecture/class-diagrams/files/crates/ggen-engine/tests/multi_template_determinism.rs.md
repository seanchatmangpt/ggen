# `crates/ggen-engine/tests/multi_template_determinism.rs`

Source SHA-256: `8ee6b85c73e40609af234337b11c0412b4f553c929f5681015fe9ee73c5989fd`

```mermaid
classDiagram
    class fn_template {
      <<fn>>
    }
    class fn_scaffold {
      <<fn>>
    }
    class fn_read_receipt {
      <<fn>>
    }
    class fn_written_ordering_is_stable_and_lexicographic_regardless_of_creation_order {
      <<fn>>
    }
    class fn_receipt_payload_bytes_identical_across_fresh_syncs_of_identical_input {
      <<fn>>
    }
    class fn_second_sync_of_multi_template_project_is_fully_unchanged {
      <<fn>>
    }
```

## Dependencies

- `ggen_engine::sync::{sync, SyncOptions, SyncReceipt, RECEIPT_REL_PATH}`
- `std::path::{Path, PathBuf}`
- `tempfile::TempDir`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
