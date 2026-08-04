# `crates/ggen-engine/tests/constitution_obligation_e2e.rs`

Source SHA-256: `784b61f754bee603092b186d09b69c8dfc0a9595e41480ec9821575387b5d42f`

```mermaid
classDiagram
    class fn_scaffold {
      <<fn>>
    }
    class fn_read_obligation_count {
      <<fn>>
    }
    class fn_obligation_count_required_matches_real_count_of_unmechanized_laws {
      <<fn>>
    }
    class fn_flipping_one_law_to_mechanized_decreases_obligation_count_by_exactly_one {
      <<fn>>
    }
    class fn_project_without_any_ccn_law_individuals_reports_zero_required {
      <<fn>>
    }
```

## Dependencies

- `ggen_engine::sync::{sync, SyncOptions, SyncReceipt, RECEIPT_REL_PATH}`
- `praxis_core::receipt_epoch::{read_receipt_epoch, ObligationCount}`
- `std::path::Path`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
