# `crates/ggen-engine/tests/receipt_epoch_e2e.rs`

Source SHA-256: `dfd8bb62e14f27b385f3480eb3626b6a1d82c0dee2985bf7a050f6cf16868d26`

```mermaid
classDiagram
    class fn_scaffold {
      <<fn>>
    }
    class fn_v1_record {
      <<fn>>
    }
    class fn_v2_record {
      <<fn>>
    }
    class fn_v1_only_reader_refuses_to_parse_a_v2_receipt {
      <<fn>>
    }
    class fn_v1_receipt_read_by_v2_aware_reader_is_legacy_bounded {
      <<fn>>
    }
    class fn_full_v2_receipt_round_trips_with_all_fields_intact {
      <<fn>>
    }
    class fn_v1_hardcoded_green_never_leaks_into_a_derived_green {
      <<fn>>
    }
    class fn_zero_admission_items_yields_explicit_zero_not_unknown {
      <<fn>>
    }
    class fn_migration_receipt_links_a_real_v1_hash_to_a_real_v2_hash {
      <<fn>>
    }
    class fn_incomplete_equivalence_map_is_a_construction_error {
      <<fn>>
    }
    class fn_unknown_equivalence_value_round_trips_unchanged {
      <<fn>>
    }
    class fn_quarantined_admission_item_derives_yellow_and_blocks_promotion {
      <<fn>>
    }
    class fn_refused_admission_item_derives_red {
      <<fn>>
    }
    class fn_ceiling_above_the_meet_is_refused_at_construction {
      <<fn>>
    }
    class fn_replay_chain {
      <<fn>>
    }
    class fn_replaying_a_mixed_chain_twice_is_deterministic {
      <<fn>>
    }
```

## Dependencies

- `ggen_engine::sync::{sync, SyncOptions, RECEIPT_REL_PATH}`
- `praxis_core::{ error::CoreError, law::Andon, receipt_epoch::{ read_receipt_epoch, AdmissionDecision, AdmissionItem, AdmissionLedger, AndonLevel, CeilingLevel, ComponentLevels, EquivalenceMap, EquivalenceStatus, MigrationReceipt, ObligationCount, ObservedOutcome, ReceiptEpochV2, ReceiptEpochV2Builder, ReceiptRecordV1Legacy, MIGRATION_LAW_1_TO_2, SCHEMA_V1, SCHEMA_V2, }, receipt_record::{ReceiptRecord, RECEIPT_RECORD_VERSION}, }`
- `std::path::Path`
- `tempfile::TempDir`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
