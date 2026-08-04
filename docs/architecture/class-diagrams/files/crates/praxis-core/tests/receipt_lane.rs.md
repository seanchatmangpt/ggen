# `crates/praxis-core/tests/receipt_lane.rs`

Source SHA-256: `94662f1cb05397626d892a07c0a99f5e7d5e5b14839a5b73e4d36f921598587c`

```mermaid
classDiagram
    class fn_signing_guard {
      <<fn>>
    }
    class fn_admitted_value {
      <<fn>>
    }
    class fn_chained_records {
      <<fn>>
    }
    class fn_three_chained_receipts_validate_clean {
      <<fn>>
    }
    class fn_tampering_payload_hash_is_caught_by_chain_recompute {
      <<fn>>
    }
    class fn_broken_linkage_is_caught {
      <<fn>>
    }
    class fn_non_monotonic_instruction_id_is_caught {
      <<fn>>
    }
    class fn_fixed_clock_future_timestamp_is_caught {
      <<fn>>
    }
    class fn_fixed_clock_present_timestamp_passes_monotonic {
      <<fn>>
    }
    class fn_lawful_lifecycle_replay_has_fitness_one {
      <<fn>>
    }
    class fn_out_of_order_lifecycle_replay_is_a_violation {
      <<fn>>
    }
    class fn_store_append_load_all_and_last_chain_hash_round_trip {
      <<fn>>
    }
    class fn_ocel_export_round_trips_with_valid_rfc3339_timestamps {
      <<fn>>
    }
    class fn_tampering_andon_is_caught_by_validator {
      <<fn>>
    }
    class fn_tampering_genesis_prev_chain_hash_is_caught_by_linkage {
      <<fn>>
    }
```

## Dependencies

- `praxis_core::{ law::ReceiptMeta, lifecycle::Raw, receipt_validator::{FixedClock, ReceiptValidator, SystemClock}, replay_adapter::{self, LifecycleStep}, Admit, DefaultLaw, Judge, LawObject, ReceiptStore, }`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
