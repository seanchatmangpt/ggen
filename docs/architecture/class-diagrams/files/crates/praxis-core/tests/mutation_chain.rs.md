# `crates/praxis-core/tests/mutation_chain.rs`

Source SHA-256: `7061110a974742571ab14fe42eccc6f762d824860197e5a39cd68e22d6540f54`

```mermaid
classDiagram
    class fn_ensure_signing_key {
      <<fn>>
    }
    class fn_lawful_chain {
      <<fn>>
    }
    class fn_emit {
      <<fn>>
    }
    class fn_reseal_from {
      <<fn>>
    }
    class fn_validate {
      <<fn>>
    }
    class fn_failing_stages {
      <<fn>>
    }
    class fn_assert_killed_at {
      <<fn>>
    }
    class fn_baseline_lawful_chain_passes_every_stage {
      <<fn>>
    }
    class fn_mutant_event_drop_interior_killed_at_chain_linkage {
      <<fn>>
    }
    class fn_mutant_event_reorder_killed_at_monotonic_and_linkage {
      <<fn>>
    }
    class fn_mutant_field_flip_payload_hash_killed_at_chain_recompute {
      <<fn>>
    }
    class fn_mutant_field_flip_ts_ns_without_reseal_killed_at_chain_recompute {
      <<fn>>
    }
    class fn_mutant_field_flip_activity_idx_killed_at_chain_recompute {
      <<fn>>
    }
    class fn_mutant_field_flip_node_kind_killed_at_chain_recompute {
      <<fn>>
    }
    class fn_mutant_hash_truncate_killed_at_schema {
      <<fn>>
    }
    class fn_mutant_timestamp_skew_resealed_killed_at_monotonic {
      <<fn>>
    }
    class fn_mutant_timestamp_future_killed_at_monotonic_under_bounded_clock {
      <<fn>>
    }
    class fn_mutant_instruction_id_regression_killed_at_monotonic {
      <<fn>>
    }
    class fn_mutant_version_bump_killed_at_schema {
      <<fn>>
    }
    class fn_boundary_ts_equal_to_now_is_not_future {
      <<fn>>
    }
    class fn_boundary_equal_consecutive_ts_is_allowed {
      <<fn>>
    }
    class fn_mutant_andon_flip_killed_at_chain_recompute {
      <<fn>>
    }
    class fn_mutant_obligation_count_flip_killed_at_chain_recompute {
      <<fn>>
    }
    class fn_mutant_object_ids_mutation_killed_at_chain_recompute {
      <<fn>>
    }
    class fn_survivor_tail_drop_is_a_documented_gap {
      <<fn>>
    }
    class fn_mutant_head_drop_killed_at_chain_linkage {
      <<fn>>
    }
```

## Dependencies

- `praxis_core::{ law::{Andon, ReceiptMeta}, lifecycle::Raw, receipt_record::RECEIPT_RECORD_VERSION, receipt_validator::{CheckOutcome, FixedClock, ReceiptValidator, Verdict}, Admit, DefaultLaw, Judge, LawObject, ReceiptRecord, }`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
