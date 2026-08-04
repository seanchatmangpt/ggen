# `crates/ggen-engine/tests/receipt_genesis_ceiling_e2e.rs`

Source SHA-256: `41fd310ace2112cf55076374b3b8d7d46e0f985653c2a927ac0853298fc3d0f1`

```mermaid
classDiagram
    class fn_scaffold {
      <<fn>>
    }
    class fn_write_ontology {
      <<fn>>
    }
    class fn_write_ontology_with_evidence {
      <<fn>>
    }
    class fn_run_sync {
      <<fn>>
    }
    class fn_read_log {
      <<fn>>
    }
    class fn_genesis_sync_receipt_has_computed_ceiling_not_legacy_sentinel {
      <<fn>>
    }
    class fn_genesis_equivalence_source_and_config_are_unknown {
      <<fn>>
    }
    class fn_identical_resync_is_equivalent_and_ontology_edit_diverges_source_only {
      <<fn>>
    }
    class fn_ggen_toml_edit_diverges_config_not_source {
      <<fn>>
    }
    class fn_ver_admission_outcome {
      <<fn>>
    }
    class fn_verify_evidence_makes_tests_and_gates_classes_evaluable {
      <<fn>>
    }
    class fn_red_test_check_lowers_ceiling_and_diverges_tests_class {
      <<fn>>
    }
    class fn_write_ontology_with_producers {
      <<fn>>
    }
    class fn_all_producers_make_all_eight_classes_evaluated_on_second_sync {
      <<fn>>
    }
    class fn_tampered_doc_makes_docs_class_divergent {
      <<fn>>
    }
    class fn_red_receipts_check_diverges_receipts_and_lowers_ceiling {
      <<fn>>
    }
    class fn_green_receipts_check_with_stale_head_is_divergent_not_equivalent {
      <<fn>>
    }
    class fn_red_evidence_check_diverges_evidence_and_lowers_ceiling {
      <<fn>>
    }
    class fn_binary_skip_marker_stays_unknown_but_recorded_and_red_binary_diverges {
      <<fn>>
    }
    class fn_allowlisted_red_check_is_recorded_fail_but_does_not_lower_ceiling {
      <<fn>>
    }
```

## Dependencies

- `ggen_engine::sync::{sync, SyncOptions, SyncReceipt, RECEIPT_LOG_REL_PATH}`
- `praxis_core::receipt_epoch::{ AdmissionLedger, AndonLevel, CeilingLevel, EquivalenceStatus, ObservedOutcome, }`
- `std::path::Path`
- `tempfile::TempDir`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
