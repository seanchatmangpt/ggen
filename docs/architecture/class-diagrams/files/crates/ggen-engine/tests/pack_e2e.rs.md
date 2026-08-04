# `crates/ggen-engine/tests/pack_e2e.rs`

Source SHA-256: `90dcf6c0992326c1d9c5f97c54f56e2395377db8fed51348e673c8711cac4021`

```mermaid
classDiagram
    class fn_examples_dir {
      <<fn>>
    }
    class fn_copy_tree {
      <<fn>>
    }
    class fn_scaffold {
      <<fn>>
    }
    class fn_pack_sync_end_to_end {
      <<fn>>
    }
    class fn_tampered_receipt_decision_fails_verification {
      <<fn>>
    }
    class fn_dry_run_never_writes_or_mutates_lock {
      <<fn>>
    }
    class fn_unreachable_git_pack_url_fails_closed_with_a_typed_error {
      <<fn>>
    }
    class fn_broken_packs_refuse_by_name {
      <<fn>>
    }
    class fn_write_pack {
      <<fn>>
    }
    class fn_write_two_pack_project {
      <<fn>>
    }
    class fn_two_packs_disjoint_outputs_both_succeed {
      <<fn>>
    }
    class fn_two_packs_colliding_output_aborts_sync_without_rollback_or_lock {
      <<fn>>
    }
    class fn_write_pack_with_intra_pack_collision {
      <<fn>>
    }
    class fn_same_pack_two_templates_colliding_output_aborts_sync_identically {
      <<fn>>
    }
    class fn_contamination_gate_source {
      <<fn>>
    }
    class fn_write_two_pack_project_with_contamination_gate {
      <<fn>>
    }
    class fn_cross_pack_conflicting_rdf_type_aborts_sync_citing_contamination_gate {
      <<fn>>
    }
    class fn_git {
      <<fn>>
    }
    class fn_make_git_pack_source {
      <<fn>>
    }
    class fn_git_resolved_pack_syncs_end_to_end_and_caches_across_runs {
      <<fn>>
    }
    class fn_write_extra_ontology_project {
      <<fn>>
    }
    class fn_extra_ontology_syncs_and_its_edit_invalidates_the_lock {
      <<fn>>
    }
    class fn_missing_extra_ontology_refuses_with_a_typed_error {
      <<fn>>
    }
    class fn_write_two_pack_project_with_lock {
      <<fn>>
    }
    class fn_write_single_pack_project {
      <<fn>>
    }
    class fn_lock_false_pack_is_never_locked_and_its_edit_does_not_refuse {
      <<fn>>
    }
    class fn_default_locked_pack_still_refuses_on_tampering_regression_guard {
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
