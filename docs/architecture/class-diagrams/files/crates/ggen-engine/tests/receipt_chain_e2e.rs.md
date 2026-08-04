# `crates/ggen-engine/tests/receipt_chain_e2e.rs`

Source SHA-256: `0a9c89024f663bced9f79185af62a6638d5835745d6a98d153b1eac61464cffb`

```mermaid
classDiagram
    class fn_scaffold {
      <<fn>>
    }
    class fn_write_ontology {
      <<fn>>
    }
    class fn_read_log {
      <<fn>>
    }
    class fn_three_syncs_form_a_verifiable_chain {
      <<fn>>
    }
    class fn_tampering_middle_line_payload_fails_naming_index_1 {
      <<fn>>
    }
    class fn_removing_or_reordering_lines_fails_history_verification {
      <<fn>>
    }
    class fn_missing_or_empty_log_fails_closed {
      <<fn>>
    }
    class fn_sync_refuses_to_extend_a_tampered_head {
      <<fn>>
    }
    class fn_missing_receipt_json_chains_from_log_tail {
      <<fn>>
    }
    class fn_legacy_payload_without_optional_fields_verifies {
      <<fn>>
    }
    class fn_dry_run_touches_neither_receipt_nor_log {
      <<fn>>
    }
    class fn_template_edit_changes_receipt_closure_even_with_identical_outputs {
      <<fn>>
    }
    class fn_closure_marks_missing_inputs_instead_of_dropping_them {
      <<fn>>
    }
    class fn_sign_then_verify_reports_signed_and_signature_valid_true {
      <<fn>>
    }
    class fn_ggen_signing_key_env_var_takes_precedence_over_key_file {
      <<fn>>
    }
    class fn_malformed_ggen_signing_key_env_var_errors_loudly {
      <<fn>>
    }
    class fn_tampered_chain_hash_fails_closed_and_is_distinguished_from_signature_failure {
      <<fn>>
    }
    class fn_tampered_signature_fails_closed_and_is_distinguished_from_chain_failure {
      <<fn>>
    }
    class fn_legacy_unsigned_receipt_still_chain_verifies_with_signed_false {
      <<fn>>
    }
```

## Dependencies

- `chicago_tdd_tools::cli_proof::CliHarness`
- `ggen_engine::sync::{sync, SyncOptions, SyncReceipt, RECEIPT_LOG_REL_PATH, RECEIPT_REL_PATH}`
- `praxis_core::Andon`
- `praxis_core::receipt_record::{ReceiptRecord, RECEIPT_RECORD_VERSION}`
- `std::path::Path`
- `tempfile::TempDir`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
