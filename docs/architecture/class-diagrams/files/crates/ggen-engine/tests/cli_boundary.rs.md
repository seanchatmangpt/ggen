# `crates/ggen-engine/tests/cli_boundary.rs`

Source SHA-256: `123f6cea14a4e5b74691ed8aa136d055a0d0c02b514e8b8e0ed11e6751fbe90d`

```mermaid
classDiagram
    class fn_scaffold {
      <<fn>>
    }
    class fn_root_help_exits_zero_and_lists_all_nouns {
      <<fn>>
    }
    class fn_root_help_gives_each_noun_a_non_blank_description {
      <<fn>>
    }
    class fn_root_version_exits_zero {
      <<fn>>
    }
    class fn_root_no_args_exits_zero_or_prints_usage {
      <<fn>>
    }
    class fn_unknown_noun_exits_nonzero {
      <<fn>>
    }
    class fn_unknown_flag_exits_nonzero {
      <<fn>>
    }
    class fn_sync_noun_help_exits_zero_and_lists_run {
      <<fn>>
    }
    class fn_sync_run_help_lists_dry_run_flag {
      <<fn>>
    }
    class fn_sync_run_help_lists_watch_flag {
      <<fn>>
    }
    class fn_sync_run_help_gives_each_flag_a_non_blank_description {
      <<fn>>
    }
    class fn_sync_run_generates_expected_file {
      <<fn>>
    }
    class fn_sync_run_dry_run_writes_nothing {
      <<fn>>
    }
    class fn_sync_run_second_invocation_is_idempotent {
      <<fn>>
    }
    class fn_sync_run_missing_manifest_exits_nonzero {
      <<fn>>
    }
    class fn_watch_for_stderr {
      <<fn>>
    }
    class fn_ggen_bin {
      <<fn>>
    }
    class fn_sync_run_watch_runs_initial_sync_then_blocks_watching {
      <<fn>>
    }
    class fn_sync_run_watch_and_dry_run_combined_writes_nothing {
      <<fn>>
    }
    class fn_sync_run_watch_missing_manifest_exits_nonzero_without_hanging {
      <<fn>>
    }
    class fn_sync_run_unbound_template_variable_exits_nonzero {
      <<fn>>
    }
    class fn_graph_validate_valid_project_exits_zero {
      <<fn>>
    }
    class fn_graph_validate_missing_manifest_exits_nonzero {
      <<fn>>
    }
    class fn_graph_validate_malformed_ontology_exits_nonzero {
      <<fn>>
    }
    class fn_receipt_verify_missing_receipt_exits_nonzero {
      <<fn>>
    }
    class fn_receipt_verify_succeeds_after_sync_and_fails_on_tamper {
      <<fn>>
    }
    class fn_receipt_history_missing_log_exits_nonzero {
      <<fn>>
    }
    class fn_receipt_history_after_two_syncs_exits_zero {
      <<fn>>
    }
    class fn_receipt_history_tampered_middle_record_exits_nonzero {
      <<fn>>
    }
    class fn_introspect_emits_json_schema_and_exits_zero {
      <<fn>>
    }
    class fn_format_json_flag_produces_parseable_json_on_success {
      <<fn>>
    }
```

## Dependencies

- `chicago_tdd_tools::cli_proof::CliHarness`
- `std::io::Read`
- `std::process::{Command, Stdio}`
- `std::time::{Duration, Instant}`
- `tempfile::TempDir`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
