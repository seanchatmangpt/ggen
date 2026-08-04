# `crates/ggen-cli/tests/lifecycle_e2e_test.rs`

Source SHA-256: `0406e0afde037f4566ed1174bef1aee937b5b0d29c5646c198a0e0fdbf969bb0`

```mermaid
classDiagram
    class fn_create_test_project {
      <<fn>>
    }
    class fn_ggen_cmd {
      <<fn>>
    }
    class fn_assert_state_exists {
      <<fn>>
    }
    class fn_test_lifecycle_list_shows_all_phases {
      <<fn>>
    }
    class fn_test_lifecycle_list_without_make_toml {
      <<fn>>
    }
    class fn_test_lifecycle_show_displays_phase_details {
      <<fn>>
    }
    class fn_test_lifecycle_show_with_metadata {
      <<fn>>
    }
    class fn_test_lifecycle_show_missing_phase {
      <<fn>>
    }
    class fn_test_lifecycle_run_executes_phase {
      <<fn>>
    }
    class fn_test_lifecycle_run_creates_state_file {
      <<fn>>
    }
    class fn_test_lifecycle_run_with_environment {
      <<fn>>
    }
    class fn_test_lifecycle_run_missing_phase {
      <<fn>>
    }
    class fn_test_lifecycle_pipeline_sequential_execution {
      <<fn>>
    }
    class fn_test_lifecycle_pipeline_with_environment {
      <<fn>>
    }
    class fn_test_lifecycle_pipeline_stops_on_missing_phase {
      <<fn>>
    }
    class fn_test_state_persistence_across_runs {
      <<fn>>
    }
    class fn_test_lifecycle_list_shows_last_executed_phase {
      <<fn>>
    }
    class fn_test_lifecycle_help_output {
      <<fn>>
    }
    class fn_test_lifecycle_list_help {
      <<fn>>
    }
    class fn_test_lifecycle_show_help {
      <<fn>>
    }
    class fn_test_lifecycle_run_help {
      <<fn>>
    }
    class fn_test_lifecycle_pipeline_help {
      <<fn>>
    }
    class fn_test_hooks_execution_order {
      <<fn>>
    }
    class fn_test_multiple_commands_in_phase {
      <<fn>>
    }
    class fn_test_empty_phase_list {
      <<fn>>
    }
    class fn_test_state_directory_creation {
      <<fn>>
    }
    class fn_test_concurrent_safe_state_updates {
      <<fn>>
    }
    class fn_test_phase_without_hooks {
      <<fn>>
    }
    class fn_test_performance_fast_execution {
      <<fn>>
    }
```

## Dependencies

- `assert_cmd::Command`
- `predicates::prelude::*`
- `std::fs`
- `tempfile::TempDir`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
