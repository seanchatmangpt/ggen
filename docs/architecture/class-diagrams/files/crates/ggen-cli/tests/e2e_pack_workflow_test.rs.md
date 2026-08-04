# `crates/ggen-cli/tests/e2e_pack_workflow_test.rs`

Source SHA-256: `ecc760b1c69c929b7f8a08e67c5f368bd716990c43d01de651882f117ce0c5ce`

```mermaid
classDiagram
    class fn_ggen {
      <<fn>>
    }
    class fn_create_test_pack_metadata {
      <<fn>>
    }
    class fn_create_test_lockfile {
      <<fn>>
    }
    class fn_create_test_receipt {
      <<fn>>
    }
    class fn_parse_json {
      <<fn>>
    }
    class fn_verify_lockfile_structure {
      <<fn>>
    }
    class fn_count_lockfile_packs {
      <<fn>>
    }
    class fn_test_pack_install_creates_lockfile {
      <<fn>>
    }
    class fn_test_pack_install_tracks_packs {
      <<fn>>
    }
    class fn_test_pack_install_returns_valid_json {
      <<fn>>
    }
    class fn_test_pack_install_fails_on_unknown_pack {
      <<fn>>
    }
    class fn_test_pack_list_shows_installed_packs {
      <<fn>>
    }
    class fn_test_pack_validate_checks_pack {
      <<fn>>
    }
    class fn_test_capability_enable_expands_to_atomic_packs {
      <<fn>>
    }
    class fn_test_capability_enable_with_projection {
      <<fn>>
    }
    class fn_test_capability_enable_updates_lockfile {
      <<fn>>
    }
    class fn_test_capability_list_shows_capabilities {
      <<fn>>
    }
    class fn_test_capability_inspect_shows_details {
      <<fn>>
    }
    class fn_test_lockfile_created_after_pack_install {
      <<fn>>
    }
    class fn_test_lockfile_persists_across_commands {
      <<fn>>
    }
    class fn_test_lockfile_format_is_valid {
      <<fn>>
    }
    class fn_test_lockfile_tracks_multiple_packs {
      <<fn>>
    }
    class fn_test_lockfile_reproducibility {
      <<fn>>
    }
    class fn_test_receipt_generated_after_pack_install {
      <<fn>>
    }
    class fn_test_receipt_verify_works {
      <<fn>>
    }
    class fn_test_receipt_info_shows_details {
      <<fn>>
    }
    class fn_test_receipt_format_is_valid {
      <<fn>>
    }
    class fn_test_receipt_chain_verification {
      <<fn>>
    }
    class fn_test_policy_validate_checks_lockfile {
      <<fn>>
    }
    class fn_test_policy_list_shows_profiles {
      <<fn>>
    }
    class fn_test_policy_show_displays_profile_details {
      <<fn>>
    }
    class fn_test_policy_validation_without_lockfile_fails_gracefully {
      <<fn>>
    }
    class fn_test_policy_enforces_trust_requirements {
      <<fn>>
    }
    class fn_test_full_workflow_install_to_receipt {
      <<fn>>
    }
    class fn_test_full_workflow_capability_to_policy {
      <<fn>>
    }
    class fn_test_full_workflow_with_receipt_verification {
      <<fn>>
    }
    class fn_test_concurrent_operations_with_lockfile {
      <<fn>>
    }
    class fn_test_workflow_error_handling {
      <<fn>>
    }
    class fn_test_full_workflow_multiple_packs {
      <<fn>>
    }
    class fn_test_workflow_state_consistency {
      <<fn>>
    }
```

## Dependencies

- `assert_cmd::Command`
- `predicates::prelude::*`
- `serde_json::Value`
- `std::fs::{self, File}`
- `std::io::Write`
- `std::path::{Path, PathBuf}`
- `tempfile::TempDir`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
