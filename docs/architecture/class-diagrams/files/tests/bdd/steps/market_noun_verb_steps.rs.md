# `tests/bdd/steps/market_noun_verb_steps.rs`

Source SHA-256: `bafb1c1bf654796ff45bff69faba50b76fcb42dc114541d21647ea1afa05aa8c`

```mermaid
classDiagram
    class struct_EnvVarGuard {
      <<struct>>
      +"key: &'static str"
      +"previous: Option~std::ffi::OsString~"
    }
    class fn_marketplace_registry_available {
      <<fn>>
    }
    class fn_marketplace_registry_available_for_market {
      <<fn>>
    }
    class fn_have_installed_gpack {
      <<fn>>
    }
    class fn_have_installed_gpack_with_version {
      <<fn>>
    }
    class fn_newer_version_available {
      <<fn>>
    }
    class fn_gpack_has_pqc_signature {
      <<fn>>
    }
    class fn_run_ggen_market_command {
      <<fn>>
    }
    class fn_command_should_succeed {
      <<fn>>
    }
    class fn_should_see_rust_results {
      <<fn>>
    }
    class fn_should_see_in_output {
      <<fn>>
    }
    class fn_results_should_show_rust_category {
      <<fn>>
    }
    class fn_output_should_be_valid_json {
      <<fn>>
    }
    class fn_json_should_contain_array {
      <<fn>>
    }
    class fn_gpack_should_be_in_lockfile {
      <<fn>>
    }
    class fn_gpack_should_be_cached {
      <<fn>>
    }
    class fn_lockfile_should_show_version {
      <<fn>>
    }
    class fn_gpack_should_not_be_in_lockfile {
      <<fn>>
    }
    class fn_command_should_fail {
      <<fn>>
    }
    class fn_should_see_in_stderr {
      <<fn>>
    }
    class fn_should_see_version_information {
      <<fn>>
    }
    class fn_should_see_source_urls {
      <<fn>>
    }
    class fn_gpack_updated_to_latest {
      <<fn>>
    }
    class fn_should_see_popular_categories {
      <<fn>>
    }
    class fn_should_see_package_metadata {
      <<fn>>
    }
    class fn_should_see_description {
      <<fn>>
    }
    class fn_should_see_sha256_hashes {
      <<fn>>
    }
    class fn_sha256_should_be_valid {
      <<fn>>
    }
    class fn_lockfile_should_contain_pqc_signature {
      <<fn>>
    }
    class fn_lockfile_should_contain_pqc_public_key {
      <<fn>>
    }
    class fn_run_market_search {
      <<fn>>
    }
    class fn_run_market_remove {
      <<fn>>
    }
    class fn_run_market_info {
      <<fn>>
    }
    class fn_run_market_categories {
      <<fn>>
    }
    class fn_run_market_list {
      <<fn>>
    }
    class fn_run_market_update {
      <<fn>>
    }
    class fn_run_market_update_specific {
      <<fn>>
    }
    class fn_should_see_search_results {
      <<fn>>
    }
    class fn_should_see_no_results {
      <<fn>>
    }
    class fn_gpack_should_be_removed_from_lockfile {
      <<fn>>
    }
    note "Drop for EnvVarGuard"
    note "EnvVarGuard"
```

## Dependencies

- `assert_cmd::Command`
- `cucumber::{given, then, when}`
- `std::fs`
- `super::super::world::GgenWorld`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
