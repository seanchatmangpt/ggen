# `tests/e2e_production_marketplace.rs`

Source SHA-256: `e2519c2804a55a0464f16704ea6e412dd81b041162e1ca0a4f686c3d05e62416`

```mermaid
classDiagram
    class mod_test_config {
      <<mod>>
    }
    class fn_test_search_production_registry {
      <<fn>>
    }
    class fn_test_add_from_production_registry {
      <<fn>>
    }
    class fn_test_lockfile_created_with_sha256 {
      <<fn>>
    }
    class fn_test_production_registry_index_accessible {
      <<fn>>
    }
    class fn_test_search_with_detailed_output {
      <<fn>>
    }
    class fn_test_search_with_category_filter {
      <<fn>>
    }
    class fn_test_search_with_json_output {
      <<fn>>
    }
    class fn_test_packs_command_lists_installed {
      <<fn>>
    }
    class fn_test_categories_command {
      <<fn>>
    }
    class fn_test_show_command_displays_pack_info {
      <<fn>>
    }
    class fn_test_marketplace_workflow_end_to_end {
      <<fn>>
    }
    class fn_test_registry_fallback_to_default {
      <<fn>>
    }
```

## Dependencies

- `anyhow::Result`
- `assert_cmd::Command`
- `std::fs`
- `tempfile::TempDir`
- `test_config::integration_timeout`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
