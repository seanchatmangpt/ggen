# `tests/integration/packs/pack_cli_integration_test.rs`

Source SHA-256: `20510cd304b2fe440923ef47b6a61c4a610fe6df8b6f3a544dd94bbef901cbda`

```mermaid
classDiagram
    class fn_test_list_all_test_packs {
      <<fn>>
    }
    class fn_test_pack_version_filtering {
      <<fn>>
    }
    class fn_test_show_pack_complete_metadata {
      <<fn>>
    }
    class fn_test_discover_pack_templates {
      <<fn>>
    }
    class fn_test_discover_pack_rdf_files {
      <<fn>>
    }
    class fn_test_discover_pack_sparql_queries {
      <<fn>>
    }
    class fn_test_discover_pack_shacl_shapes {
      <<fn>>
    }
    class fn_test_template_contains_valid_frontmatter {
      <<fn>>
    }
    class fn_test_rdf_file_is_valid_turtle {
      <<fn>>
    }
    class fn_test_sparql_query_is_valid {
      <<fn>>
    }
    class fn_test_shacl_shape_is_valid {
      <<fn>>
    }
    class fn_test_pack_has_valid_dependencies {
      <<fn>>
    }
    class fn_test_dependency_resolution {
      <<fn>>
    }
    class fn_test_validate_pack_structure {
      <<fn>>
    }
    class fn_test_pack_compatibility_version {
      <<fn>>
    }
    class fn_test_load_nonexistent_pack {
      <<fn>>
    }
    class fn_test_discover_files_in_empty_pack {
      <<fn>>
    }
    class fn_test_pack_with_circular_dependency {
      <<fn>>
    }
    class fn_test_pack_operations_are_fast {
      <<fn>>
    }
```

## Dependencies

- `ggen_core::gpack::GpackManifest`
- `std::fs`
- `std::path::PathBuf`
- `std::process::Command`
- `std::time::Instant`
- `tempfile::TempDir`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
