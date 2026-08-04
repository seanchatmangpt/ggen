# `tests/unit/packs/gpack_manifest_test.rs`

Source SHA-256: `74d44e0a0ae0a8f18cde3901e9730a399337b723e32466869e02960936d04967`

```mermaid
classDiagram
    class fn_test_parse_minimal_manifest {
      <<fn>>
    }
    class fn_test_parse_full_manifest {
      <<fn>>
    }
    class fn_test_parse_manifest_invalid_toml {
      <<fn>>
    }
    class fn_test_parse_manifest_missing_required_field {
      <<fn>>
    }
    class fn_test_discover_templates_default_patterns {
      <<fn>>
    }
    class fn_test_discover_templates_custom_patterns {
      <<fn>>
    }
    class fn_test_discover_templates_empty_directory {
      <<fn>>
    }
    class fn_test_discover_rdf_files_default_patterns {
      <<fn>>
    }
    class fn_test_discover_rdf_files_custom_patterns {
      <<fn>>
    }
    class fn_test_discover_query_files_default_patterns {
      <<fn>>
    }
    class fn_test_discover_query_files_custom_patterns {
      <<fn>>
    }
    class fn_test_discover_shape_files_default_patterns {
      <<fn>>
    }
    class fn_test_load_manifest_from_nonexistent_file {
      <<fn>>
    }
    class fn_test_discover_files_with_invalid_glob_pattern {
      <<fn>>
    }
    class fn_test_manifest_with_empty_strings {
      <<fn>>
    }
    class fn_test_manifest_with_special_characters {
      <<fn>>
    }
    class fn_test_manifest_with_very_long_strings {
      <<fn>>
    }
    class fn_test_pack_conventions_defaults {
      <<fn>>
    }
    class fn_minimal_manifest {
      <<fn>>
    }
    class fn_create_file {
      <<fn>>
    }
```

## Dependencies

- `ggen_core::gpack::{GpackManifest, PackConventions}`
- `std::collections::BTreeMap`
- `std::fs`
- `std::io::Write`
- `std::path::{Path, PathBuf}`
- `tempfile::TempDir`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
