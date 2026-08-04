# `tests/unit/packs/pack_edge_cases_test.rs`

Source SHA-256: `1cddb6704eb616f4f364e13b2a72486d5ba36f541165086d0444446d6a37e1f4`

```mermaid
classDiagram
    class fn_test_pack_id_with_spaces {
      <<fn>>
    }
    class fn_test_pack_id_very_long {
      <<fn>>
    }
    class fn_test_pack_id_with_special_characters {
      <<fn>>
    }
    class fn_test_version_with_prerelease {
      <<fn>>
    }
    class fn_test_version_with_build_metadata {
      <<fn>>
    }
    class fn_test_version_invalid_semver {
      <<fn>>
    }
    class fn_test_detect_self_dependency {
      <<fn>>
    }
    class fn_test_transitive_circular_dependency {
      <<fn>>
    }
    class fn_test_missing_template_files {
      <<fn>>
    }
    class fn_test_template_with_no_frontmatter {
      <<fn>>
    }
    class fn_test_malformed_rdf_file {
      <<fn>>
    }
    class fn_test_invalid_sparql_query {
      <<fn>>
    }
    class fn_test_duplicate_template_names {
      <<fn>>
    }
    class fn_test_conflicting_rdf_prefixes {
      <<fn>>
    }
    class fn_test_empty_pack {
      <<fn>>
    }
    class fn_test_pack_with_only_dependencies {
      <<fn>>
    }
    class fn_test_zero_length_files {
      <<fn>>
    }
    class fn_test_template_with_unicode_path {
      <<fn>>
    }
    class fn_test_very_deep_directory_nesting {
      <<fn>>
    }
    class fn_test_symlink_handling {
      <<fn>>
    }
    class fn_test_very_large_manifest {
      <<fn>>
    }
    class fn_test_many_small_files {
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

- `ggen_core::gpack::{GpackManifest, GpackMetadata}`
- `std::collections::BTreeMap`
- `std::fs`
- `std::io::Write`
- `std::os::unix::fs as unix_fs`
- `std::path::PathBuf`
- `tempfile::TempDir`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
