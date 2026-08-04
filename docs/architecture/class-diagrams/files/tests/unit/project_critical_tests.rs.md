# `tests/unit/project_critical_tests.rs`

Source SHA-256: `8624b73e8521f74748cdd7f04592c3a17f5fb0d315b91402ed4d0404833e9050`

```mermaid
classDiagram
    class fn_test_project_init_creates_basic_structure {
      <<fn>>
    }
    class fn_test_project_init_with_preset {
      <<fn>>
    }
    class fn_test_project_init_prevents_path_traversal {
      <<fn>>
    }
    class fn_test_project_init_validates_name_format {
      <<fn>>
    }
    class fn_test_project_gen_substitutes_variables {
      <<fn>>
    }
    class fn_test_project_gen_expands_loops {
      <<fn>>
    }
    class fn_test_project_gen_with_rdf_integration {
      <<fn>>
    }
    class fn_test_project_gen_dry_run_mode {
      <<fn>>
    }
    class fn_test_project_gen_processes_rdf_namespaces {
      <<fn>>
    }
```

## Dependencies

- `std::fs`
- `std::path::PathBuf`
- `tempfile::TempDir`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
