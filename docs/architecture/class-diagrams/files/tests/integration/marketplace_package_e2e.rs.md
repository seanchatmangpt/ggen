# `tests/integration/marketplace_package_e2e.rs`

Source SHA-256: `792cffd276d419b5280d9346f6a2c0c021a7ccea77ef9eb154084d335b3f1b25`

```mermaid
classDiagram
    class mod_common {
      <<mod>>
    }
    class struct_ProjectSnapshot {
      <<struct>>
      +"file_count: usize"
      +"dir_count: usize"
      +"git_status_hash: String"
    }
    class fn_capture_project_snapshot {
      <<fn>>
    }
    class fn_marketplace_package_e2e {
      <<fn>>
    }
    class fn_run_marketplace_package_validation {
      <<fn>>
    }
```

## Dependencies

- `chicago_tdd_tools::testcontainers::{ exec::SUCCESS_EXIT_CODE, ContainerClient, GenericContainer, TestcontainersResult, }`
- `common::require_docker`
- `std::process::Command`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
