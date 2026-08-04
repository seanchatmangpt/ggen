# `tests/bdd/steps/common_steps.rs`

Source SHA-256: `225d38a437658262d330756cbe8b8fea1003a30f3dff65380714ffd0ad5f5dd6`

```mermaid
classDiagram
    class fn_clean_project_directory {
      <<fn>>
    }
    class fn_ggen_is_installed {
      <<fn>>
    }
    class fn_run_generic_command {
      <<fn>>
    }
    class fn_command_should_succeed {
      <<fn>>
    }
    class fn_should_see_text {
      <<fn>>
    }
    class fn_file_should_exist {
      <<fn>>
    }
```

## Dependencies

- `assert_cmd::Command`
- `cucumber::{given, then, when}`
- `std::fs`
- `super::super::world::GgenWorld`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
