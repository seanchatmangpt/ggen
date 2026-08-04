# `tests/integration/lifecycle_simple_tests.rs`

Source SHA-256: `16ce557facef83080b74fa0cae78991cc4237d3b657fe21fd12d17bcce8c2dca`

```mermaid
classDiagram
    class mod_common {
      <<mod>>
    }
    class fn_test_temp_dir_creation {
      <<fn>>
    }
    class fn_test_echo_command_helper {
      <<fn>>
    }
    class fn_test_fixtures_are_accessible {
      <<fn>>
    }
```

## Dependencies

- `anyhow::Result`
- `common::{create_temp_dir, echo_command}`
- `common::{sample_make_toml, sample_template_content}`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
