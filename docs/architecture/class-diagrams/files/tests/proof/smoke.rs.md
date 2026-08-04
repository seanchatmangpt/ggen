# `tests/proof/smoke.rs`

Source SHA-256: `1deb9fe937e28616217bbd8faebaaeb54969f191e82e1c197e34d36051e18310`

```mermaid
classDiagram
    class type_TestResult {
      <<type>>
    }
    class fn_setup_minimal_ggen_toml {
      <<fn>>
    }
    class fn_setup_minimal_ttl {
      <<fn>>
    }
    class fn_smoke_01_help_command_exits_zero {
      <<fn>>
    }
    class fn_smoke_02_doctor_boot_succeeds {
      <<fn>>
    }
```

## Dependencies

- `assert_cmd::Command`
- `std::fs`
- `tempfile::TempDir`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
