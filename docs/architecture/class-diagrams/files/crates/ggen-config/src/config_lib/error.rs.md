# `crates/ggen-config/src/config_lib/error.rs`

Source SHA-256: `4c22c55f886af1e5c40ad1981ea3eae031e52100bda6c98987663f470f23b78a`

```mermaid
classDiagram
    class type_Result {
      <<type>>
    }
    class enum_ConfigError {
      <<enum>>
    }
    class fn_format_single_star_toml_error {
      <<fn>>
    }
    class fn_format_star_toml_errors {
      <<fn>>
    }
    note "From~star_toml::Error~ for ConfigError"
```

## Dependencies

- `std::path::PathBuf`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
