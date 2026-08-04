# `crates/ggen-cli/tests/llm_e2e_test.rs`

Source SHA-256: `eb1bc2f20da615e92e02741978962135bab18b5b0ed5aa955a2fdf5c7300ae92`

```mermaid
classDiagram
    class struct_TestProject {
      <<struct>>
      +"temp_dir: TempDir"
      +"project_dir: PathBuf"
    }
    class fn_test_llm_integration_e2e_with_real_api {
      <<fn>>
    }
    class fn_test_llm_integration_without_api_key_fails_gracefully {
      <<fn>>
    }
    note "TestProject"
```

## Dependencies

- `std::fs`
- `std::path::PathBuf`
- `std::process::Command`
- `tempfile::TempDir`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
