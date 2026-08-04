# `tests/bdd/world.rs`

Source SHA-256: `8e51f4c680bd6b5796921457f089070280cdf69697edd5cde115a0ba4232d34c`

```mermaid
classDiagram
    class struct_GgenWorld {
      <<struct>>
      +"temp_dir: Option~TempDir~"
      +"project_dir: PathBuf"
      +"last_output: Option~Output~"
      +"last_exit_code: Option~i32~"
      +"captured_files: HashMap~String"
      +"captured_hashes: Vec~String~"
      +"registry_url: Option~String~"
      +"mock_server: Option~mockito::Server~"
    }
    note "GgenWorld"
```

## Dependencies

- `std::collections::HashMap`
- `std::path::PathBuf`
- `std::process::Output`
- `tempfile::TempDir`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
