# `crates/chicago-tdd-tools/src/cli_proof/harness.rs`

Source SHA-256: `537917fb9507cadcf32c4b42566b771ef95a78bd8c44dd52ec24a5a28f559e5e`

```mermaid
classDiagram
    class enum_CliHarnessError {
      <<enum>>
    }
    class enum_BinarySpec {
      <<enum>>
    }
    class struct_CliHarness {
      <<struct>>
      +"binary: BinarySpec"
      +"args: Vec~String~"
      +"envs: HashMap~String"
      +"cwd: Option~PathBuf~"
    }
    class fn_which_in_path {
      <<fn>>
    }
    class struct_CliOutput {
      <<struct>>
      +"stdout: String"
      +"stderr: String"
      +"exit_code: i32"
      +"duration: Duration"
    }
    note "CliHarness"
    note "CliOutput"
    note "From~std::io::Error~ for CliHarnessError"
    note "std::error::Error for CliHarnessError"
    note "std::fmt::Display for CliHarnessError"
```

## Dependencies

- `crate::cli_proof::TempWorkspace`
- `std::collections::HashMap`
- `std::ffi::OsStr`
- `std::path::{Path, PathBuf}`
- `std::process::Command`
- `std::time::{Duration, Instant}`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
