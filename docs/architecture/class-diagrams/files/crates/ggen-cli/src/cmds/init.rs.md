# `crates/ggen-cli/src/cmds/init.rs`

Source SHA-256: `9d07adec3575802e3956f8fcd81991d5a86b75bd183869ebe179f767def727bc`

```mermaid
classDiagram
    class type_Result {
      <<type>>
    }
    class struct_InitOutput {
      <<struct>>
      +"status: String"
      +"project_dir: String"
      +"files_created: Vec~String~"
      +"files_overwritten: Option~Vec~String~~"
      +"files_preserved: Option~Vec~String~~"
      +"directories_created: Vec~String~"
      +"error: Option~String~"
      +"warning: Option~String~"
      +"next_steps: Vec~String~"
      +"transaction: Option~TransactionInfo~"
      +"git_hooks: Option~super::git_hooks::HooksInstallOutput~"
    }
    class struct_TransactionInfo {
      <<struct>>
      +"total_files: usize"
      +"backups_created: usize"
      +"committed: bool"
    }
    class fn_init {
      <<fn>>
    }
    class fn_parse_bool_flag {
      <<fn>>
    }
    class fn_perform_init {
      <<fn>>
    }
    class mod_tests {
      <<mod>>
    }
```

## Dependencies

- `clap_noun_verb::Result as VerbResult`
- `clap_noun_verb_macros::verb`
- `crate::error::GgenError`
- `crate::scaffolding::preflight::PreFlightValidator`
- `crate::scaffolding::transaction::FileTransaction`
- `serde::Serialize`
- `std::fs`
- `std::os::unix::fs::PermissionsExt`
- `std::path::Path`
- `std::path::PathBuf`
- `super::*`
- `tempfile::tempdir`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
