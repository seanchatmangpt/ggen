# `crates/ggen-cli/src/cmds/git_hooks.rs`

Source SHA-256: `890badabad336850ce5662bc00e489e0c32e852d8f003b78f1f084660d2e99ef`

```mermaid
classDiagram
    class struct_HookInstallation {
      <<struct>>
      +"hook_name: String"
      +"installed: bool"
      +"skipped: bool"
      +"reason: Option~String~"
    }
    class struct_HooksInstallOutput {
      <<struct>>
      +"git_repo_detected: bool"
      +"hooks_installed: Vec~HookInstallation~"
      +"warnings: Vec~String~"
    }
    class fn_is_git_repo {
      <<fn>>
    }
    class fn_get_hooks_dir {
      <<fn>>
    }
    class fn_is_hook_installed {
      <<fn>>
    }
    class fn_install_hook {
      <<fn>>
    }
    class fn_install_git_hooks {
      <<fn>>
    }
    class mod_tests {
      <<mod>>
    }
```

## Dependencies

- `std::fs`
- `std::os::unix::fs::PermissionsExt`
- `std::path::{Path, PathBuf}`
- `super::*`
- `tempfile::TempDir`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
