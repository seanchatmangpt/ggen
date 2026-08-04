# `crates/ggen-cli/tests/init_git_hooks_test.rs`

Source SHA-256: `74962e8d8a879cc5b9f11ca803a3d82ce8c92aebbdd017c93dbd95bc0c6729d2`

```mermaid
classDiagram
    class fn_test_init_installs_git_hooks_in_git_repo {
      <<fn>>
    }
    class fn_test_init_skips_hooks_with_flag {
      <<fn>>
    }
    class fn_test_init_handles_non_git_repo_gracefully {
      <<fn>>
    }
    class fn_test_hooks_are_executable_on_unix {
      <<fn>>
    }
    class fn_test_hook_content_includes_cargo_make {
      <<fn>>
    }
    class fn_test_existing_hooks_are_not_overwritten {
      <<fn>>
    }
```

## Dependencies

- `std::fs`
- `std::os::unix::fs::PermissionsExt`
- `tempfile::TempDir`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
