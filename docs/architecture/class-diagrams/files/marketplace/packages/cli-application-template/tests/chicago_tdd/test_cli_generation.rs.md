# `marketplace/packages/cli-application-template/tests/chicago_tdd/test_cli_generation.rs`

Source SHA-256: `a07ae24478dd66f48051ccb319ba9e46d587b2420afea8274055f721265d4a93`

```mermaid
classDiagram
    class fn_test_cli_help_output {
      <<fn>>
    }
    class fn_test_command_execution_with_arguments {
      <<fn>>
    }
    class fn_test_subcommand_execution {
      <<fn>>
    }
    class fn_test_required_argument_validation {
      <<fn>>
    }
    class fn_test_option_with_default_value {
      <<fn>>
    }
    class fn_test_multiple_option_values {
      <<fn>>
    }
    class fn_test_config_file_loading_toml {
      <<fn>>
    }
    class fn_test_config_file_loading_yaml {
      <<fn>>
    }
    class fn_test_config_file_loading_json {
      <<fn>>
    }
    class fn_test_environment_variable_binding {
      <<fn>>
    }
    class fn_test_validation_regex_pattern {
      <<fn>>
    }
    class fn_test_validation_range_check {
      <<fn>>
    }
    class fn_test_enum_option_validation {
      <<fn>>
    }
    class fn_test_shell_completion_bash {
      <<fn>>
    }
    class fn_test_shell_completion_zsh {
      <<fn>>
    }
    class fn_test_shell_completion_fish {
      <<fn>>
    }
    class fn_test_verbose_logging_levels {
      <<fn>>
    }
    class fn_test_version_flag {
      <<fn>>
    }
    class fn_test_file_input_processing {
      <<fn>>
    }
    class fn_test_file_output_creation {
      <<fn>>
    }
    class fn_test_interactive_prompt_simulation {
      <<fn>>
    }
    class fn_test_error_handling_invalid_path {
      <<fn>>
    }
    class fn_test_error_handling_permission_denied {
      <<fn>>
    }
    class fn_test_concurrent_command_execution {
      <<fn>>
    }
    class fn_test_signal_handling_ctrl_c {
      <<fn>>
    }
    class fn_test_path_argument_validation {
      <<fn>>
    }
    class fn_test_color_output_with_env_var {
      <<fn>>
    }
    class fn_test_custom_help_template {
      <<fn>>
    }
    class fn_test_command_alias {
      <<fn>>
    }
    class fn_test_global_options_inheritance {
      <<fn>>
    }
```

## Dependencies

- `assert_cmd::Command`
- `nix::sys::signal::{self, Signal}`
- `nix::unistd::Pid`
- `predicates::prelude::*`
- `std::fs`
- `std::io::Write`
- `std::os::unix::fs::PermissionsExt`
- `std::path::PathBuf`
- `std::process::Stdio`
- `std::process::{Command, Stdio}`
- `std::sync::Arc`
- `std::thread`
- `std::time::Duration`
- `tempfile::TempDir`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
