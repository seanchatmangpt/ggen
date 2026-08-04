# `tests/security/v2_security_audit.rs`

Source SHA-256: `2b6eac2b4e17e79c59e0de9cccfedbd298788849b997d6c8514c94a50c941f0a`

```mermaid
classDiagram
    class mod_test_config {
      <<mod>>
    }
    class fn_test_path_traversal_in_template_path {
      <<fn>>
    }
    class fn_test_path_traversal_in_output_path {
      <<fn>>
    }
    class fn_test_absolute_path_injection {
      <<fn>>
    }
    class fn_test_null_byte_path_injection {
      <<fn>>
    }
    class fn_test_symlink_path_traversal {
      <<fn>>
    }
    class fn_test_unicode_path_traversal {
      <<fn>>
    }
    class fn_test_template_code_execution_prevention {
      <<fn>>
    }
    class fn_test_sparql_injection_protection {
      <<fn>>
    }
    class fn_test_rdf_injection_protection {
      <<fn>>
    }
    class fn_test_shell_hook_command_injection {
      <<fn>>
    }
    class fn_test_environment_variable_injection {
      <<fn>>
    }
    class fn_test_backtick_command_substitution {
      <<fn>>
    }
    class fn_test_process_substitution_attack {
      <<fn>>
    }
    class fn_test_symlink_attack_prevention {
      <<fn>>
    }
    class fn_test_race_condition_toctou {
      <<fn>>
    }
    class fn_test_permission_escalation_prevention {
      <<fn>>
    }
    class fn_test_cli_argument_injection {
      <<fn>>
    }
    class fn_test_yaml_bomb_prevention {
      <<fn>>
    }
    class fn_test_regex_dos_prevention {
      <<fn>>
    }
    class fn_test_zip_slip_attack {
      <<fn>>
    }
    class fn_test_sensitive_data_not_logged {
      <<fn>>
    }
    class fn_test_error_messages_no_info_disclosure {
      <<fn>>
    }
    class fn_test_timing_attack_resistance {
      <<fn>>
    }
```

## Dependencies

- `assert_cmd::Command`
- `assert_fs::TempDir`
- `assert_fs::prelude::*`
- `predicates::prelude::*`
- `std::fs`
- `std::os::unix::fs as unix_fs`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
