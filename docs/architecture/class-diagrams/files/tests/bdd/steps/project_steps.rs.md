# `tests/bdd/steps/project_steps.rs`

Source SHA-256: `01a9a0a3c8af2125726a9eb21d2f4d63cc8d915eec78340735eb43b921bb0939`

```mermaid
classDiagram
    class fn_clean_project_directory {
      <<fn>>
    }
    class fn_create_project_template_file {
      <<fn>>
    }
    class fn_create_existing_file {
      <<fn>>
    }
    class fn_create_plan_file {
      <<fn>>
    }
    class fn_run_ggen_project_command {
      <<fn>>
    }
    class fn_command_should_succeed {
      <<fn>>
    }
    class fn_file_should_exist {
      <<fn>>
    }
    class fn_file_should_contain {
      <<fn>>
    }
    class fn_should_see_in_output {
      <<fn>>
    }
    class fn_file_should_not_exist {
      <<fn>>
    }
    class fn_file_should_still_contain {
      <<fn>>
    }
    class fn_command_should_fail {
      <<fn>>
    }
    class fn_should_see_in_stderr {
      <<fn>>
    }
    class fn_should_see_pattern_in_output {
      <<fn>>
    }
    class fn_both_outputs_should_be_identical {
      <<fn>>
    }
    class fn_should_see_generated_in_output {
      <<fn>>
    }
    class fn_run_project_gen_with_seed {
      <<fn>>
    }
    class fn_run_project_gen_with_variable {
      <<fn>>
    }
    class fn_run_project_gen_with_multiple_variables {
      <<fn>>
    }
    class fn_run_project_plan_with_seed {
      <<fn>>
    }
    class fn_run_project_diff_with_variable {
      <<fn>>
    }
    class fn_run_project_apply {
      <<fn>>
    }
    class fn_output_should_be_deterministic {
      <<fn>>
    }
    class fn_should_see_plan_output {
      <<fn>>
    }
    class fn_should_see_diff_output {
      <<fn>>
    }
```

## Dependencies

- `assert_cmd::Command`
- `cucumber::{given, then, when}`
- `std::fs`
- `super::super::world::GgenWorld`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
