# `tests/bdd/steps/template_management_steps.rs`

Source SHA-256: `a35f695685534b9dec9f0d2bc1f47d0f614a4d5b327b2563e053bcc2184ec8b4`

```mermaid
classDiagram
    class fn_have_local_template {
      <<fn>>
    }
    class fn_have_file_with_content {
      <<fn>>
    }
    class fn_have_multiple_templates {
      <<fn>>
    }
    class fn_have_templates_with_descriptions {
      <<fn>>
    }
    class fn_have_management_template_with_field {
      <<fn>>
    }
    class fn_run_ggen_template_management_command {
      <<fn>>
    }
    class fn_answer_interactive_prompt {
      <<fn>>
    }
    class fn_command_should_succeed {
      <<fn>>
    }
    class fn_should_see_gpacks_templates_in_output {
      <<fn>>
    }
    class fn_should_not_see_gpacks_templates_in_output {
      <<fn>>
    }
    class fn_file_should_exist {
      <<fn>>
    }
    class fn_file_should_contain_yaml_frontmatter {
      <<fn>>
    }
    class fn_should_see_in_output {
      <<fn>>
    }
    class fn_template_should_have_field {
      <<fn>>
    }
    class fn_template_should_have_variables {
      <<fn>>
    }
    class fn_should_see_gpack_templates {
      <<fn>>
    }
    class fn_should_not_see_gpack_templates {
      <<fn>>
    }
    class fn_should_see_template_metadata {
      <<fn>>
    }
    class fn_rdf_validation_should_pass {
      <<fn>>
    }
    class fn_command_should_fail {
      <<fn>>
    }
    class fn_run_ggen_template_command {
      <<fn>>
    }
    class fn_have_template_with_content_named {
      <<fn>>
    }
    class fn_have_template_named {
      <<fn>>
    }
    class fn_have_template_with_version {
      <<fn>>
    }
    class fn_have_multiple_templates_with_descriptions {
      <<fn>>
    }
    class fn_should_see_in_stderr {
      <<fn>>
    }
    class fn_template_created_from_file {
      <<fn>>
    }
    class fn_should_not_see_in_output {
      <<fn>>
    }
    class fn_should_see_preview_of_output {
      <<fn>>
    }
    class fn_template_should_have_rdf_section {
      <<fn>>
    }
    class fn_template_should_have_sparql_section {
      <<fn>>
    }
    class fn_should_see_descriptions_for_templates {
      <<fn>>
    }
    class fn_should_see_compatibility_info {
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
