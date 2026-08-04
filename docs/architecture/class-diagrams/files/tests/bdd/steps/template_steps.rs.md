# `tests/bdd/steps/template_steps.rs`

Source SHA-256: `207b6df9fd0baf3461bba4e188bec0a9664f21057a1f8e5fe1e8ff7eeb11e681`

```mermaid
classDiagram
    class fn_have_template_file_at_path {
      <<fn>>
    }
    class fn_have_template_file_located_at {
      <<fn>>
    }
    class fn_have_template_with_content {
      <<fn>>
    }
    class fn_have_basic_template_with_content {
      <<fn>>
    }
    class fn_have_templates_in_directory {
      <<fn>>
    }
    class fn_have_template_with_rdf_inline_data {
      <<fn>>
    }
    class fn_have_template_with_sparql_query_definition {
      <<fn>>
    }
    class fn_have_template_with_determinism_configuration {
      <<fn>>
    }
    class fn_have_template_with_rdf_inline {
      <<fn>>
    }
    class fn_have_template_with_sparql_query {
      <<fn>>
    }
    class fn_have_template_with_determinism_config {
      <<fn>>
    }
    class fn_have_templates_for_multiple_languages {
      <<fn>>
    }
    class fn_have_template_with_seed {
      <<fn>>
    }
    class fn_have_rdf_graph_data {
      <<fn>>
    }
    class fn_generate_code_from_template {
      <<fn>>
    }
    class fn_run_ggen_gen {
      <<fn>>
    }
    class fn_run_ggen_gen_with_seed {
      <<fn>>
    }
    class fn_run_ggen_gen_multiple_times {
      <<fn>>
    }
    class fn_run_ggen_gen_with_seed_again {
      <<fn>>
    }
    class fn_generated_file_should_contain {
      <<fn>>
    }
    class fn_a_file_should_be_generated {
      <<fn>>
    }
    class fn_output_should_be_deterministic {
      <<fn>>
    }
    class fn_rdf_graph_should_be_processed {
      <<fn>>
    }
    class fn_output_should_use_rdf_extracted_variables {
      <<fn>>
    }
    class fn_sparql_variables_should_be_extracted {
      <<fn>>
    }
    class fn_output_should_use_queried_values {
      <<fn>>
    }
    class fn_all_outputs_should_be_byte_identical {
      <<fn>>
    }
    class fn_file_should_be_generated_at {
      <<fn>>
    }
    class fn_file_should_use_gpack_template {
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
