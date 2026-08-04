# `tests/bdd/steps/rdf_steps.rs`

Source SHA-256: `61e0e53b65de32f593aa0ee06e06c53e886c892506364f8041f1d92542ed1232`

```mermaid
classDiagram
    class fn_have_rdf_ontology_file {
      <<fn>>
    }
    class fn_have_sparql_query_file {
      <<fn>>
    }
    class fn_run_sparql_query {
      <<fn>>
    }
    class fn_should_see_query_results {
      <<fn>>
    }
    class fn_results_should_be_in_format {
      <<fn>>
    }
    class fn_have_rdf_file {
      <<fn>>
    }
    class fn_have_template_referencing_rdf {
      <<fn>>
    }
    class fn_have_template_with_inline_turtle_rdf {
      <<fn>>
    }
    class fn_have_rdf_data_with_entities {
      <<fn>>
    }
    class fn_have_template_with_sparql_query_for_entities {
      <<fn>>
    }
    class fn_have_template_with_custom_prefixes {
      <<fn>>
    }
    class fn_rdf_data_should_be_loaded {
      <<fn>>
    }
    class fn_rdf_should_be_available_for_sparql {
      <<fn>>
    }
    class fn_inline_rdf_should_be_parsed {
      <<fn>>
    }
    class fn_rdf_should_be_added_to_graph {
      <<fn>>
    }
    class fn_variables_should_be_extracted_via_sparql {
      <<fn>>
    }
    class fn_variables_should_be_available_in_template {
      <<fn>>
    }
    class fn_prefixes_should_be_registered {
      <<fn>>
    }
    class fn_prefixes_should_be_usable_in_rdf_and_sparql {
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
