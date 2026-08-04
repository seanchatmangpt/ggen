# `tests/bdd/steps/graph_steps.rs`

Source SHA-256: `94d56b5727786fc601983559f9a682068cf496a41da637bfe0b422c3d0806e8e`

```mermaid
classDiagram
    class fn_create_rdf_file {
      <<fn>>
    }
    class fn_create_rdf_xml_file {
      <<fn>>
    }
    class fn_create_graph_with_person_data {
      <<fn>>
    }
    class fn_create_sparql_query_file {
      <<fn>>
    }
    class fn_create_graph_with_n_triples {
      <<fn>>
    }
    class fn_create_graph_with_multiple_types {
      <<fn>>
    }
    class fn_create_graph_with_n_people {
      <<fn>>
    }
    class fn_run_ggen_graph_command {
      <<fn>>
    }
    class fn_should_see_in_output {
      <<fn>>
    }
    class fn_graph_should_contain_triples {
      <<fn>>
    }
    class fn_should_see_query_results {
      <<fn>>
    }
    class fn_should_see_two_values_in_results {
      <<fn>>
    }
    class fn_output_should_be_valid_json {
      <<fn>>
    }
    class fn_should_see_formatted_table {
      <<fn>>
    }
    class fn_file_should_exist {
      <<fn>>
    }
    class fn_file_should_contain_valid_turtle {
      <<fn>>
    }
    class fn_file_should_be_valid_jsonld {
      <<fn>>
    }
    class fn_file_should_contain_ntriples {
      <<fn>>
    }
    class fn_should_see_all_triples {
      <<fn>>
    }
    class fn_should_see_number_in_results {
      <<fn>>
    }
    class fn_should_see_all_unique_classes {
      <<fn>>
    }
    class fn_should_see_all_properties_for_alice {
      <<fn>>
    }
    class fn_command_should_fail {
      <<fn>>
    }
    class fn_should_see_in_stderr {
      <<fn>>
    }
    class fn_create_rdf_file_simple {
      <<fn>>
    }
    class fn_create_multiple_rdf_files {
      <<fn>>
    }
    class fn_create_named_graph_triples {
      <<fn>>
    }
    class fn_create_graph_with_relationships {
      <<fn>>
    }
    class fn_create_large_graph {
      <<fn>>
    }
    class fn_create_graph_with_50_people {
      <<fn>>
    }
    class fn_create_shacl_shapes {
      <<fn>>
    }
    class fn_create_shacl_shapes_for_validation {
      <<fn>>
    }
    class fn_should_see_validation_report {
      <<fn>>
    }
    class fn_should_see_validation_violations {
      <<fn>>
    }
    class fn_should_see_subject_count {
      <<fn>>
    }
    class fn_should_see_predicate_count {
      <<fn>>
    }
    class fn_should_see_object_count {
      <<fn>>
    }
    class fn_should_see_total_triple_count {
      <<fn>>
    }
    class fn_should_see_graph_statistics {
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
