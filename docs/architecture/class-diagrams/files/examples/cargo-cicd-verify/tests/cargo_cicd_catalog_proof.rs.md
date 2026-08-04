# `examples/cargo-cicd-verify/tests/cargo_cicd_catalog_proof.rs`

Source SHA-256: `452bfe78f51a36dadac0edccf24a4170cd2d0e1bfe2d7456fa442789d698a7d8`

```mermaid
classDiagram
    class mod_cargo_cicd_catalog {
      <<generated>>
    }
    class fn_catalog_row_count_matches_ontology_query {
      <<generated>>
    }
    class fn_every_queried_row_is_present_with_matching_fields {
      <<generated>>
    }
    class fn_parsed_arg_count_matches_declared_arg_count_for_every_row {
      <<generated>>
    }
    class fn_zero_arg_command_parses_to_empty_arg_list {
      <<generated>>
    }
    class fn_doctor_repo_parses_five_typed_arguments {
      <<generated>>
    }
    class fn_no_duplicate_noun_verb_pairs {
      <<generated>>
    }
    class fn_catalog_is_sorted_by_noun_then_verb {
      <<generated>>
    }
    class fn_distinct_nouns_is_sorted_and_nonempty {
      <<generated>>
    }
    class fn_doctor_noun_has_real_verbs_not_only_the_deprecated_bare_row {
      <<generated>>
    }
    class fn_unknown_command_is_not_valid {
      <<generated>>
    }
    class fn_find_command_returns_none_for_unknown_pair {
      <<generated>>
    }
    class fn_error_variant_count_matches_ontology_query {
      <<generated>>
    }
    class fn_every_queried_error_variant_is_present_and_sorted {
      <<generated>>
    }
    class fn_no_empty_or_duplicate_error_variants {
      <<generated>>
    }
    class fn_return_contract_is_populated_and_uniform {
      <<generated>>
    }
```

## Dependencies

- `cargo_cicd_catalog::{ all_commands_share_return_contract, args_are_consistent, commands_for_noun, distinct_nouns, find_command, is_valid_command, parse_args, CARGO_CICD_COMMANDS, CARGO_CICD_ERROR_ENUM, CARGO_CICD_ERROR_VARIANTS, CARGO_CICD_RETURN_TYPE, }`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
