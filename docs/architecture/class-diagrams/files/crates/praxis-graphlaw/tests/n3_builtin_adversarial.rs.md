# `crates/praxis-graphlaw/tests/n3_builtin_adversarial.rs`

Source SHA-256: `6a9241a9d0d68f80ceb0cf8324fdfa864945efb2061adf23ccc9d86a5835047c`

```mermaid
classDiagram
    class fn_decode_all {
      <<fn>>
    }
    class fn_materialize {
      <<fn>>
    }
    class fn_math_logarithm_of_zero_yields_neg_infinity_literal_not_a_crash {
      <<fn>>
    }
    class fn_math_logarithm_of_negative_yields_nan_literal_not_a_crash {
      <<fn>>
    }
    class fn_math_greater_than_rejects_non_numeric_literal {
      <<fn>>
    }
    class fn_math_sum_rejects_non_numeric_list_member {
      <<fn>>
    }
    class fn_string_length_of_empty_string_is_zero {
      <<fn>>
    }
    class fn_string_concat_preserves_unicode {
      <<fn>>
    }
    class fn_string_matches_invalid_regex_fails_closed_no_panic {
      <<fn>>
    }
    class fn_string_not_matches_invalid_regex_fails_closed_no_panic {
      <<fn>>
    }
    class fn_string_contains_ignoring_case_boundary {
      <<fn>>
    }
    class fn_list_length_of_empty_list_is_zero {
      <<fn>>
    }
    class fn_list_first_and_last_of_empty_list_do_not_derive {
      <<fn>>
    }
    class fn_list_first_and_last_of_single_element_list_are_the_same_element {
      <<fn>>
    }
    class fn_list_rest_of_single_element_list_is_empty {
      <<fn>>
    }
    class fn_log_equal_to_literal_vs_iri_are_not_equal {
      <<fn>>
    }
    class fn_log_equal_to_iri_vs_blank_node_are_not_equal {
      <<fn>>
    }
    class fn_log_equal_to_literal_vs_blank_node_are_not_equal {
      <<fn>>
    }
    class fn_log_equal_to_same_iri_is_equal {
      <<fn>>
    }
    class fn_log_equal_to_numeric_literals_different_lexical_same_value_are_equal {
      <<fn>>
    }
    class fn_log_bound_on_genuinely_unbound_variable_never_fires {
      <<fn>>
    }
    class fn_log_bound_on_a_bound_variable_fires {
      <<fn>>
    }
```

## Dependencies

- `praxis_graphlaw::TripleStore`
- `proptest::prelude::*`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
