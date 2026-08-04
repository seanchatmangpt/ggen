# `crates/praxis-graphlaw/src/builtins/builtins_test.rs`

Source SHA-256: `9c26104271f3c2270b464d1f4aadcf216aedf449ec52e0bd33cec05c2bf5d7f3`

```mermaid
classDiagram
    class fn_num {
      <<fn>>
    }
    class fn_s {
      <<fn>>
    }
    class fn_v {
      <<fn>>
    }
    class fn_iri {
      <<fn>>
    }
    class fn_ground_triple {
      <<fn>>
    }
    class fn_decoded_number {
      <<fn>>
    }
    class fn_decoded_string {
      <<fn>>
    }
    class fn_classify_recognizes_all_procedural_builtins {
      <<fn>>
    }
    class fn_classify_registers_reasoner_level_builtins_as_known {
      <<fn>>
    }
    class fn_classify_returns_none_for_unknown_predicate {
      <<fn>>
    }
    class fn_classify_returns_none_for_variable_predicate {
      <<fn>>
    }
    class fn_evaluate_returns_none_for_reasoner_level_kind {
      <<fn>>
    }
    class fn_log_equal_to_holds_for_equal_numbers {
      <<fn>>
    }
    class fn_log_equal_to_rejects_unequal_numbers {
      <<fn>>
    }
    class fn_log_not_equal_to_holds_for_unequal_numbers {
      <<fn>>
    }
    class fn_log_not_equal_to_rejects_equal_numbers {
      <<fn>>
    }
    class fn_math_greater_than_holds {
      <<fn>>
    }
    class fn_math_greater_than_rejects_wrong_direction {
      <<fn>>
    }
    class fn_math_greater_than_rejects_non_numeric_operand {
      <<fn>>
    }
    class fn_math_less_than_holds {
      <<fn>>
    }
    class fn_math_not_less_than_holds_for_equal {
      <<fn>>
    }
    class fn_math_not_greater_than_holds_for_equal {
      <<fn>>
    }
    class fn_math_equal_to_holds {
      <<fn>>
    }
    class fn_math_equal_to_rejects_unequal {
      <<fn>>
    }
    class fn_math_sum_computes_total {
      <<fn>>
    }
    class fn_math_sum_rejects_non_list_subject {
      <<fn>>
    }
    class fn_math_difference_computes_result {
      <<fn>>
    }
    class fn_math_difference_rejects_wrong_arity {
      <<fn>>
    }
    class fn_math_product_computes_total {
      <<fn>>
    }
    class fn_math_quotient_computes_result {
      <<fn>>
    }
    class fn_math_quotient_rejects_division_by_zero {
      <<fn>>
    }
    class fn_math_remainder_computes_result {
      <<fn>>
    }
    class fn_math_remainder_rejects_division_by_zero {
      <<fn>>
    }
    class fn_string_length_counts_chars {
      <<fn>>
    }
    class fn_string_length_rejects_non_var_object {
      <<fn>>
    }
    class fn_string_concat_joins_members {
      <<fn>>
    }
    class fn_string_concat_rejects_non_list_subject {
      <<fn>>
    }
    class fn_string_less_than_holds {
      <<fn>>
    }
    class fn_string_less_than_rejects_wrong_order {
      <<fn>>
    }
    class fn_list_length_counts_members {
      <<fn>>
    }
    class fn_list_length_rejects_non_list_subject {
      <<fn>>
    }
    class fn_list_in_generates_each_member {
      <<fn>>
    }
    class fn_list_in_rejects_non_var_subject {
      <<fn>>
    }
    class fn_list_append_concatenates_lists {
      <<fn>>
    }
    class fn_list_append_rejects_wrong_arity {
      <<fn>>
    }
    class fn_list_first_returns_first_member {
      <<fn>>
    }
    class fn_list_first_rejects_empty_list {
      <<fn>>
    }
    class fn_list_rest_drops_first_member {
      <<fn>>
    }
    class fn_list_last_returns_last_member {
      <<fn>>
    }
    class fn_list_member_generates_each_value {
      <<fn>>
    }
    class fn_list_member_rejects_non_var_object {
      <<fn>>
    }
    class fn_list_member_at_returns_indexed_value {
      <<fn>>
    }
    class fn_list_member_at_rejects_out_of_range_index {
      <<fn>>
    }
    class fn_list_remove_drops_matching_items {
      <<fn>>
    }
    class fn_list_sort_orders_numerically {
      <<fn>>
    }
    class fn_list_unique_dedupes_preserving_order {
      <<fn>>
    }
    class fn_list_reverse_reverses_members {
      <<fn>>
    }
    class fn_list_iterate_generates_index_item_pairs {
      <<fn>>
    }
    class fn_func_lang_from_plain_literal_extracts_tag {
      <<fn>>
    }
    class fn_func_lang_from_plain_literal_defaults_to_empty_string {
      <<fn>>
    }
    class fn_func_lang_from_plain_literal_rejects_wrong_arity {
      <<fn>>
    }
    class fn_evaluate_dispatches_sum_same_as_direct_call {
      <<fn>>
    }
```

## Dependencies

- `crate::{Triple, VarOrTerm}`
- `super::*`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
