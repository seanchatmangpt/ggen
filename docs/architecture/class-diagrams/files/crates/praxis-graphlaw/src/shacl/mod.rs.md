# `crates/praxis-graphlaw/src/shacl/mod.rs`

Source SHA-256: `3b27e8f981976747452bfaae515a21fe71a173e1599bf505726e487a5c1fb0c4`

```mermaid
classDiagram
    class mod_canonicalization {
      <<mod>>
    }
    class mod_closure {
      <<mod>>
    }
    class mod_equivalence {
      <<mod>>
    }
    class mod_index_utils {
      <<mod>>
    }
    class mod_messages {
      <<mod>>
    }
    class mod_model {
      <<mod>>
    }
    class mod_report {
      <<mod>>
    }
    class mod_shacl_test {
      <<mod>>
    }
    class mod_sparql {
      <<mod>>
    }
    class mod_targets_paths {
      <<mod>>
    }
    class mod_validate {
      <<mod>>
    }
    class mod_values {
      <<mod>>
    }
    class struct_Vocab {
      <<struct>>
      +"rdf_type: usize"
      +"rdf_first: usize"
      +"rdf_rest: usize"
      +"rdf_nil: usize"
      +"rdfs_class: usize"
      +"rdfs_subclass_of: usize"
      +"rdfs_sub_property_of: usize"
      +"owl_same_as: usize"
      +"owl_equivalent_class: usize"
      +"owl_equivalent_property: usize"
      +"sh_node_shape: usize"
      +"sh_property_shape: usize"
      +"sh_target_class: usize"
      +"sh_target_node: usize"
      +"sh_target_subjects_of: usize"
      +"sh_target_objects_of: usize"
      +"sh_property: usize"
      +"sh_node: usize"
      +"sh_path: usize"
      +"sh_min_count: usize"
      +"sh_max_count: usize"
      +"sh_datatype: usize"
      +"sh_class: usize"
      +"sh_pattern: usize"
      +"sh_flags: usize"
      +"sh_in: usize"
      +"sh_and: usize"
      +"sh_or: usize"
      +"sh_not: usize"
      +"sh_xone: usize"
      +"sh_node_kind: usize"
      +"sh_has_value: usize"
      +"sh_min_length: usize"
      +"sh_max_length: usize"
      +"sh_min_exclusive: usize"
      +"sh_min_inclusive: usize"
      +"sh_max_exclusive: usize"
      +"sh_max_inclusive: usize"
      +"sh_language_in: usize"
      +"sh_unique_lang: usize"
      +"sh_equals: usize"
      +"sh_disjoint: usize"
      +"sh_less_than: usize"
      +"sh_less_than_or_equals: usize"
      +"sh_qualified_value_shape: usize"
      +"sh_qualified_min_count: usize"
      +"sh_qualified_max_count: usize"
      +"sh_closed: usize"
      +"sh_ignored_properties: usize"
      +"sh_deactivated: usize"
      +"sh_message: usize"
      +"sh_severity: usize"
      +"sh_violation: usize"
      +"sh_warning: usize"
      +"sh_info: usize"
      +"sh_validation_report: usize"
      +"sh_conforms: usize"
      +"sh_result: usize"
      +"sh_validation_result: usize"
      +"sh_focus_node: usize"
      +"sh_result_path: usize"
      +"sh_value: usize"
      +"sh_result_message: usize"
      +"sh_source_constraint_component: usize"
      +"sh_source_shape: usize"
      +"sh_result_severity: usize"
      +"sh_alternative_path: usize"
      +"sh_inverse_path: usize"
      +"sh_zero_or_more_path: usize"
      +"sh_one_or_more_path: usize"
      +"sh_zero_or_one_path: usize"
      +"sh_sparql: usize"
      +"sh_select: usize"
      +"sh_ask: usize"
      +"sh_prefixes: usize"
      +"sh_target: usize"
      +"sh_sparql_target: usize"
      +"sh_iri: usize"
      +"sh_blank_node: usize"
      +"sh_literal: usize"
      +"sh_blank_node_or_iri: usize"
      +"sh_blank_node_or_literal: usize"
      +"sh_iri_or_literal: usize"
      +"sh_min_count_constraint_component: usize"
      +"sh_max_count_constraint_component: usize"
      +"sh_datatype_constraint_component: usize"
      +"sh_class_constraint_component: usize"
      +"sh_pattern_constraint_component: usize"
      +"sh_in_constraint_component: usize"
      +"sh_and_constraint_component: usize"
      +"sh_or_constraint_component: usize"
      +"sh_not_constraint_component: usize"
      +"sh_xone_constraint_component: usize"
      +"sh_node_kind_constraint_component: usize"
      +"sh_has_value_constraint_component: usize"
      +"sh_min_length_constraint_component: usize"
      +"sh_max_length_constraint_component: usize"
      +"sh_min_exclusive_constraint_component: usize"
      +"sh_min_inclusive_constraint_component: usize"
      +"sh_max_exclusive_constraint_component: usize"
      +"sh_max_inclusive_constraint_component: usize"
      +"sh_language_in_constraint_component: usize"
      +"sh_unique_lang_constraint_component: usize"
      +"sh_equals_constraint_component: usize"
      +"sh_disjoint_constraint_component: usize"
      +"sh_less_than_constraint_component: usize"
      +"sh_less_than_or_equals_constraint_component: usize"
      +"sh_qualified_value_shape_constraint_component: usize"
      +"sh_closed_constraint_component: usize"
      +"sh_node_constraint_component: usize"
      +"sh_sparql_constraint_component: usize"
    }
    note "Default for Vocab"
    note "Vocab"
```

## Dependencies

- `canonicalization::UnionFind`
- `closure::{ClosureMatrix, SubclassClosure}`
- `crate::encoding::Encoder`
- `equivalence::{ canonicalize_equivalences, render_all_equivalences_canonical, EquivalenceCanonical, }`
- `index_utils::{ get_datatype, get_objects, is_blank_node, is_iri, is_lexically_valid_for_datatype, is_literal, }`
- `model::{ CompiledConstraint, CompiledShape, CompiledTarget, CostClass, PropertyMask, ShapesGraph, TargetType, SHACL_SPARQL_BOUNDARY, }`
- `report::{ValidationReport, ValidationResult, Validator}`
- `values::{compare_numeric, decode_to_term, get_lang_tag, get_lexical_form, match_regex}`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
