// Vendored research-lineage engine (RoXi lineage): API reshaping is out of scope;
// lints below are documented scoped allows, not silent drift.
#![allow(clippy::too_many_arguments, deprecated)]

use crate::encoding::Encoder;

// Module declarations
pub mod canonicalization;
pub mod closure;
pub mod equivalence;
pub mod index_utils;
pub mod messages;
pub mod model;
pub mod report;
mod shacl_test;
pub mod sparql;
pub mod targets_paths;
pub mod validate;
pub mod values;

// Re-exports
pub use canonicalization::UnionFind;
pub use closure::{ClosureMatrix, SubclassClosure};
pub use equivalence::{canonicalize_equivalences, EquivalenceCanonical};
pub use model::{
    CompiledConstraint, CompiledShape, CompiledTarget, CostClass, PropertyMask, ShapesGraph,
    TargetType, SHACL_SPARQL_BOUNDARY,
};
pub use report::{ValidationReport, ValidationResult, Validator};

// Re-export utility functions for shex_native and other modules
pub use index_utils::{
    get_datatype, get_objects, is_blank_node, is_iri, is_lexically_valid_for_datatype, is_literal,
};
pub use values::{compare_numeric, decode_to_term, get_lang_tag, get_lexical_form, match_regex};

// ---------------------------------------------------------------------------
// Vocabulary
// ---------------------------------------------------------------------------

pub struct Vocab {
    pub rdf_type: usize,
    pub rdf_first: usize,
    pub rdf_rest: usize,
    pub rdf_nil: usize,
    pub rdfs_class: usize,
    pub rdfs_subclass_of: usize,
    pub rdfs_sub_property_of: usize,
    pub owl_same_as: usize,
    pub owl_equivalent_class: usize,
    pub owl_equivalent_property: usize,
    pub sh_node_shape: usize,
    pub sh_property_shape: usize,
    pub sh_target_class: usize,
    pub sh_target_node: usize,
    pub sh_target_subjects_of: usize,
    pub sh_target_objects_of: usize,
    pub sh_property: usize,
    pub sh_node: usize,
    pub sh_path: usize,
    pub sh_min_count: usize,
    pub sh_max_count: usize,
    pub sh_datatype: usize,
    pub sh_class: usize,
    pub sh_pattern: usize,
    pub sh_flags: usize,
    pub sh_in: usize,
    pub sh_and: usize,
    pub sh_or: usize,
    pub sh_not: usize,
    pub sh_xone: usize,
    pub sh_node_kind: usize,
    pub sh_has_value: usize,
    pub sh_min_length: usize,
    pub sh_max_length: usize,
    pub sh_min_exclusive: usize,
    pub sh_min_inclusive: usize,
    pub sh_max_exclusive: usize,
    pub sh_max_inclusive: usize,
    pub sh_language_in: usize,
    pub sh_unique_lang: usize,
    pub sh_equals: usize,
    pub sh_disjoint: usize,
    pub sh_less_than: usize,
    pub sh_less_than_or_equals: usize,
    pub sh_qualified_value_shape: usize,
    pub sh_qualified_min_count: usize,
    pub sh_qualified_max_count: usize,
    pub sh_closed: usize,
    pub sh_ignored_properties: usize,
    pub sh_deactivated: usize,
    pub sh_message: usize,
    pub sh_severity: usize,
    pub sh_violation: usize,
    pub sh_warning: usize,
    pub sh_info: usize,
    pub sh_validation_report: usize,
    pub sh_conforms: usize,
    pub sh_result: usize,
    pub sh_validation_result: usize,
    pub sh_focus_node: usize,
    pub sh_result_path: usize,
    pub sh_value: usize,
    pub sh_result_message: usize,
    pub sh_source_constraint_component: usize,
    pub sh_source_shape: usize,
    pub sh_result_severity: usize,
    pub sh_alternative_path: usize,
    pub sh_inverse_path: usize,
    pub sh_zero_or_more_path: usize,
    pub sh_one_or_more_path: usize,
    pub sh_zero_or_one_path: usize,

    // SPARQL-based constraints (sh:sparql / SPARQLConstraintComponent) and targets
    pub sh_sparql: usize,
    pub sh_select: usize,
    pub sh_ask: usize,
    pub sh_prefixes: usize,
    pub sh_target: usize,
    pub sh_sparql_target: usize,

    // sh:nodeKind values
    pub sh_iri: usize,
    pub sh_blank_node: usize,
    pub sh_literal: usize,
    pub sh_blank_node_or_iri: usize,
    pub sh_blank_node_or_literal: usize,
    pub sh_iri_or_literal: usize,

    // Constraint Components
    pub sh_min_count_constraint_component: usize,
    pub sh_max_count_constraint_component: usize,
    pub sh_datatype_constraint_component: usize,
    pub sh_class_constraint_component: usize,
    pub sh_pattern_constraint_component: usize,
    pub sh_in_constraint_component: usize,
    pub sh_and_constraint_component: usize,
    pub sh_or_constraint_component: usize,
    pub sh_not_constraint_component: usize,
    pub sh_xone_constraint_component: usize,
    pub sh_node_kind_constraint_component: usize,
    pub sh_has_value_constraint_component: usize,
    pub sh_min_length_constraint_component: usize,
    pub sh_max_length_constraint_component: usize,
    pub sh_min_exclusive_constraint_component: usize,
    pub sh_min_inclusive_constraint_component: usize,
    pub sh_max_exclusive_constraint_component: usize,
    pub sh_max_inclusive_constraint_component: usize,
    pub sh_language_in_constraint_component: usize,
    pub sh_unique_lang_constraint_component: usize,
    pub sh_equals_constraint_component: usize,
    pub sh_disjoint_constraint_component: usize,
    pub sh_less_than_constraint_component: usize,
    pub sh_less_than_or_equals_constraint_component: usize,
    pub sh_qualified_value_shape_constraint_component: usize,
    pub sh_closed_constraint_component: usize,
    pub sh_node_constraint_component: usize,
    pub sh_sparql_constraint_component: usize,
}

impl Vocab {
    pub fn new() -> Self {
        Vocab {
            rdf_type: Encoder::add("<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>".to_string()),
            rdf_first: Encoder::add(
                "<http://www.w3.org/1999/02/22-rdf-syntax-ns#first>".to_string(),
            ),
            rdf_rest: Encoder::add("<http://www.w3.org/1999/02/22-rdf-syntax-ns#rest>".to_string()),
            rdf_nil: Encoder::add("<http://www.w3.org/1999/02/22-rdf-syntax-ns#nil>".to_string()),
            rdfs_class: Encoder::add("<http://www.w3.org/2000/01/rdf-schema#Class>".to_string()),
            rdfs_subclass_of: Encoder::add(
                "<http://www.w3.org/2000/01/rdf-schema#subClassOf>".to_string(),
            ),
            rdfs_sub_property_of: Encoder::add(
                "<http://www.w3.org/2000/01/rdf-schema#subPropertyOf>".to_string(),
            ),
            owl_same_as: Encoder::add("<http://www.w3.org/2002/07/owl#sameAs>".to_string()),
            owl_equivalent_class: Encoder::add(
                "<http://www.w3.org/2002/07/owl#equivalentClass>".to_string(),
            ),
            owl_equivalent_property: Encoder::add(
                "<http://www.w3.org/2002/07/owl#equivalentProperty>".to_string(),
            ),
            sh_node_shape: Encoder::add("<http://www.w3.org/ns/shacl#NodeShape>".to_string()),
            sh_property_shape: Encoder::add(
                "<http://www.w3.org/ns/shacl#PropertyShape>".to_string(),
            ),
            sh_target_class: Encoder::add("<http://www.w3.org/ns/shacl#targetClass>".to_string()),
            sh_target_node: Encoder::add("<http://www.w3.org/ns/shacl#targetNode>".to_string()),
            sh_target_subjects_of: Encoder::add(
                "<http://www.w3.org/ns/shacl#targetSubjectsOf>".to_string(),
            ),
            sh_target_objects_of: Encoder::add(
                "<http://www.w3.org/ns/shacl#targetObjectsOf>".to_string(),
            ),
            sh_property: Encoder::add("<http://www.w3.org/ns/shacl#property>".to_string()),
            sh_node: Encoder::add("<http://www.w3.org/ns/shacl#node>".to_string()),
            sh_path: Encoder::add("<http://www.w3.org/ns/shacl#path>".to_string()),
            sh_min_count: Encoder::add("<http://www.w3.org/ns/shacl#minCount>".to_string()),
            sh_max_count: Encoder::add("<http://www.w3.org/ns/shacl#maxCount>".to_string()),
            sh_datatype: Encoder::add("<http://www.w3.org/ns/shacl#datatype>".to_string()),
            sh_class: Encoder::add("<http://www.w3.org/ns/shacl#class>".to_string()),
            sh_pattern: Encoder::add("<http://www.w3.org/ns/shacl#pattern>".to_string()),
            sh_flags: Encoder::add("<http://www.w3.org/ns/shacl#flags>".to_string()),
            sh_in: Encoder::add("<http://www.w3.org/ns/shacl#in>".to_string()),
            sh_and: Encoder::add("<http://www.w3.org/ns/shacl#and>".to_string()),
            sh_or: Encoder::add("<http://www.w3.org/ns/shacl#or>".to_string()),
            sh_not: Encoder::add("<http://www.w3.org/ns/shacl#not>".to_string()),
            sh_xone: Encoder::add("<http://www.w3.org/ns/shacl#xone>".to_string()),
            sh_node_kind: Encoder::add("<http://www.w3.org/ns/shacl#nodeKind>".to_string()),
            sh_has_value: Encoder::add("<http://www.w3.org/ns/shacl#hasValue>".to_string()),
            sh_min_length: Encoder::add("<http://www.w3.org/ns/shacl#minLength>".to_string()),
            sh_max_length: Encoder::add("<http://www.w3.org/ns/shacl#maxLength>".to_string()),
            sh_min_exclusive: Encoder::add("<http://www.w3.org/ns/shacl#minExclusive>".to_string()),
            sh_min_inclusive: Encoder::add("<http://www.w3.org/ns/shacl#minInclusive>".to_string()),
            sh_max_exclusive: Encoder::add("<http://www.w3.org/ns/shacl#maxExclusive>".to_string()),
            sh_max_inclusive: Encoder::add("<http://www.w3.org/ns/shacl#maxInclusive>".to_string()),
            sh_language_in: Encoder::add("<http://www.w3.org/ns/shacl#languageIn>".to_string()),
            sh_unique_lang: Encoder::add("<http://www.w3.org/ns/shacl#uniqueLang>".to_string()),
            sh_equals: Encoder::add("<http://www.w3.org/ns/shacl#equals>".to_string()),
            sh_disjoint: Encoder::add("<http://www.w3.org/ns/shacl#disjoint>".to_string()),
            sh_less_than: Encoder::add("<http://www.w3.org/ns/shacl#lessThan>".to_string()),
            sh_less_than_or_equals: Encoder::add(
                "<http://www.w3.org/ns/shacl#lessThanOrEquals>".to_string(),
            ),
            sh_qualified_value_shape: Encoder::add(
                "<http://www.w3.org/ns/shacl#qualifiedValueShape>".to_string(),
            ),
            sh_qualified_min_count: Encoder::add(
                "<http://www.w3.org/ns/shacl#qualifiedMinCount>".to_string(),
            ),
            sh_qualified_max_count: Encoder::add(
                "<http://www.w3.org/ns/shacl#qualifiedMaxCount>".to_string(),
            ),
            sh_closed: Encoder::add("<http://www.w3.org/ns/shacl#closed>".to_string()),
            sh_ignored_properties: Encoder::add(
                "<http://www.w3.org/ns/shacl#ignoredProperties>".to_string(),
            ),
            sh_deactivated: Encoder::add("<http://www.w3.org/ns/shacl#deactivated>".to_string()),
            sh_message: Encoder::add("<http://www.w3.org/ns/shacl#message>".to_string()),
            sh_severity: Encoder::add("<http://www.w3.org/ns/shacl#severity>".to_string()),
            sh_violation: Encoder::add("<http://www.w3.org/ns/shacl#Violation>".to_string()),
            sh_warning: Encoder::add("<http://www.w3.org/ns/shacl#Warning>".to_string()),
            sh_info: Encoder::add("<http://www.w3.org/ns/shacl#Info>".to_string()),
            sh_validation_report: Encoder::add(
                "<http://www.w3.org/ns/shacl#ValidationReport>".to_string(),
            ),
            sh_conforms: Encoder::add("<http://www.w3.org/ns/shacl#conforms>".to_string()),
            sh_result: Encoder::add("<http://www.w3.org/ns/shacl#result>".to_string()),
            sh_validation_result: Encoder::add(
                "<http://www.w3.org/ns/shacl#ValidationResult>".to_string(),
            ),
            sh_focus_node: Encoder::add("<http://www.w3.org/ns/shacl#focusNode>".to_string()),
            sh_result_path: Encoder::add("<http://www.w3.org/ns/shacl#resultPath>".to_string()),
            sh_value: Encoder::add("<http://www.w3.org/ns/shacl#value>".to_string()),
            sh_result_message: Encoder::add(
                "<http://www.w3.org/ns/shacl#resultMessage>".to_string(),
            ),
            sh_source_constraint_component: Encoder::add(
                "<http://www.w3.org/ns/shacl#sourceConstraintComponent>".to_string(),
            ),
            sh_source_shape: Encoder::add("<http://www.w3.org/ns/shacl#sourceShape>".to_string()),
            sh_result_severity: Encoder::add(
                "<http://www.w3.org/ns/shacl#resultSeverity>".to_string(),
            ),
            sh_alternative_path: Encoder::add(
                "<http://www.w3.org/ns/shacl#alternativePath>".to_string(),
            ),
            sh_inverse_path: Encoder::add("<http://www.w3.org/ns/shacl#inversePath>".to_string()),
            sh_zero_or_more_path: Encoder::add(
                "<http://www.w3.org/ns/shacl#zeroOrMorePath>".to_string(),
            ),
            sh_one_or_more_path: Encoder::add(
                "<http://www.w3.org/ns/shacl#oneOrMorePath>".to_string(),
            ),
            sh_zero_or_one_path: Encoder::add(
                "<http://www.w3.org/ns/shacl#zeroOrOnePath>".to_string(),
            ),

            // SPARQL-based constraints (sh:sparql / SPARQLConstraintComponent) and targets
            sh_sparql: Encoder::add("<http://www.w3.org/ns/shacl#sparql>".to_string()),
            sh_select: Encoder::add("<http://www.w3.org/ns/shacl#select>".to_string()),
            sh_ask: Encoder::add("<http://www.w3.org/ns/shacl#ask>".to_string()),
            sh_prefixes: Encoder::add("<http://www.w3.org/ns/shacl#prefixes>".to_string()),
            sh_target: Encoder::add("<http://www.w3.org/ns/shacl#target>".to_string()),
            sh_sparql_target: Encoder::add("<http://www.w3.org/ns/shacl#SPARQLTarget>".to_string()),

            // sh:nodeKind values
            sh_iri: Encoder::add("<http://www.w3.org/ns/shacl#IRI>".to_string()),
            sh_blank_node: Encoder::add("<http://www.w3.org/ns/shacl#BlankNode>".to_string()),
            sh_literal: Encoder::add("<http://www.w3.org/ns/shacl#Literal>".to_string()),
            sh_blank_node_or_iri: Encoder::add(
                "<http://www.w3.org/ns/shacl#BlankNodeOrIRI>".to_string(),
            ),
            sh_blank_node_or_literal: Encoder::add(
                "<http://www.w3.org/ns/shacl#BlankNodeOrLiteral>".to_string(),
            ),
            sh_iri_or_literal: Encoder::add(
                "<http://www.w3.org/ns/shacl#IRIOrLiteral>".to_string(),
            ),

            // Constraint Components
            sh_min_count_constraint_component: Encoder::add(
                "<http://www.w3.org/ns/shacl#MinCountConstraintComponent>".to_string(),
            ),
            sh_max_count_constraint_component: Encoder::add(
                "<http://www.w3.org/ns/shacl#MaxCountConstraintComponent>".to_string(),
            ),
            sh_datatype_constraint_component: Encoder::add(
                "<http://www.w3.org/ns/shacl#DatatypeConstraintComponent>".to_string(),
            ),
            sh_class_constraint_component: Encoder::add(
                "<http://www.w3.org/ns/shacl#ClassConstraintComponent>".to_string(),
            ),
            sh_pattern_constraint_component: Encoder::add(
                "<http://www.w3.org/ns/shacl#PatternConstraintComponent>".to_string(),
            ),
            sh_in_constraint_component: Encoder::add(
                "<http://www.w3.org/ns/shacl#InConstraintComponent>".to_string(),
            ),
            sh_and_constraint_component: Encoder::add(
                "<http://www.w3.org/ns/shacl#AndConstraintComponent>".to_string(),
            ),
            sh_or_constraint_component: Encoder::add(
                "<http://www.w3.org/ns/shacl#OrConstraintComponent>".to_string(),
            ),
            sh_not_constraint_component: Encoder::add(
                "<http://www.w3.org/ns/shacl#NotConstraintComponent>".to_string(),
            ),
            sh_xone_constraint_component: Encoder::add(
                "<http://www.w3.org/ns/shacl#XoneConstraintComponent>".to_string(),
            ),
            sh_node_kind_constraint_component: Encoder::add(
                "<http://www.w3.org/ns/shacl#NodeKindConstraintComponent>".to_string(),
            ),
            sh_has_value_constraint_component: Encoder::add(
                "<http://www.w3.org/ns/shacl#HasValueConstraintComponent>".to_string(),
            ),
            sh_min_length_constraint_component: Encoder::add(
                "<http://www.w3.org/ns/shacl#MinLengthConstraintComponent>".to_string(),
            ),
            sh_max_length_constraint_component: Encoder::add(
                "<http://www.w3.org/ns/shacl#MaxLengthConstraintComponent>".to_string(),
            ),
            sh_min_exclusive_constraint_component: Encoder::add(
                "<http://www.w3.org/ns/shacl#MinExclusiveConstraintComponent>".to_string(),
            ),
            sh_min_inclusive_constraint_component: Encoder::add(
                "<http://www.w3.org/ns/shacl#MinInclusiveConstraintComponent>".to_string(),
            ),
            sh_max_exclusive_constraint_component: Encoder::add(
                "<http://www.w3.org/ns/shacl#MaxExclusiveConstraintComponent>".to_string(),
            ),
            sh_max_inclusive_constraint_component: Encoder::add(
                "<http://www.w3.org/ns/shacl#MaxInclusiveConstraintComponent>".to_string(),
            ),
            sh_language_in_constraint_component: Encoder::add(
                "<http://www.w3.org/ns/shacl#LanguageInConstraintComponent>".to_string(),
            ),
            sh_unique_lang_constraint_component: Encoder::add(
                "<http://www.w3.org/ns/shacl#UniqueLangConstraintComponent>".to_string(),
            ),
            sh_equals_constraint_component: Encoder::add(
                "<http://www.w3.org/ns/shacl#EqualsConstraintComponent>".to_string(),
            ),
            sh_disjoint_constraint_component: Encoder::add(
                "<http://www.w3.org/ns/shacl#DisjointConstraintComponent>".to_string(),
            ),
            sh_less_than_constraint_component: Encoder::add(
                "<http://www.w3.org/ns/shacl#LessThanConstraintComponent>".to_string(),
            ),
            sh_less_than_or_equals_constraint_component: Encoder::add(
                "<http://www.w3.org/ns/shacl#LessThanOrEqualsConstraintComponent>".to_string(),
            ),
            sh_qualified_value_shape_constraint_component: Encoder::add(
                "<http://www.w3.org/ns/shacl#QualifiedValueShapeConstraintComponent>".to_string(),
            ),
            sh_closed_constraint_component: Encoder::add(
                "<http://www.w3.org/ns/shacl#ClosedConstraintComponent>".to_string(),
            ),
            sh_node_constraint_component: Encoder::add(
                "<http://www.w3.org/ns/shacl#NodeConstraintComponent>".to_string(),
            ),
            sh_sparql_constraint_component: Encoder::add(
                "<http://www.w3.org/ns/shacl#SPARQLConstraintComponent>".to_string(),
            ),
        }
    }
}

impl Default for Vocab {
    fn default() -> Self {
        Self::new()
    }
}
