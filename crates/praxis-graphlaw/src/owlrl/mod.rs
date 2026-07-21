use crate::encoding::Encoder;
use crate::tripleindex::TripleIndex;
use std::sync::Arc;

mod rules;
pub use rules::*;

/// OWL RL vocabulary: interned IRI ids for the daily profile features.
pub struct OwlRlVocab {
    pub rdf_type: usize,
    pub rdfs_subclass_of: usize,
    pub rdfs_subproperty_of: usize,
    pub rdfs_domain: usize,
    pub rdfs_range: usize,
    pub owl_equivalent_class: usize,
    pub owl_equivalent_property: usize,
    pub owl_inverse_of: usize,
    pub owl_symmetric_property: usize,
    pub owl_transitive_property: usize,
    pub owl_same_as: usize,
    pub owl_property_chain_axiom: usize,
    pub owl_cardinality: usize,
    pub owl_min_cardinality: usize,
    pub owl_max_cardinality: usize,
    pub owl_union_of: usize,
    pub owl_intersection_of: usize,
    pub owl_one_of: usize,
    pub owl_imports: usize,
}

impl OwlRlVocab {
    pub fn new() -> Self {
        Self {
            // NOTE: canonical stored form for IRIs elsewhere in this crate (see
            // shacl.rs's SHACL_VOCAB) is the bracketed `<iri>` form, because
            // that's what the pest-based N3/Turtle parser (make_term ->
            // PrefixMapper::expand) interns for every parsed term. Encoder::add
            // does *not* normalize brackets away -- `Encoder::add("<x>")` and
            // `Encoder::add("x")` are two distinct interned values -- so this
            // vocab must match the parser's bracketed convention or every OWL
            // RL rule pattern below silently fails to match any real triple.
            rdf_type: Encoder::add("<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>".to_string()),
            rdfs_subclass_of: Encoder::add(
                "<http://www.w3.org/2000/01/rdf-schema#subClassOf>".to_string(),
            ),
            rdfs_subproperty_of: Encoder::add(
                "<http://www.w3.org/2000/01/rdf-schema#subPropertyOf>".to_string(),
            ),
            rdfs_domain: Encoder::add("<http://www.w3.org/2000/01/rdf-schema#domain>".to_string()),
            rdfs_range: Encoder::add("<http://www.w3.org/2000/01/rdf-schema#range>".to_string()),
            owl_equivalent_class: Encoder::add(
                "<http://www.w3.org/2002/07/owl#equivalentClass>".to_string(),
            ),
            owl_equivalent_property: Encoder::add(
                "<http://www.w3.org/2002/07/owl#equivalentProperty>".to_string(),
            ),
            owl_inverse_of: Encoder::add("<http://www.w3.org/2002/07/owl#inverseOf>".to_string()),
            owl_symmetric_property: Encoder::add(
                "<http://www.w3.org/2002/07/owl#SymmetricProperty>".to_string(),
            ),
            owl_transitive_property: Encoder::add(
                "<http://www.w3.org/2002/07/owl#TransitiveProperty>".to_string(),
            ),
            owl_same_as: Encoder::add("<http://www.w3.org/2002/07/owl#sameAs>".to_string()),
            owl_property_chain_axiom: Encoder::add(
                "<http://www.w3.org/2002/07/owl#propertyChainAxiom>".to_string(),
            ),
            owl_cardinality: Encoder::add(
                "<http://www.w3.org/2002/07/owl#cardinality>".to_string(),
            ),
            owl_min_cardinality: Encoder::add(
                "<http://www.w3.org/2002/07/owl#minCardinality>".to_string(),
            ),
            owl_max_cardinality: Encoder::add(
                "<http://www.w3.org/2002/07/owl#maxCardinality>".to_string(),
            ),
            owl_union_of: Encoder::add("<http://www.w3.org/2002/07/owl#unionOf>".to_string()),
            owl_intersection_of: Encoder::add(
                "<http://www.w3.org/2002/07/owl#intersectionOf>".to_string(),
            ),
            owl_one_of: Encoder::add("<http://www.w3.org/2002/07/owl#oneOf>".to_string()),
            owl_imports: Encoder::add("<http://www.w3.org/2002/07/owl#imports>".to_string()),
        }
    }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub enum OwlRlFeature {
    SubClassOf,
    SubPropertyOf,
    Domain,
    Range,
    EquivalentClass,
    EquivalentProperty,
    InverseOf,
    SymmetricProperty,
    TransitiveProperty,
    SameAs,
    PropertyChainAxiom,
    Cardinality,
    ComplexClassExpression,
    Imports,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum OwlRlDecision {
    Supported,
    Unsupported {
        feature: OwlRlFeature,
        reason: &'static str,
    },
    ExternalBoundaryRequired {
        feature: OwlRlFeature,
        reason: &'static str,
    },
}

pub fn classify_owlrl_feature(feature: OwlRlFeature) -> OwlRlDecision {
    match feature {
        OwlRlFeature::SubClassOf
        | OwlRlFeature::SubPropertyOf
        | OwlRlFeature::Domain
        | OwlRlFeature::Range
        | OwlRlFeature::EquivalentClass
        | OwlRlFeature::EquivalentProperty
        | OwlRlFeature::InverseOf
        | OwlRlFeature::SymmetricProperty
        | OwlRlFeature::TransitiveProperty => OwlRlDecision::Supported,
        OwlRlFeature::SameAs => OwlRlDecision::ExternalBoundaryRequired {
            feature,
            reason: "unrestricted sameAs closure is outside the bounded daily profile; equivalence merging is a later profile",
        },
        OwlRlFeature::PropertyChainAxiom => OwlRlDecision::Unsupported {
            feature,
            reason: "property chain axiom requires forward-chaining schema computation not supported in daily profile",
        },
        OwlRlFeature::Cardinality => OwlRlDecision::Unsupported {
            feature,
            reason: "cardinality constraints (cardinality/minCardinality/maxCardinality) require constraint solving outside daily profile",
        },
        OwlRlFeature::ComplexClassExpression => OwlRlDecision::Unsupported {
            feature,
            reason: "complex class expressions (unionOf/intersectionOf/oneOf/restrictions) require class-expression evaluation not supported in daily profile",
        },
        OwlRlFeature::Imports => OwlRlDecision::Unsupported {
            feature,
            reason: "owl:imports requires remote ontology loading and is outside the bounded daily profile",
        },
    }
}

#[derive(Debug, Clone)]
pub struct ScanReport {
    pub supported: Vec<(OwlRlFeature, usize)>,
    pub refused: Vec<(OwlRlFeature, usize, &'static str)>,
}

/// Scan ontology for OWL RL features, supporting both Arc and reference patterns.
///
/// # Pattern 5 (v26.7.8)
/// Accepts both Arc<TripleIndex> (immutable snapshots) and &TripleIndex (references)
/// for flexible zero-copy sharing across multiple consumers.
pub fn scan_ontology(index: &TripleIndex, vocab: &OwlRlVocab) -> ScanReport {
    let mut supported = Vec::new();
    let mut refused = Vec::new();

    let features_to_scan = vec![
        (OwlRlFeature::SubClassOf, vocab.rdfs_subclass_of),
        (OwlRlFeature::SubPropertyOf, vocab.rdfs_subproperty_of),
        (OwlRlFeature::Domain, vocab.rdfs_domain),
        (OwlRlFeature::Range, vocab.rdfs_range),
        (OwlRlFeature::EquivalentClass, vocab.owl_equivalent_class),
        (
            OwlRlFeature::EquivalentProperty,
            vocab.owl_equivalent_property,
        ),
        (OwlRlFeature::InverseOf, vocab.owl_inverse_of),
        (
            OwlRlFeature::SymmetricProperty,
            vocab.owl_symmetric_property,
        ),
        (
            OwlRlFeature::TransitiveProperty,
            vocab.owl_transitive_property,
        ),
        (OwlRlFeature::SameAs, vocab.owl_same_as),
        (
            OwlRlFeature::PropertyChainAxiom,
            vocab.owl_property_chain_axiom,
        ),
        (OwlRlFeature::Cardinality, vocab.owl_cardinality),
        (OwlRlFeature::Cardinality, vocab.owl_min_cardinality),
        (OwlRlFeature::Cardinality, vocab.owl_max_cardinality),
        (OwlRlFeature::ComplexClassExpression, vocab.owl_union_of),
        (
            OwlRlFeature::ComplexClassExpression,
            vocab.owl_intersection_of,
        ),
        (OwlRlFeature::ComplexClassExpression, vocab.owl_one_of),
        (OwlRlFeature::Imports, vocab.owl_imports),
    ];

    for (feature, vocab_pred_id) in features_to_scan {
        use crate::term::Triple;
        use crate::term::VarOrTerm;

        let query_pattern = Triple {
            s: VarOrTerm::new_var("?s".to_string()),
            p: VarOrTerm::new_term(Encoder::decode(&vocab_pred_id).unwrap_or_default()),
            o: VarOrTerm::new_var("?o".to_string()),
            g: None,
        };

        let count = index.query(&query_pattern, None).map(|_| 1).unwrap_or(0);
        if count > 0 {
            let decision = classify_owlrl_feature(feature);
            match decision {
                OwlRlDecision::Supported => {
                    if let Some((_, ref mut c)) = supported.iter_mut().find(|(f, _)| *f == feature)
                    {
                        *c += count;
                    } else {
                        supported.push((feature, count));
                    }
                }
                OwlRlDecision::Unsupported { feature: _, reason } => {
                    if let Some((_, ref mut c, _)) =
                        refused.iter_mut().find(|(f, _, _)| *f == feature)
                    {
                        *c += count;
                    } else {
                        refused.push((feature, count, reason));
                    }
                }
                OwlRlDecision::ExternalBoundaryRequired { feature: _, reason } => {
                    if let Some((_, ref mut c, _)) =
                        refused.iter_mut().find(|(f, _, _)| *f == feature)
                    {
                        *c += count;
                    } else {
                        refused.push((feature, count, reason));
                    }
                }
            }
        }
    }

    ScanReport { supported, refused }
}

pub struct OwlRlEngine {
    vocab: OwlRlVocab,
}

impl OwlRlEngine {
    pub fn new() -> Self {
        OwlRlEngine {
            vocab: OwlRlVocab::new(),
        }
    }

    /// Compile OWL RL rules from an ontology index.
    ///
    /// # Pattern 5 (v26.7.8)
    /// Accepts &TripleIndex references to support both Arc<TripleIndex> snapshots
    /// (via Arc::as_ref()) and mutable TripleStore indexes without copying.
    /// Multiple compile() calls on the same snapshot share zero-copy read-only access.
    pub fn compile(
        &self,
        index: &TripleIndex,
    ) -> Result<(Vec<crate::rule::Rule>, ScanReport), String> {
        let mut rules = Vec::new();

        // Add all supported daily-profile rules unconditionally.
        // These 9 features are the bounded OWL RL profile for v26.7.8.
        rules.push(rule_subclass_transitive(&self.vocab)?);
        rules.push(rule_subclass_type_propagation(&self.vocab)?);
        rules.push(rule_subproperty_transitive(&self.vocab)?);
        rules.push(rule_subproperty_assertion_propagation(&self.vocab)?);
        rules.push(rule_domain(&self.vocab)?);
        rules.push(rule_range(&self.vocab)?);
        let eq_class_rules = rules_equivalent_class(&self.vocab)?;
        rules.push(eq_class_rules[0].clone());
        rules.push(eq_class_rules[1].clone());
        let eq_prop_rules = rules_equivalent_property(&self.vocab)?;
        rules.push(eq_prop_rules[0].clone());
        rules.push(eq_prop_rules[1].clone());
        rules.push(rule_inverse_of(&self.vocab)?);
        rules.push(rule_symmetric_property(&self.vocab)?);
        rules.push(rule_transitive_property(&self.vocab)?);

        // Scan the actual ontology for unsupported/external-boundary features
        // (owl:sameAs, cardinality, complex class expressions, propertyChainAxiom,
        // owl:imports) so callers (see core.rs's status mapping) can refuse or
        // flag them instead of silently admitting inputs the daily profile
        // cannot correctly reason over.
        let report = scan_ontology(index, &self.vocab);

        Ok((rules, report))
    }
}

#[cfg(test)]
mod owlrl_test {
    use super::*;
    use crate::TripleStore;

    #[test]
    fn test_vocabulary_initialized() {
        let vocab = OwlRlVocab::new();
        // Encoder returns 0-indexed IDs; the observable invariant is that
        // distinct vocabulary terms get distinct encodings.
        assert_ne!(
            vocab.rdf_type, vocab.rdfs_subclass_of,
            "rdf:type and rdfs:subClassOf must encode to distinct IDs"
        );
    }

    #[test]
    fn test_classify_supported_features() {
        assert_eq!(
            classify_owlrl_feature(OwlRlFeature::SubClassOf),
            OwlRlDecision::Supported
        );
        assert_eq!(
            classify_owlrl_feature(OwlRlFeature::Domain),
            OwlRlDecision::Supported
        );
    }

    #[test]
    fn test_classify_refused_features() {
        match classify_owlrl_feature(OwlRlFeature::SameAs) {
            OwlRlDecision::ExternalBoundaryRequired { .. } => {}
            _ => panic!("SameAs should be ExternalBoundaryRequired"),
        }

        match classify_owlrl_feature(OwlRlFeature::PropertyChainAxiom) {
            OwlRlDecision::Unsupported { .. } => {}
            _ => panic!("PropertyChainAxiom should be Unsupported"),
        }
    }

    #[test]
    fn test_engine_creation() {
        let engine = OwlRlEngine::new();
        // The engine's embedded vocabulary must be initialized with distinct
        // encodings, same invariant as test_vocabulary_initialized.
        assert_ne!(engine.vocab.rdf_type, engine.vocab.rdfs_subclass_of);
    }

    #[test]
    fn test_rule_subclass_transitive() {
        let vocab = OwlRlVocab::new();
        let rule = rule_subclass_transitive(&vocab).unwrap();
        assert_eq!(rule.body.len(), 2);
        assert_eq!(
            rule.head.s.to_encoded(),
            rule.body[0].pattern.s.to_encoded()
        );
    }

    #[test]
    fn test_rule_subclass_type_propagation() {
        let vocab = OwlRlVocab::new();
        let rule = rule_subclass_type_propagation(&vocab).unwrap();
        assert_eq!(rule.body.len(), 2);
    }

    #[test]
    fn test_rule_domain() {
        let vocab = OwlRlVocab::new();
        let rule = rule_domain(&vocab).unwrap();
        assert_eq!(rule.body.len(), 2);
    }

    #[test]
    fn test_rule_range() {
        let vocab = OwlRlVocab::new();
        let rule = rule_range(&vocab).unwrap();
        assert_eq!(rule.body.len(), 2);
    }

    #[test]
    fn test_rules_equivalent_class() {
        let vocab = OwlRlVocab::new();
        let rules = rules_equivalent_class(&vocab).unwrap();
        assert_eq!(rules.len(), 2);
        assert_eq!(rules[0].body.len(), 1);
        assert_eq!(rules[1].body.len(), 1);
    }

    #[test]
    fn test_rules_equivalent_property() {
        let vocab = OwlRlVocab::new();
        let rules = rules_equivalent_property(&vocab).unwrap();
        assert_eq!(rules.len(), 2);
    }

    #[test]
    fn test_rule_inverse_of() {
        let vocab = OwlRlVocab::new();
        let rule = rule_inverse_of(&vocab).unwrap();
        assert_eq!(rule.body.len(), 2);
    }

    #[test]
    fn test_rule_symmetric_property() {
        let vocab = OwlRlVocab::new();
        let rule = rule_symmetric_property(&vocab).unwrap();
        assert_eq!(rule.body.len(), 2);
    }

    #[test]
    fn test_rule_transitive_property() {
        let vocab = OwlRlVocab::new();
        let rule = rule_transitive_property(&vocab).unwrap();
        assert_eq!(rule.body.len(), 3);
    }

    #[test]
    fn test_scan_ontology_empty() {
        let store = TripleStore::new();
        let vocab = OwlRlVocab::new();
        let report = scan_ontology(&store.triple_index, &vocab);
        assert!(report.supported.is_empty());
        assert!(report.refused.is_empty());
    }

    #[test]
    fn test_engine_compile_empty() {
        let store = TripleStore::new();
        let engine = OwlRlEngine::new();
        let (rules, _report) = engine.compile(&store.triple_index).unwrap();
        // Daily profile v26.7.8 unconditionally adds all 9 supported rules.
        // Each gets compiled: subclass/subproperty/domain/range are 1 rule each,
        // while equivalent_class/equivalent_property each expand to 2 rules,
        // and inverse_of/symmetric_property/transitive_property are 1 each.
        assert!(!rules.is_empty(), "daily profile must add rules");
        // Every rule must have a non-empty body (at least one literal to pattern-match on)
        assert!(rules.iter().all(|r| !r.body.is_empty()));
    }
}
