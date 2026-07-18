use super::closure::SubclassClosure;
use super::model::ShapesGraph;
use super::validate::validate_shape;
use crate::tripleindex::TripleIndex;
/// SHACL validation report types
///
/// Structures for representing validation results and conformance reports.
use crate::triples::{Term, Triple, VarOrTerm};
use std::collections::HashSet;

/// A single validation result (violation, warning, or info)
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct ValidationResult {
    pub focus_node: Term,
    pub result_path: Option<Term>,
    pub value: Option<Term>,
    pub source_constraint_component: Term,
    pub source_shape: Term,
    pub severity: Term,
    pub message: Option<String>,
}

/// Complete SHACL validation report
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct ValidationReport {
    pub conforms: bool,
    pub results: Vec<ValidationResult>,
}

impl ValidationReport {
    /// Serialise the report as SHACL-conformant RDF triples.
    pub fn to_triples(&self) -> Vec<Triple> {
        let mut triples = Vec::new();

        let report_id = format!("report_{}", self.results.len());
        let report_term = VarOrTerm::new_blank_node(report_id);

        let rdf_type =
            VarOrTerm::convert("http://www.w3.org/1999/02/22-rdf-syntax-ns#type".to_string());
        let sh_validation_report =
            VarOrTerm::convert("http://www.w3.org/ns/shacl#ValidationReport".to_string());
        let sh_conforms = VarOrTerm::convert("http://www.w3.org/ns/shacl#conforms".to_string());
        let sh_result = VarOrTerm::convert("http://www.w3.org/ns/shacl#result".to_string());
        let sh_focus_node = VarOrTerm::convert("http://www.w3.org/ns/shacl#focusNode".to_string());
        let sh_result_path =
            VarOrTerm::convert("http://www.w3.org/ns/shacl#resultPath".to_string());
        let sh_value = VarOrTerm::convert("http://www.w3.org/ns/shacl#value".to_string());
        let sh_source_constraint_component =
            VarOrTerm::convert("http://www.w3.org/ns/shacl#sourceConstraintComponent".to_string());
        let sh_source_shape =
            VarOrTerm::convert("http://www.w3.org/ns/shacl#sourceShape".to_string());
        let sh_result_severity =
            VarOrTerm::convert("http://www.w3.org/ns/shacl#resultSeverity".to_string());
        let sh_result_message =
            VarOrTerm::convert("http://www.w3.org/ns/shacl#resultMessage".to_string());
        let sh_validation_result =
            VarOrTerm::convert("http://www.w3.org/ns/shacl#ValidationResult".to_string());

        triples.push(Triple {
            s: report_term.clone(),
            p: rdf_type.clone(),
            o: sh_validation_report,
            g: None,
        });

        let conforms_str = if self.conforms { "true" } else { "false" };
        let conforms_literal = VarOrTerm::new_literal(
            conforms_str.to_string(),
            Some("<http://www.w3.org/2001/XMLSchema#boolean>".to_string()),
            None,
        );
        triples.push(Triple {
            s: report_term.clone(),
            p: sh_conforms,
            o: conforms_literal,
            g: None,
        });

        for (i, res) in self.results.iter().enumerate() {
            let res_term = VarOrTerm::new_blank_node(format!("result_{}", i));

            triples.push(Triple {
                s: report_term.clone(),
                p: sh_result.clone(),
                o: res_term.clone(),
                g: None,
            });

            triples.push(Triple {
                s: res_term.clone(),
                p: rdf_type.clone(),
                o: sh_validation_result.clone(),
                g: None,
            });

            triples.push(Triple {
                s: res_term.clone(),
                p: sh_focus_node.clone(),
                o: VarOrTerm::Term(res.focus_node.clone()),
                g: None,
            });

            triples.push(Triple {
                s: res_term.clone(),
                p: sh_source_constraint_component.clone(),
                o: VarOrTerm::Term(res.source_constraint_component.clone()),
                g: None,
            });

            triples.push(Triple {
                s: res_term.clone(),
                p: sh_source_shape.clone(),
                o: VarOrTerm::Term(res.source_shape.clone()),
                g: None,
            });

            triples.push(Triple {
                s: res_term.clone(),
                p: sh_result_severity.clone(),
                o: VarOrTerm::Term(res.severity.clone()),
                g: None,
            });

            if let Some(ref path) = res.result_path {
                triples.push(Triple {
                    s: res_term.clone(),
                    p: sh_result_path.clone(),
                    o: VarOrTerm::Term(path.clone()),
                    g: None,
                });
            }

            if let Some(ref val) = res.value {
                triples.push(Triple {
                    s: res_term.clone(),
                    p: sh_value.clone(),
                    o: VarOrTerm::Term(val.clone()),
                    g: None,
                });
            }

            if let Some(ref msg) = res.message {
                let msg_literal = VarOrTerm::new_literal(msg.clone(), None, None);
                triples.push(Triple {
                    s: res_term.clone(),
                    p: sh_result_message.clone(),
                    o: msg_literal,
                    g: None,
                });
            }
        }

        triples
    }
}

/// Validator entry point
pub struct Validator;

impl Validator {
    pub fn validate(data: &TripleIndex, shapes: &ShapesGraph) -> ValidationReport {
        use super::index_utils::{get_objects, get_subjects, get_triples_by_predicate};
        use super::Vocab;

        let vocab = Vocab::new();
        let shapes_index = shapes.raw_index.as_ref();
        let closure = SubclassClosure::new(data, vocab.rdfs_subclass_of);

        let mut shape_nodes = HashSet::new();

        // Subjects of rdf:type sh:NodeShape or sh:PropertyShape
        for s in get_subjects(shapes_index, vocab.rdf_type, vocab.sh_node_shape) {
            shape_nodes.insert(s);
        }
        for s in get_subjects(shapes_index, vocab.rdf_type, vocab.sh_property_shape) {
            shape_nodes.insert(s);
        }

        // Subjects of any target declaration
        for t in [
            vocab.sh_target_class,
            vocab.sh_target_node,
            vocab.sh_target_subjects_of,
            vocab.sh_target_objects_of,
            vocab.sh_target,
        ] {
            if let Some(objs) = shapes_index.pos.get(&t) {
                for subjs in objs.values() {
                    for (s, _, _) in subjs {
                        shape_nodes.insert(*s);
                    }
                }
            }
        }

        // Objects of sh:property are property shapes
        for (_, ps) in get_triples_by_predicate(shapes_index, vocab.sh_property) {
            shape_nodes.insert(ps);
        }

        let mut results = Vec::new();

        // Sort before iterating: `shape_nodes`/`focus_nodes` are `std::collections::HashSet<usize>`,
        // whose default `RandomState` hasher is reseeded per process, so iterating them directly
        // (as this loop previously did) pushes `ValidationResult`s into `results` in a different
        // order on every process run even for byte-identical input triples/shapes -- the ordering
        // then flows unmodified into `hook_hash`'s BLAKE3 receipt-chain digest (via
        // `hooks/condition.rs` -> `hooks/evaluate.rs` -> `cng`'s `workday.rs`), silently breaking
        // this repo's own determinism invariant at the point a receipt is finalized. `usize`
        // symbol IDs sort numerically; the set's actual membership is unchanged, so sorting here
        // changes only presentation order, not which violations are reported.
        let mut sorted_shape_nodes: Vec<usize> = shape_nodes.into_iter().collect();
        sorted_shape_nodes.sort_unstable();

        for shape in sorted_shape_nodes {
            use super::index_utils::is_shape_deactivated;
            use super::targets_paths::get_focus_nodes;

            // Skip deactivated shapes
            if is_shape_deactivated(shapes_index, shape, &vocab) {
                continue;
            }
            let focus_nodes = get_focus_nodes(data, shapes_index, shape, &vocab);
            let mut sorted_focus_nodes: Vec<usize> = focus_nodes.into_iter().collect();
            sorted_focus_nodes.sort_unstable();
            for focus in sorted_focus_nodes {
                let mut visited = HashSet::new();
                validate_shape(
                    data,
                    shapes_index,
                    &vocab,
                    focus,
                    shape,
                    &mut results,
                    &mut visited,
                    &closure,
                    0,
                );
            }
        }

        // Per SHACL Core (1.0), sh:conforms is false whenever ANY validation
        // result exists, regardless of severity -- Info and Warning results
        // count too, not just Violation. This is confirmed by the real W3C
        // data-shapes test suite (e.g. misc/severity-001.ttl: a shape with
        // sh:severity sh:Warning still expects sh:conforms "false"). SHACL
        // 1.2 introduces an opt-in sh:conformanceDisallows to narrow this,
        // but that's a distinct, newer mechanism this validator doesn't
        // implement -- the unconditional default (all severities disallow
        // conformance) is what SHACL Core actually specifies and what every
        // vendored conformance case expects.
        let conforms = results.is_empty();
        ValidationReport { conforms, results }
    }
}
