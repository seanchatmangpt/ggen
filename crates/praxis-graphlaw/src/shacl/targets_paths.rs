use super::index_utils::{get_objects, get_rdf_list, get_subjects, is_blank_node};
use super::sparql::evaluate_sparql_text;
use super::values::get_lexical_form;
use super::Vocab;
use crate::encoding::Encoder;
/// Target resolution and path evaluation
///
/// Handles extraction of focus nodes from target declarations
/// and evaluation of property paths.
use crate::tripleindex::TripleIndex;
use std::collections::HashSet;

/// Check if triple (s, p, o) exists in index
fn contains_triple(index: &TripleIndex, s: usize, p: usize, o: usize) -> bool {
    if let Some(preds) = index.spo.get(&s) {
        if let Some(objs) = preds.get(&p) {
            return objs.iter().any(|(obj_val, _, _)| *obj_val == o);
        }
    }
    false
}

/// Get all focus nodes for a given shape, applying all target declarations
pub(crate) fn get_focus_nodes(
    data: &TripleIndex, shapes: &TripleIndex, shape_id: usize, vocab: &Vocab,
) -> HashSet<usize> {
    let mut focus_nodes = HashSet::new();

    // 1. sh:targetClass
    for class in get_objects(shapes, shape_id, vocab.sh_target_class) {
        for inst in get_subjects(data, vocab.rdf_type, class) {
            focus_nodes.insert(inst);
        }
    }

    // 2. sh:targetNode
    for node in get_objects(shapes, shape_id, vocab.sh_target_node) {
        focus_nodes.insert(node);
    }

    // 3. sh:targetSubjectsOf
    for pred in get_objects(shapes, shape_id, vocab.sh_target_subjects_of) {
        if let Some(objs) = data.pos.get(&pred) {
            for subjs in objs.values() {
                for (s, _, _) in subjs {
                    focus_nodes.insert(*s);
                }
            }
        }
    }

    // 4. sh:targetObjectsOf
    for pred in get_objects(shapes, shape_id, vocab.sh_target_objects_of) {
        if let Some(objs) = data.pos.get(&pred) {
            for o in objs.keys() {
                focus_nodes.insert(*o);
            }
        }
    }

    // 5. sh:target [ a sh:SPARQLTarget ; sh:select "SELECT ?this WHERE {...}" ]
    let sparql_targets = get_objects(shapes, shape_id, vocab.sh_target);
    for target_node in &sparql_targets {
        let is_sparql_target =
            contains_triple(shapes, *target_node, vocab.rdf_type, vocab.sh_sparql_target);
        if !is_sparql_target {
            continue;
        }
        let Some(select_lit) = get_objects(shapes, *target_node, vocab.sh_select)
            .first()
            .copied()
        else {
            continue;
        };
        let Some(query_text) = get_lexical_form(select_lit) else {
            continue;
        };
        if let Ok(rows) = evaluate_sparql_text(data, &query_text) {
            for row in rows {
                for b in &row {
                    let var_name = Encoder::decode(&b.var).unwrap_or_default();
                    if var_name.trim_start_matches('?') == "this" {
                        focus_nodes.insert(b.val);
                    }
                }
            }
        }
    }

    // Implicit class target: only when shape is also declared as a class AND has no other targets
    if !is_blank_node(shape_id) {
        let has_explicit_target = !get_objects(shapes, shape_id, vocab.sh_target_class).is_empty()
            || !get_objects(shapes, shape_id, vocab.sh_target_node).is_empty()
            || !get_objects(shapes, shape_id, vocab.sh_target_subjects_of).is_empty()
            || !get_objects(shapes, shape_id, vocab.sh_target_objects_of).is_empty()
            || !sparql_targets.is_empty();

        if !has_explicit_target {
            let is_class = contains_triple(shapes, shape_id, vocab.rdf_type, vocab.rdfs_class)
                || contains_triple(data, shape_id, vocab.rdf_type, vocab.rdfs_class);
            if is_class {
                for inst in get_subjects(data, vocab.rdf_type, shape_id) {
                    focus_nodes.insert(inst);
                }
            }
        }
    }

    focus_nodes
}

/// Evaluate a SHACL property path expression, returning all nodes reachable
/// from `focus_node` by following the path.
pub(crate) fn eval_path(
    data: &TripleIndex, shapes: &TripleIndex, focus_node: usize, path_node: usize,
) -> Vec<usize> {
    // Sequence path (RDF list of steps)
    let rdf_first = Encoder::get("<http://www.w3.org/1999/02/22-rdf-syntax-ns#first>").unwrap_or(0);
    if !get_objects(shapes, path_node, rdf_first).is_empty() {
        let steps = get_rdf_list(shapes, path_node);
        let mut current = vec![focus_node];
        for step in steps {
            let mut next = Vec::new();
            for node in current {
                next.extend(eval_path(data, shapes, node, step));
            }
            current = next;
            if current.is_empty() {
                break;
            }
        }
        return current;
    }

    // sh:inversePath
    let sh_inverse_path = Encoder::get("<http://www.w3.org/ns/shacl#inversePath>").unwrap_or(0);
    let inverse = get_objects(shapes, path_node, sh_inverse_path);
    if !inverse.is_empty() {
        let mut results = Vec::new();
        for inv in inverse {
            results.extend(get_subjects(data, inv, focus_node));
        }
        return results;
    }

    // sh:alternativePath
    let sh_alternative_path =
        Encoder::get("<http://www.w3.org/ns/shacl#alternativePath>").unwrap_or(0);
    let alternative = get_objects(shapes, path_node, sh_alternative_path);
    if !alternative.is_empty() {
        let mut results = Vec::new();
        for alt_list in alternative {
            for p in get_rdf_list(shapes, alt_list) {
                results.extend(eval_path(data, shapes, focus_node, p));
            }
        }
        return results;
    }

    // sh:zeroOrMorePath
    let sh_zero_or_more = Encoder::get("<http://www.w3.org/ns/shacl#zeroOrMorePath>").unwrap_or(0);
    let zero_or_more = get_objects(shapes, path_node, sh_zero_or_more);
    if !zero_or_more.is_empty() {
        let mut results = vec![focus_node];
        let mut visited = HashSet::new();
        visited.insert(focus_node);
        let mut queue = vec![focus_node];
        while !queue.is_empty() {
            let node = queue.remove(0);
            for target in &zero_or_more {
                for n in eval_path(data, shapes, node, *target) {
                    if visited.insert(n) {
                        results.push(n);
                        queue.push(n);
                    }
                }
            }
        }
        return results;
    }

    // sh:oneOrMorePath
    let sh_one_or_more = Encoder::get("<http://www.w3.org/ns/shacl#oneOrMorePath>").unwrap_or(0);
    let one_or_more = get_objects(shapes, path_node, sh_one_or_more);
    if !one_or_more.is_empty() {
        let mut results = Vec::new();
        let mut visited = HashSet::new();
        let mut queue = vec![focus_node];
        while !queue.is_empty() {
            let node = queue.remove(0);
            for target in &one_or_more {
                for n in eval_path(data, shapes, node, *target) {
                    if visited.insert(n) {
                        results.push(n);
                        queue.push(n);
                    }
                }
            }
        }
        return results;
    }

    // sh:zeroOrOnePath
    let sh_zero_or_one = Encoder::get("<http://www.w3.org/ns/shacl#zeroOrOnePath>").unwrap_or(0);
    let zero_or_one = get_objects(shapes, path_node, sh_zero_or_one);
    if !zero_or_one.is_empty() {
        let mut results = vec![focus_node];
        for target in zero_or_one {
            for n in eval_path(data, shapes, focus_node, target) {
                if !results.contains(&n) {
                    results.push(n);
                }
            }
        }
        return results;
    }

    // Direct IRI property
    get_objects(data, focus_node, path_node)
}
