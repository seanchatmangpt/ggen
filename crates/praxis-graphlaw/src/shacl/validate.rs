use super::closure::SubclassClosure;
use super::index_utils::get_objects;
use super::messages::{get_severity, get_shape_messages, make_result, pick_preferred_message};
use super::report::ValidationResult;
use super::sparql::{check_sparql_boundary, validate_sparql_constraint};
use super::targets_paths::eval_path;
use super::values::{decode_to_term, get_integer_value, get_string_representation};
use super::Vocab;
/// SHACL shape validation engine
///
/// Core recursive validation logic for SHACL constraints.
/// These functions must remain together as they are mutually recursive.
use crate::tripleindex::TripleIndex;
use std::collections::HashSet;

/// Maximum recursion depth across the mutually-recursive `validate_shape` /
/// `conforms_to_shape` / `validate_property_shape` trio.
///
/// SHACL shapes graphs are arbitrary user-supplied RDF and can encode
/// arbitrarily long `sh:node`/`sh:and`/`sh:or`/`sh:xone`/`sh:not` chains, or
/// nested `sh:property` chains. The existing `visited` cycle detection in
/// `validate_shape` only catches a shape being revisited (directly or via a
/// true cycle) for the *same* focus node -- it does not bound a long
/// *acyclic* chain of distinct shapes, and `validate_property_shape`'s
/// nested-`sh:property` recursion has no cycle detection at all.
///
/// Confirmed by direct adversarial testing (2026-07-17, via `ggen graph
/// validate --shapes`): an acyclic chain of ~500-1000 distinct
/// `sh:node`-linked shapes, or ~200-500 nested `sh:property` shapes, crashes
/// the whole process with a real stack overflow (SIGABRT) on an 8MB default
/// *main*-thread stack -- not a `panic!` that `catch_unwind` could
/// intercept.
///
/// Critically, this bound must hold regardless of which thread reaches
/// this code, and thread stack sizes vary widely: a `cargo test` worker
/// thread defaults to roughly 2MB, a fraction of a typical process main
/// thread. An initial guard set to 128 (safe on the 8MB main thread, per
/// the thresholds above) was directly observed to *still* stack-overflow
/// when the exact same adversarial shapes graphs were validated from a
/// `cargo test` worker thread (see `ggen-engine`'s
/// `validate_hardening_test.rs`) -- proving a depth bound calibrated
/// against one thread's stack is not portable. This constant is
/// deliberately conservative -- comfortably below the extrapolated
/// small-thread crash threshold with a large safety margin, and far more
/// than any legitimate hand-written SHACL ontology nests in practice (real
/// shapes graphs nest single digits to low tens of levels deep, not
/// hundreds) -- rather than tuned tightly to any one observed threshold.
pub(crate) const MAX_SHACL_VALIDATION_DEPTH: usize = 32;

/// Returns true if the node conforms to the given shape (no violations).
pub(crate) fn conforms_to_shape(
    data: &TripleIndex, shapes: &TripleIndex, vocab: &Vocab, node: usize, shape_node: usize,
    visited: &mut HashSet<(usize, usize)>, closure: &SubclassClosure, depth: usize,
) -> bool {
    let mut temp = Vec::new();
    validate_shape(
        data, shapes, vocab, node, shape_node, &mut temp, visited, closure, depth,
    );
    temp.is_empty()
}

/// Main shape validation function - validates a focus node against a shape
pub(crate) fn validate_shape(
    data: &TripleIndex, shapes: &TripleIndex, vocab: &Vocab, focus_node: usize, shape_node: usize,
    results: &mut Vec<ValidationResult>, visited: &mut HashSet<(usize, usize)>,
    closure: &SubclassClosure, depth: usize,
) {
    use super::closure::has_class;
    use super::index_utils::{is_blank_node, is_iri, is_literal, is_shape_deactivated};
    use super::values::{compare_numeric, get_lang_tag, match_regex};

    // Cycle detection: if we are already validating (focus, shape), skip.
    if !visited.insert((focus_node, shape_node)) {
        return;
    }

    // Depth guard: see `MAX_SHACL_VALIDATION_DEPTH` -- an acyclic chain of
    // distinct shapes is not caught by the cycle check above and would
    // otherwise recurse until the process stack-overflows. Fail closed
    // (a real violation, not a silent pass) rather than keep recursing.
    if depth > MAX_SHACL_VALIDATION_DEPTH {
        results.push(make_result(
            focus_node,
            None,
            Some(focus_node),
            vocab.sh_node_constraint_component,
            shape_node,
            vocab.sh_violation,
            Some(format!(
                "shapes graph exceeded the maximum safe validation depth ({MAX_SHACL_VALIDATION_DEPTH}); \
                 refusing further recursive evaluation to avoid a stack overflow"
            )),
        ));
        visited.remove(&(focus_node, shape_node));
        return;
    }

    // Deactivated check
    if is_shape_deactivated(shapes, shape_node, vocab) {
        visited.remove(&(focus_node, shape_node));
        return;
    }

    let severity = get_severity(shapes, shape_node, vocab);
    let messages = get_shape_messages(shapes, shape_node, vocab);
    let default_msg = pick_preferred_message(&messages);

    // -----------------------------------------------------------------------
    // Self-as-property-shape: a PropertyShape can declare its own targets
    // directly (sh:targetNode/sh:targetClass/etc alongside sh:path), rather
    // than only ever being reached indirectly via another shape's
    // sh:property. `validate_property_shape` normally only runs for shapes
    // reached through `sh:property` below; a shape validated here that
    // itself has sh:path would otherwise have its own path/minCount/maxCount
    // constraints silently skipped. Real bug found vendoring the W3C
    // core/path test suite (path-sequence-001 and siblings): these are all
    // top-level PropertyShapes with their own sh:targetNode.
    // -----------------------------------------------------------------------
    if !get_objects(shapes, shape_node, vocab.sh_path).is_empty() {
        validate_property_shape(
            data,
            shapes,
            vocab,
            focus_node,
            shape_node,
            results,
            visited,
            closure,
            depth + 1,
        );
    }

    // -----------------------------------------------------------------------
    // Node-level constraints
    // -----------------------------------------------------------------------

    // sh:nodeKind
    for nk in get_objects(shapes, shape_node, vocab.sh_node_kind) {
        let ok = if nk == vocab.sh_iri {
            is_iri(focus_node)
        } else if nk == vocab.sh_blank_node {
            is_blank_node(focus_node)
        } else if nk == vocab.sh_literal {
            is_literal(focus_node)
        } else if nk == vocab.sh_blank_node_or_iri {
            is_blank_node(focus_node) || is_iri(focus_node)
        } else if nk == vocab.sh_blank_node_or_literal {
            is_blank_node(focus_node) || is_literal(focus_node)
        } else if nk == vocab.sh_iri_or_literal {
            is_iri(focus_node) || is_literal(focus_node)
        } else {
            true
        };
        if !ok {
            results.push(make_result(
                focus_node,
                None,
                Some(focus_node),
                vocab.sh_node_kind_constraint_component,
                shape_node,
                severity,
                default_msg.clone(),
            ));
        }
    }

    // sh:datatype (node-level)
    for dt in get_objects(shapes, shape_node, vocab.sh_datatype) {
        if !super::index_utils::check_datatype(focus_node, dt) {
            results.push(make_result(
                focus_node,
                None,
                Some(focus_node),
                vocab.sh_datatype_constraint_component,
                shape_node,
                severity,
                default_msg.clone(),
            ));
        }
    }

    // sh:class (node-level)
    for class in get_objects(shapes, shape_node, vocab.sh_class) {
        if !has_class(data, focus_node, class, vocab.rdf_type, closure) {
            results.push(make_result(
                focus_node,
                None,
                Some(focus_node),
                vocab.sh_class_constraint_component,
                shape_node,
                severity,
                default_msg.clone(),
            ));
        }
    }

    // sh:hasValue (node-level)
    for hv in get_objects(shapes, shape_node, vocab.sh_has_value) {
        if focus_node != hv {
            results.push(make_result(
                focus_node,
                None,
                Some(focus_node),
                vocab.sh_has_value_constraint_component,
                shape_node,
                severity,
                default_msg.clone(),
            ));
        }
    }

    // sh:in (node-level)
    for in_list in get_objects(shapes, shape_node, vocab.sh_in) {
        let allowed = super::index_utils::get_rdf_list(shapes, in_list);
        if !allowed.contains(&focus_node) {
            results.push(make_result(
                focus_node,
                None,
                Some(focus_node),
                vocab.sh_in_constraint_component,
                shape_node,
                severity,
                default_msg.clone(),
            ));
        }
    }

    // sh:pattern + sh:flags (node-level)
    let flags_str = get_objects(shapes, shape_node, vocab.sh_flags)
        .first()
        .and_then(|f| super::values::get_lexical_form(*f))
        .unwrap_or_default();
    for pat in get_objects(shapes, shape_node, vocab.sh_pattern) {
        if let Some(pat_str) = super::values::get_lexical_form(pat) {
            let focus_str = super::values::get_lexical_form(focus_node).unwrap_or_default();
            if !match_regex(&pat_str, &focus_str, &flags_str) {
                results.push(make_result(
                    focus_node,
                    None,
                    Some(focus_node),
                    vocab.sh_pattern_constraint_component,
                    shape_node,
                    severity,
                    default_msg.clone(),
                ));
            }
        }
    }

    // sh:minLength / sh:maxLength (node-level)
    let char_len = get_string_representation(focus_node).map(|lex| lex.chars().count() as i64);
    for ml in get_objects(shapes, shape_node, vocab.sh_min_length) {
        if let Ok(v) = get_integer_value(ml) {
            if char_len.is_none_or(|len| len < v) {
                results.push(make_result(
                    focus_node,
                    None,
                    Some(focus_node),
                    vocab.sh_min_length_constraint_component,
                    shape_node,
                    severity,
                    default_msg.clone(),
                ));
            }
        }
    }
    for ml in get_objects(shapes, shape_node, vocab.sh_max_length) {
        if let Ok(v) = get_integer_value(ml) {
            if char_len.is_none_or(|len| len > v) {
                results.push(make_result(
                    focus_node,
                    None,
                    Some(focus_node),
                    vocab.sh_max_length_constraint_component,
                    shape_node,
                    severity,
                    default_msg.clone(),
                ));
            }
        }
    }

    // sh:minExclusive / sh:minInclusive / sh:maxExclusive / sh:maxInclusive (node-level)
    for bound in get_objects(shapes, shape_node, vocab.sh_min_exclusive) {
        match compare_numeric(focus_node, bound) {
            Ok(Some(std::cmp::Ordering::Greater)) => {}
            _ => {
                results.push(make_result(
                    focus_node,
                    None,
                    Some(focus_node),
                    vocab.sh_min_exclusive_constraint_component,
                    shape_node,
                    severity,
                    default_msg.clone(),
                ));
            }
        }
    }
    for bound in get_objects(shapes, shape_node, vocab.sh_min_inclusive) {
        match compare_numeric(focus_node, bound) {
            Ok(Some(std::cmp::Ordering::Greater)) | Ok(Some(std::cmp::Ordering::Equal)) => {}
            _ => {
                results.push(make_result(
                    focus_node,
                    None,
                    Some(focus_node),
                    vocab.sh_min_inclusive_constraint_component,
                    shape_node,
                    severity,
                    default_msg.clone(),
                ));
            }
        }
    }
    for bound in get_objects(shapes, shape_node, vocab.sh_max_exclusive) {
        match compare_numeric(focus_node, bound) {
            Ok(Some(std::cmp::Ordering::Less)) => {}
            _ => {
                results.push(make_result(
                    focus_node,
                    None,
                    Some(focus_node),
                    vocab.sh_max_exclusive_constraint_component,
                    shape_node,
                    severity,
                    default_msg.clone(),
                ));
            }
        }
    }
    for bound in get_objects(shapes, shape_node, vocab.sh_max_inclusive) {
        match compare_numeric(focus_node, bound) {
            Ok(Some(std::cmp::Ordering::Less)) | Ok(Some(std::cmp::Ordering::Equal)) => {}
            _ => {
                results.push(make_result(
                    focus_node,
                    None,
                    Some(focus_node),
                    vocab.sh_max_inclusive_constraint_component,
                    shape_node,
                    severity,
                    default_msg.clone(),
                ));
            }
        }
    }

    // sh:and (node-level)
    for and_list in get_objects(shapes, shape_node, vocab.sh_and) {
        let sub_shapes = super::index_utils::get_rdf_list(shapes, and_list);
        let conforms = sub_shapes.iter().all(|&sub| {
            conforms_to_shape(
                data,
                shapes,
                vocab,
                focus_node,
                sub,
                visited,
                closure,
                depth + 1,
            )
        });
        if !conforms {
            results.push(make_result(
                focus_node,
                None,
                Some(focus_node),
                vocab.sh_and_constraint_component,
                shape_node,
                severity,
                default_msg.clone(),
            ));
        }
    }

    // sh:or (node-level)
    for or_list in get_objects(shapes, shape_node, vocab.sh_or) {
        let sub_shapes = super::index_utils::get_rdf_list(shapes, or_list);
        let conforms = sub_shapes.iter().any(|&sub| {
            conforms_to_shape(
                data,
                shapes,
                vocab,
                focus_node,
                sub,
                visited,
                closure,
                depth + 1,
            )
        });
        if !conforms {
            results.push(make_result(
                focus_node,
                None,
                Some(focus_node),
                vocab.sh_or_constraint_component,
                shape_node,
                severity,
                default_msg.clone(),
            ));
        }
    }

    // sh:xone (node-level)
    for xone_list in get_objects(shapes, shape_node, vocab.sh_xone) {
        let sub_shapes = super::index_utils::get_rdf_list(shapes, xone_list);
        let count = sub_shapes
            .iter()
            .filter(|&&sub| {
                conforms_to_shape(
                    data,
                    shapes,
                    vocab,
                    focus_node,
                    sub,
                    visited,
                    closure,
                    depth + 1,
                )
            })
            .count();
        if count != 1 {
            results.push(make_result(
                focus_node,
                None,
                Some(focus_node),
                vocab.sh_xone_constraint_component,
                shape_node,
                severity,
                default_msg.clone(),
            ));
        }
    }

    // sh:not (node-level)
    for not_shape in get_objects(shapes, shape_node, vocab.sh_not) {
        if conforms_to_shape(
            data,
            shapes,
            vocab,
            focus_node,
            not_shape,
            visited,
            closure,
            depth + 1,
        ) {
            results.push(make_result(
                focus_node,
                None,
                Some(focus_node),
                vocab.sh_not_constraint_component,
                shape_node,
                severity,
                default_msg.clone(),
            ));
        }
    }

    // sh:node (node-level)
    for node_shape in get_objects(shapes, shape_node, vocab.sh_node) {
        if !conforms_to_shape(
            data,
            shapes,
            vocab,
            focus_node,
            node_shape,
            visited,
            closure,
            depth + 1,
        ) {
            results.push(make_result(
                focus_node,
                None,
                Some(focus_node),
                vocab.sh_node_constraint_component,
                shape_node,
                severity,
                default_msg.clone(),
            ));
        }
    }

    // sh:equals (node-level)
    for eq_prop in get_objects(shapes, shape_node, vocab.sh_equals) {
        let other_values: HashSet<usize> = eval_path(data, shapes, focus_node, eq_prop)
            .into_iter()
            .collect();
        let self_values: HashSet<usize> = std::iter::once(focus_node).collect();
        if self_values != other_values {
            if !other_values.contains(&focus_node) {
                results.push(make_result(
                    focus_node,
                    None,
                    Some(focus_node),
                    vocab.sh_equals_constraint_component,
                    shape_node,
                    severity,
                    default_msg.clone(),
                ));
            }
            for &v in &other_values {
                if v != focus_node {
                    results.push(make_result(
                        focus_node,
                        None,
                        Some(v),
                        vocab.sh_equals_constraint_component,
                        shape_node,
                        severity,
                        default_msg.clone(),
                    ));
                }
            }
        }
    }

    // sh:disjoint (node-level)
    for disj_prop in get_objects(shapes, shape_node, vocab.sh_disjoint) {
        let other_values: HashSet<usize> = eval_path(data, shapes, focus_node, disj_prop)
            .into_iter()
            .collect();
        if other_values.contains(&focus_node) {
            results.push(make_result(
                focus_node,
                None,
                Some(focus_node),
                vocab.sh_disjoint_constraint_component,
                shape_node,
                severity,
                default_msg.clone(),
            ));
        }
    }

    // sh:languageIn (node-level)
    for lang_list in get_objects(shapes, shape_node, vocab.sh_language_in) {
        let allowed_langs: Vec<String> = super::index_utils::get_rdf_list(shapes, lang_list)
            .into_iter()
            .filter_map(super::values::get_lexical_form)
            .map(|s| s.to_lowercase())
            .collect();
        if let Some(tag) = get_lang_tag(focus_node) {
            if !allowed_langs
                .iter()
                .any(|al| tag.to_lowercase().starts_with(al.as_str()))
            {
                results.push(make_result(
                    focus_node,
                    None,
                    Some(focus_node),
                    vocab.sh_language_in_constraint_component,
                    shape_node,
                    severity,
                    default_msg.clone(),
                ));
            }
        } else {
            results.push(make_result(
                focus_node,
                None,
                Some(focus_node),
                vocab.sh_language_in_constraint_component,
                shape_node,
                severity,
                default_msg.clone(),
            ));
        }
    }

    // sh:sparql
    if check_sparql_boundary(shapes, shape_node, vocab) {
        for sparql_node in get_objects(shapes, shape_node, vocab.sh_sparql) {
            validate_sparql_constraint(
                data,
                shapes,
                vocab,
                focus_node,
                shape_node,
                sparql_node,
                severity,
                &default_msg,
                results,
            );
        }
    }

    // -----------------------------------------------------------------------
    // sh:property — property shape constraints
    // -----------------------------------------------------------------------
    for ps in get_objects(shapes, shape_node, vocab.sh_property) {
        validate_property_shape(
            data,
            shapes,
            vocab,
            focus_node,
            ps,
            results,
            visited,
            closure,
            depth + 1,
        );
    }

    validate_shape_closed_and_targets_tail(
        data,
        shapes,
        vocab,
        focus_node,
        shape_node,
        severity,
        default_msg.clone(),
        results,
        visited,
    );
}

/// Validate property shape (sh:property) constraints
pub(crate) fn validate_property_shape(
    data: &TripleIndex, shapes: &TripleIndex, vocab: &Vocab, focus_node: usize, ps: usize,
    results: &mut Vec<ValidationResult>, visited: &mut HashSet<(usize, usize)>,
    closure: &SubclassClosure, depth: usize,
) {
    use super::closure::has_class;
    use super::values::{compare_numeric, get_lang_tag, match_regex};

    // Depth guard: unlike `validate_shape`, nested `sh:property` recursion
    // (below) has no `visited`-based cycle detection at all -- a shape
    // referencing itself via `sh:property` (directly or through a cycle of
    // property shapes), or simply a long nested chain, would otherwise
    // recurse until the process stack-overflows. See
    // `MAX_SHACL_VALIDATION_DEPTH`; confirmed by direct testing that this
    // path crashes at a *lower* chain length than `validate_shape`'s
    // sh:node chain does. Fail closed with a violation rather than recurse
    // further.
    if depth > MAX_SHACL_VALIDATION_DEPTH {
        results.push(make_result(
            focus_node,
            None,
            None,
            vocab.sh_node_constraint_component,
            ps,
            vocab.sh_violation,
            Some(format!(
                "shapes graph exceeded the maximum safe validation depth ({MAX_SHACL_VALIDATION_DEPTH}); \
                 refusing further recursive sh:property evaluation to avoid a stack overflow"
            )),
        ));
        return;
    }

    let paths = get_objects(shapes, ps, vocab.sh_path);
    if paths.is_empty() {
        return;
    }
    let path = paths[0];
    let v_nodes = eval_path(data, shapes, focus_node, path);
    let ps_severity = get_severity(shapes, ps, vocab);
    let ps_messages = get_shape_messages(shapes, ps, vocab);
    let ps_msg = pick_preferred_message(&ps_messages);

    // sh:minCount
    for mc in get_objects(shapes, ps, vocab.sh_min_count) {
        if let Ok(mc_val) = get_integer_value(mc) {
            if (v_nodes.len() as i64) < mc_val {
                results.push(make_result(
                    focus_node,
                    Some(path),
                    None,
                    vocab.sh_min_count_constraint_component,
                    ps,
                    ps_severity,
                    ps_msg.clone(),
                ));
            }
        }
    }

    // sh:maxCount
    for mc in get_objects(shapes, ps, vocab.sh_max_count) {
        if let Ok(mc_val) = get_integer_value(mc) {
            if (v_nodes.len() as i64) > mc_val {
                results.push(make_result(
                    focus_node,
                    Some(path),
                    None,
                    vocab.sh_max_count_constraint_component,
                    ps,
                    ps_severity,
                    ps_msg.clone(),
                ));
            }
        }
    }

    // sh:datatype (per-value)
    for dt in get_objects(shapes, ps, vocab.sh_datatype) {
        for &v in &v_nodes {
            if !super::index_utils::check_datatype(v, dt) {
                results.push(make_result(
                    focus_node,
                    Some(path),
                    Some(v),
                    vocab.sh_datatype_constraint_component,
                    ps,
                    ps_severity,
                    ps_msg.clone(),
                ));
            }
        }
    }

    // sh:nodeKind (per-value)
    use super::index_utils::{is_blank_node, is_iri, is_literal};
    for nk in get_objects(shapes, ps, vocab.sh_node_kind) {
        for &v in &v_nodes {
            let ok = if nk == vocab.sh_iri {
                is_iri(v)
            } else if nk == vocab.sh_blank_node {
                is_blank_node(v)
            } else if nk == vocab.sh_literal {
                is_literal(v)
            } else if nk == vocab.sh_blank_node_or_iri {
                is_blank_node(v) || is_iri(v)
            } else if nk == vocab.sh_blank_node_or_literal {
                is_blank_node(v) || is_literal(v)
            } else if nk == vocab.sh_iri_or_literal {
                is_iri(v) || is_literal(v)
            } else {
                true
            };
            if !ok {
                results.push(make_result(
                    focus_node,
                    Some(path),
                    Some(v),
                    vocab.sh_node_kind_constraint_component,
                    ps,
                    ps_severity,
                    ps_msg.clone(),
                ));
            }
        }
    }

    // sh:class (per-value)
    for class in get_objects(shapes, ps, vocab.sh_class) {
        for &v in &v_nodes {
            if !has_class(data, v, class, vocab.rdf_type, closure) {
                results.push(make_result(
                    focus_node,
                    Some(path),
                    Some(v),
                    vocab.sh_class_constraint_component,
                    ps,
                    ps_severity,
                    ps_msg.clone(),
                ));
            }
        }
    }

    // sh:hasValue (per-property)
    for hv in get_objects(shapes, ps, vocab.sh_has_value) {
        if !v_nodes.contains(&hv) {
            results.push(make_result(
                focus_node,
                Some(path),
                None,
                vocab.sh_has_value_constraint_component,
                ps,
                ps_severity,
                ps_msg.clone(),
            ));
        }
    }

    // sh:in (per-value)
    for in_list in get_objects(shapes, ps, vocab.sh_in) {
        let allowed = super::index_utils::get_rdf_list(shapes, in_list);
        for &v in &v_nodes {
            if !allowed.contains(&v) {
                results.push(make_result(
                    focus_node,
                    Some(path),
                    Some(v),
                    vocab.sh_in_constraint_component,
                    ps,
                    ps_severity,
                    ps_msg.clone(),
                ));
            }
        }
    }

    // sh:pattern + sh:flags (per-value)
    let ps_flags = get_objects(shapes, ps, vocab.sh_flags)
        .first()
        .and_then(|f| super::values::get_lexical_form(*f))
        .unwrap_or_default();
    for pat in get_objects(shapes, ps, vocab.sh_pattern) {
        if let Some(pat_str) = super::values::get_lexical_form(pat) {
            for &v in &v_nodes {
                let v_str = super::values::get_lexical_form(v).unwrap_or_default();
                if !match_regex(&pat_str, &v_str, &ps_flags) {
                    results.push(make_result(
                        focus_node,
                        Some(path),
                        Some(v),
                        vocab.sh_pattern_constraint_component,
                        ps,
                        ps_severity,
                        ps_msg.clone(),
                    ));
                }
            }
        }
    }

    // sh:minLength / sh:maxLength (per-value)
    for ml in get_objects(shapes, ps, vocab.sh_min_length) {
        if let Ok(min) = get_integer_value(ml) {
            for &v in &v_nodes {
                let violates = match get_string_representation(v) {
                    Some(s) => (s.chars().count() as i64) < min,
                    None => true,
                };
                if violates {
                    results.push(make_result(
                        focus_node,
                        Some(path),
                        Some(v),
                        vocab.sh_min_length_constraint_component,
                        ps,
                        ps_severity,
                        ps_msg.clone(),
                    ));
                }
            }
        }
    }
    for ml in get_objects(shapes, ps, vocab.sh_max_length) {
        if let Ok(max) = get_integer_value(ml) {
            for &v in &v_nodes {
                let violates = match get_string_representation(v) {
                    Some(s) => (s.chars().count() as i64) > max,
                    None => true,
                };
                if violates {
                    results.push(make_result(
                        focus_node,
                        Some(path),
                        Some(v),
                        vocab.sh_max_length_constraint_component,
                        ps,
                        ps_severity,
                        ps_msg.clone(),
                    ));
                }
            }
        }
    }

    // Numeric range constraints (per-value)
    for bound in get_objects(shapes, ps, vocab.sh_min_exclusive) {
        for &v in &v_nodes {
            match compare_numeric(v, bound) {
                Ok(Some(std::cmp::Ordering::Greater)) => {}
                _ => {
                    results.push(make_result(
                        focus_node,
                        Some(path),
                        Some(v),
                        vocab.sh_min_exclusive_constraint_component,
                        ps,
                        ps_severity,
                        ps_msg.clone(),
                    ));
                }
            }
        }
    }
    for bound in get_objects(shapes, ps, vocab.sh_min_inclusive) {
        for &v in &v_nodes {
            match compare_numeric(v, bound) {
                Ok(Some(std::cmp::Ordering::Greater)) | Ok(Some(std::cmp::Ordering::Equal)) => {}
                _ => {
                    results.push(make_result(
                        focus_node,
                        Some(path),
                        Some(v),
                        vocab.sh_min_inclusive_constraint_component,
                        ps,
                        ps_severity,
                        ps_msg.clone(),
                    ));
                }
            }
        }
    }
    for bound in get_objects(shapes, ps, vocab.sh_max_exclusive) {
        for &v in &v_nodes {
            match compare_numeric(v, bound) {
                Ok(Some(std::cmp::Ordering::Less)) => {}
                _ => {
                    results.push(make_result(
                        focus_node,
                        Some(path),
                        Some(v),
                        vocab.sh_max_exclusive_constraint_component,
                        ps,
                        ps_severity,
                        ps_msg.clone(),
                    ));
                }
            }
        }
    }
    for bound in get_objects(shapes, ps, vocab.sh_max_inclusive) {
        for &v in &v_nodes {
            match compare_numeric(v, bound) {
                Ok(Some(std::cmp::Ordering::Less)) | Ok(Some(std::cmp::Ordering::Equal)) => {}
                _ => {
                    results.push(make_result(
                        focus_node,
                        Some(path),
                        Some(v),
                        vocab.sh_max_inclusive_constraint_component,
                        ps,
                        ps_severity,
                        ps_msg.clone(),
                    ));
                }
            }
        }
    }

    // sh:languageIn (per-value)
    for lang_list in get_objects(shapes, ps, vocab.sh_language_in) {
        let allowed_langs: Vec<String> = super::index_utils::get_rdf_list(shapes, lang_list)
            .into_iter()
            .filter_map(super::values::get_lexical_form)
            .map(|s| s.to_lowercase())
            .collect();
        for &v in &v_nodes {
            if let Some(tag) = get_lang_tag(v) {
                if !allowed_langs
                    .iter()
                    .any(|al| tag.to_lowercase().starts_with(al.as_str()))
                {
                    results.push(make_result(
                        focus_node,
                        Some(path),
                        Some(v),
                        vocab.sh_language_in_constraint_component,
                        ps,
                        ps_severity,
                        ps_msg.clone(),
                    ));
                }
            } else {
                results.push(make_result(
                    focus_node,
                    Some(path),
                    Some(v),
                    vocab.sh_language_in_constraint_component,
                    ps,
                    ps_severity,
                    ps_msg.clone(),
                ));
            }
        }
    }

    // sh:uniqueLang (per-property)
    let unique_lang_vals = get_objects(shapes, ps, vocab.sh_unique_lang);
    for ul in unique_lang_vals {
        if let Some(lex) = super::values::get_lexical_form(ul) {
            if lex == "true" || lex == "1" {
                let mut lang_counts: std::collections::HashMap<String, usize> =
                    std::collections::HashMap::new();
                for &v in &v_nodes {
                    if let Some(tag) = get_lang_tag(v) {
                        *lang_counts.entry(tag.to_lowercase()).or_insert(0) += 1;
                    }
                }
                for count in lang_counts.values() {
                    if *count > 1 {
                        results.push(make_result(
                            focus_node,
                            Some(path),
                            None,
                            vocab.sh_unique_lang_constraint_component,
                            ps,
                            ps_severity,
                            ps_msg.clone(),
                        ));
                    }
                }
            }
        }
    }

    // sh:equals (values must equal values for the given path)
    for eq_prop in get_objects(shapes, ps, vocab.sh_equals) {
        let other_values: HashSet<usize> = eval_path(data, shapes, focus_node, eq_prop)
            .into_iter()
            .collect();
        let self_values: HashSet<usize> = v_nodes.iter().cloned().collect();
        if self_values != other_values {
            for &v in &v_nodes {
                if !other_values.contains(&v) {
                    results.push(make_result(
                        focus_node,
                        Some(path),
                        Some(v),
                        vocab.sh_equals_constraint_component,
                        ps,
                        ps_severity,
                        ps_msg.clone(),
                    ));
                }
            }
            for &v in &other_values {
                if !self_values.contains(&v) {
                    results.push(make_result(
                        focus_node,
                        Some(path),
                        Some(v),
                        vocab.sh_equals_constraint_component,
                        ps,
                        ps_severity,
                        ps_msg.clone(),
                    ));
                }
            }
        }
    }

    // sh:disjoint
    for disj_prop in get_objects(shapes, ps, vocab.sh_disjoint) {
        let other_values: HashSet<usize> = eval_path(data, shapes, focus_node, disj_prop)
            .into_iter()
            .collect();
        for &v in &v_nodes {
            if other_values.contains(&v) {
                results.push(make_result(
                    focus_node,
                    Some(path),
                    Some(v),
                    vocab.sh_disjoint_constraint_component,
                    ps,
                    ps_severity,
                    ps_msg.clone(),
                ));
            }
        }
    }

    // sh:lessThan
    for lt_prop in get_objects(shapes, ps, vocab.sh_less_than) {
        let other_values: Vec<usize> = eval_path(data, shapes, focus_node, lt_prop);
        for &v in &v_nodes {
            for &ov in &other_values {
                match compare_numeric(v, ov) {
                    Ok(Some(std::cmp::Ordering::Less)) => {}
                    _ => {
                        results.push(make_result(
                            focus_node,
                            Some(path),
                            Some(v),
                            vocab.sh_less_than_constraint_component,
                            ps,
                            ps_severity,
                            ps_msg.clone(),
                        ));
                    }
                }
            }
        }
    }

    // sh:lessThanOrEquals
    for lte_prop in get_objects(shapes, ps, vocab.sh_less_than_or_equals) {
        let other_values: Vec<usize> = eval_path(data, shapes, focus_node, lte_prop);
        for &v in &v_nodes {
            for &ov in &other_values {
                match compare_numeric(v, ov) {
                    Ok(Some(std::cmp::Ordering::Less)) | Ok(Some(std::cmp::Ordering::Equal)) => {}
                    _ => {
                        results.push(make_result(
                            focus_node,
                            Some(path),
                            Some(v),
                            vocab.sh_less_than_or_equals_constraint_component,
                            ps,
                            ps_severity,
                            ps_msg.clone(),
                        ));
                    }
                }
            }
        }
    }

    // sh:qualifiedValueShape / sh:qualifiedMinCount / sh:qualifiedMaxCount
    let qvs_list = get_objects(shapes, ps, vocab.sh_qualified_value_shape);
    if !qvs_list.is_empty() {
        let qvs = qvs_list[0];
        let conforming_count = v_nodes
            .iter()
            .filter(|&&v| {
                conforms_to_shape(data, shapes, vocab, v, qvs, visited, closure, depth + 1)
            })
            .count() as i64;
        for qmin in get_objects(shapes, ps, vocab.sh_qualified_min_count) {
            if let Ok(min) = get_integer_value(qmin) {
                if conforming_count < min {
                    results.push(make_result(
                        focus_node,
                        Some(path),
                        None,
                        vocab.sh_qualified_value_shape_constraint_component,
                        ps,
                        ps_severity,
                        ps_msg.clone(),
                    ));
                }
            }
        }
        for qmax in get_objects(shapes, ps, vocab.sh_qualified_max_count) {
            if let Ok(max) = get_integer_value(qmax) {
                if conforming_count > max {
                    results.push(make_result(
                        focus_node,
                        Some(path),
                        None,
                        vocab.sh_qualified_value_shape_constraint_component,
                        ps,
                        ps_severity,
                        ps_msg.clone(),
                    ));
                }
            }
        }
    }

    // Logical constraints on property shape values
    for and_list in get_objects(shapes, ps, vocab.sh_and) {
        let sub_shapes = super::index_utils::get_rdf_list(shapes, and_list);
        for &v in &v_nodes {
            let conforms = sub_shapes.iter().all(|&sub| {
                conforms_to_shape(data, shapes, vocab, v, sub, visited, closure, depth + 1)
            });
            if !conforms {
                results.push(make_result(
                    focus_node,
                    Some(path),
                    Some(v),
                    vocab.sh_and_constraint_component,
                    ps,
                    ps_severity,
                    ps_msg.clone(),
                ));
            }
        }
    }

    for or_list in get_objects(shapes, ps, vocab.sh_or) {
        let sub_shapes = super::index_utils::get_rdf_list(shapes, or_list);
        for &v in &v_nodes {
            let conforms = sub_shapes.iter().any(|&sub| {
                conforms_to_shape(data, shapes, vocab, v, sub, visited, closure, depth + 1)
            });
            if !conforms {
                results.push(make_result(
                    focus_node,
                    Some(path),
                    Some(v),
                    vocab.sh_or_constraint_component,
                    ps,
                    ps_severity,
                    ps_msg.clone(),
                ));
            }
        }
    }

    for xone_list in get_objects(shapes, ps, vocab.sh_xone) {
        let sub_shapes = super::index_utils::get_rdf_list(shapes, xone_list);
        for &v in &v_nodes {
            let count = sub_shapes
                .iter()
                .filter(|&&sub| {
                    conforms_to_shape(data, shapes, vocab, v, sub, visited, closure, depth + 1)
                })
                .count();
            if count != 1 {
                results.push(make_result(
                    focus_node,
                    Some(path),
                    Some(v),
                    vocab.sh_xone_constraint_component,
                    ps,
                    ps_severity,
                    ps_msg.clone(),
                ));
            }
        }
    }

    for not_shape in get_objects(shapes, ps, vocab.sh_not) {
        for &v in &v_nodes {
            if conforms_to_shape(
                data,
                shapes,
                vocab,
                v,
                not_shape,
                visited,
                closure,
                depth + 1,
            ) {
                results.push(make_result(
                    focus_node,
                    Some(path),
                    Some(v),
                    vocab.sh_not_constraint_component,
                    ps,
                    ps_severity,
                    ps_msg.clone(),
                ));
            }
        }
    }

    for node_shape in get_objects(shapes, ps, vocab.sh_node) {
        for &v in &v_nodes {
            if !conforms_to_shape(
                data,
                shapes,
                vocab,
                v,
                node_shape,
                visited,
                closure,
                depth + 1,
            ) {
                results.push(make_result(
                    focus_node,
                    Some(path),
                    Some(v),
                    vocab.sh_node_constraint_component,
                    ps,
                    ps_severity,
                    ps_msg.clone(),
                ));
            }
        }
    }

    // sh:property nested inside a property shape
    for ps_nested in get_objects(shapes, ps, vocab.sh_property) {
        for &v in &v_nodes {
            validate_property_shape(
                data,
                shapes,
                vocab,
                v,
                ps_nested,
                results,
                visited,
                closure,
                depth + 1,
            );
        }
    }
}

/// Validate sh:closed and remove from visited set
fn validate_shape_closed_and_targets_tail(
    data: &TripleIndex, shapes: &TripleIndex, vocab: &Vocab, focus_node: usize, shape_node: usize,
    severity: usize, default_msg: Option<String>, results: &mut Vec<ValidationResult>,
    visited: &mut HashSet<(usize, usize)>,
) {
    // sh:closed / sh:ignoredProperties
    let closed_vals = get_objects(shapes, shape_node, vocab.sh_closed);
    for cv in closed_vals {
        if let Some(lex) = super::values::get_lexical_form(cv) {
            if lex == "true" || lex == "1" {
                let mut allowed_preds: HashSet<usize> = HashSet::new();
                for ps in get_objects(shapes, shape_node, vocab.sh_property) {
                    for p in get_objects(shapes, ps, vocab.sh_path) {
                        allowed_preds.insert(p);
                    }
                }
                for ig_list in get_objects(shapes, shape_node, vocab.sh_ignored_properties) {
                    for ig in super::index_utils::get_rdf_list(shapes, ig_list) {
                        allowed_preds.insert(ig);
                    }
                }
                if let Some(preds) = data.spo.get(&focus_node) {
                    // Sort before iterating: `preds` is an `FxHashMap<usize, ...>` (rustc_hash,
                    // deterministic hasher but NOT insertion-order-preserving), so its key
                    // iteration order depends on bucket layout, which can shift with insertion
                    // sequence (e.g. differing resize/rehash history across two loaders of the
                    // same logical graph, or two independent SPARQL CONSTRUCT evaluations). This
                    // `results` push order flows unmodified into the same hook-receipt digest
                    // chain swarm finding #22 fixed in shacl/report.rs's `Validator::validate` --
                    // matching that fix's discipline: a real `usize` symbol ID sorts numerically
                    // without changing which violations are reported, only their order.
                    let mut sorted_preds: Vec<usize> = preds.keys().copied().collect();
                    sorted_preds.sort_unstable();
                    for pred in sorted_preds {
                        if !allowed_preds.contains(&pred) {
                            results.push(make_result(
                                focus_node,
                                Some(pred),
                                None,
                                vocab.sh_closed_constraint_component,
                                shape_node,
                                severity,
                                default_msg.clone(),
                            ));
                        }
                    }
                }
            }
        }
    }

    visited.remove(&(focus_node, shape_node));
}
