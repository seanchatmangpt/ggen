use super::index_utils::get_objects;
use super::messages::make_result;
use super::report::ValidationResult;
use super::values::get_lexical_form;
use super::Vocab;
use crate::encoding::Encoder;
/// SPARQL-based constraint and target evaluation
///
/// Handles evaluation of sh:sparql constraints and sh:target declarations
/// using the embedded SPARQL engine.
use crate::tripleindex::TripleIndex;
use crate::triples::Term;

/// Render a term in the textual SPARQL syntax needed to splice it into a
/// query string in place of `$this` (or similar substitution points).
pub(crate) fn term_to_sparql_syntax(id: usize) -> String {
    match super::values::decode_to_term(id) {
        Term::Iri(_) => {
            let lex = get_lexical_form(id).unwrap_or_default();
            format!("<{}>", lex)
        }
        Term::BlankNode(_) => {
            let lex = get_lexical_form(id).unwrap_or_default();
            format!("_:{}", lex)
        }
        Term::Literal(lit) => {
            let lex = get_lexical_form(id).unwrap_or_default();
            let escaped = lex.replace('\\', "\\\\").replace('"', "\\\"");
            if let Some(lang_id) = lit.lang {
                let lang = Encoder::decode(&lang_id).unwrap_or_default();
                format!("\"{}\"@{}", escaped, lang)
            } else if let Some(dt_id) = lit.datatype {
                let dt_lex = get_lexical_form(dt_id).unwrap_or_default();
                format!("\"{}\"^^<{}>", escaped, dt_lex)
            } else {
                format!("\"{}\"", escaped)
            }
        }
    }
}

/// Parse and evaluate a raw SPARQL query string against `data` using the
/// engine in `crate::sparql`. Returns one Vec<EncodedBinding> per solution
/// row (for ASK queries this is the underlying WHERE-pattern's solutions —
/// non-empty means the ASK would return true).
pub(crate) fn evaluate_sparql_text(
    data: &TripleIndex, query_text: &str,
) -> Result<Vec<Vec<crate::tripleindex::EncodedBinding>>, String> {
    let query = spargebra::Query::parse(query_text, None).map_err(|e| e.to_string())?;
    let plan = crate::plan_query_or_refuse(&query, data)?;
    Ok(crate::sparql::evaluate_plan(&plan, data).collect())
}

/// Rewrite `$this` occurrences in a SHACL SPARQL constraint's query text
/// into a real, valid pre-bound SPARQL variable: replace every `$this`
/// with `?this`, then inject `BIND(<iri> AS ?this)` immediately after the
/// query's first `{` (the opening of its WHERE block).
///
/// This replaces an earlier approach that textually substituted `$this`
/// with the focus node's raw syntactic form (e.g. `<http://example.org/x>`)
/// everywhere in the query text -- which breaks for the single most common
/// real-world SHACL SPARQL idiom, `SELECT $this WHERE { ... }`: substituting
/// `$this` there produces `SELECT <http://example.org/x> WHERE { ... }`,
/// which is not valid SPARQL (a bare IRI is not a legal SELECT projection),
/// so the query failed to parse and the caller's `if let Ok(rows) = ...`
/// silently swallowed the error -- reporting zero violations instead of
/// evaluating the constraint at all. Found via SHACL sh:sparql interaction
/// testing (a real bug in roxi's own code, not a delegated third-party
/// crate).
pub(crate) fn substitute_this_as_bound_variable(query_text: &str, this_syntax: &str) -> String {
    let with_var = query_text.replace("$this", "?this");
    if let Some(brace_pos) = with_var.find('{') {
        let (before, after) = with_var.split_at(brace_pos + 1);
        format!("{before} BIND({this_syntax} AS ?this) {after}")
    } else {
        with_var
    }
}

/// Check SHACL-SPARQL dialect boundary and reject SPARQL constraints if
/// CORE_ONLY is active (PROJ-407 Step 2). Returns true if constraint should
/// be evaluated, false if it violates the boundary and should be skipped.
pub(crate) fn check_sparql_boundary(
    shapes: &TripleIndex, shape_node: usize, vocab: &Vocab,
) -> bool {
    use super::model::SHACL_SPARQL_BOUNDARY;

    // CORE_ONLY boundary: reject all sh:sparql constraints at load time
    if SHACL_SPARQL_BOUNDARY == "CORE_ONLY" {
        // Check if this shape contains any sh:sparql constraints
        if !get_objects(shapes, shape_node, vocab.sh_sparql).is_empty() {
            return false; // Boundary violated, skip SPARQL evaluation
        }
    }
    // If boundary allows or no sh:sparql found, proceed with evaluation
    true
}

/// Evaluate a single sh:sparql constraint (a blank/IRI node carrying
/// sh:select or sh:ask) against `this_node`, appending any violations to
/// `results`. See `substitute_this_as_bound_variable` for how `$this` is
/// handled.
///
/// NOTE: PROJ-407 Step 2 enforces SHACL-SPARQL dialect boundary (CORE_ONLY).
/// SPARQL constraints are rejected at load time via check_sparql_boundary().
pub(crate) fn validate_sparql_constraint(
    data: &TripleIndex, shapes: &TripleIndex, vocab: &Vocab, this_node: usize, shape_node: usize,
    sparql_node: usize, default_severity: usize, default_msg: &Option<String>,
    results: &mut Vec<ValidationResult>,
) {
    use super::messages::{get_shape_messages, pick_preferred_message};

    // A sh:sparql constraint node may itself carry sh:severity / sh:message
    // overriding the enclosing shape's.
    let local_sevs = get_objects(shapes, sparql_node, vocab.sh_severity);
    let severity = local_sevs.first().copied().unwrap_or(default_severity);
    let local_messages = get_shape_messages(shapes, sparql_node, vocab);
    let local_msg = pick_preferred_message(&local_messages).or_else(|| default_msg.clone());

    let this_syntax = term_to_sparql_syntax(this_node);

    if let Some(select_lit) = get_objects(shapes, sparql_node, vocab.sh_select)
        .first()
        .copied()
    {
        let Some(query_text) = get_lexical_form(select_lit) else {
            return;
        };
        let substituted = substitute_this_as_bound_variable(&query_text, &this_syntax);
        if let Ok(rows) = evaluate_sparql_text(data, &substituted) {
            // Minimal SPARQLConstraintComponent semantics: any solution row is
            // a violation. When the SELECT projects ?message/?path/?value,
            // honour those bindings per spec; otherwise fall back to the
            // shape's own message and the focus node as the value.
            for row in rows {
                let mut msg = local_msg.clone();
                let mut path = None;
                let mut value = Some(this_node);
                for b in &row {
                    let var_name = Encoder::decode(&b.var).unwrap_or_default();
                    match var_name.trim_start_matches('?') {
                        "message" => {
                            if let Some(m) = get_lexical_form(b.val) {
                                msg = Some(m);
                            }
                        }
                        "path" => path = Some(b.val),
                        "value" => value = Some(b.val),
                        _ => {}
                    }
                }
                results.push(make_result(
                    this_node,
                    path,
                    value,
                    vocab.sh_sparql_constraint_component,
                    shape_node,
                    severity,
                    msg,
                ));
            }
        }
        // A malformed/unparseable query is silently skipped (documented
        // limitation): the SHACL SPARQL constraint components spec assumes a
        // pre-validated shapes graph, and surfacing a parse error as a Rust
        // panic/error would abort validation for the whole report.
    } else if let Some(ask_lit) = get_objects(shapes, sparql_node, vocab.sh_ask)
        .first()
        .copied()
    {
        if let Some(query_text) = get_lexical_form(ask_lit) {
            let substituted = substitute_this_as_bound_variable(&query_text, &this_syntax);
            if let Ok(rows) = evaluate_sparql_text(data, &substituted) {
                if rows.is_empty() {
                    results.push(make_result(
                        this_node,
                        None,
                        Some(this_node),
                        vocab.sh_sparql_constraint_component,
                        shape_node,
                        severity,
                        local_msg,
                    ));
                }
            }
        }
    }
}
