//! `log:` namespace builtins handled procedurally (one-shot, not fixpoint).
//!
//! `log:implies`/`log:collectAllIn`/`log:notIncludes`/`log:includes`/
//! `log:forAllIn`/`log:ifThenElseIn`/`log:conclusion` are NOT here -- they
//! need to query or materialize against the live `TripleIndex` (a genuine
//! sub-query, not just the row of `Binding`s already accumulated), so they
//! are dispatched at the `Reasoner::materialize` level -- see
//! `reasoner/log_implies.rs`, `reasoner/log_collect_all_in.rs`,
//! `reasoner/log_not_includes.rs`, `reasoner/log_includes.rs`,
//! `reasoner/log_for_all_in.rs`, `reasoner/log_if_then_else_in.rs`,
//! `reasoner/log_conclusion.rs`.

use super::{
    eval_functional, eval_row_constraint, intern_string, lexical_value, numeric_value,
    resolve_operand, subject_list_members,
};
use crate::{Binding, Encoder, Term, Triple, VarOrTerm};

pub(crate) const LOG_EQUAL_TO: &str = "<http://www.w3.org/2000/10/swap/log#equalTo>";
pub(crate) const LOG_NOT_EQUAL_TO: &str = "<http://www.w3.org/2000/10/swap/log#notEqualTo>";
pub(crate) const LOG_DTLIT: &str = "<http://www.w3.org/2000/10/swap/log#dtlit>";
pub(crate) const LOG_RAW_TYPE: &str = "<http://www.w3.org/2000/10/swap/log#rawType>";
pub(crate) const LOG_URI: &str = "<http://www.w3.org/2000/10/swap/log#uri>";
pub(crate) const LOG_LOCAL_NAME: &str = "<http://www.w3.org/2000/10/swap/log#localName>";
pub(crate) const LOG_BOUND: &str = "<http://www.w3.org/2000/10/swap/log#bound>";
pub(crate) const LOG_N3_STRING: &str = "<http://www.w3.org/2000/10/swap/log#n3String>";
pub(crate) const LOG_PARSED_AS_N3: &str = "<http://www.w3.org/2000/10/swap/log#parsedAsN3>";
pub(crate) const LOG_CONJUNCTION: &str = "<http://www.w3.org/2000/10/swap/log#conjunction>";

pub(crate) fn eval_equal_to(pattern: &Triple, bindings: &Binding) -> Option<Binding> {
    eval_row_constraint(pattern, bindings, |s, o| {
        s == o || matches!((numeric_value(s), numeric_value(o)), (Some(a), Some(b)) if a == b)
    })
}

pub(crate) fn eval_not_equal_to(pattern: &Triple, bindings: &Binding) -> Option<Binding> {
    eval_row_constraint(pattern, bindings, |s, o| {
        !(s == o || matches!((numeric_value(s), numeric_value(o)), (Some(a), Some(b)) if a == b))
    })
}

/// `(lexical datatype) log:dtlit ?typed` -- construct a typed literal from a
/// two-element list `(lexicalString datatypeIri)`.
pub(crate) fn eval_dtlit(pattern: &Triple, bindings: &Binding) -> Option<Binding> {
    eval_functional(pattern, bindings, |pattern, bindings, row| {
        let members = subject_list_members(&pattern.s, bindings, row)?;
        if members.len() != 2 {
            return None;
        }
        let lexical = lexical_value(members[0])?;
        let datatype = Encoder::decode(&members[1])?;
        Some(VarOrTerm::new_literal(lexical, Some(datatype), None).to_encoded())
    })
}

/// `?x log:rawType ?type` -- the "raw" (syntactic) type of a term: a typed
/// literal's datatype IRI, `xsd:string` for a plain (untyped/no-lang)
/// literal, or `rdf:langString` for a language-tagged literal. Fails
/// (no solution) for IRIs/blank nodes, which have no literal "raw type".
pub(crate) fn eval_raw_type(pattern: &Triple, bindings: &Binding) -> Option<Binding> {
    eval_functional(pattern, bindings, |pattern, bindings, row| {
        let s = resolve_operand(&pattern.s, bindings, row)?;
        match Encoder::decode_to_term(s)? {
            Term::Literal(lit) => {
                if let Some(dt) = lit.datatype {
                    Some(dt)
                } else if lit.lang.is_some() {
                    Some(Encoder::add(
                        "<http://www.w3.org/1999/02/22-rdf-syntax-ns#langString>".to_string(),
                    ))
                } else {
                    Some(Encoder::add(
                        "<http://www.w3.org/2001/XMLSchema#string>".to_string(),
                    ))
                }
            }
            _ => None,
        }
    })
}

/// `?resource log:uri ?string` -- the plain-string form of a resource's IRI.
pub(crate) fn eval_uri(pattern: &Triple, bindings: &Binding) -> Option<Binding> {
    eval_functional(pattern, bindings, |pattern, bindings, row| {
        let s = resolve_operand(&pattern.s, bindings, row)?;
        let decoded = Encoder::decode(&s)?;
        let iri_text = decoded.trim_matches(|c| c == '<' || c == '>').to_string();
        Some(intern_string(iri_text))
    })
}

/// `?resource log:localName ?string` -- the local name of an IRI: the
/// substring after its last `#` or, failing that, its last `/`.
pub(crate) fn eval_local_name(pattern: &Triple, bindings: &Binding) -> Option<Binding> {
    eval_functional(pattern, bindings, |pattern, bindings, row| {
        let s = resolve_operand(&pattern.s, bindings, row)?;
        let decoded = Encoder::decode(&s)?;
        let iri_text = decoded.trim_matches(|c| c == '<' || c == '>');
        let local = if let Some(idx) = iri_text.rfind('#') {
            &iri_text[idx + 1..]
        } else if let Some(idx) = iri_text.rfind('/') {
            &iri_text[idx + 1..]
        } else {
            iri_text
        };
        Some(intern_string(local.to_string()))
    })
}

/// `?x log:bound ?ignored` -- a row constraint that keeps only the rows
/// where `?x` (the subject) already resolves to a value; `resolve_operand`
/// failing for a variable that has no binding yet in that row *is* the
/// "unbound" case, so simply requiring both operands to resolve (the same
/// mechanism every other row-constraint builtin uses) implements `bound`.
pub(crate) fn eval_bound(pattern: &Triple, bindings: &Binding) -> Option<Binding> {
    eval_row_constraint(pattern, bindings, |_s, _o| true)
}

/// `{formula} log:n3String ?string` -- serialize a quoted graph's triples
/// back to N3 text (via the existing `TripleStore::decode_triples`
/// formatter).
pub(crate) fn eval_n3_string(pattern: &Triple, bindings: &Binding) -> Option<Binding> {
    eval_functional(pattern, bindings, |pattern, bindings, row| {
        let s = resolve_operand(&pattern.s, bindings, row)?;
        let triples = VarOrTerm::formula_triples(s)?;
        let text = crate::TripleStore::decode_triples(&triples);
        Some(intern_string(text))
    })
}

/// `"n3 text" log:parsedAsN3 ?formula` -- parse a string literal as an N3
/// document and bind the object to a fresh quoted-graph formula holding the
/// parsed triples. Any top-level rules present in the parsed text are
/// dropped (not reified) -- N3 formulas built this way are for triple-level
/// use (e.g. feeding `log:conclusion`, `log:includes`), not for smuggling
/// new top-level inference rules into the running program.
pub(crate) fn eval_parsed_as_n3(pattern: &Triple, bindings: &Binding) -> Option<Binding> {
    eval_functional(pattern, bindings, |pattern, bindings, row| {
        let s = resolve_operand(&pattern.s, bindings, row)?;
        let text = lexical_value(s)?;
        let (triples, _rules) = crate::parser::Parser::parse_n3_document(&text).ok()?;
        Some(VarOrTerm::new_formula(triples).to_encoded())
    })
}

/// `(formula1 formula2 ...) log:conjunction ?formula` -- bind the object to
/// a fresh formula whose triples are the concatenation of every listed
/// formula's own triples (set union of their quoted graphs).
pub(crate) fn eval_conjunction(pattern: &Triple, bindings: &Binding) -> Option<Binding> {
    eval_functional(pattern, bindings, |pattern, bindings, row| {
        let members = subject_list_members(&pattern.s, bindings, row)?;
        let mut combined = Vec::new();
        for m in members {
            let triples = VarOrTerm::formula_triples(m)?;
            for t in triples {
                if !combined.contains(&t) {
                    combined.push(t);
                }
            }
        }
        Some(VarOrTerm::new_formula(combined).to_encoded())
    })
}
