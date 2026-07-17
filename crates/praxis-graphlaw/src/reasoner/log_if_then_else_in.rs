use super::Reasoner;
use crate::{
    Binding, BodyLiteral, Encoder, QueryEngine, Rule, SimpleQueryEngine, Triple, TripleIndex,
    VarOrTerm,
};

impl Reasoner {
    /// The IRI of the `log:ifThenElseIn` built-in predicate.
    ///
    /// Semantics used here: `(condFormula thenTerm elseTerm) log:ifThenElseIn
    /// ?result` -- the subject is a 3-element list; `?result` (the object)
    /// is bound to `thenTerm` if `condFormula` (a quoted graph) holds
    /// against the live store (at least one solution), otherwise to
    /// `elseTerm`. Needs the live `TripleIndex` to test `condFormula`, so
    /// (like `log:notIncludes`/`log:includes`/`log:forAllIn`) this is
    /// reasoner-level rather than a procedural `builtins::evaluate` case.
    const LOG_IF_THEN_ELSE_IN: &'static str = "<http://www.w3.org/2000/10/swap/log#ifThenElseIn>";

    pub(crate) fn find_log_if_then_else_in_literal(rule: &Rule) -> Option<usize> {
        rule.body.iter().position(|lit| {
            !lit.negated
                && lit.pattern.p.is_term()
                && Encoder::decode(&lit.pattern.p.to_encoded()).as_deref()
                    == Some(Self::LOG_IF_THEN_ELSE_IN)
        })
    }

    pub(crate) fn process_log_if_then_else_in_rule(
        rule: &Rule,
        idx: usize,
        triple_index: &TripleIndex,
        prev_limit: Option<usize>,
        current_limit: usize,
    ) -> Vec<Triple> {
        let mut results = Vec::new();
        let lit = &rule.body[idx];
        let list_template = lit.pattern.s.to_encoded();
        let result_var = lit.pattern.o.clone();

        let other_body: Vec<BodyLiteral> = rule
            .body
            .iter()
            .enumerate()
            .filter(|(i, _)| *i != idx)
            .map(|(_, l)| l.clone())
            .collect();

        let outer_bindings = if other_body.is_empty() {
            Some(Binding::new())
        } else if let Some(prev_limit) = prev_limit {
            <SimpleQueryEngine as QueryEngine>::query_semi_naive(
                triple_index,
                &other_body,
                prev_limit,
                current_limit,
            )
        } else {
            SimpleQueryEngine::query(triple_index, &other_body, None)
        };
        let Some(outer_bindings) = outer_bindings else {
            return results;
        };
        let num_rows = if outer_bindings.is_empty() {
            1
        } else {
            outer_bindings.len()
        };

        for row in 0..num_rows {
            let mut row_binding = Binding::new();
            for (&k, v) in outer_bindings.iter() {
                if let Some(&val) = v.get(row) {
                    row_binding.add(&k, val);
                }
            }

            let Some(members) = VarOrTerm::list_members(list_template) else {
                continue;
            };
            if members.len() != 3 {
                continue;
            }
            let (cond_formula, then_val, else_val) = (members[0], members[1], members[2]);
            if VarOrTerm::formula_triples(cond_formula).is_none() {
                continue;
            }
            let cond_holds =
                Self::eval_embedded_formula_against_store(cond_formula, &row_binding, triple_index)
                    .is_some();
            let chosen = if cond_holds { then_val } else { else_val };

            let mut row_binding_with_result = row_binding.clone();
            if result_var.is_var() {
                row_binding_with_result.add(&result_var.to_encoded(), chosen);
            }

            let head_t = Self::substitute_single_row(&rule.head, &row_binding_with_result);
            if Self::is_ground(&head_t) && !results.contains(&head_t) {
                results.push(head_t);
            }
        }

        results
    }
}
