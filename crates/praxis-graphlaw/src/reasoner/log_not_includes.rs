use super::Reasoner;
use crate::{
    Binding, BodyLiteral, Encoder, QueryEngine, Rule, SimpleQueryEngine, Triple, TripleIndex,
    VarOrTerm,
};

impl Reasoner {
    /// The IRI of the `log:notIncludes` built-in predicate.
    const LOG_NOT_INCLUDES: &'static str = "<http://www.w3.org/2000/10/swap/log#notIncludes>";

    /// Find the (first) non-negated body literal of `rule` whose predicate is
    /// `log:notIncludes`.
    pub(crate) fn find_log_not_includes_literal(rule: &Rule) -> Option<usize> {
        rule.body.iter().position(|lit| {
            !lit.negated
                && lit.pattern.p.is_term()
                && Encoder::decode(&lit.pattern.p.to_encoded()).as_deref()
                    == Some(Self::LOG_NOT_INCLUDES)
        })
    }

    /// `log:notIncludes` -- SNAF (simple negation-as-failure) guard.
    ///
    /// The body literal `?Scope log:notIncludes { pattern }` succeeds for a
    /// given row of the rule's *other* body literals iff substituting that
    /// row's bindings into `{ pattern }` and querying the result against the
    /// current `triple_index` (via `eval_embedded_formula_against_store`)
    /// yields **zero** solutions. Rows for which it fails are dropped
    /// (filtered out), mirroring how negated body literals are handled in
    /// `SimpleQueryEngine::query`, except this needs the full `TripleIndex`
    /// (to run a genuine sub-query rather than a single ground lookup) so it
    /// is dispatched here rather than through the ordinary builtins module.
    pub(crate) fn process_log_not_includes_rule(
        rule: &Rule,
        not_includes_idx: usize,
        triple_index: &TripleIndex,
        prev_limit: Option<usize>,
        current_limit: usize,
    ) -> Vec<Triple> {
        let mut results = Vec::new();
        let lit = &rule.body[not_includes_idx];
        let formula_id_template = lit.pattern.o.to_encoded();

        let other_body: Vec<BodyLiteral> = rule
            .body
            .iter()
            .enumerate()
            .filter(|(i, _)| *i != not_includes_idx)
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

            if VarOrTerm::formula_triples(formula_id_template).is_none() {
                continue;
            }
            let guard_result = Self::eval_embedded_formula_against_store(
                formula_id_template,
                &row_binding,
                triple_index,
            );

            if guard_result.is_none() {
                // Zero solutions for the (row-instantiated) guard formula:
                // the SNAF guard passes, so this row survives.
                let head_t = Self::substitute_single_row(&rule.head, &row_binding);
                if Self::is_ground(&head_t) && !results.contains(&head_t) {
                    results.push(head_t);
                }
            }
        }

        results
    }
}
