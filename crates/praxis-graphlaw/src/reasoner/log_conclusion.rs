use super::Reasoner;
use crate::{
    Binding, BodyLiteral, Encoder, QueryEngine, Rule, SimpleQueryEngine, Triple, TripleIndex,
    VarOrTerm,
};

impl Reasoner {
    /// The IRI of the `log:conclusion` built-in predicate.
    ///
    /// `{formula} log:conclusion ?result` binds `?result` to a fresh quoted
    /// graph holding the closure of `formula`'s own triples: any triple in
    /// `formula` whose predicate is `log:implies` is treated as a nested
    /// rule (`antecedentFormula log:implies consequentFormula`, both
    /// themselves quoted graphs); the closure repeatedly adds `consequent`'s
    /// triples whenever every one of `antecedent`'s (ground) triples is
    /// already present, until a fixpoint is reached. This is a *ground*
    /// (ISO-free, ground-subset match) closure -- no unification against
    /// variables inside the nested antecedent/consequent -- which is enough
    /// for the common "quoted ruleset" idiom while staying simple and total.
    /// Purely a function of the formula's own content, so (unlike
    /// `log:notIncludes`/`log:includes`/`log:forAllIn`) it never needs to
    /// consult the live `TripleIndex` itself; it's still dispatched from the
    /// reasoner (rather than `builtins::evaluate`) for symmetry with the
    /// other quoted-formula builtins and so `other_body` literals in the
    /// same rule can still be resolved against the store.
    const LOG_CONCLUSION: &'static str = "<http://www.w3.org/2000/10/swap/log#conclusion>";
    const LOG_CONCLUSION_IMPLIES: &'static str = "<http://www.w3.org/2000/10/swap/log#implies>";

    pub(crate) fn find_log_conclusion_literal(rule: &Rule) -> Option<usize> {
        rule.body.iter().position(|lit| {
            !lit.negated
                && lit.pattern.p.is_term()
                && Encoder::decode(&lit.pattern.p.to_encoded()).as_deref()
                    == Some(Self::LOG_CONCLUSION)
        })
    }

    /// Compute the ground closure of `formula_id`'s triples per the doc
    /// comment above, returning the id of a freshly interned formula.
    fn closure_of_formula(formula_id: usize) -> Option<usize> {
        let triples = VarOrTerm::formula_triples(formula_id)?;
        let implies_iri_id = Encoder::add(Self::LOG_CONCLUSION_IMPLIES.to_string());

        let mut facts: Vec<Triple> = Vec::new();
        let mut nested_rules: Vec<(Vec<Triple>, Vec<Triple>)> = Vec::new();
        for t in &triples {
            if t.p.is_term() && t.p.to_encoded() == implies_iri_id {
                if let (Some(ante), Some(cons)) = (
                    VarOrTerm::formula_triples(t.s.to_encoded()),
                    VarOrTerm::formula_triples(t.o.to_encoded()),
                ) {
                    nested_rules.push((ante, cons));
                    continue;
                }
            }
            facts.push(t.clone());
        }

        let mut changed = true;
        while changed {
            changed = false;
            for (ante, cons) in &nested_rules {
                if ante.iter().all(|a| facts.contains(a)) {
                    for c in cons {
                        if !facts.contains(c) {
                            facts.push(c.clone());
                            changed = true;
                        }
                    }
                }
            }
        }

        Some(VarOrTerm::new_formula(facts).to_encoded())
    }

    /// Like `log_collect_all_in.rs`'s `process_log_collect_all_in_rule`:
    /// body literals *before* `log:conclusion` (the "pre" body) are queried
    /// first to establish any outer bindings the formula template might
    /// reference; body literals *after* it (the "post" body, e.g. `?result
    /// log:n3String ?s`) are evaluated once the freshly-computed `?result`
    /// binding exists, via `eval_post_literals` -- they can't be
    /// pre-computed by an ordinary whole-body query since `?result` doesn't
    /// exist as data until this step produces it.
    pub(crate) fn process_log_conclusion_rule(
        rule: &Rule, idx: usize, triple_index: &TripleIndex, prev_limit: Option<usize>,
        current_limit: usize,
    ) -> Vec<Triple> {
        let mut results = Vec::new();
        let lit = &rule.body[idx];
        let formula_template = lit.pattern.s.to_encoded();
        let result_var = lit.pattern.o.clone();

        let pre_body: Vec<BodyLiteral> = rule.body[..idx].to_vec();
        let post_body: Vec<BodyLiteral> = rule.body[idx + 1..].to_vec();

        let outer_bindings = if pre_body.is_empty() {
            Some(Binding::new())
        } else if let Some(prev_limit) = prev_limit {
            <SimpleQueryEngine as QueryEngine>::query_semi_naive(
                triple_index,
                &pre_body,
                prev_limit,
                current_limit,
            )
        } else {
            SimpleQueryEngine::query(triple_index, &pre_body, None)
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

            let Some(concluded) = Self::closure_of_formula(formula_template) else {
                continue;
            };

            let mut merged = row_binding.clone();
            if result_var.is_var() {
                merged.add(&result_var.to_encoded(), concluded);
            }

            let Some(final_binding) = Self::eval_post_literals(&post_body, merged, triple_index)
            else {
                continue;
            };
            let num_final_rows = if final_binding.is_empty() {
                1
            } else {
                final_binding.len()
            };
            for final_row in 0..num_final_rows {
                let mut single = Binding::new();
                for (&k, v) in final_binding.iter() {
                    if let Some(&val) = v.get(final_row) {
                        single.add(&k, val);
                    }
                }
                let head_t = Self::substitute_single_row(&rule.head, &single);
                if Self::is_ground(&head_t) && !results.contains(&head_t) {
                    results.push(head_t);
                }
            }
        }

        results
    }
}
