use super::Reasoner;
use crate::{
    Binding, BodyLiteral, Encoder, QueryEngine, Rule, SimpleQueryEngine, Triple, TripleIndex,
    VarOrTerm,
};

impl Reasoner {
    /// The IRI of the `log:collectAllIn` built-in predicate.
    const LOG_COLLECT_ALL_IN: &'static str = "<http://www.w3.org/2000/10/swap/log#collectAllIn>";

    /// Find the (first) non-negated body literal of `rule` whose predicate is
    /// `log:collectAllIn`.
    pub(crate) fn find_log_collect_all_in_literal(rule: &Rule) -> Option<usize> {
        rule.body.iter().position(|lit| {
            !lit.negated
                && lit.pattern.p.is_term()
                && Encoder::decode(&lit.pattern.p.to_encoded()).as_deref()
                    == Some(Self::LOG_COLLECT_ALL_IN)
        })
    }

    /// `log:collectAllIn` -- collect every distinct binding of a variable
    /// across all solutions of a quoted-graph formula into an RDF list.
    ///
    /// The body literal has the shape `(?Var {quoted-graph} ?List) log:collectAllIn ?Scope`:
    /// its *subject* is a 3-element list term `(?Var {formula} ?List)` (the
    /// scope/object is simply ignored -- this engine only ever has one,
    /// current, triple store to collect against). For each row produced by
    /// evaluating the rule's *other* body literals (the "outer" bindings,
    /// e.g. binding `?Subject` before the quoted formula references it),
    /// this substitutes those outer bindings into the formula's own triples,
    /// runs them as a sub-query via `eval_embedded_formula_against_store`,
    /// dedups the resulting `?Var` column (preserving first-seen order) into
    /// an RDF list, and binds that list to `?List`. Any body literals that
    /// come *after* the `log:collectAllIn` literal (e.g. `?List math:sum
    /// ?Count`, which depends on the freshly-built list) are then evaluated
    /// against the merged row using the same builtin dispatch
    /// `SimpleQueryEngine::query` uses internally, since they can't be
    /// pre-computed by an ordinary whole-body query (the list doesn't exist
    /// as data until this step produces it).
    pub(crate) fn process_log_collect_all_in_rule(
        rule: &Rule, collect_idx: usize, triple_index: &TripleIndex, prev_limit: Option<usize>,
        current_limit: usize,
    ) -> Vec<Triple> {
        let mut results = Vec::new();
        let collect_lit = &rule.body[collect_idx];

        let Some(members) = VarOrTerm::list_members(collect_lit.pattern.s.to_encoded()) else {
            return results;
        };
        if members.len() != 3 {
            return results;
        }
        let collect_var_id = members[0];
        let formula_id_template = members[1];
        let target_id = members[2];

        let pre_body: Vec<BodyLiteral> = rule.body[..collect_idx].to_vec();
        let post_body: Vec<BodyLiteral> = rule.body[collect_idx + 1..].to_vec();

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
        let num_outer_rows = if outer_bindings.is_empty() {
            1
        } else {
            outer_bindings.len()
        };

        for row in 0..num_outer_rows {
            let mut row_binding = Binding::new();
            for (&k, v) in outer_bindings.iter() {
                if let Some(&val) = v.get(row) {
                    row_binding.add(&k, val);
                }
            }

            // The quoted formula may itself be a variable (rare) or, as in
            // every known corpus case, written directly in the rule -- if
            // `formula_id_template` happens to be bound as a variable in
            // this row, use that binding; otherwise it already names the
            // formula directly.
            let formula_id = row_binding
                .get(&formula_id_template)
                .and_then(|v| v.first())
                .copied()
                .unwrap_or(formula_id_template);

            if VarOrTerm::formula_triples(formula_id).is_none() {
                continue;
            }
            let ante_bindings =
                Self::eval_embedded_formula_against_store(formula_id, &row_binding, triple_index);

            // Dedup the collected variable's column across all antecedent
            // solutions, preserving first-seen order (deterministic). If
            // `collect_var_id` never appears as a binding key, the template
            // isn't actually a variable (e.g. EYE's `dog` corpus case uses
            // the ground template `1`, purely to *count* solutions via a
            // following `math:sum` -- see the `1 { ... } ?List` shape): in
            // that case every antecedent solution contributes one (un-deduped)
            // copy of the ground value, since the point is the count of
            // solutions, not their distinct values.
            let mut collected: Vec<usize> = Vec::new();
            if let Some(ante_bindings) = &ante_bindings {
                if let Some(vals) = ante_bindings.get(&collect_var_id) {
                    for &v in vals {
                        if !collected.contains(&v) {
                            collected.push(v);
                        }
                    }
                } else {
                    let num_ante_rows = if ante_bindings.is_empty() {
                        1
                    } else {
                        ante_bindings.len()
                    };
                    for _ in 0..num_ante_rows {
                        collected.push(collect_var_id);
                    }
                }
            }

            let list_members: Vec<VarOrTerm> = collected
                .into_iter()
                .map(VarOrTerm::new_encoded_term)
                .collect();
            let list_id = VarOrTerm::new_list(list_members).to_encoded();

            let mut merged = row_binding.clone();
            merged.add(&target_id, list_id);

            // Evaluate any remaining body literals (typically builtins like
            // math:sum/list:length/math:greaterThan operating on the new
            // list/its derived values) against the merged single row.
            let Some(final_binding) = Self::eval_post_literals(&post_body, merged, triple_index)
            else {
                continue;
            };

            // A post-literal generator (e.g. `list:in`, one row per list
            // member) expands `final_binding` back out to multiple rows --
            // every row must produce its own head substitution, not just
            // row 0. Using `substitute_single_row` (which always reads
            // column index 0) silently dropped every row past the first
            // here, e.g. deriving a fact for only the first of N members a
            // `list:in` literal produced after a `log:collectAllIn` step.
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
