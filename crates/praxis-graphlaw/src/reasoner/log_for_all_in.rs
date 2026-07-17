use super::Reasoner;
use crate::{
    Binding, BodyLiteral, Encoder, QueryEngine, Rule, SimpleQueryEngine, Triple, TripleIndex,
    VarOrTerm,
};

impl Reasoner {
    /// The IRI of the `log:forAllIn` built-in predicate.
    ///
    /// Chosen semantics (documented here since `log:forAllIn` isn't part of
    /// the core EYE/cwm builtin set with a single canonical definition):
    /// `(list) log:forAllIn { pattern }` succeeds for a row iff, for
    /// *every* member `m` of `list`, substituting `m` for the first
    /// variable that appears in `{ pattern }`'s triples (in traversal
    /// order -- the formula's implicit "loop variable") and querying the
    /// result against the live store yields at least one solution. An
    /// empty list is vacuously true. A pattern with no free variable at
    /// all is evaluated once (ignoring list membership) and its truth
    /// value applies uniformly to every member.
    const LOG_FOR_ALL_IN: &'static str = "<http://www.w3.org/2000/10/swap/log#forAllIn>";

    pub(crate) fn find_log_for_all_in_literal(rule: &Rule) -> Option<usize> {
        rule.body.iter().position(|lit| {
            !lit.negated
                && lit.pattern.p.is_term()
                && Encoder::decode(&lit.pattern.p.to_encoded()).as_deref()
                    == Some(Self::LOG_FOR_ALL_IN)
        })
    }

    /// The first variable's encoded id found (by DFS over s/p/o) across
    /// `triples`, or `None` if the formula is fully ground.
    fn first_formula_variable(triples: &[Triple]) -> Option<usize> {
        for t in triples {
            for term in [&t.s, &t.p, &t.o] {
                if term.is_var() {
                    return Some(term.to_encoded());
                }
            }
        }
        None
    }

    pub(crate) fn process_log_for_all_in_rule(
        rule: &Rule,
        for_all_idx: usize,
        triple_index: &TripleIndex,
        prev_limit: Option<usize>,
        current_limit: usize,
    ) -> Vec<Triple> {
        let mut results = Vec::new();
        let lit = &rule.body[for_all_idx];
        let list_template = lit.pattern.s.to_encoded();
        let formula_id_template = lit.pattern.o.to_encoded();

        let other_body: Vec<BodyLiteral> = rule
            .body
            .iter()
            .enumerate()
            .filter(|(i, _)| *i != for_all_idx)
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

            let Some(list_id) = (if row_binding.get(&list_template).is_some() {
                row_binding
                    .get(&list_template)
                    .and_then(|v| v.first())
                    .copied()
            } else {
                Some(list_template)
            }) else {
                continue;
            };
            let Some(members) = VarOrTerm::list_members(list_id) else {
                continue;
            };
            let Some(formula_triples) = VarOrTerm::formula_triples(formula_id_template) else {
                continue;
            };

            let loop_var = Self::first_formula_variable(&formula_triples);
            let all_hold = if members.is_empty() {
                true
            } else {
                members.iter().all(|&m| {
                    let mut member_binding = row_binding.clone();
                    if let Some(lv) = loop_var {
                        member_binding.add(&lv, m);
                    }
                    Self::eval_embedded_formula_against_store(
                        formula_id_template,
                        &member_binding,
                        triple_index,
                    )
                    .is_some()
                })
            };

            if all_hold {
                let head_t = Self::substitute_single_row(&rule.head, &row_binding);
                if Self::is_ground(&head_t) && !results.contains(&head_t) {
                    results.push(head_t);
                }
            }
        }

        results
    }
}
