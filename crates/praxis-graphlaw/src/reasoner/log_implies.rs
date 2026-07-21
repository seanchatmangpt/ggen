use super::Reasoner;
use crate::{
    Binding, BodyLiteral, Encoder, QueryEngine, Rule, SimpleQueryEngine, Triple, TripleIndex,
    VarOrTerm,
};

impl Reasoner {
    /// The IRI of the `log:implies` built-in predicate.
    const LOG_IMPLIES: &'static str = "<http://www.w3.org/2000/10/swap/log#implies>";

    /// Find *every* non-negated body literal of `rule` whose predicate is
    /// `log:implies`, in body order. Each such literal's subject is expected
    /// to be bound (at evaluation time) to a formula term, and its object is
    /// a formula written directly in the rule (the consequent template).
    ///
    /// A rule body may contain more than one `log:implies` literal -- this is
    /// treated as N independent reified sub-rules sharing the rest of the
    /// body as their common "outer" bindings (see `process_log_implies_rule`
    /// for how each is evaluated). This is a deliberate semantic choice, not
    /// an omission: N3/EYE has no notion of a body literal "consuming" or
    /// sequencing other body literals, so two `log:implies` literals in one
    /// body are just two ordinary triple patterns that both happen to use
    /// the `log:implies` predicate -- each is independently satisfied (or
    /// not) against the shared outer bindings, exactly as any other pair of
    /// body literals would be conjoined. There is no ambiguity to reject.
    pub(crate) fn find_log_implies_literals(rule: &Rule) -> Vec<usize> {
        rule.body
            .iter()
            .enumerate()
            .filter(|(_, lit)| {
                !lit.negated
                    && lit.pattern.p.is_term()
                    && Encoder::decode(&lit.pattern.p.to_encoded()).as_deref()
                        == Some(Self::LOG_IMPLIES)
            })
            .map(|(i, _)| i)
            .collect()
    }

    /// Dynamic rule reification for `log:implies`.
    ///
    /// Given a rule body containing `?formula log:implies { consequent }`
    /// (alongside other, ordinary body literals), this:
    ///  1. Runs the *other* body literals as a normal query to bind
    ///     `?formula` (and anything else in scope) -- call this the "outer"
    ///     bindings, one row per match.
    ///  2. For each outer row, resolves `?formula` to a concrete formula
    ///     term and looks up the triples it was built from (its
    ///     "antecedent") via `VarOrTerm::formula_triples`.
    ///  3. Matches the antecedent's triples against the *current* data via
    ///     the ordinary query engine -- exactly like testing whether a
    ///     nested sub-query holds -- producing zero or more "antecedent"
    ///     bindings for any variables internal to the antecedent formula.
    ///  4. For each (outer row, antecedent row) pair, merges the two into a
    ///     single-row binding and substitutes it into both the consequent
    ///     formula's own triples *and* the rule's declared head, asserting
    ///     whichever come out fully ground. In a well-formed rule (as in
    ///     this crate's test fixtures) the two are the same shape and this
    ///     produces the same triple twice, which the caller dedupes; keeping
    ///     both substitutions handles the general case where they differ.
    ///
    /// Variables are matched across antecedent/consequent/outer scopes purely
    /// by *name* (this engine uses one flat, process-wide variable
    /// namespace -- there is no `@forSome`/`@forAll`-style scoping), so e.g.
    /// reusing `?citizen` inside both the quoted antecedent and the rule's
    /// own head is exactly how a variable "threads through" the implication.
    /// Processes *all* `log:implies` literals in `rule`'s body (see
    /// `find_log_implies_literals`), each treated as its own independent
    /// antecedent/consequent pair conjoined with the shared "outer" bindings
    /// from the rest of the body (i.e. every body literal that is not itself
    /// one of the `log:implies` literals). Results from every literal are
    /// pooled together and deduped.
    pub(crate) fn process_log_implies_rule(
        rule: &Rule, implies_indices: &[usize], triple_index: &TripleIndex,
        prev_limit: Option<usize>, current_limit: usize,
    ) -> Vec<Triple> {
        let mut results = Vec::new();

        let regular_body: Vec<BodyLiteral> = rule
            .body
            .iter()
            .enumerate()
            .filter(|(i, _)| !implies_indices.contains(i))
            .map(|(_, lit)| lit.clone())
            .collect();

        let outer_bindings = if regular_body.is_empty() {
            Some(Binding::new())
        } else if let Some(prev_limit) = prev_limit {
            <SimpleQueryEngine as QueryEngine>::query_semi_naive(
                triple_index,
                &regular_body,
                prev_limit,
                current_limit,
            )
        } else {
            SimpleQueryEngine::query(triple_index, &regular_body, None)
        };
        let Some(outer_bindings) = outer_bindings else {
            return results;
        };
        let num_outer_rows = if outer_bindings.is_empty() {
            1
        } else {
            outer_bindings.len()
        };

        for &implies_idx in implies_indices {
            let implies_lit = &rule.body[implies_idx];

            // The consequent is written directly in the rule (the log:implies
            // literal's object), so it never needs a bindings lookup.
            let consequent_id = implies_lit.pattern.o.to_encoded();
            let Some(consequent_triples) = VarOrTerm::formula_triples(consequent_id) else {
                continue;
            };

            for row in 0..num_outer_rows {
                let formula_id = if implies_lit.pattern.s.is_var() {
                    match outer_bindings
                        .get(&implies_lit.pattern.s.to_encoded())
                        .and_then(|v| v.get(row))
                    {
                        Some(&id) => id,
                        None => continue,
                    }
                } else {
                    implies_lit.pattern.s.to_encoded()
                };

                let Some(antecedent_triples) = VarOrTerm::formula_triples(formula_id) else {
                    continue;
                };
                if antecedent_triples.is_empty() {
                    continue;
                }
                let antecedent_body: Vec<BodyLiteral> = antecedent_triples
                    .into_iter()
                    .map(|pattern| BodyLiteral {
                        negated: false,
                        pattern,
                    })
                    .collect();

                let Some(antecedent_bindings) =
                    SimpleQueryEngine::query(triple_index, &antecedent_body, None)
                else {
                    continue;
                };
                let num_ante_rows = if antecedent_bindings.is_empty() {
                    1
                } else {
                    antecedent_bindings.len()
                };

                for a_row in 0..num_ante_rows {
                    let mut merged = Binding::new();
                    for (&k, v) in outer_bindings.iter() {
                        if let Some(&val) = v.get(row) {
                            merged.add(&k, val);
                        }
                    }
                    for (&k, v) in antecedent_bindings.iter() {
                        if let Some(&val) = v.get(a_row) {
                            merged.add(&k, val);
                        }
                    }

                    for consequent_pattern in &consequent_triples {
                        let t = Self::substitute_single_row(consequent_pattern, &merged);
                        if Self::is_ground(&t) && !results.contains(&t) {
                            results.push(t);
                        }
                    }
                    let head_t = Self::substitute_single_row(&rule.head, &merged);
                    if Self::is_ground(&head_t) && !results.contains(&head_t) {
                        results.push(head_t);
                    }
                }
            }
        }

        results
    }
}
