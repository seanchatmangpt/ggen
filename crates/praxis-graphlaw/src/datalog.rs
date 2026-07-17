use crate::encoding::Encoder;
use crate::triples::{Aggregate, Rule, Triple, VarOrTerm};
use std::collections::{HashMap, HashSet};

/// Recursively collect the variables mentioned inside a quoted-graph
/// ("formula") term, if `term` names one. Used so that a variable which only
/// appears *nested inside* a `log:implies` consequent formula (e.g.
/// `?formula log:implies { ?citizen a :TaxPayer }`) still counts as bound by
/// that positive body literal for the safety check below -- the dynamic
/// rule-reification mechanism in `Reasoner::materialize` is what actually
/// binds it at runtime (by matching the antecedent formula against the data),
/// which the static safety check here has no way to simulate, so we simply
/// trust that any variable reachable from a positive-literal's operands
/// (directly or via a nested formula) is a legitimate binding site.
fn collect_formula_vars(term: &VarOrTerm, out: &mut HashSet<usize>) {
    if !term.is_term() {
        return;
    }
    if let Some(triples) = VarOrTerm::formula_triples(term.to_encoded()) {
        for t in &triples {
            if t.s.is_var() {
                out.insert(t.s.to_encoded());
            } else {
                collect_formula_vars(&t.s, out);
            }
            if t.p.is_var() {
                out.insert(t.p.to_encoded());
            }
            if t.o.is_var() {
                out.insert(t.o.to_encoded());
            } else {
                collect_formula_vars(&t.o, out);
            }
        }
    }
}

/// Identifies the "relation" a triple pattern talks about.
/// For rdf:type patterns we use the class (object) as the relation identifier,
/// since typing statements group by class. For all other patterns we use the
/// predicate. This is the standard Datalog-over-RDF stratification heuristic.
fn relation_of(pattern: &Triple) -> usize {
    let p_decoded = Encoder::decode(&pattern.p.to_encoded()).unwrap_or_default();
    let p_lower = p_decoded.to_lowercase();
    let is_type_pred = p_lower.contains("type") || p_lower == "a" || p_lower == "<a>";
    if is_type_pred && pattern.o.is_term() {
        pattern.o.to_encoded()
    } else {
        pattern.p.to_encoded()
    }
}

/// Validate and stratify a set of Datalog rules.
///
/// Returns `Ok(strata)` where `strata[i]` is the stratum index for `rules[i]`,
/// or `Err(msg)` if the rules are unsafe or contain a cycle through negation /
/// aggregation that prevents stratification.
///
/// Safety rules enforced:
/// - Every variable in a rule head must appear in at least one *positive* body literal.
/// - Every variable in a negated body literal must share at least one variable with
///   some positive body literal (range-restriction).
pub fn validate_rules(
    rules: &[Rule],
    aggregates: &HashMap<Rule, Aggregate>,
) -> Result<Vec<usize>, String> {
    // An empty ruleset is trivially stratifiable (zero predicates, zero strata) -- return
    // before the Bellman-Ford propagation below, which always executes its loop body at
    // least once (`changed` starts `true`), incrementing `iteration` to 1 even though there
    // are zero edges to relax. With `num_predicates == 0` that spuriously satisfies the
    // post-loop cycle check (`iteration > num_predicates` is `1 > 0`), reporting a
    // "not stratifiable" cycle for input that has no rules at all, hence no cycle. This
    // guard is specific to the `num_predicates == 0` boundary; the loop's iteration bound is
    // otherwise correct (`num_predicates` predicates need at most `num_predicates` relaxation
    // passes to converge for any legitimately stratifiable, i.e. acyclic, dependency graph).
    if rules.is_empty() {
        return Ok(Vec::new());
    }

    // -----------------------------------------------------------------------
    // 1. Safety check
    // -----------------------------------------------------------------------
    for (idx, rule) in rules.iter().enumerate() {
        // Collect variables bound by positive body literals
        let mut positive_vars: HashSet<usize> = HashSet::new();
        for lit in &rule.body {
            if !lit.negated {
                if lit.pattern.s.is_var() {
                    positive_vars.insert(lit.pattern.s.to_encoded());
                } else {
                    collect_formula_vars(&lit.pattern.s, &mut positive_vars);
                }
                if lit.pattern.p.is_var() {
                    positive_vars.insert(lit.pattern.p.to_encoded());
                }
                if lit.pattern.o.is_var() {
                    positive_vars.insert(lit.pattern.o.to_encoded());
                } else {
                    collect_formula_vars(&lit.pattern.o, &mut positive_vars);
                }
            }
        }

        // If there is an aggregate, the target variable is considered bound by the engine
        if let Some(agg) = aggregates.get(rule) {
            let target_var_encoded = VarOrTerm::convert(agg.target_var.clone()).to_encoded();
            positive_vars.insert(target_var_encoded);
        }

        // Check that every head variable is bound by a positive body literal
        let mut head_vars: HashSet<usize> = HashSet::new();
        if rule.head.s.is_var() {
            head_vars.insert(rule.head.s.to_encoded());
        }
        if rule.head.p.is_var() {
            head_vars.insert(rule.head.p.to_encoded());
        }
        if rule.head.o.is_var() {
            head_vars.insert(rule.head.o.to_encoded());
        }

        for v in &head_vars {
            if !positive_vars.contains(v) {
                let var_name = Encoder::decode(v).unwrap_or_else(|| v.to_string());
                return Err(format!(
                    "Rule {} is unsafe: variable {} in head is not bound by any positive body literal",
                    idx, var_name
                ));
            }
        }

        // Check that every negated literal shares at least one variable with the positive body
        for lit in &rule.body {
            if lit.negated {
                let mut neg_vars: HashSet<usize> = HashSet::new();
                if lit.pattern.s.is_var() {
                    neg_vars.insert(lit.pattern.s.to_encoded());
                }
                if lit.pattern.p.is_var() {
                    neg_vars.insert(lit.pattern.p.to_encoded());
                }
                if lit.pattern.o.is_var() {
                    neg_vars.insert(lit.pattern.o.to_encoded());
                }

                if !neg_vars.is_empty() {
                    let has_shared = neg_vars.iter().any(|v| positive_vars.contains(v));
                    if !has_shared {
                        let var_names: Vec<String> = neg_vars
                            .iter()
                            .map(|v| Encoder::decode(v).unwrap_or_else(|| v.to_string()))
                            .collect();
                        return Err(format!(
                            "Rule {} is unsafe: negated literal has variables {:?} but none are bound by a positive body literal",
                            idx, var_names
                        ));
                    }
                }
            }
        }
    }

    // -----------------------------------------------------------------------
    // 2. Stratification via Bellman-Ford relaxation
    // -----------------------------------------------------------------------
    let mut predicates: HashSet<usize> = HashSet::new();
    let mut head_relations: Vec<usize> = Vec::with_capacity(rules.len());
    let mut body_relations: Vec<Vec<usize>> = Vec::with_capacity(rules.len());

    for rule in rules {
        let hr = relation_of(&rule.head);
        predicates.insert(hr);
        head_relations.push(hr);

        let brs: Vec<usize> = rule
            .body
            .iter()
            .map(|lit| {
                let br = relation_of(&lit.pattern);
                predicates.insert(br);
                br
            })
            .collect();
        body_relations.push(brs);
    }

    // `log:notIncludes` (a SNAF/negation-as-failure guard: `?scope
    // log:notIncludes { pattern }`) is itself a POSITIVE body literal whose
    // own predicate is `log:notIncludes` -- the *real* dependency this rule
    // has is on whatever relation `pattern` (the quoted-formula object)
    // talks about, and that dependency is negative (this rule's stratum
    // must be strictly greater than that relation's), exactly like an
    // ordinary `not { ... }` literal. Without this, stratification is blind
    // to the guard entirely (its analysis only ever inspects a literal's
    // own top-level predicate), so a rule guarded by `log:notIncludes` on a
    // predicate some sibling rule of the same stratum derives can run
    // before that sibling ever fires -- the real EYE `nixon_diamond` corpus
    // case (Rule 1's `log:notIncludes { ?x a ex:NonPacifist }` guard must
    // be stratified strictly after Rule 2, which derives ex:NonPacifist).
    let notincludes_inner_relations: Vec<Vec<usize>> = rules
        .iter()
        .map(|rule| {
            let mut inner = Vec::new();
            for lit in &rule.body {
                if lit.negated || !lit.pattern.p.is_term() {
                    continue;
                }
                const LOG_NOT_INCLUDES: &str = "<http://www.w3.org/2000/10/swap/log#notIncludes>";
                if Encoder::decode(&lit.pattern.p.to_encoded()).as_deref() != Some(LOG_NOT_INCLUDES)
                {
                    continue;
                }
                if let Some(triples) = VarOrTerm::formula_triples(lit.pattern.o.to_encoded()) {
                    for t in &triples {
                        let r = relation_of(t);
                        predicates.insert(r);
                        inner.push(r);
                    }
                }
            }
            inner
        })
        .collect();

    let predicate_list: Vec<usize> = predicates.into_iter().collect();
    let num_predicates = predicate_list.len();

    let pred_to_idx: HashMap<usize, usize> = predicate_list
        .iter()
        .enumerate()
        .map(|(i, &p)| (p, i))
        .collect();

    // Build dependency edges: (body_relation_idx, head_relation_idx, is_negative)
    // Negative dependency means stratum[head] must be strictly greater than stratum[body].
    let mut edges: Vec<(usize, usize, bool)> = Vec::new();
    for (rule_idx, rule) in rules.iter().enumerate() {
        let head_p = head_relations[rule_idx];
        let head_idx = match pred_to_idx.get(&head_p) {
            Some(&idx) => idx,
            None => {
                return Err(format!(
                    "Internal stratification error: predicate {} not found",
                    Encoder::decode(&head_p).unwrap_or_else(|| head_p.to_string())
                ))
            }
        };
        let has_agg = aggregates.contains_key(rule);

        for (lit_idx, lit) in rule.body.iter().enumerate() {
            let body_p = body_relations[rule_idx][lit_idx];
            let body_idx = match pred_to_idx.get(&body_p) {
                Some(&idx) => idx,
                None => {
                    return Err(format!(
                        "Internal stratification error: body predicate {} not found",
                        Encoder::decode(&body_p).unwrap_or_else(|| body_p.to_string())
                    ))
                }
            };
            edges.push((body_idx, head_idx, lit.negated || has_agg));
        }

        for &inner_r in &notincludes_inner_relations[rule_idx] {
            let inner_idx = pred_to_idx[&inner_r];
            edges.push((inner_idx, head_idx, true));
        }
    }

    // Bellman-Ford propagation:
    //   positive dep → stratum[head] >= stratum[body]
    //   negative dep → stratum[head] >= stratum[body] + 1
    let mut stratum = vec![0usize; num_predicates];
    let mut changed = true;
    let mut iteration = 0usize;

    while changed && iteration <= num_predicates {
        changed = false;
        iteration += 1;
        for &(u, v, is_neg) in &edges {
            let required = if is_neg { stratum[u] + 1 } else { stratum[u] };
            if stratum[v] < required {
                stratum[v] = required;
                changed = true;
            }
        }
    }

    if iteration > num_predicates {
        return Err(
            "Ruleset is not stratifiable: cycle containing negation or aggregation detected"
                .to_string(),
        );
    }

    // Map each rule to the stratum of its head relation
    let rule_strata: Vec<usize> = rules
        .iter()
        .map(|rule| {
            let hr = relation_of(&rule.head);
            pred_to_idx.get(&hr).map(|&idx| stratum[idx]).unwrap_or(0)
        })
        .collect();

    Ok(rule_strata)
}
