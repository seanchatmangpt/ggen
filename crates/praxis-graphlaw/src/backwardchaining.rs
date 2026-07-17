// Vendored research-lineage engine (RoXi lineage): API reshaping is out of scope;
// lints below are documented scoped allows, not silent drift.
#![allow(clippy::type_complexity)]

use crate::fastmap::{FxHashMap, FxHashSet};
use crate::queryengine::{QueryEngine, SimpleQueryEngine};
use crate::{Binding, BodyLiteral, Rule, RuleIndex, Triple, TripleIndex, TripleStore, VarOrTerm};
use log::{debug, warn};
use std::sync::Arc; // Use log crate when building application

pub struct BackwardChainer;

impl BackwardChainer {
    /// Prove a fully ground goal triple (e.g. `5 :moreInterestingThan 3`)
    /// against the rule set + facts, goal-directed: unify each candidate
    /// rule's head against the concrete goal (binding the rule's own
    /// variables to the goal's ground values, NOT the `find_subrules`
    /// var-to-var *renaming* protocol below, which assumes both sides are
    /// variables -- a genuinely different case), substitute that binding
    /// into the rule's body literals, and check the substituted body holds
    /// -- reusing `SimpleQueryEngine::query` for each literal so builtin
    /// dispatch (`math:greaterThan` etc.) and ordinary EDB fact lookups
    /// both work for free, and recursing into `prove` for any body literal
    /// whose predicate matches another rule's head (IDB), with a
    /// visited-goal cycle guard mirroring `eval_backward_inner`'s.
    ///
    /// This exists because closing the real EYE `backward_math` corpus case
    /// requires exactly this: EYE proves it goal-directed by seeding the
    /// concrete query `5 :moreInterestingThan 3` and working backward to
    /// check `5 math:greaterThan 3` -- something pure forward-chaining
    /// materialization can never do (there are no facts to iterate `?X`/`?Y`
    /// candidates over).
    pub fn prove(triple_index: &TripleIndex, rule_index: &RuleIndex, goal: &Triple) -> bool {
        let mut history = FxHashSet::default();
        Self::prove_inner(triple_index, rule_index, goal, &mut history)
    }

    fn prove_inner(
        triple_index: &TripleIndex,
        rule_index: &RuleIndex,
        goal: &Triple,
        history: &mut FxHashSet<Triple>,
    ) -> bool {
        if triple_index.contains(goal) {
            return true;
        }
        if !history.insert(goal.clone()) {
            return false;
        }

        let candidates: &[Arc<Rule>] = if goal.p.is_term() {
            rule_index
                .head_by_pred
                .get(&goal.p.to_encoded())
                .map(|v| v.as_slice())
                .unwrap_or(&[])
        } else {
            rule_index.rules.as_slice()
        };

        for rule in candidates {
            let Some(subst) = Self::unify_ground(&rule.head, goal) else {
                continue;
            };
            let substituted_body: Vec<BodyLiteral> = rule
                .body
                .iter()
                .map(|lit| BodyLiteral {
                    negated: lit.negated,
                    pattern: Self::substitute(&lit.pattern, &subst),
                })
                .collect();

            let mut body_ok = true;
            for lit in &substituted_body {
                if lit.negated {
                    // Negation: succeeds iff the (substituted) pattern does
                    // NOT hold -- checked the same way SimpleQueryEngine
                    // treats negated literals (a direct EDB miss), since a
                    // negated body literal is never itself something to
                    // prove goal-directed.
                    if triple_index.contains(&lit.pattern) {
                        body_ok = false;
                        break;
                    }
                    continue;
                }
                let is_idb = lit.pattern.p.is_term()
                    && rule_index
                        .head_by_pred
                        .contains_key(&lit.pattern.p.to_encoded());
                if Self::is_ground(&lit.pattern) && is_idb {
                    // Fully ground and another rule could derive it:
                    // recurse goal-directed rather than only checking EDB
                    // facts or builtin dispatch (neither of which apply to
                    // an IDB predicate).
                    if !Self::prove_inner(triple_index, rule_index, &lit.pattern, history) {
                        body_ok = false;
                        break;
                    }
                } else {
                    // Ordinary EDB lookup or an N3 builtin (math:/string:/
                    // list:/log:) -- SimpleQueryEngine::query already
                    // dispatches both.
                    match SimpleQueryEngine::query(triple_index, &vec![lit.clone()], None) {
                        Some(b) if !b.is_empty() || Self::is_ground(&lit.pattern) => {}
                        _ => {
                            body_ok = false;
                            break;
                        }
                    }
                }
            }

            if body_ok {
                history.remove(goal);
                return true;
            }
        }

        history.remove(goal);
        false
    }

    /// Unify a (possibly variable-containing) rule head against a fully
    /// ground goal triple, returning a `rule_var_id -> concrete_value_id`
    /// substitution map if every position is consistent (a rule variable
    /// used twice must bind to the same value both times; a rule constant
    /// must equal the goal's value at that position exactly).
    fn unify_ground(head: &Triple, goal: &Triple) -> Option<FxHashMap<usize, usize>> {
        let mut subst = FxHashMap::default();
        for (h, g) in [(&head.s, &goal.s), (&head.p, &goal.p), (&head.o, &goal.o)] {
            if h.is_var() {
                let var_id = h.to_encoded();
                let goal_val = g.to_encoded();
                match subst.get(&var_id) {
                    Some(&existing) if existing != goal_val => return None,
                    _ => {
                        subst.insert(var_id, goal_val);
                    }
                }
            } else if VarOrTerm::is_nonground_list_pattern(h.to_encoded()) {
                // A list-structured head position (e.g. Peano arithmetic's
                // `(?A (:s ?B))` head subject) needs the same structural
                // list unification `TripleIndex::query`'s `query_list_pattern`
                // fallback uses -- exact-id equality alone can never match a
                // rule's own pattern list against a differently-constructed
                // goal list, even a structurally compatible one.
                let mut list_bindings = Vec::new();
                if !VarOrTerm::unify_list_pattern(
                    h.to_encoded(),
                    g.to_encoded(),
                    &mut list_bindings,
                ) {
                    return None;
                }
                for (var_id, goal_val) in list_bindings {
                    match subst.get(&var_id) {
                        Some(&existing) if existing != goal_val => return None,
                        _ => {
                            subst.insert(var_id, goal_val);
                        }
                    }
                }
            } else if h.to_encoded() != g.to_encoded() {
                return None;
            }
        }
        Some(subst)
    }

    /// Substitute every variable in `pattern` per `subst`, leaving any
    /// variable absent from `subst` (not bound by the head unification) as
    /// itself -- `SimpleQueryEngine::query` still handles genuinely-unbound
    /// variables via its normal EDB/builtin matching.
    fn substitute(pattern: &Triple, subst: &FxHashMap<usize, usize>) -> Triple {
        // Recurses INSIDE list-term structures via `VarOrTerm::substitute_deep`
        // (same fix as `reasoner.rs`'s three substitution paths) -- Peano
        // arithmetic's body literals are themselves list-headed (e.g.
        // `(?A ?B) :add ?C`), so a shallow top-level-only substitution would
        // leave the list's internal variables unbound.
        let resolve = |var_id: usize| -> Option<usize> { subst.get(&var_id).copied() };
        let sub_term = |t: &VarOrTerm| VarOrTerm::substitute_deep(t, &resolve);
        Triple {
            s: sub_term(&pattern.s),
            p: sub_term(&pattern.p),
            o: sub_term(&pattern.o),
            g: pattern.g.as_ref().map(sub_term),
        }
    }

    /// A list-term wrapper is always `is_term()` even when its own members
    /// still contain unresolved variables (see `substitute`'s comment) --
    /// so groundness must recurse into list structure, not just check the
    /// top-level wrapper.
    fn is_ground(pattern: &Triple) -> bool {
        let ground_term = |t: &VarOrTerm| -> bool {
            t.is_term() && !VarOrTerm::is_nonground_list_pattern(t.to_encoded())
        };
        ground_term(&pattern.s) && ground_term(&pattern.p) && ground_term(&pattern.o)
    }

    pub fn eval_backward(
        triple_index: &TripleIndex,
        rule_index: &RuleIndex,
        rule_head: &Triple,
    ) -> Binding {
        let mut history = FxHashSet::default();
        Self::eval_backward_inner(triple_index, rule_index, rule_head, &mut history)
    }

    fn eval_backward_inner(
        triple_index: &TripleIndex,
        rule_index: &RuleIndex,
        rule_head: &Triple,
        history: &mut FxHashSet<Triple>,
    ) -> Binding {
        if !history.insert(rule_head.clone()) {
            return Binding::new();
        }
        let sub_rules: Vec<(Arc<Rule>, Vec<(usize, usize)>)> =
            Self::find_subrules(rule_index, rule_head);
        let mut all_bindings = Binding::new();
        for (sub_rule, var_subs) in sub_rules.into_iter() {
            debug!(
                "Backchaining rule: {:?}",
                TripleStore::decode_rule(&sub_rule)
            );
            // Initialize rule_bindings as "empty" — join identity.
            // For each body literal, we:
            //   1. Collect bindings from direct EDB lookup (triple_index.query)
            //   2. Collect bindings from recursive IDB derivation (eval_backward_inner)
            //   3. Union both via combine (bag union of result rows)
            //   4. Join the per-literal bindings into the running rule_bindings
            //      (natural join on shared variables across body literals)
            let mut rule_bindings = Binding::new();
            let mut rule_failed = false;
            for body_lit in &sub_rule.body {
                let rule_atom = &body_lit.pattern;
                debug!("Matching body: {:?}", TripleStore::decode_triple(rule_atom));

                // Collect bindings for this literal from both sources.
                let mut lit_bindings = Binding::new();

                // Direct EDB lookup.
                if let Some(result_bindings) = triple_index.query(rule_atom, None) {
                    debug!(
                        "   Found matching body (direct): {:?}",
                        TripleStore::decode_bindings(&result_bindings)
                    );
                    lit_bindings.combine(result_bindings);
                }

                // Recursive IDB derivation (skip for negated literals).
                if !body_lit.negated {
                    let recursive_bindings =
                        Self::eval_backward_inner(triple_index, rule_index, rule_atom, history);
                    if !recursive_bindings.is_empty() {
                        lit_bindings.combine(recursive_bindings);
                    }
                }

                // Join per-literal bindings with the running result.
                rule_bindings = rule_bindings.join(&lit_bindings);

                // Short-circuit: if neither direct nor recursive found anything,
                // this rule body cannot be satisfied.
                if lit_bindings.is_empty() {
                    rule_failed = true;
                    break;
                }
            }
            if rule_failed {
                continue;
            }
            // Rename rule-internal variables to query variables via var_subs.
            let renamed = rule_bindings.rename(var_subs);
            all_bindings.combine(renamed);
        }
        history.remove(rule_head);
        all_bindings
    }

    /// Find all rules whose head unifies with `rule_head`.
    /// Returns the matching rules paired with variable substitution pairs
    /// `(rule_var, query_var)` for renaming after evaluation.
    pub fn find_subrules(
        rules_index: &RuleIndex,
        rule_head: &Triple,
    ) -> Vec<(Arc<Rule>, Vec<(usize, usize)>)> {
        let candidates: &[Arc<Rule>] = if rule_head.p.is_term() {
            // Fast path: look up only rules whose head predicate matches
            rules_index
                .head_by_pred
                .get(&rule_head.p.to_encoded())
                .map(|v| v.as_slice())
                .unwrap_or(&[])
        } else {
            // Variable predicate: must check all rules
            rules_index.rules.as_slice()
        };
        let mut rule_matches = Vec::new();
        for rule in candidates.iter() {
            let head: &Triple = &rule.head;
            let mut var_names_subs: Vec<(usize, usize)> = Vec::new();
            if Self::eval_triple_element(&head.s, &rule_head.s, &mut var_names_subs)
                && Self::eval_triple_element(&head.p, &rule_head.p, &mut var_names_subs)
                && Self::eval_triple_element(&head.o, &rule_head.o, &mut var_names_subs)
            {
                rule_matches.push((rule.clone(), var_names_subs));
            }
        }
        rule_matches
    }

    fn eval_triple_element(
        left: &VarOrTerm,
        right: &VarOrTerm,
        var_names_sub: &mut Vec<(usize, usize)>,
    ) -> bool {
        if let (VarOrTerm::Var(left_name), VarOrTerm::Var(right_name)) = (left, right) {
            var_names_sub.push((left_name.name, right_name.name));
            true
        } else {
            left.eq(right)
        }
    }
}

impl BackwardChainer {
    /// Recursion depth limit for `solve`/`solve_inner`, guarding against
    /// non-terminating rule sets that the visited-goal cycle guard alone
    /// can't catch (e.g. rules that keep generating structurally distinct
    /// goals, so no goal ever repeats exactly).
    const MAX_SOLVE_DEPTH: usize = 200;

    /// Full SLD-style resolution: prove `goal_pattern` (which may contain
    /// variables anywhere, including nested inside list terms) against the
    /// rule set + facts and return every binding row that satisfies it.
    /// Generalizes `prove`/`prove_inner` (ground-goal-only) and
    /// `find_subrules` (which only handled variable-to-variable renaming,
    /// not variable-to-term binding or list-pattern unification) into a
    /// single goal-directed resolution path with proper chase-based
    /// unification (`unify_term`/`walk`), so a rule variable that gets
    /// aliased to a goal variable in one head position and later forced to
    /// a concrete term in another (e.g. Peano's nullary base case
    /// `{(?A 0) :add ?A} <= true.` applied to a goal whose second
    /// component is already bound) correctly propagates that term back to
    /// the goal variable.
    ///
    /// Returns one `Binding` per solution row (each with at most one value
    /// per variable) rather than a single multi-row `Binding` table,
    /// specifically so a ground, provable goal (`vec![Binding::new()]` --
    /// one empty row) is distinguishable from a ground, unprovable one
    /// (`vec![]` -- no rows); the underlying multi-row `Binding`
    /// representation can't express that distinction on its own
    /// (`Binding::new().len() == 0` means both "one row, no columns" and
    /// "zero rows").
    pub fn solve(
        triple_index: &TripleIndex,
        rule_index: &RuleIndex,
        goal_pattern: &Triple,
    ) -> Vec<Binding> {
        let mut history = FxHashSet::default();
        let Some(raw) = Self::solve_inner(triple_index, rule_index, goal_pattern, &mut history, 0)
        else {
            return Vec::new();
        };
        let goal_vars = Self::collect_vars(goal_pattern);
        let mut rows = Self::binding_to_rows(raw);
        for row in rows.iter_mut() {
            row.retain_vars(&goal_vars);
        }
        let mut out: Vec<Binding> = Vec::new();
        for row in rows {
            if !out.contains(&row) {
                out.push(row);
            }
        }
        out
    }

    /// Split a multi-row `Binding` table into one single-row `Binding` per
    /// row, per `solve`'s doc comment. A table with no variables at all
    /// represents a single ground success (one row, zero columns), not
    /// zero rows.
    fn binding_to_rows(b: Binding) -> Vec<Binding> {
        if b.vars().is_empty() {
            return vec![Binding::new()];
        }
        let n = b.len();
        (0..n)
            .map(|i| {
                let mut row = Binding::new();
                for (&k, vals) in b.iter() {
                    if let Some(&val) = vals.get(i) {
                        row.add(&k, val);
                    }
                }
                row
            })
            .collect()
    }

    /// Returns `None` when the goal is not provable at all, and
    /// `Some(binding)` when it is -- `binding` may itself have zero columns
    /// (a ground success), which is why success/failure is tracked via this
    /// `Option` wrapper rather than `Binding::len() == 0` (that would be
    /// ambiguous between "zero rows" and "one row, zero columns", exactly
    /// the ambiguity `solve`'s doc comment above warns about). This was a
    /// real bug fixed by an independent adversarial review: previously this
    /// returned a bare `Binding`, so a goal reducing to a *failed* ground
    /// builtin check (e.g. `3 math:greaterThan 5`) and a goal reducing to a
    /// *successful* ground builtin check (`5 math:greaterThan 3`) both ended
    /// up represented by the same zero-column `Binding::new()`, and
    /// `TripleStore::prove`/`solve` reported both as proven.
    fn solve_inner(
        triple_index: &TripleIndex,
        rule_index: &RuleIndex,
        goal: &Triple,
        history: &mut FxHashSet<Triple>,
        depth: usize,
    ) -> Option<Binding> {
        if depth > Self::MAX_SOLVE_DEPTH {
            warn!(
                "BackwardChainer::solve: recursion depth limit ({}) exceeded for goal {:?}",
                Self::MAX_SOLVE_DEPTH,
                TripleStore::decode_triple(goal)
            );
            return None;
        }
        if !history.insert(goal.clone()) {
            return None;
        }

        let mut any_success = false;
        let mut all_bindings = Binding::new();

        // Direct facts / N3 builtins for this (possibly variable-containing)
        // goal -- both `TripleIndex::query` and `crate::builtins::evaluate`
        // already handle variable (including list-pattern) goals natively.
        if let Some(kind) = crate::builtins::classify(&goal.p) {
            if let Some(b) = crate::builtins::evaluate(kind, goal, &Binding::new()) {
                any_success = true;
                all_bindings.combine(b);
            }
        } else if let Some(b) = {
            crate::builtins::reject_if_unsupported_builtin(&goal.p);
            triple_index.query(goal, None)
        } {
            any_success = true;
            all_bindings.combine(b);
        }

        // Rule-derived (IDB) solutions.
        let candidates: &[Arc<Rule>] = if goal.p.is_term() {
            rule_index
                .head_by_pred
                .get(&goal.p.to_encoded())
                .map(|v| v.as_slice())
                .unwrap_or(&[])
        } else {
            rule_index.rules.as_slice()
        };

        for rule in candidates {
            let mut subst: FxHashMap<usize, VarOrTerm> = FxHashMap::default();
            let head_matches = Self::unify_term(&rule.head.s, &goal.s, &mut subst)
                && Self::unify_term(&rule.head.p, &goal.p, &mut subst)
                && Self::unify_term(&rule.head.o, &goal.o, &mut subst);
            if !head_matches {
                continue;
            }

            let substituted_body: Vec<BodyLiteral> = rule
                .body
                .iter()
                .map(|lit| BodyLiteral {
                    negated: lit.negated,
                    pattern: Self::resolve_triple(&lit.pattern, &subst),
                })
                .collect();

            let mut rule_bindings = Binding::new();
            let mut rule_failed = false;
            for body_lit in &substituted_body {
                if body_lit.negated {
                    // Negation as EDB failure, mirroring `prove_inner`.
                    crate::builtins::reject_if_unsupported_builtin(&body_lit.pattern.p);
                    if triple_index.contains(&body_lit.pattern) {
                        rule_failed = true;
                        break;
                    }
                    continue;
                }

                crate::builtins::reject_if_unsupported_builtin(&body_lit.pattern.p);
                let mut lit_success = false;
                let mut lit_bindings = Binding::new();
                if let Some(result_bindings) = triple_index.query(&body_lit.pattern, None) {
                    lit_success = true;
                    lit_bindings.combine(result_bindings);
                }
                if let Some(recursive_bindings) = Self::solve_inner(
                    triple_index,
                    rule_index,
                    &body_lit.pattern,
                    history,
                    depth + 1,
                ) {
                    lit_success = true;
                    lit_bindings.combine(recursive_bindings);
                }

                if !lit_success {
                    rule_failed = true;
                    break;
                }
                rule_bindings = rule_bindings.join(&lit_bindings);
            }
            if rule_failed {
                continue;
            }

            // Merge bindings the head unification alone determined -- a
            // rule variable can appear in two head positions, one aliasing
            // a goal variable and the other forcing a concrete value, and
            // that must reach the goal variable even when the body
            // contributes no rows of its own (e.g. an empty/`true` body).
            let goal_vars = Self::collect_vars(goal);
            let mut head_only = Binding::new();
            for &v in &goal_vars {
                if rule_bindings.vars().contains(&&v) {
                    continue;
                }
                let resolved = Self::walk(&VarOrTerm::new_encoded_var(v), &subst);
                if resolved.is_term() {
                    head_only.add(&v, resolved.to_encoded());
                }
            }
            if !head_only.vars().is_empty() {
                rule_bindings.combine(head_only);
            }

            any_success = true;
            all_bindings.combine(rule_bindings);
        }

        history.remove(goal);
        if !any_success {
            return None;
        }
        Some(all_bindings)
    }

    /// Chase-based unification of two (possibly variable-containing) terms,
    /// extending `subst` in place. Unlike `unify_ground` (which assumes the
    /// right-hand side is always a fully ground goal value), this allows
    /// either side -- or both -- to be a variable, including one variable
    /// unifying with another, so a rule variable can transitively force a
    /// goal variable to a concrete value discovered later in the very same
    /// head (see `solve_inner`'s doc comment).
    fn unify_term(a: &VarOrTerm, b: &VarOrTerm, subst: &mut FxHashMap<usize, VarOrTerm>) -> bool {
        let a = Self::walk(a, subst);
        let b = Self::walk(b, subst);
        if a == b {
            return true;
        }
        if a.is_var() {
            subst.insert(a.to_encoded(), b);
            return true;
        }
        if b.is_var() {
            subst.insert(b.to_encoded(), a);
            return true;
        }
        let (a_id, b_id) = (a.to_encoded(), b.to_encoded());
        match (
            VarOrTerm::list_members_typed(a_id),
            VarOrTerm::list_members_typed(b_id),
        ) {
            (Some(am), Some(bm)) => {
                am.len() == bm.len()
                    && am
                        .iter()
                        .zip(bm.iter())
                        .all(|(x, y)| Self::unify_term(x, y, subst))
            }
            _ => a_id == b_id,
        }
    }

    /// Follow a variable through `subst` until reaching either a
    /// non-variable term or a variable with no binding yet. Bounded to
    /// guard against a pathological/cyclic substitution.
    fn walk(term: &VarOrTerm, subst: &FxHashMap<usize, VarOrTerm>) -> VarOrTerm {
        let mut current = term.clone();
        for _ in 0..1000 {
            if !current.is_var() {
                return current;
            }
            match subst.get(&current.to_encoded()) {
                Some(next) => current = next.clone(),
                None => return current,
            }
        }
        current
    }

    /// Deeply resolve a term through `subst`, rebuilding list-term
    /// structure with substituted members (mirroring `substitute`'s use of
    /// `VarOrTerm::substitute_deep`, but chase-based so it also follows
    /// var-to-var chains).
    fn resolve_term(term: &VarOrTerm, subst: &FxHashMap<usize, VarOrTerm>) -> VarOrTerm {
        let walked = Self::walk(term, subst);
        if walked.is_var() {
            return walked;
        }
        let id = walked.to_encoded();
        if let Some(members) = VarOrTerm::list_members_typed(id) {
            let resolved_members: Vec<VarOrTerm> = members
                .iter()
                .map(|m| Self::resolve_term(m, subst))
                .collect();
            if resolved_members != members {
                return VarOrTerm::new_list(resolved_members);
            }
        }
        walked
    }

    fn resolve_triple(pattern: &Triple, subst: &FxHashMap<usize, VarOrTerm>) -> Triple {
        Triple {
            s: Self::resolve_term(&pattern.s, subst),
            p: Self::resolve_term(&pattern.p, subst),
            o: Self::resolve_term(&pattern.o, subst),
            g: pattern.g.as_ref().map(|g| Self::resolve_term(g, subst)),
        }
    }

    /// Collect every variable id appearing in `pattern`, including nested
    /// arbitrarily deep inside list-term structure, in first-occurrence
    /// order without duplicates.
    fn collect_vars_term(term: &VarOrTerm, out: &mut Vec<usize>) {
        if term.is_var() {
            let id = term.to_encoded();
            if !out.contains(&id) {
                out.push(id);
            }
            return;
        }
        if let Some(members) = VarOrTerm::list_members_typed(term.to_encoded()) {
            for m in &members {
                Self::collect_vars_term(m, out);
            }
        }
    }

    fn collect_vars(pattern: &Triple) -> Vec<usize> {
        let mut out = Vec::new();
        Self::collect_vars_term(&pattern.s, &mut out);
        Self::collect_vars_term(&pattern.p, &mut out);
        Self::collect_vars_term(&pattern.o, &mut out);
        if let Some(g) = &pattern.g {
            Self::collect_vars_term(g, &mut out);
        }
        out
    }
}

#[cfg(test)]
#[path = "backwardchaining_test.rs"]
mod backwardchaining_test;
