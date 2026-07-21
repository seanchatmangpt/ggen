// Vendored research-lineage engine (RoXi lineage): API reshaping is out of scope;
// lints below are documented scoped allows, not silent drift.
#![allow(clippy::ptr_arg)]

use crate::{Binding, BodyLiteral, TripleIndex, VarOrTerm};

// Re-exported so existing call sites using `crate::queryengine::builtins::*`
// / `crate::queryengine::BuiltinKind` (tests, benches, js/, server/) keep
// compiling unchanged after the builtin registry moved to the top-level
// `crate::builtins` module.
pub use crate::builtins;
pub use crate::builtins::BuiltinKind;

pub trait QueryEngine {
    fn query(
        data: &TripleIndex, query_triples: &Vec<BodyLiteral>, triple_counter: Option<usize>,
    ) -> Option<Binding>;

    fn query_semi_naive(
        data: &TripleIndex, query_triples: &Vec<BodyLiteral>, prev_limit: usize,
        current_limit: usize,
    ) -> Option<Binding>;
}
pub struct SimpleQueryEngine;

fn collect_vars(term: &VarOrTerm, vars: &mut std::collections::HashSet<usize>) {
    match term {
        VarOrTerm::Var(_) => {
            vars.insert(term.to_encoded());
        }
        VarOrTerm::Term(_) => {
            if let Some(members) = VarOrTerm::list_members_typed(term.to_encoded()) {
                for m in members {
                    collect_vars(&m, vars);
                }
            }
        }
    }
}

fn builtin_input_vars(lit: &BodyLiteral) -> std::collections::HashSet<usize> {
    let mut inputs = std::collections::HashSet::new();
    if let Some(kind) = crate::builtins::classify(&lit.pattern.p) {
        match kind {
            // list:in: object is input, subject is output
            BuiltinKind::ListIn => {
                collect_vars(&lit.pattern.o, &mut inputs);
            }
            // Comparison/test builtins: both subject and object must be bound (both are inputs)
            BuiltinKind::EqualTo
            | BuiltinKind::GreaterThan
            | BuiltinKind::LessThan
            | BuiltinKind::MathEqualTo
            | BuiltinKind::MathNotEqualTo
            | BuiltinKind::NotEqualTo
            | BuiltinKind::NotLessThan
            | BuiltinKind::NotGreaterThan
            | BuiltinKind::StringLessThan
            | BuiltinKind::StringContains
            | BuiltinKind::StringContainsIgnoringCase
            | BuiltinKind::StringStartsWith
            | BuiltinKind::StringEndsWith
            | BuiltinKind::StringMatches
            | BuiltinKind::StringNotMatches
            | BuiltinKind::StringEqualIgnoringCase
            | BuiltinKind::StringNotEqualIgnoringCase
            | BuiltinKind::StringGreaterThan
            | BuiltinKind::ListNotMember
            | BuiltinKind::FuncNumericEqual
            | BuiltinKind::FuncNumericLessThan
            | BuiltinKind::FuncNumericGreaterThan => {
                collect_vars(&lit.pattern.s, &mut inputs);
                collect_vars(&lit.pattern.o, &mut inputs);
            }
            // All other builtins (generative): subject is input, object is output
            _ => {
                collect_vars(&lit.pattern.s, &mut inputs);
            }
        }
    }
    inputs
}

impl QueryEngine for SimpleQueryEngine {
    fn query(
        data: &TripleIndex, query_triples: &Vec<BodyLiteral>, triple_counter: Option<usize>,
    ) -> Option<Binding> {
        let positive_lits: Vec<&BodyLiteral> =
            query_triples.iter().filter(|lit| !lit.negated).collect();
        let negated_lits: Vec<&BodyLiteral> =
            query_triples.iter().filter(|lit| lit.negated).collect();

        let mut rdf_lits: Vec<BodyLiteral> = positive_lits
            .iter()
            .filter(|lit| crate::builtins::classify(&lit.pattern.p).is_none())
            .map(|&lit| lit.clone())
            .collect();
        let mut builtin_lits: Vec<BodyLiteral> = positive_lits
            .iter()
            .filter(|lit| crate::builtins::classify(&lit.pattern.p).is_some())
            .map(|&lit| lit.clone())
            .collect();

        let mut bindings = Binding::new();
        let mut bound_vars = std::collections::HashSet::new();
        let mut first = true;
        let mut progress = true;

        while progress && (!rdf_lits.is_empty() || !builtin_lits.is_empty()) {
            progress = false;

            // 1. Try to evaluate a ready builtin first
            let mut ready_builtin_idx = None;
            for (idx, lit) in builtin_lits.iter().enumerate() {
                let inputs = builtin_input_vars(lit);
                if inputs.iter().all(|v| bound_vars.contains(v)) {
                    ready_builtin_idx = Some(idx);
                    break;
                }
            }

            if let Some(idx) = ready_builtin_idx {
                let next_lit = builtin_lits.remove(idx);
                let kind = crate::builtins::classify(&next_lit.pattern.p).unwrap();
                match crate::builtins::evaluate(kind, &next_lit.pattern, &bindings) {
                    Some(new_bindings) => {
                        bindings = new_bindings;
                        for &var in bindings.vars() {
                            bound_vars.insert(var);
                        }
                        first = false;
                        progress = true;
                        continue;
                    }
                    None => return None,
                }
            }

            // 2. Evaluate best RDF literal
            if !rdf_lits.is_empty() {
                let mut best_idx = 0;
                let mut best_score = -1;
                for (idx, lit) in rdf_lits.iter().enumerate() {
                    let mut score = 0;
                    let is_bound =
                        |t: &VarOrTerm| t.is_term() || bound_vars.contains(&t.to_encoded());
                    if is_bound(&lit.pattern.s) {
                        score += 1;
                    }
                    if is_bound(&lit.pattern.p) {
                        score += 1;
                    }
                    if is_bound(&lit.pattern.o) {
                        score += 1;
                    }
                    if let Some(ref g) = lit.pattern.g {
                        if is_bound(g) {
                            score += 1;
                        }
                    }
                    if score > best_score {
                        best_score = score;
                        best_idx = idx;
                    }
                }

                let next_lit = rdf_lits.remove(best_idx);
                crate::builtins::reject_if_unsupported_builtin(&next_lit.pattern.p);

                if first {
                    if let Some(current_bindings) = data.query(&next_lit.pattern, triple_counter) {
                        bindings = current_bindings;
                        for &var in bindings.vars() {
                            bound_vars.insert(var);
                        }
                        first = false;
                        progress = true;
                    } else {
                        return None;
                    }
                } else {
                    let mut next_bindings = Binding::new();
                    let mut matched_any_row = false;
                    let num_rows = if bindings.is_empty() {
                        1
                    } else {
                        bindings.len()
                    };
                    for c in 0..num_rows {
                        let resolve =
                            |v_id: usize| bindings.get(&v_id).and_then(|vals| vals.get(c).copied());
                        let mut ground_pattern = next_lit.pattern.clone();
                        ground_pattern.s = VarOrTerm::substitute_deep(&ground_pattern.s, &resolve);
                        ground_pattern.p = VarOrTerm::substitute_deep(&ground_pattern.p, &resolve);
                        ground_pattern.o = VarOrTerm::substitute_deep(&ground_pattern.o, &resolve);
                        if let Some(ref mut g) = ground_pattern.g {
                            *g = VarOrTerm::substitute_deep(g, &resolve);
                        }

                        if let Some(current_bindings) = data.query(&ground_pattern, triple_counter)
                        {
                            matched_any_row = true;
                            if current_bindings.is_empty() {
                                for (&var_id, vals) in bindings.iter() {
                                    next_bindings.add(&var_id, vals[c]);
                                }
                            } else {
                                for cur_row in 0..current_bindings.len() {
                                    for (&var_id, vals) in bindings.iter() {
                                        next_bindings.add(&var_id, vals[c]);
                                    }
                                    for (&var_id, vals) in current_bindings.iter() {
                                        if bindings.get(&var_id).is_none() {
                                            next_bindings.add(&var_id, vals[cur_row]);
                                        }
                                    }
                                }
                            }
                        }
                    }
                    if next_bindings.is_empty() {
                        if bindings.is_empty() && matched_any_row {
                            // Success, return empty bindings
                        } else {
                            return None;
                        }
                    }
                    bindings = next_bindings;
                    for &var in bindings.vars() {
                        bound_vars.insert(var);
                    }
                    progress = true;
                }
            }
        }

        if !rdf_lits.is_empty() || !builtin_lits.is_empty() {
            return None;
        }

        if negated_lits.is_empty() {
            return Some(bindings);
        }

        let mut filtered_bindings = Binding::new();
        let num_rows = if bindings.is_empty() {
            1
        } else {
            bindings.len()
        };

        for c in 0..num_rows {
            let mut satisfied = true;
            for negated_lit in &negated_lits {
                let resolve =
                    |v_id: usize| bindings.get(&v_id).and_then(|vals| vals.get(c).copied());
                let mut ground_pattern = negated_lit.pattern.clone();
                ground_pattern.s = VarOrTerm::substitute_deep(&ground_pattern.s, &resolve);
                ground_pattern.p = VarOrTerm::substitute_deep(&ground_pattern.p, &resolve);
                ground_pattern.o = VarOrTerm::substitute_deep(&ground_pattern.o, &resolve);
                if let Some(ref mut g) = ground_pattern.g {
                    *g = VarOrTerm::substitute_deep(g, &resolve);
                }

                crate::builtins::reject_if_unsupported_builtin(&ground_pattern.p);
                if data.query(&ground_pattern, triple_counter).is_some() {
                    satisfied = false;
                    break;
                }
            }

            if satisfied && !positive_lits.is_empty() {
                for (&var_id, vals) in bindings.iter() {
                    filtered_bindings.add(&var_id, vals[c]);
                }
            }
        }

        if !filtered_bindings.is_empty() || (bindings.is_empty() && num_rows == 1) {
            Some(filtered_bindings)
        } else {
            None
        }
    }

    fn query_semi_naive(
        data: &TripleIndex, query_triples: &Vec<BodyLiteral>, prev_limit: usize,
        current_limit: usize,
    ) -> Option<Binding> {
        let positive_lits: Vec<&BodyLiteral> =
            query_triples.iter().filter(|lit| !lit.negated).collect();
        let negated_lits: Vec<&BodyLiteral> =
            query_triples.iter().filter(|lit| lit.negated).collect();

        if positive_lits.is_empty() {
            return Self::query(data, query_triples, Some(current_limit));
        }

        let mut union_bindings = Binding::new();

        // println!("--- query_semi_naive: prev={}, curr={} ---", prev_limit, current_limit);
        for j in 0..positive_lits.len() {
            if crate::builtins::classify(&positive_lits[j].pattern.p).is_some() {
                // Builtins are evaluated procedurally, they don't produce new delta triples.
                // We only need to check delta for RDF positive literals.
                continue;
            }

            // Set up ranges for each positive literal in the j-th sub-query
            let mut rdf_lits: Vec<(BodyLiteral, usize, usize)> = positive_lits
                .iter()
                .enumerate()
                .filter_map(|(i, lit)| {
                    if crate::builtins::classify(&lit.pattern.p).is_some() {
                        None
                    } else {
                        let (min_c, max_c) = if i < j {
                            (0, current_limit)
                        } else if i == j {
                            (prev_limit, current_limit)
                        } else {
                            (0, prev_limit)
                        };
                        Some(((*lit).clone(), min_c, max_c))
                    }
                })
                .collect();

            let mut builtin_lits: Vec<BodyLiteral> = positive_lits
                .iter()
                .filter(|lit| crate::builtins::classify(&lit.pattern.p).is_some())
                .map(|&lit| lit.clone())
                .collect();

            // println!("  Sub-query j={}:", j);
            // for (lit, min, max) in &rdf_lits {
            //     println!("    lit={:?} range=[{}, {})", lit.pattern, min, max);
            // }

            let mut sub_bindings = Binding::new();
            let mut bound_vars = std::collections::HashSet::new();
            let mut first = true;
            let mut progress = true;

            while progress && (!rdf_lits.is_empty() || !builtin_lits.is_empty()) {
                progress = false;

                // 1. Try to evaluate a ready builtin first
                let mut ready_builtin_idx = None;
                for (idx, lit) in builtin_lits.iter().enumerate() {
                    let inputs = builtin_input_vars(lit);
                    if inputs.iter().all(|v| bound_vars.contains(v)) {
                        ready_builtin_idx = Some(idx);
                        break;
                    }
                }

                if let Some(idx) = ready_builtin_idx {
                    let next_lit = builtin_lits.remove(idx);
                    let kind = crate::builtins::classify(&next_lit.pattern.p).unwrap();
                    // println!("    builtins: evaluating {:?}", next_lit.pattern);
                    match crate::builtins::evaluate(kind, &next_lit.pattern, &sub_bindings) {
                        Some(new_bindings) => {
                            sub_bindings = new_bindings;
                            for &var in sub_bindings.vars() {
                                bound_vars.insert(var);
                            }
                            first = false;
                            progress = true;
                            continue;
                        }
                        None => {
                            // println!("      builtin failed!");
                            progress = false;
                            break;
                        }
                    }
                }

                // 2. Evaluate best RDF literal
                if !rdf_lits.is_empty() {
                    let mut best_idx = 0;
                    let mut best_score = -1;
                    for (idx, (lit, _, _)) in rdf_lits.iter().enumerate() {
                        let mut score = 0;
                        let is_bound =
                            |t: &VarOrTerm| t.is_term() || bound_vars.contains(&t.to_encoded());
                        if is_bound(&lit.pattern.s) {
                            score += 1;
                        }
                        if is_bound(&lit.pattern.p) {
                            score += 1;
                        }
                        if is_bound(&lit.pattern.o) {
                            score += 1;
                        }
                        if let Some(ref g) = lit.pattern.g {
                            if is_bound(g) {
                                score += 1;
                            }
                        }
                        if score > best_score {
                            best_score = score;
                            best_idx = idx;
                        }
                    }

                    let (next_lit, min_c, max_c) = rdf_lits.remove(best_idx);
                    crate::builtins::reject_if_unsupported_builtin(&next_lit.pattern.p);

                    // println!("    best lit idx={}: {:?} range=[{}, {})", best_idx, next_lit.pattern, min_c, max_c);

                    if first {
                        if let Some(current_bindings) =
                            data.query_range(&next_lit.pattern, min_c, max_c)
                        {
                            sub_bindings = current_bindings;
                            for &var in sub_bindings.vars() {
                                bound_vars.insert(var);
                            }
                            first = false;
                            progress = true;
                            // println!("      first success: bindings.len={}", sub_bindings.len());
                        } else {
                            // println!("      first failed!");
                            progress = false;
                            break;
                        }
                    } else {
                        let mut next_bindings = Binding::new();
                        let mut matched_any_row = false;
                        let num_rows = if sub_bindings.is_empty() {
                            1
                        } else {
                            sub_bindings.len()
                        };
                        for c in 0..num_rows {
                            let resolve = |v_id: usize| {
                                sub_bindings
                                    .get(&v_id)
                                    .and_then(|vals| vals.get(c).copied())
                            };
                            let mut ground_pattern = next_lit.pattern.clone();
                            ground_pattern.s =
                                VarOrTerm::substitute_deep(&ground_pattern.s, &resolve);
                            ground_pattern.p =
                                VarOrTerm::substitute_deep(&ground_pattern.p, &resolve);
                            ground_pattern.o =
                                VarOrTerm::substitute_deep(&ground_pattern.o, &resolve);
                            if let Some(ref mut g) = ground_pattern.g {
                                *g = VarOrTerm::substitute_deep(g, &resolve);
                            }

                            if let Some(current_bindings) =
                                data.query_range(&ground_pattern, min_c, max_c)
                            {
                                matched_any_row = true;
                                if current_bindings.is_empty() {
                                    for (&var_id, vals) in sub_bindings.iter() {
                                        next_bindings.add(&var_id, vals[c]);
                                    }
                                } else {
                                    for cur_row in 0..current_bindings.len() {
                                        for (&var_id, vals) in sub_bindings.iter() {
                                            next_bindings.add(&var_id, vals[c]);
                                        }
                                        for (&var_id, vals) in current_bindings.iter() {
                                            if sub_bindings.get(&var_id).is_none() {
                                                next_bindings.add(&var_id, vals[cur_row]);
                                            }
                                        }
                                    }
                                }
                            }
                        }
                        if next_bindings.is_empty() {
                            if sub_bindings.is_empty() && matched_any_row {
                                // println!("      join ground success (empty bindings)");
                            } else {
                                // println!("      join failed!");
                                progress = false;
                                break;
                            }
                        } else {
                            // println!("      join success: bindings.len={}", next_bindings.len());
                        }
                        sub_bindings = next_bindings;
                        for &var in sub_bindings.vars() {
                            bound_vars.insert(var);
                        }
                        progress = true;
                    }
                }
            }

            if progress
                && rdf_lits.is_empty()
                && builtin_lits.is_empty()
                && !sub_bindings.is_empty()
            {
                // println!("    Sub-query j={} SUCCESS!", j);
                union_bindings.union(sub_bindings);
            } else {
                // println!("    Sub-query j={} FAILED!", j);
            }
        }

        if union_bindings.is_empty() {
            // println!("  query_semi_naive FAILED overall!");
            return None;
        }

        // println!("  query_semi_naive SUCCESS overall! union_len={}", union_bindings.len());
        if negated_lits.is_empty() {
            return Some(union_bindings);
        }

        let mut filtered_bindings = Binding::new();
        let num_rows = if union_bindings.is_empty() {
            1
        } else {
            union_bindings.len()
        };
        for c in 0..num_rows {
            let mut satisfied = true;
            for negated_lit in &negated_lits {
                let resolve = |v_id: usize| {
                    union_bindings
                        .get(&v_id)
                        .and_then(|vals| vals.get(c).copied())
                };
                let mut ground_pattern = negated_lit.pattern.clone();
                ground_pattern.s = VarOrTerm::substitute_deep(&ground_pattern.s, &resolve);
                ground_pattern.p = VarOrTerm::substitute_deep(&ground_pattern.p, &resolve);
                ground_pattern.o = VarOrTerm::substitute_deep(&ground_pattern.o, &resolve);
                if let Some(ref mut g) = ground_pattern.g {
                    *g = VarOrTerm::substitute_deep(g, &resolve);
                }

                crate::builtins::reject_if_unsupported_builtin(&ground_pattern.p);
                if data.query(&ground_pattern, Some(current_limit)).is_some() {
                    satisfied = false;
                    break;
                }
            }

            if satisfied {
                for (&var_id, vals) in union_bindings.iter() {
                    filtered_bindings.add(&var_id, vals[c]);
                }
            }
        }

        if !filtered_bindings.is_empty() || (union_bindings.is_empty() && num_rows == 1) {
            Some(filtered_bindings)
        } else {
            None
        }
    }
}
