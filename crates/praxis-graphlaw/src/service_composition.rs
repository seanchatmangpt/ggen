//! Service composition module.
//!
//! Provides utilities for composing RDF-based services (e.g. ML model pipelines)
//! via backward chaining over N3 rules.  The primary entry point is
//! [`eval_backward_with_history`], which performs a history-aware backward
//! chain to avoid cycles when services form a DAG.

// Vendored research-lineage engine (RoXi lineage): API reshaping is out of scope;
// lints below are documented scoped allows, not silent drift.
#![allow(clippy::type_complexity, dead_code)]

use crate::bindings::Binding;
use crate::reasoner::Reasoner;
use crate::{BackwardChainer, Rule, RuleIndex, Triple, TripleIndex, TripleStore};
use log::debug;
use std::sync::Arc;

/// Perform backward chaining with a visited-triple history to avoid cycles.
///
/// This is similar to [`BackwardChainer::eval_backward`] but maintains a
/// `history` of already-visited ground triples so that the same derivation
/// path is not followed twice.  This is particularly useful when reasoning
/// over service DAGs where the same intermediate result may be reachable via
/// multiple rules.
///
/// # Arguments
/// * `triple_index` – EDB (Extensional Database) of ground triples.
/// * `rule_index`   – Set of Horn-clause rules to chain over.
/// * `rule_head`    – The goal triple pattern to satisfy.
/// * `history`      – Accumulator of already-visited ground triples; pass an
///   empty `Vec` on the first call.
///
/// # Returns
/// A [`Binding`] mapping variable IDs to their possible ground values.
pub fn eval_backward_with_history(
    triple_index: &TripleIndex,
    rule_index: &RuleIndex,
    rule_head: &Triple,
    history: &mut Vec<Triple>,
) -> Binding {
    let sub_rules: Vec<(Arc<Rule>, Vec<(usize, usize)>)> =
        BackwardChainer::find_subrules(rule_index, rule_head);
    let mut all_bindings = Binding::new();

    for (sub_rule, var_subs) in sub_rules.into_iter() {
        debug!(
            "Backward chaining rule: {:?}",
            TripleStore::decode_rule(&sub_rule)
        );
        let mut rule_bindings = Binding::new();

        for body_lit in &sub_rule.body {
            let rule_atom = &body_lit.pattern;
            debug!("Matching body: {:?}", TripleStore::decode_triple(rule_atom));

            if let Some(result_bindings) = triple_index.query(rule_atom, None) {
                debug!(
                    "   Found matching body: {:?}",
                    TripleStore::decode_bindings(&result_bindings)
                );

                // Build ground triples for visited tracking
                let visited_triples =
                    Reasoner::substitute_triple_with_bindings(rule_atom, &result_bindings);

                // Skip if all derived triples are already in history (cycle guard)
                if visited_triples.iter().all(|item| history.contains(item)) {
                    break;
                }
                history.extend(visited_triples);
                rule_bindings = rule_bindings.join(&result_bindings);
            }

            // Recursive call for IDB derivation
            let recursive_bindings =
                eval_backward_with_history(triple_index, rule_index, rule_atom, history);
            rule_bindings.combine(recursive_bindings);
        }

        // Rename rule-internal variables to query variables
        let renamed = rule_bindings.rename(var_subs);
        all_bindings.combine(renamed);
    }

    all_bindings
}

#[cfg(test)]
#[path = "service_composition_test.rs"]
mod service_composition_test;
