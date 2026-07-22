// Vendored research-lineage engine (RoXi lineage): API reshaping is out of scope;
// lints below are documented scoped allows, not silent drift.
#![allow(clippy::ptr_arg)]

use crate::aggregation::{
    Accumulator, AccumulatorImpl, AvgAccumulator, CountAccumulator, MaxAccumulator, MinAccumulator,
    SumAccumulator,
};
use crate::builtins::math;
use crate::fastmap::{FxHashMap, FxHashSet};
use crate::hooks::{clean_term, CmpOp, EffectKind, HookCondition};
use crate::parser::Parser;
use crate::queryengine::{QueryEngine, SimpleQueryEngine};
use crate::triples::AggregateFunction;
use crate::{Binding, BodyLiteral, Rule, Triple, TripleIndex, TripleStore, VarOrTerm};
use log::debug;
use std::collections::HashMap;

mod log_collect_all_in;
mod log_conclusion;
mod log_for_all_in;
mod log_if_then_else_in;
mod log_implies;
mod log_includes;
mod log_not_includes;
mod substitution;

#[cfg(test)]
#[path = "reasoner_test.rs"]
mod reasoner_test;

/// A fact store for semi-naive materialization.
/// Maintains explicit delta/all sets to track new facts and all known facts separately.
///
/// # Complexity
/// - `add_fact`: O(1) amortized (hash set insertion)
/// - `take_delta`: O(n) where n is the size of delta
/// - Space: O(|all_facts| + |delta|)
#[derive(Debug, Clone)]
pub struct FactStore {
    /// All known facts (cumulative).
    pub all_facts: FxHashSet<Triple>,
    /// Facts newly derived in this round (delta).
    pub delta: FxHashSet<Triple>,
}

impl Default for FactStore {
    fn default() -> Self {
        Self::new()
    }
}

impl FactStore {
    /// Create a new empty fact store.
    pub fn new() -> Self {
        FactStore {
            all_facts: FxHashSet::default(),
            delta: FxHashSet::default(),
        }
    }

    /// Add a fact to the store. Returns true if the fact is new (not in all_facts),
    /// false if it was already known.
    /// New facts are added to both all_facts and delta.
    pub fn add_fact(&mut self, fact: Triple) -> bool {
        if self.all_facts.insert(fact.clone()) {
            self.delta.insert(fact);
            true
        } else {
            false
        }
    }

    /// Drain and return the delta, resetting it to empty for the next round.
    /// Returns an empty set if delta is already empty.
    pub fn take_delta(&mut self) -> FxHashSet<Triple> {
        std::mem::take(&mut self.delta)
    }

    /// Read-only access to all known facts.
    pub fn all(&self) -> &FxHashSet<Triple> {
        &self.all_facts
    }

    /// Read-only access to the current delta.
    pub fn delta(&self) -> &FxHashSet<Triple> {
        &self.delta
    }
}

/// Canonical derivation record for duplicate suppression.
/// Tracks `(fact, rule, sorted_premises)` to prevent re-derivation of the same
/// fact via the same rule and premises.
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct CanonicalDerivation {
    /// The derived fact.
    pub fact: Triple,
    /// ID/index of the rule that derived this fact (for provenance tracking).
    pub rule_id: usize,
    /// Sorted premises that triggered this derivation (canonicalized for comparison).
    pub sorted_premises: Vec<Triple>,
    /// Round/timestamp at which this was derived.
    pub round: usize,
}

/// Derivation gate for canonical provenance tracking and duplicate suppression.
/// Records all derivations as (fact, rule, sorted-premises) triples.
/// A derivation is admitted only if we haven't seen that exact triple before.
///
/// # Complexity
/// - `admit_derivation`: O(n log n) where n is the number of premises (due to sorting)
/// - Space: O(d) where d is the total number of unique derivations
#[derive(Debug, Clone)]
pub struct DerivationGate {
    /// Map from (fact, rule_id) to the canonical derivation record.
    /// Used for fast lookup to detect duplicate derivations.
    pub derivations: FxHashMap<(Triple, usize), CanonicalDerivation>,
}

impl Default for DerivationGate {
    fn default() -> Self {
        Self::new()
    }
}

impl DerivationGate {
    /// Create a new empty derivation gate.
    pub fn new() -> Self {
        DerivationGate {
            derivations: FxHashMap::default(),
        }
    }

    /// Admit a derivation if it's new (not previously seen with the same rule and premises).
    /// Returns true if this is the first time seeing this (fact, rule, premises) combination.
    /// Returns false if we've already recorded an identical derivation.
    ///
    /// Premises are canonicalized (sorted) before comparison to ensure
    /// `{P1, P2}` and `{P2, P1}` are recognized as the same.
    pub fn admit_derivation(
        &mut self, fact: Triple, rule_id: usize, mut premises: Vec<Triple>, round: usize,
    ) -> bool {
        // Canonicalize premises by sorting
        premises.sort_by(|a, b| {
            // Sort by encoded triple values for deterministic ordering
            let a_key = format!("{:?}", a);
            let b_key = format!("{:?}", b);
            a_key.cmp(&b_key)
        });

        let key = (fact.clone(), rule_id);
        let derivation = CanonicalDerivation {
            fact,
            rule_id,
            sorted_premises: premises,
            round,
        };

        // Insert if not already present; return true if this is new
        self.derivations.insert(key, derivation).is_none()
    }

    /// Get read-only access to all recorded derivations.
    pub fn all_derivations(
        &self,
    ) -> impl Iterator<Item = (&(Triple, usize), &CanonicalDerivation)> {
        self.derivations.iter()
    }
}

pub struct Reasoner;

impl Reasoner {
    pub fn materialize(
        &self, triple_index: &mut TripleIndex, rules: &Vec<Rule>, strata: &Vec<usize>,
        aggregates: &std::collections::HashMap<Rule, crate::triples::Aggregate>,
        hooks: &Vec<crate::hooks::CompiledHook>, receipts: &mut Vec<crate::hooks::HookReceipt>,
        verdicts: &mut Vec<crate::hooks::HookVerdictRecord>, removals: &mut Vec<Triple>,
    ) -> Result<Vec<Triple>, String> {
        let mut inferred = Vec::new();
        if rules.is_empty() && hooks.is_empty() {
            return Ok(inferred);
        }

        let max_stratum = *strata.iter().max().unwrap_or(&0);
        let initial_snapshot: Vec<Triple> = triple_index.triples.clone();
        let mut first_hook_round = true;

        for s in 0..=max_stratum {
            let stratum_rules: Vec<&Rule> = rules
                .iter()
                .enumerate()
                // `.get(i).copied().unwrap_or(0)` rather than `strata[i]`: if
                // stratification failed validation (e.g. `strata` is shorter
                // than `rules`, or empty), fall back to treating the rule as
                // stratum 0 instead of panicking on an out-of-bounds index.
                .filter(|(i, _)| strata.get(*i).copied().unwrap_or(0) == s)
                .map(|(_, r)| r)
                .collect();

            if stratum_rules.is_empty() && hooks.is_empty() {
                continue;
            }

            let mut stratum_start_counter = None;
            let mut changed = true;

            let stratum_rollback_len = triple_index.len();
            let stratum_rollback_inferred_len = inferred.len();
            let mut stratum_history: Vec<Vec<Triple>> = Vec::new();
            let mut previous_round_hook_additions = Vec::new();

            while changed {
                changed = false;
                let next_start_counter = triple_index.len();
                let mut current_round_hook_additions = Vec::new();

                for rule in &stratum_rules {
                    if rule.is_denial() {
                        // A denial/consistency-check rule (`=> false.`, e.g.
                        // SKOS's disjointness constraints) never derives a
                        // fact -- its sentinel head (`Rule::DENIAL_HEAD_MARKER`)
                        // exists purely so `Rule` doesn't need restructuring,
                        // and must never itself be asserted as if it were a
                        // real triple. Violations are checked separately via
                        // `TripleStore::check_denials` once materialize()
                        // has reached a fixpoint (see its doc comment).
                        continue;
                    }
                    if let Some(agg) = aggregates.get(rule) {
                        if let Some(bindings) = SimpleQueryEngine::query(
                            triple_index,
                            &rule.body,
                            stratum_start_counter,
                        ) {
                            let len = bindings.len();
                            if len > 0 {
                                let group_var_ids: Vec<usize> = agg
                                    .group_vars
                                    .iter()
                                    .map(|v| VarOrTerm::convert(v.clone()).to_encoded())
                                    .collect();
                                let source_var_id =
                                    VarOrTerm::convert(agg.source_var.clone()).to_encoded();
                                let target_var_id =
                                    VarOrTerm::convert(agg.target_var.clone()).to_encoded();

                                let mut groups: HashMap<Vec<usize>, Vec<usize>> = HashMap::new();
                                for c in 0..len {
                                    let mut group_key = Vec::new();
                                    for &var_id in &group_var_ids {
                                        if let Some(vals) = bindings.get(&var_id) {
                                            group_key.push(vals[c]);
                                        } else {
                                            group_key.push(0);
                                        }
                                    }
                                    if let Some(vals) = bindings.get(&source_var_id) {
                                        groups.entry(group_key).or_default().push(vals[c]);
                                    }
                                }

                                // Determinism: sort group keys before iteration (HashMap order unspecified).
                                let mut sorted_groups: Vec<_> = groups.into_iter().collect();
                                sorted_groups.sort_by(|a, b| a.0.cmp(&b.0));

                                for (group_key, source_vals) in sorted_groups {
                                    let mut acc = match agg.function {
                                        AggregateFunction::Count => {
                                            AccumulatorImpl::Count(CountAccumulator::default())
                                        }
                                        AggregateFunction::Sum => {
                                            AccumulatorImpl::Sum(SumAccumulator::default())
                                        }
                                        AggregateFunction::Min => {
                                            AccumulatorImpl::Min(MinAccumulator::default())
                                        }
                                        AggregateFunction::Max => {
                                            AccumulatorImpl::Max(MaxAccumulator::default())
                                        }
                                        AggregateFunction::Avg => {
                                            AccumulatorImpl::Avg(AvgAccumulator::default())
                                        }
                                    };
                                    for val in source_vals {
                                        acc.add(val);
                                    }
                                    let target_val = acc.get();

                                    let mut head = rule.head.clone();
                                    let substitute = |term: &mut VarOrTerm| {
                                        if term.is_var() {
                                            let var_id = term.to_encoded();
                                            if var_id == target_var_id {
                                                *term = VarOrTerm::new_encoded_term(target_val);
                                            } else {
                                                for (i, &gv_id) in group_var_ids.iter().enumerate()
                                                {
                                                    if var_id == gv_id {
                                                        *term = VarOrTerm::new_encoded_term(
                                                            group_key[i],
                                                        );
                                                        break;
                                                    }
                                                }
                                            }
                                        }
                                    };
                                    substitute(&mut head.s);
                                    substitute(&mut head.p);
                                    substitute(&mut head.o);
                                    if let Some(ref mut g) = head.g {
                                        substitute(g);
                                    }

                                    if Self::apply_new_triple(head, triple_index, &mut inferred) {
                                        changed = true;
                                    }
                                }
                            }
                        }
                    } else if !Self::find_log_implies_literals(rule).is_empty() {
                        let implies_indices = Self::find_log_implies_literals(rule);
                        // log:implies dynamic rule reification (see
                        // process_log_implies_rule doc comment).
                        for new_head in Self::process_log_implies_rule(
                            rule,
                            &implies_indices,
                            triple_index,
                            stratum_start_counter,
                            next_start_counter,
                        ) {
                            if Self::apply_new_triple(new_head, triple_index, &mut inferred) {
                                changed = true;
                            }
                        }
                    } else if let Some(collect_idx) = Self::find_log_collect_all_in_literal(rule) {
                        // log:collectAllIn (see process_log_collect_all_in_rule doc comment).
                        for new_head in Self::process_log_collect_all_in_rule(
                            rule,
                            collect_idx,
                            triple_index,
                            stratum_start_counter,
                            next_start_counter,
                        ) {
                            if Self::apply_new_triple(new_head, triple_index, &mut inferred) {
                                changed = true;
                            }
                        }
                    } else if let Some(not_includes_idx) = Self::find_log_not_includes_literal(rule)
                    {
                        // log:notIncludes SNAF guard (see process_log_not_includes_rule doc comment).
                        for new_head in Self::process_log_not_includes_rule(
                            rule,
                            not_includes_idx,
                            triple_index,
                            stratum_start_counter,
                            next_start_counter,
                        ) {
                            if Self::apply_new_triple(new_head, triple_index, &mut inferred) {
                                changed = true;
                            }
                        }
                    } else if let Some(includes_idx) = Self::find_log_includes_literal(rule) {
                        // log:includes (positive counterpart of notIncludes; see process_log_includes_rule doc comment).
                        for new_head in Self::process_log_includes_rule(
                            rule,
                            includes_idx,
                            triple_index,
                            stratum_start_counter,
                            next_start_counter,
                        ) {
                            if Self::apply_new_triple(new_head, triple_index, &mut inferred) {
                                changed = true;
                            }
                        }
                    } else if let Some(for_all_idx) = Self::find_log_for_all_in_literal(rule) {
                        // log:forAllIn (see process_log_for_all_in_rule doc comment).
                        for new_head in Self::process_log_for_all_in_rule(
                            rule,
                            for_all_idx,
                            triple_index,
                            stratum_start_counter,
                            next_start_counter,
                        ) {
                            if Self::apply_new_triple(new_head, triple_index, &mut inferred) {
                                changed = true;
                            }
                        }
                    } else if let Some(if_then_else_idx) =
                        Self::find_log_if_then_else_in_literal(rule)
                    {
                        // log:ifThenElseIn (see process_log_if_then_else_in_rule doc comment).
                        for new_head in Self::process_log_if_then_else_in_rule(
                            rule,
                            if_then_else_idx,
                            triple_index,
                            stratum_start_counter,
                            next_start_counter,
                        ) {
                            if Self::apply_new_triple(new_head, triple_index, &mut inferred) {
                                changed = true;
                            }
                        }
                    } else if let Some(conclusion_idx) = Self::find_log_conclusion_literal(rule) {
                        // log:conclusion (see process_log_conclusion_rule doc comment).
                        for new_head in Self::process_log_conclusion_rule(
                            rule,
                            conclusion_idx,
                            triple_index,
                            stratum_start_counter,
                            next_start_counter,
                        ) {
                            if Self::apply_new_triple(new_head, triple_index, &mut inferred) {
                                changed = true;
                            }
                        }
                    } else {
                        let bindings_opt = if let Some(prev_limit) = stratum_start_counter {
                            <SimpleQueryEngine as QueryEngine>::query_semi_naive(
                                triple_index,
                                &rule.body,
                                prev_limit,
                                next_start_counter,
                            )
                        } else {
                            SimpleQueryEngine::query(triple_index, &rule.body, None)
                        };

                        if let Some(bindings) = bindings_opt {
                            let new_heads =
                                Self::substitute_head_with_bindings(&rule.head, &bindings);

                            for new_head in new_heads {
                                if Self::apply_new_triple(new_head, triple_index, &mut inferred) {
                                    changed = true;
                                }
                            }
                        }
                    }
                }

                let mut round_additions = if next_start_counter < triple_index.len() {
                    triple_index.triples[next_start_counter..].to_vec()
                } else {
                    Vec::new()
                };
                round_additions.extend(previous_round_hook_additions);

                if first_hook_round {
                    first_hook_round = false;
                    let mut merged = initial_snapshot.clone();
                    merged.extend(round_additions);
                    round_additions = merged;
                }

                let mut hook_changed = false;

                for hook in hooks {
                    let gated = match hook.on.as_str() {
                        "assert" => round_additions.is_empty(),
                        "retract" => removals.is_empty(),
                        _ => false,
                    };
                    if gated {
                        verdicts.push(crate::hooks::HookVerdictRecord {
                            hook_id: hook.id,
                            hook_iri: hook.iri.clone(),
                            hook_name: hook.name.clone(),
                            condition_kind: hook.condition.kind().to_string(),
                            condition_hash: hook.condition.condition_hash().unwrap_or_default(),
                            verdict: crate::hooks::HookVerdict::Gated,
                            effect: hook.effect.clone(),
                            action_iri: hook.action.clone(),
                            diagnostics: None,
                            delta_hash: None,
                            idempotency_key: None,
                        });
                        continue;
                    }

                    let mut fired = false;
                    let mut bindings = Vec::new();

                    match &hook.condition {
                        HookCondition::Delta { var } => {
                            fired = delta_touches(&round_additions, var)
                                || delta_touches(removals, var);
                        }
                        HookCondition::Threshold { var, op, k } => {
                            let count = count_pred(triple_index, var);
                            fired = cmp_holds(op, count, *k);
                        }
                        HookCondition::Count { var, op, k } => {
                            let count = delta_count(&round_additions, var);
                            fired = cmp_holds(op, count, *k);
                        }
                        HookCondition::Window { var, op, k, window } => {
                            let mut total = delta_count(&round_additions, var);
                            // `saturating_sub(1)`, not `- 1`: `window: u8` is now refused at
                            // parse time when 0 (hooks/parsing.rs), but this reasoner loop is a
                            // second, independent path a `HookCondition::Window` can reach --
                            // defense-in-depth against any future caller that constructs one
                            // directly, bypassing that admission-time check. Matches the
                            // identical, already-established `saturating_sub(1)` pattern in
                            // `hooks/condition.rs::evaluate_condition`'s own Window arm (a
                            // sibling implementation of this same computation for a different
                            // caller) -- this file was the one outlier still using raw `- 1`,
                            // which underflowed and panicked for `window == 0` under this
                            // workspace's default overflow-checked build profile (found by an
                            // adversarial dogfood audit this session, reproduced directly).
                            for past_adds in stratum_history
                                .iter()
                                .rev()
                                .take(usize::from(*window).saturating_sub(1))
                            {
                                total += delta_count(past_adds, var);
                            }
                            fired = cmp_holds(op, total, *k);
                        }
                        HookCondition::Shacl { shapes } => {
                            let mut temp_store = TripleStore::new();
                            for t in &triple_index.triples {
                                temp_store.add(t.clone());
                            }
                            if let Ok(report) = temp_store.validate_shacl(shapes) {
                                fired = !report.conforms;
                            }
                        }
                        HookCondition::Shex { schema, shape_map } => {
                            let shape_map_parsed = parse_shape_map(shape_map);
                            let report = if schema.trim().starts_with('{') {
                                crate::shex::validate_shex(&triple_index, schema, &shape_map_parsed)
                            } else {
                                let mut temp_store = TripleStore::new();
                                for t in &triple_index.triples {
                                    temp_store.add(t.clone());
                                }
                                temp_store
                                    .validate_shex_c(schema, &shape_map_parsed)
                                    .map_err(|e| {
                                        Box::new(std::io::Error::new(std::io::ErrorKind::Other, e))
                                            as Box<dyn std::error::Error>
                                    })
                            };
                            if let Ok(rep) = report {
                                fired = !rep.conforms;
                            }
                        }
                        HookCondition::N3 { rules } => {
                            let mut temp_store = TripleStore::from(rules);
                            for t in &triple_index.triples {
                                temp_store.add(t.clone());
                            }
                            let before_len = temp_store.len();
                            if temp_store.materialize().is_ok() {
                                fired = temp_store.len() > before_len;
                            }
                        }
                        HookCondition::Sparql { query } => {
                            let mut temp_store = TripleStore::new();
                            for t in &triple_index.triples {
                                temp_store.add(t.clone());
                            }
                            if let Ok(rows) = temp_store.query(query) {
                                if !rows.is_empty() {
                                    fired = true;
                                    for row in rows {
                                        let mut row_vars = Vec::new();
                                        let mut has_digit_vars = false;
                                        for b in &row {
                                            if b.var.chars().all(|c| c.is_ascii_digit()) {
                                                has_digit_vars = true;
                                                break;
                                            }
                                        }
                                        if has_digit_vars {
                                            let mut digit_vars: Vec<(usize, String)> = row
                                                .iter()
                                                .filter_map(|b| {
                                                    b.var
                                                        .parse::<usize>()
                                                        .ok()
                                                        .map(|idx| (idx, b.val.clone()))
                                                })
                                                .collect();
                                            digit_vars.sort_by_key(|(idx, _)| *idx);
                                            for (_idx, val) in digit_vars {
                                                row_vars.push(val);
                                            }
                                        } else {
                                            let mut sorted_row = row.clone();
                                            sorted_row.sort_by(|a, b| a.var.cmp(&b.var));
                                            for b in sorted_row {
                                                row_vars.push(b.val);
                                            }
                                        }
                                        bindings.push(row_vars);
                                    }
                                }
                            }
                        }
                        HookCondition::Datalog { program, goal } => {
                            let translated = translate_datalog_program(program)?;
                            // `TripleStore::from(&str)` decides its parser by
                            // `data.contains("@prefix")` -- `translated` is all
                            // fully-qualified `<...>` IRIs (`translate_datalog_program`
                            // never emits `@prefix`), so `from` would route it through
                            // the legacy line-based `Parser::parse` (see that
                            // function's doc comment), which mints every bare token
                            // (e.g. a `FILTER`-translated numeric literal like `1000`)
                            // as an opaque, unbracketed "IRI" rather than a real
                            // `xsd:integer` literal -- silently breaking any
                            // `math:greaterThan`-style numeric comparison against a
                            // properly-typed fact (e.g. a Turtle-loaded `"1200"`).
                            // `load_rules` (`Parser::parse_rules` ->
                            // `n3rule_parser::parse`, the same pest grammar `from`
                            // uses for its `@prefix`-gated path) parses `translated`
                            // correctly regardless of `@prefix` presence, so build the
                            // temp store directly and load rules through it instead of
                            // going through the heuristic. Found via
                            // `test_c3_datalog_construct_delta_cascade`.
                            let mut temp_store = TripleStore::new();
                            temp_store.load_rules(&translated)?;
                            for t in &triple_index.triples {
                                temp_store.add(t.clone());
                            }
                            temp_store.materialize()?;
                            // `goal` is Datalog-atom syntax (`kh:goal "vip(?s)"`, matching
                            // `kh:program`'s head-atom convention), not a bare predicate name --
                            // strip the `(...)` argument list the same way
                            // `translate_datalog_program` strips it from a rule head, so the
                            // comparison below is against the plain predicate name
                            // (`translate_datalog_program` materializes `?s a
                            // <...kh#vip>`, never `<...kh#vip(?s)>`). Previously unexercised by
                            // any test through a real `materialize()` fire -- the only two
                            // prior `kh:goal "pred(...)"` users either never called
                            // `materialize()` at all (`test_f3_datalog_trigger_format`, a
                            // load-only format check) or used a program malformed by design
                            // (`test_b3_datalog_program_size_limit`'s placeholder `"{}"`).
                            let goal_pred = goal.split('(').next().unwrap_or(goal.as_str()).trim();
                            let goal_type = format!(
                                "<http://seanchatmangpt.github.io/praxis/kh#{}>",
                                goal_pred
                            );
                            for t in &temp_store.triple_index.triples {
                                let p_str = crate::encoding::Encoder::decode(&t.p.to_encoded())
                                    .unwrap_or_default();
                                let o_str = crate::encoding::Encoder::decode(&t.o.to_encoded())
                                    .unwrap_or_default();
                                if clean_term(&p_str)
                                    == "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"
                                    && clean_term(&o_str) == clean_term(&goal_type)
                                {
                                    let s_str = crate::encoding::Encoder::decode(&t.s.to_encoded())
                                        .unwrap_or_default();
                                    bindings.push(vec![s_str]);
                                }
                            }
                            fired = !bindings.is_empty();
                        }
                    }

                    // For delta hooks with emit-delta and no action, pre-populate hook_additions
                    let mut delta_hook_additions = Vec::new();
                    let mut delta_hook_removals = Vec::new();
                    if fired {
                        if let HookCondition::Delta { var } = &hook.condition {
                            if hook.action.is_none() && matches!(hook.effect, EffectKind::EmitDelta)
                            {
                                for t in &round_additions {
                                    let p_str = crate::encoding::Encoder::decode(&t.p.to_encoded())
                                        .unwrap_or_default();
                                    if clean_term(&p_str) == clean_term(var) {
                                        delta_hook_additions.push(t.clone());
                                    }
                                }
                                for t in removals.iter() {
                                    let p_str = crate::encoding::Encoder::decode(&t.p.to_encoded())
                                        .unwrap_or_default();
                                    if clean_term(&p_str) == clean_term(var) {
                                        delta_hook_removals.push(t.clone());
                                    }
                                }
                            }
                        }
                    }

                    if !fired {
                        verdicts.push(crate::hooks::HookVerdictRecord {
                            hook_id: hook.id,
                            hook_iri: hook.iri.clone(),
                            hook_name: hook.name.clone(),
                            condition_kind: hook.condition.kind().to_string(),
                            condition_hash: hook.condition.condition_hash().unwrap_or_default(),
                            verdict: crate::hooks::HookVerdict::NotFired,
                            effect: hook.effect.clone(),
                            action_iri: hook.action.clone(),
                            diagnostics: None,
                            delta_hash: None,
                            idempotency_key: None,
                        });
                        continue;
                    }

                    match hook.effect {
                        EffectKind::Refuse => {
                            let added_triples: Vec<Triple> =
                                triple_index.triples[stratum_rollback_len..].to_vec();
                            for t in added_triples {
                                triple_index.remove_ref(&t);
                            }
                            inferred.truncate(stratum_rollback_inferred_len);
                            let reason = hook.reason.as_deref().unwrap_or("no reason").to_string();
                            let diag = crate::hooks::TriggerDiagnostic {
                                hook_iri: hook.iri.clone(),
                                conforms: false,
                                details: vec![crate::hooks::DiagnosticDetail {
                                    focus_node: None,
                                    result_path: None,
                                    value: None,
                                    severity: Some("Violation".to_string()),
                                    message: reason.clone(),
                                }],
                            };
                            verdicts.push(crate::hooks::HookVerdictRecord {
                                hook_id: hook.id,
                                hook_iri: hook.iri.clone(),
                                hook_name: hook.name.clone(),
                                condition_kind: hook.condition.kind().to_string(),
                                condition_hash: hook.condition.condition_hash().unwrap_or_default(),
                                verdict: crate::hooks::HookVerdict::Fired,
                                effect: hook.effect.clone(),
                                action_iri: hook.action.clone(),
                                diagnostics: Some(diag),
                                delta_hash: None,
                                idempotency_key: None,
                            });
                            return Err(format!("refused by hook '{}': {}", hook.name, reason));
                        }
                        EffectKind::EmitDelta | EffectKind::GroundAction => {
                            let mut hook_additions = delta_hook_additions.clone();
                            let mut hook_removals = delta_hook_removals.clone();
                            if let Some(action_iri) = &hook.action {
                                let adds_pred =
                                    "http://seanchatmangpt.github.io/praxis/kh#adds_ttl";
                                let mut adds_ttl = String::new();
                                for t in &triple_index.triples {
                                    let s_str = crate::encoding::Encoder::decode(&t.s.to_encoded())
                                        .unwrap_or_default();
                                    let p_str = crate::encoding::Encoder::decode(&t.p.to_encoded())
                                        .unwrap_or_default();
                                    if clean_term(&s_str) == clean_term(action_iri)
                                        && clean_term(&p_str) == adds_pred
                                    {
                                        if let Some(s) =
                                            crate::encoding::Encoder::decode(&t.o.to_encoded())
                                        {
                                            adds_ttl = clean_term(&s).to_string();
                                            break;
                                        }
                                    }
                                }
                                if !adds_ttl.is_empty() {
                                    let mut projected_adds_list = Vec::new();
                                    if bindings.is_empty() {
                                        projected_adds_list.push(adds_ttl);
                                    } else {
                                        for vars in &bindings {
                                            projected_adds_list
                                                .push(project_delta_template(&adds_ttl, vars));
                                        }
                                    }
                                    for projected_adds in projected_adds_list {
                                        if let Ok(parsed_triples) = Parser::parse_triples(
                                            &projected_adds,
                                            crate::parser::Syntax::Turtle,
                                        ) {
                                            for t in parsed_triples {
                                                if Self::apply_new_triple(
                                                    t.clone(),
                                                    triple_index,
                                                    &mut inferred,
                                                ) {
                                                    hook_changed = true;
                                                    hook_additions.push(t.clone());
                                                    current_round_hook_additions.push(t);
                                                }
                                            }
                                        }
                                    }
                                } else {
                                    // Check for SPARQL CONSTRUCT query action
                                    let query_pred =
                                        "http://seanchatmangpt.github.io/praxis/kh#query";
                                    let mut action_query = String::new();
                                    for t in &triple_index.triples {
                                        let s_str =
                                            crate::encoding::Encoder::decode(&t.s.to_encoded())
                                                .unwrap_or_default();
                                        let p_str =
                                            crate::encoding::Encoder::decode(&t.p.to_encoded())
                                                .unwrap_or_default();
                                        if clean_term(&s_str) == clean_term(action_iri)
                                            && clean_term(&p_str) == query_pred
                                        {
                                            if let Some(s) =
                                                crate::encoding::Encoder::decode(&t.o.to_encoded())
                                            {
                                                action_query = clean_term(&s).to_string();
                                                break;
                                            }
                                        }
                                    }
                                    if !action_query.is_empty() {
                                        if let Ok((adds, _dels)) = crate::hooks::evaluate_construct(
                                            &action_query,
                                            triple_index,
                                        ) {
                                            for t in adds {
                                                if Self::apply_new_triple(
                                                    t.clone(),
                                                    triple_index,
                                                    &mut inferred,
                                                ) {
                                                    hook_changed = true;
                                                    hook_additions.push(t.clone());
                                                    current_round_hook_additions.push(t.clone());
                                                }
                                            }
                                            for t in _dels {
                                                if triple_index.contains(&t) {
                                                    triple_index.remove_ref(&t);
                                                    hook_changed = true;
                                                    hook_removals.push(t.clone());
                                                }
                                            }
                                        }
                                    }
                                }
                            }

                            // Always generate a verdict when a hook fires (this match arm is
                            // only reached once `fired` is already known true -- see the
                            // `if !fired { ...continue; }` guard above); receipts are more
                            // selective -- see the two `else` arms below.
                            let mut delta_hash = None;
                            let mut idempotency_key = None;
                            if !hook_additions.is_empty() || !hook_removals.is_empty() {
                                let mut lines = Vec::new();
                                for t in &hook_additions {
                                    crate::hooks::serialize_delta_quad(
                                        &hook.iri, t, true, &mut lines,
                                    );
                                }
                                for t in &hook_removals {
                                    crate::hooks::serialize_delta_quad(
                                        &hook.iri, t, false, &mut lines,
                                    );
                                }
                                lines.sort();
                                let delta_quads = lines.join("\n");
                                let d_hash =
                                    blake3::hash(delta_quads.as_bytes()).to_hex().to_string();
                                let i_key = blake3::hash(
                                    format!("praxis:idempotency-key:v1{}", d_hash).as_bytes(),
                                )
                                .to_hex()
                                .to_string();

                                let receipt = crate::hooks::HookReceipt {
                                    hook_name: hook.name.clone(),
                                    delta_hash: d_hash.clone(),
                                    idempotency_key: i_key.clone(),
                                    delta_quads,
                                };
                                receipts.push(receipt);
                                delta_hash = Some(d_hash);
                                idempotency_key = Some(i_key);
                            } else if hook.action.is_none() {
                                // Hook fired but has no `kh:action` at all (e.g. a bare
                                // threshold/count/window/delta trigger with no projection) --
                                // the condition match itself is the payload being tracked, so
                                // still emit an empty receipt to record the firing. Relied on
                                // by e.g. `test_f3_delta_trigger`/`test_f3_threshold_trigger`/
                                // `test_c3_threshold_count_window_concurrency` (no-action hooks
                                // that assert `!receipts.is_empty()` purely from firing) and
                                // `materialize_clears_verdicts_on_rollback_not_just_triple_index`
                                // (lib_test.rs, an `action: None` hook whose doc comment already
                                // documents this exact branch).
                                if !receipts.iter().any(|r| r.hook_name == hook.name) {
                                    let receipt = crate::hooks::HookReceipt {
                                        hook_name: hook.name.clone(),
                                        delta_hash: String::new(),
                                        idempotency_key: String::new(),
                                        delta_quads: String::new(),
                                    };
                                    receipts.push(receipt);
                                }
                            } else {
                                // Hook fired *and* has a `kh:action` (e.g. sparql-construct),
                                // but that action's WHERE clause matched nothing this round, so
                                // it produced zero triples. Per the documented invariant "A
                                // CONSTRUCT projection yielding zero changes must NOT generate
                                // any BLAKE3 receipts" (`test_c3_construct_empty_no_receipt`),
                                // no receipt is recorded here -- unlike the no-action branch
                                // above, there is no condition-match payload worth tracking
                                // when the projection itself is empty. The verdict is still
                                // recorded (the condition genuinely matched), just without a
                                // receipt/delta_hash.
                            }

                            verdicts.push(crate::hooks::HookVerdictRecord {
                                hook_id: hook.id,
                                hook_iri: hook.iri.clone(),
                                hook_name: hook.name.clone(),
                                condition_kind: hook.condition.kind().to_string(),
                                condition_hash: hook.condition.condition_hash().unwrap_or_default(),
                                verdict: crate::hooks::HookVerdict::Fired,
                                effect: hook.effect.clone(),
                                action_iri: hook.action.clone(),
                                diagnostics: None,
                                delta_hash,
                                idempotency_key,
                            });
                        }
                    }
                }

                if hook_changed {
                    changed = true;
                }

                previous_round_hook_additions = current_round_hook_additions;
                stratum_history.push(round_additions);
                stratum_start_counter = Some(next_start_counter);
            }
        }

        Ok(inferred)
    }

    /// Add `head` to `triple_index` (and `inferred`) immediately if it's not
    /// already present, returning whether anything was actually added.
    /// Applying each rule's derivations to the live index right away --
    /// rather than batching all rules' output until the whole iteration
    /// finishes -- lets a later rule in the same stratum/iteration observe
    /// an earlier rule's same-iteration derivations. This is required for
    /// the real EYE `nixon_diamond` corpus case: Rule 2 ("Republicans are
    /// NonPacifist") must fire and be visible before Rule 1's
    /// `log:notIncludes { ?x a ex:NonPacifist }` guard is evaluated in the
    /// same pass, so the guard correctly suppresses the Pacifist derivation.
    /// Declaration order becomes the de facto rule-priority order this
    /// needs (matching upstream EYE's textual rule order).
    pub(crate) fn apply_new_triple(
        head: Triple, triple_index: &mut TripleIndex, inferred: &mut Vec<Triple>,
    ) -> bool {
        if triple_index.contains(&head) {
            return false;
        }
        debug!("Inferred: {:?}", TripleStore::decode_triple(&head));
        inferred.push(head.clone());
        triple_index.add(head);
        true
    }

    /// Evaluate a sequence of body literals (typically N3 builtins, e.g.
    /// `math:sum`/`math:greaterThan`) against a single already-built row of
    /// bindings, in order -- used by `process_log_collect_all_in_rule` to
    /// handle literals that depend on values (like the freshly-built list)
    /// which don't exist as queryable data. Returns `None` if any literal
    /// fails to hold for this row.
    pub(crate) fn eval_post_literals(
        post_body: &[BodyLiteral], start: Binding, triple_index: &TripleIndex,
    ) -> Option<Binding> {
        let mut bindings = start;
        for lit in post_body {
            if lit.negated {
                continue;
            }
            if let Some(kind) = crate::queryengine::builtins::classify(&lit.pattern.p) {
                bindings = crate::queryengine::builtins::evaluate(kind, &lit.pattern, &bindings)?;
            } else if let Some(cur) = triple_index.query(&lit.pattern, None) {
                bindings = bindings.join(&cur);
            } else {
                return None;
            }
        }
        Some(bindings)
    }

    /// Shared helper for `log:collectAllIn` / `log:notIncludes`: resolve
    /// `formula_id` to its quoted-graph triples, substitute any variables
    /// already bound in `row_binding` (e.g. an outer `?Subject` referenced
    /// inside the quoted formula) into each of them, and evaluate the result
    /// as a one-shot sub-query against the *current* `triple_index` -- the
    /// same "match this antecedent's triples against the live store" idea as
    /// the antecedent-matching step of `process_log_implies_rule`. Returns
    /// `None` when the formula is unknown or has zero solutions, `Some(bindings)`
    /// otherwise (possibly a zero-column `Binding` when the formula's triples
    /// are already fully ground and match).
    pub(crate) fn eval_embedded_formula_against_store(
        formula_id: usize, row_binding: &Binding, triple_index: &TripleIndex,
    ) -> Option<Binding> {
        let triples = VarOrTerm::formula_triples(formula_id)?;
        if triples.is_empty() {
            return Some(Binding::new());
        }
        let body: Vec<BodyLiteral> = triples
            .iter()
            .map(|p| BodyLiteral {
                negated: false,
                pattern: Self::substitute_single_row(p, row_binding),
            })
            .collect();
        SimpleQueryEngine::query(triple_index, &body, None)
    }

    pub fn infer_rule_heads(
        triple_index: &TripleIndex, counter: Option<usize>, matching_rules: Vec<Rule>,
    ) -> Vec<Triple> {
        let mut new_triples = Vec::new();
        for rule in matching_rules {
            if let Some(temp_bindings) = SimpleQueryEngine::query(triple_index, &rule.body, counter)
            {
                let new_heads = Reasoner::substitute_head_with_bindings(&rule.head, &temp_bindings);

                for new_head in new_heads {
                    new_triples.push(new_head);
                }
            }
        }
        new_triples
    }
}

pub(crate) fn query(query_triple: &Triple, match_triple: &Triple) -> Option<Binding> {
    let mut bindings = Binding::new();
    let Triple { s, p, o, g: _ } = match_triple;
    match &query_triple.s {
        VarOrTerm::Var(s_var) => bindings.add(&s_var.name, s.as_term().id()),
        VarOrTerm::Term(s_term) => {
            if s_term != s.as_term() {
                return None;
            }
        }
    }
    match &query_triple.p {
        VarOrTerm::Var(p_var) => bindings.add(&p_var.name, p.as_term().id()),
        VarOrTerm::Term(p_term) => {
            if p_term != p.as_term() {
                return None;
            }
        }
    }
    match &query_triple.o {
        VarOrTerm::Var(o_var) => bindings.add(&o_var.name, o.as_term().id()),
        VarOrTerm::Term(o_term) => {
            if o_term != o.as_term() {
                return None;
            }
        }
    }

    Some(bindings)
}

/// Translate a bounded `FILTER(<var> <op> <value>)` clause (the STRIPS8-style
/// subset a `kh:program` Datalog body literal may use -- one comparison, no
/// `&&`/`||`/function-call SPARQL FILTER grammar) into its N3 `math:`
/// builtin-comparison equivalent, e.g. `FILTER(?a > 1000)` -> `?a
/// <http://www.w3.org/2000/10/swap/math#greaterThan> 1000 .`. The six
/// operators map onto this crate's existing N3 comparison builtins
/// (`builtins/math.rs`), the same builtins `queryengine.rs`'s
/// `SimpleQueryEngine` already evaluates for ordinary N3 rule bodies -- no
/// new evaluation logic, just the surface syntax translation.
///
/// # Errors
/// Returns `Err` if `clause` isn't `FILTER(...)`-wrapped, doesn't contain a
/// recognized operator, or is missing either operand -- callers should treat
/// this as "outside the bounded FILTER subset", not silently drop the
/// literal (a body literal this crate can't faithfully translate must not be
/// silently ignored, which would change the rule's semantics).
///
/// # Complexity
/// O(|clause|): a handful of `find`/`split_once` scans over the clause text.
fn translate_datalog_filter(clause: &str) -> Result<String, String> {
    let inner = clause
        .strip_prefix("FILTER(")
        .and_then(|s| s.strip_suffix(')'))
        .ok_or_else(|| "Datalog FILTER clause missing '(' / ')' delimiters".to_string())?
        .trim();

    // Longest-operator-first so `>=`/`<=`/`!=` aren't mis-split on their
    // leading `>`/`<`/`!` before the two-character form is ever tried.
    const OPERATORS: [(&str, &str); 6] = [
        (">=", math::MATH_NOT_LESS_THAN),
        ("<=", math::MATH_NOT_GREATER_THAN),
        ("!=", math::MATH_NOT_EQUAL_TO),
        (">", math::MATH_GREATER_THAN),
        ("<", math::MATH_LESS_THAN),
        ("=", math::MATH_EQUAL_TO),
    ];

    for (op, builtin_iri) in OPERATORS {
        if let Some((lhs, rhs)) = inner.split_once(op) {
            let lhs = lhs.trim();
            let rhs = rhs.trim();
            if lhs.is_empty() || rhs.is_empty() {
                return Err(format!(
                    "Datalog FILTER clause missing operand around '{}'",
                    op
                ));
            }
            return Ok(format!("{} {} {} .", lhs, builtin_iri, rhs));
        }
    }
    Err(format!(
        "Datalog FILTER clause '{}' has no recognized comparison operator",
        inner
    ))
}

/// Whether `lit` is a raw RDF triple-pattern body literal (`<subject>
/// <predicate> <object>`, e.g. `?x <http://example.org/spent> ?a`) rather
/// than the Datalog-atom shape (`pred(args)`) the rest of this translator's
/// body-literal dispatch otherwise expects. `kh:program` may mix both
/// styles in one rule body (an atom referring to a derived `kh:`-namespaced
/// predicate alongside a raw pattern matching pre-existing RDF facts) --
/// see `test_c3_datalog_construct_delta_cascade`'s `vip(?x) :- ?x
/// <http://example.org/spent> ?a , FILTER(?a > 1000) .`, whose first body
/// literal is exactly this raw-pattern shape. Detected by the absence of
/// `(` -- every other body-literal form this translator supports (`pred(v)`,
/// `!pred(v)`, `t(a,b,c)`, `FILTER(...)`) always contains one.
fn is_raw_triple_pattern(lit: &str) -> bool {
    !lit.contains('(')
}

/// Split a `kh:program` Datalog source into individual rule strings on
/// top-level `.` characters only -- i.e. a rule terminator, not a `.`
/// appearing inside an IRI's authority/path (`<http://example.org/spent>`)
/// or a quoted string literal. A naive `program.split('.')` (this
/// function's predecessor) silently truncated any rule containing an IRI
/// with a dot in it, e.g. `vip(?x) :- ?x <http://example.org/spent> ?a .`
/// split into `vip(?x) :- ?x <http://example` (truncated mid-IRI, no `:-`
/// in the remaining fragments so they were dropped without error) --
/// found via `test_c3_datalog_construct_delta_cascade`. Mirrors the
/// existing bracket/quote-aware line scanner in
/// `hooks/quads.rs::strip_comments` (same `in_iri`/`in_string` state
/// machine, applied to `.` instead of `#`).
///
/// # Complexity
/// O(|program|): a single pass over the source, one `char_indices()` scan.
fn split_datalog_rules(program: &str) -> Vec<String> {
    let mut rules = Vec::new();
    let mut current = String::new();
    let mut in_iri = false;
    let mut in_string: Option<char> = None;
    let mut pending_escape = false;
    for c in program.chars() {
        if let Some(q) = in_string {
            current.push(c);
            if pending_escape {
                pending_escape = false;
            } else if c == '\\' {
                pending_escape = true;
            } else if c == q {
                in_string = None;
            }
            continue;
        }
        if in_iri {
            current.push(c);
            if c == '>' {
                in_iri = false;
            }
            continue;
        }
        match c {
            '<' => {
                in_iri = true;
                current.push(c);
            }
            '"' | '\'' => {
                in_string = Some(c);
                current.push(c);
            }
            '.' => {
                rules.push(std::mem::take(&mut current));
            }
            _ => current.push(c),
        }
    }
    if !current.trim().is_empty() {
        rules.push(current);
    }
    rules
}

fn translate_datalog_program(program: &str) -> Result<String, String> {
    let mut n3 = String::new();
    for rule_str in split_datalog_rules(program) {
        let rule_str = rule_str.trim();
        if rule_str.is_empty() {
            continue;
        }
        if let Some((head_part, body_part)) = rule_str.split_once(":-") {
            let head_part = head_part.trim();
            let body_part = body_part.trim();

            let head_name = head_part
                .split('(')
                .next()
                .ok_or_else(|| "Datalog rule head missing '(' delimiter".to_string())?
                .trim();
            let head_var = head_part
                .split('(')
                .nth(1)
                .ok_or_else(|| "Datalog rule head missing argument".to_string())?
                .split(')')
                .next()
                .ok_or_else(|| "Datalog rule head missing ')' delimiter".to_string())?
                .trim();

            let mut body_literals = Vec::new();
            for lit in body_part.split(',') {
                let lit = lit.trim();
                if lit.starts_with('!') {
                    let pred_part = &lit[1..];
                    let pred_name = pred_part
                        .split('(')
                        .next()
                        .ok_or_else(|| {
                            "Datalog negated predicate missing '(' delimiter".to_string()
                        })?
                        .trim();
                    let pred_var = pred_part
                        .split('(')
                        .nth(1)
                        .ok_or_else(|| "Datalog negated predicate missing argument".to_string())?
                        .split(')')
                        .next()
                        .ok_or_else(|| {
                            "Datalog negated predicate missing ')' delimiter".to_string()
                        })?
                        .trim();
                    body_literals.push(format!(
                        "log:notIncludes {{ {} a <http://seanchatmangpt.github.io/praxis/kh#{}> }}",
                        pred_var, pred_name
                    ));
                } else if lit.starts_with("t(") && lit.ends_with(')') {
                    let args_str = &lit[2..lit.len() - 1];
                    let args: Vec<&str> = args_str.split(',').map(|s| s.trim()).collect();
                    if args.len() == 3 {
                        body_literals.push(format!("{} {} {} .", args[0], args[1], args[2]));
                    }
                } else if lit.starts_with("FILTER(") {
                    body_literals.push(translate_datalog_filter(lit)?);
                } else if is_raw_triple_pattern(lit) {
                    let tokens: Vec<&str> = lit.split_whitespace().collect();
                    if tokens.len() != 3 {
                        return Err(format!(
                            "Datalog raw triple-pattern body literal '{}' must have exactly \
                             3 whitespace-separated tokens (subject predicate object), found {}",
                            lit,
                            tokens.len()
                        ));
                    }
                    body_literals.push(format!("{} {} {} .", tokens[0], tokens[1], tokens[2]));
                } else {
                    let pred_name = lit
                        .split('(')
                        .next()
                        .ok_or_else(|| "Datalog predicate missing '(' delimiter".to_string())?
                        .trim();
                    let pred_var = lit
                        .split('(')
                        .nth(1)
                        .ok_or_else(|| "Datalog predicate missing argument".to_string())?
                        .split(')')
                        .next()
                        .ok_or_else(|| "Datalog predicate missing ')' delimiter".to_string())?
                        .trim();
                    body_literals.push(format!(
                        "{} a <http://seanchatmangpt.github.io/praxis/kh#{}> .",
                        pred_var, pred_name
                    ));
                }
            }

            n3.push_str(&format!(
                "{{ {} }} => {{ {} a <http://seanchatmangpt.github.io/praxis/kh#{}> . }} .\n",
                body_literals.join(" "),
                head_var,
                head_name
            ));
        }
    }
    Ok(n3)
}

fn project_delta_template(adds_template: &str, vars: &[String]) -> String {
    let mut adds = adds_template.to_string();
    for (i, val) in vars.iter().enumerate() {
        let placeholder = format!("?{}", i);
        let formatted = if val.starts_with("http://") || val.starts_with("https://") {
            format!("<{}>", val)
        } else {
            val.clone()
        };
        adds = adds.replace(&placeholder, &formatted);
    }
    adds
}

fn parse_shape_map(s: &str) -> Vec<(String, String)> {
    let mut map = Vec::new();
    for entry in s.split(',') {
        let entry = entry.trim();
        if entry.is_empty() {
            continue;
        }
        if let Some((node, shape)) = entry.split_once('@') {
            let clean = |x: &str| {
                let x = x.trim();
                if x.starts_with('<') && x.ends_with('>') {
                    x[1..x.len() - 1].to_string()
                } else {
                    x.to_string()
                }
            };
            map.push((clean(node), clean(shape)));
        }
    }
    map
}

fn count_pred(triple_index: &TripleIndex, var: &str) -> u64 {
    let mut count = 0;
    for t in &triple_index.triples {
        let p_str = crate::encoding::Encoder::decode(&t.p.to_encoded()).unwrap_or_default();
        if clean_term(&p_str) == clean_term(var) {
            count += 1;
        }
    }
    count
}

fn delta_count(additions: &[Triple], var: &str) -> u64 {
    let mut count = 0;
    for t in additions {
        let p_str = crate::encoding::Encoder::decode(&t.p.to_encoded()).unwrap_or_default();
        if clean_term(&p_str) == clean_term(var) {
            count += 1;
        }
    }
    count
}

fn delta_touches(additions: &[Triple], var: &str) -> bool {
    for t in additions {
        let p_str = crate::encoding::Encoder::decode(&t.p.to_encoded()).unwrap_or_default();
        if clean_term(&p_str) == clean_term(var) {
            return true;
        }
    }
    false
}

fn cmp_holds(op: &CmpOp, lhs: u64, rhs: u64) -> bool {
    match op {
        CmpOp::Eq => lhs == rhs,
        CmpOp::Ne => lhs != rhs,
        CmpOp::Lt => lhs < rhs,
        CmpOp::Le => lhs <= rhs,
        CmpOp::Gt => lhs > rhs,
        CmpOp::Ge => lhs >= rhs,
    }
}

#[cfg(test)]
mod factstore_tests {
    use super::*;

    #[test]
    fn test_factstore_add_new_fact() {
        let mut store = FactStore::new();
        let triple = Triple {
            s: VarOrTerm::new_term("http://example.org/s".to_string()),
            p: VarOrTerm::new_term("http://example.org/p".to_string()),
            o: VarOrTerm::new_term("http://example.org/o".to_string()),
            g: None,
        };

        // First add should return true
        let is_new = store.add_fact(triple.clone());
        assert!(is_new);
        assert_eq!(store.all().len(), 1);
        assert_eq!(store.delta().len(), 1);
    }

    #[test]
    fn test_factstore_duplicate_suppression() {
        let mut store = FactStore::new();
        let triple = Triple {
            s: VarOrTerm::new_term("http://example.org/s".to_string()),
            p: VarOrTerm::new_term("http://example.org/p".to_string()),
            o: VarOrTerm::new_term("http://example.org/o".to_string()),
            g: None,
        };

        // First add should return true
        let is_new_1 = store.add_fact(triple.clone());
        assert!(is_new_1);

        // Second add should return false
        let is_new_2 = store.add_fact(triple.clone());
        assert!(!is_new_2);

        // Store should contain only one fact
        assert_eq!(store.all().len(), 1);
        assert_eq!(store.delta().len(), 1); // Delta still has one (added once)
    }

    #[test]
    fn test_factstore_take_delta() {
        let mut store = FactStore::new();
        let triple1 = Triple {
            s: VarOrTerm::new_term("http://example.org/s1".to_string()),
            p: VarOrTerm::new_term("http://example.org/p".to_string()),
            o: VarOrTerm::new_term("http://example.org/o".to_string()),
            g: None,
        };
        let triple2 = Triple {
            s: VarOrTerm::new_term("http://example.org/s2".to_string()),
            p: VarOrTerm::new_term("http://example.org/p".to_string()),
            o: VarOrTerm::new_term("http://example.org/o".to_string()),
            g: None,
        };

        store.add_fact(triple1);
        store.add_fact(triple2);
        assert_eq!(store.delta().len(), 2);

        // Take delta
        let delta = store.take_delta();
        assert_eq!(delta.len(), 2);

        // Delta should be empty after taking
        assert_eq!(store.delta().len(), 0);

        // all_facts should still have both
        assert_eq!(store.all().len(), 2);
    }
}

#[cfg(test)]
mod derivation_gate_tests {
    use super::*;

    #[test]
    fn test_derivation_gate_admit_new() {
        let mut gate = DerivationGate::new();
        let fact = Triple {
            s: VarOrTerm::new_term("http://example.org/s".to_string()),
            p: VarOrTerm::new_term("http://example.org/p".to_string()),
            o: VarOrTerm::new_term("http://example.org/o".to_string()),
            g: None,
        };

        // First admission should return true
        let admitted = gate.admit_derivation(fact.clone(), 0, vec![], 1);
        assert!(admitted);
    }

    #[test]
    fn test_derivation_gate_duplicate_suppression() {
        let mut gate = DerivationGate::new();
        let fact = Triple {
            s: VarOrTerm::new_term("http://example.org/s".to_string()),
            p: VarOrTerm::new_term("http://example.org/p".to_string()),
            o: VarOrTerm::new_term("http://example.org/o".to_string()),
            g: None,
        };
        let premises = vec![];

        // First admission should return true
        let admitted_1 = gate.admit_derivation(fact.clone(), 0, premises.clone(), 1);
        assert!(admitted_1);

        // Second identical admission should return false
        let admitted_2 = gate.admit_derivation(fact.clone(), 0, premises, 1);
        assert!(!admitted_2);
    }

    #[test]
    fn test_derivation_gate_different_rules() {
        let mut gate = DerivationGate::new();
        let fact = Triple {
            s: VarOrTerm::new_term("http://example.org/s".to_string()),
            p: VarOrTerm::new_term("http://example.org/p".to_string()),
            o: VarOrTerm::new_term("http://example.org/o".to_string()),
            g: None,
        };

        // Derive via rule 0
        let admitted_1 = gate.admit_derivation(fact.clone(), 0, vec![], 1);
        assert!(admitted_1);

        // Derive same fact via rule 1 (different rule)
        let admitted_2 = gate.admit_derivation(fact.clone(), 1, vec![], 1);
        assert!(admitted_2); // Should be admitted (different rule)
    }
}
