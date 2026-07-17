// Vendored research-lineage engine (RoXi lineage): API reshaping is out of scope;
// lints below are documented scoped allows, not silent drift.
#![allow(clippy::type_complexity, clippy::borrowed_box, dead_code)]

pub mod accumulators;
pub mod eval;
pub mod plan;

use accumulators::Accumulator;
pub use accumulators::{
    AccumulatorImpl, AvgAccumulator, CountAccumulator, MaxAccumulator, MinAccumulator,
    SumAccumulator,
};
pub use eval::{encode_term, eval_expression, to_bool, EncodedTerm};
pub use plan::{
    extract_expression, extract_query_plan, strip_variable_prefix, PlanAggregation,
    PlanAggregationFunction, PlanExpression, PlanNode,
};

use crate::tripleindex::EncodedBinding;
use crate::{Encoder, TripleIndex};
use spargebra::Query;
use std::cell::RefCell;
use std::collections::HashMap;
use std::iter::empty;
use std::rc::Rc;

// `Ord`/`PartialOrd` added (previously just Debug/Eq/PartialEq/Hash/Clone) specifically so
// `rsp::r2s::Relation2StreamOperator::eval`'s DSTREAM branch can sort `Vec<Binding>` rows
// before returning them -- see that function's own doc comment for the determinism bug this
// fixes. Purely additive (derives, no removed capability); lexicographic over (var, val), a
// well-defined total order with no ties to break arbitrarily since two distinct Bindings always
// differ in at least one field.
#[derive(Debug, Eq, PartialEq, Hash, Clone, PartialOrd, Ord)]
pub struct Binding {
    pub var: String,
    pub val: String,
}

fn decode(input: &EncodedBinding) -> Binding {
    let mut var = Encoder::decode(&input.var).unwrap_or_default();
    if var.starts_with('?') {
        var.remove(0);
    }
    Binding {
        var,
        val: Encoder::decode(&input.val).unwrap_or_default(),
    }
}

pub fn evaluate_plan_and_debug<'a>(
    plan_node: &'a PlanNode,
    triple_index: &'a TripleIndex,
) -> Box<dyn Iterator<Item = Vec<Binding>> + 'a> {
    Box::new(
        evaluate_plan(plan_node, triple_index)
            .map(|v| v.into_iter().map(|b| decode(&b)).collect::<Vec<Binding>>()),
    )
}

pub fn evaluate_plan<'a>(
    plan_node: &'a PlanNode,
    triple_index: &'a TripleIndex,
) -> Box<dyn Iterator<Item = Vec<EncodedBinding>> + 'a> {
    match plan_node {
        PlanNode::QuadPattern { pattern: triple } => triple_index.query_help(triple, None),
        PlanNode::Project { child, mapping } => {
            let child_it = evaluate_plan(child, triple_index);
            Box::new(child_it.map(move |binding| {
                binding
                    .into_iter()
                    .filter(|b| mapping.contains(&b.var))
                    .collect()
            }))
        }
        PlanNode::Join { left, right } => {
            let left_results: Vec<Vec<EncodedBinding>> =
                evaluate_plan(left, triple_index).collect();
            let right_results: Vec<Vec<EncodedBinding>> =
                evaluate_plan(right, triple_index).collect();

            if left_results.is_empty() || right_results.is_empty() {
                return Box::new(empty());
            }

            let left_vars: std::collections::HashSet<usize> =
                left_results[0].iter().map(|b| b.var).collect();
            let right_vars: std::collections::HashSet<usize> =
                right_results[0].iter().map(|b| b.var).collect();
            let intersection: Vec<usize> = right_vars
                .iter()
                .filter(|v| left_vars.contains(v))
                .cloned()
                .collect();

            if intersection.is_empty() {
                let mut product = Vec::new();
                for l in &left_results {
                    for r in &right_results {
                        let mut merged = l.clone();
                        merged.extend(r.clone());
                        product.push(merged);
                    }
                }
                return Box::new(product.into_iter());
            }

            let mut hash: HashMap<Vec<usize>, Vec<Vec<EncodedBinding>>> = HashMap::new();
            for l in left_results {
                let mut key = Vec::with_capacity(intersection.len());
                let mut has_all = true;
                for &var in &intersection {
                    if let Some(b) = l.iter().find(|b| b.var == var) {
                        key.push(b.val);
                    } else {
                        has_all = false;
                        break;
                    }
                }
                if has_all {
                    hash.entry(key).or_default().push(l);
                }
            }

            let mut joined = Vec::new();
            for r in right_results {
                let mut key = Vec::with_capacity(intersection.len());
                let mut has_all = true;
                for &var in &intersection {
                    if let Some(b) = r.iter().find(|b| b.var == var) {
                        key.push(b.val);
                    } else {
                        has_all = false;
                        break;
                    }
                }
                if has_all {
                    if let Some(matching_lefts) = hash.get(&key) {
                        for l in matching_lefts {
                            let mut merged = r.clone();
                            for b_l in l {
                                if !merged.iter().any(|b_r| b_r.var == b_l.var) {
                                    merged.push(b_l.clone());
                                }
                            }
                            joined.push(merged);
                        }
                    }
                }
            }
            Box::new(joined.into_iter())
        }
        PlanNode::Filter { child, expression } => {
            let child = evaluate_plan(child, triple_index);
            let expression = eval_expression(expression);
            Box::new(child.filter(move |bindings| {
                expression(bindings)
                    .and_then(|term| to_bool(&term))
                    .unwrap_or(false)
            }))
        }
        PlanNode::Aggregate {
            child,
            keys,
            aggregates,
        } => {
            let child = evaluate_plan(child, triple_index);

            let aggregate_vars: Vec<(PlanAggregation, usize)> = aggregates
                .iter()
                .map(|(agg_fn, agg_var)| {
                    let var_str = agg_var.as_str().to_string();
                    let var_str = strip_variable_prefix(&var_str).to_string();
                    let encoded = Encoder::get(&var_str).unwrap_or_else(|| Encoder::add(var_str));
                    (agg_fn.clone(), encoded)
                })
                .collect();

            let grouped_accumulators = Rc::new(RefCell::new(HashMap::<
                Vec<usize>,
                Vec<AccumulatorImpl>,
            >::default()));

            if keys.is_empty() {
                let default_accs: Vec<AccumulatorImpl> = aggregate_vars
                    .iter()
                    .map(|(agg_fn, _)| match agg_fn.function {
                        PlanAggregationFunction::Count => {
                            AccumulatorImpl::Count(CountAccumulator::default())
                        }
                        PlanAggregationFunction::Sum => {
                            AccumulatorImpl::Sum(SumAccumulator::default())
                        }
                        PlanAggregationFunction::Min => {
                            AccumulatorImpl::Min(MinAccumulator::default())
                        }
                        PlanAggregationFunction::Max => {
                            AccumulatorImpl::Max(MaxAccumulator::default())
                        }
                        PlanAggregationFunction::Avg => {
                            AccumulatorImpl::Avg(AvgAccumulator::default())
                        }
                    })
                    .collect();
                grouped_accumulators
                    .borrow_mut()
                    .insert(vec![], default_accs);
            }

            let local_group = grouped_accumulators.clone();
            let aggregate_vars_for_closure = aggregate_vars.clone();
            child.for_each(move |child_binding| {
                let key_values: Vec<usize> = keys
                    .iter()
                    .map(|v| {
                        let var_str = v.as_str().to_string();
                        let var_str = strip_variable_prefix(&var_str).to_string();
                        Encoder::get(&var_str).unwrap_or_else(|| Encoder::add(var_str))
                    })
                    .collect();

                let mut converted_keys = Vec::with_capacity(key_values.len());
                for &key_val in &key_values {
                    if let Some(binding) = child_binding.iter().find(|b| b.var == key_val) {
                        converted_keys.push(binding.val);
                    }
                }

                let mut temp_acc = grouped_accumulators.borrow_mut();
                let accs = temp_acc.entry(converted_keys).or_insert_with(|| {
                    aggregate_vars_for_closure
                        .iter()
                        .map(|(agg_fn, _)| match agg_fn.function {
                            PlanAggregationFunction::Count => {
                                AccumulatorImpl::Count(CountAccumulator::default())
                            }
                            PlanAggregationFunction::Sum => {
                                AccumulatorImpl::Sum(SumAccumulator::default())
                            }
                            PlanAggregationFunction::Min => {
                                AccumulatorImpl::Min(MinAccumulator::default())
                            }
                            PlanAggregationFunction::Max => {
                                AccumulatorImpl::Max(MaxAccumulator::default())
                            }
                            PlanAggregationFunction::Avg => {
                                AccumulatorImpl::Avg(AvgAccumulator::default())
                            }
                        })
                        .collect()
                });

                for (i, acc) in accs.iter_mut().enumerate() {
                    let agg_fn = &aggregate_vars_for_closure[i].0;
                    let item_to_aggregate = if let Some(ref agg_var) = agg_fn.variable {
                        let var_str = agg_var.as_str().to_string();
                        let var_str = strip_variable_prefix(&var_str).to_string();
                        // Was `.unwrap_or(0)`: if the operand variable (e.g. "o" in SUM(?o))
                        // had not yet been interned at this exact point, that produced encoded
                        // ID 0 -- a real, already-meaningful encoder slot, not a sentinel for
                        // "unknown" -- so the lookup below would silently match nothing (or
                        // worse, match an unrelated binding that happened to also be ID 0),
                        // and every row's operand value would be lost, leaving SUM/MIN/MAX/AVG
                        // stuck at their zero-valued default. Matches the same safe fallback
                        // already used for the GROUP BY keys (line 212) and the aggregate
                        // target variable (line 168): intern the string if it wasn't already
                        // known, rather than guessing ID 0.
                        let encoded_agg_var =
                            Encoder::get(&var_str).unwrap_or_else(|| Encoder::add(var_str));
                        child_binding
                            .iter()
                            .find(|b| b.var == encoded_agg_var)
                            .map(|b| b.val)
                            .unwrap_or(0)
                    } else {
                        0
                    };
                    acc.add(item_to_aggregate);
                }
            });

            {
                let temp_acc = local_group.borrow_mut();
                let mut new_bindings = Vec::with_capacity(temp_acc.len());
                let key_values: Vec<usize> = keys
                    .iter()
                    .map(|v| {
                        let var_str = v.as_str().to_string();
                        let var_str = strip_variable_prefix(&var_str).to_string();
                        Encoder::get(&var_str).unwrap_or_else(|| Encoder::add(var_str))
                    })
                    .collect();

                // Sort before iterating: `temp_acc` is a `std::collections::HashMap<Vec<usize>,
                // ...>` -- the same `RandomState`-hashed, per-process-reseeded map type as swarm
                // finding #22 (shacl/report.rs, fixed commit 89ba964c), not even the narrower
                // FxHashMap case of #23/#24 (fixed commit f08b4e41). Every SPARQL query using
                // GROUP BY (TripleStore::query's public SELECT/CONSTRUCT API, plus
                // HookCondition::Sparql inside Reasoner::materialize()) returned its result rows
                // in raw HashMap iteration order, which differs across separate process runs of
                // byte-identical input/query. `Vec<usize>` (the group key) is `Ord` lexically, so
                // sorting by key is an unambiguous, deterministic total order with no ties to
                // break arbitrarily (two distinct groups always have distinct key vectors).
                let mut sorted_groups: Vec<(&Vec<usize>, &Vec<AccumulatorImpl>)> =
                    temp_acc.iter().collect();
                sorted_groups.sort_by_key(|(k, _)| *k);
                for (group_keys, group_values) in sorted_groups {
                    let mut new_row = Vec::with_capacity(key_values.len() + aggregate_vars.len());
                    for (i, &key_val) in key_values.iter().enumerate() {
                        if let Some(&val) = group_keys.get(i) {
                            new_row.push(EncodedBinding { var: key_val, val });
                        }
                    }
                    for (i, acc) in group_values.iter().enumerate() {
                        let agg_var_encoded = aggregate_vars[i].1;
                        new_row.push(EncodedBinding {
                            var: agg_var_encoded,
                            val: acc.get(),
                        });
                    }
                    new_bindings.push(new_row);
                }

                Box::new(new_bindings.into_iter())
            }
        }
        PlanNode::Extend {
            child,
            expression,
            to,
        } => {
            let child_it = evaluate_plan(child, triple_index);
            let expression_fn = eval_expression(expression);
            let to_str = to.as_str().to_string();
            let to_str = strip_variable_prefix(&to_str).to_string();
            let encoded_to = Encoder::add(to_str);
            Box::new(child_it.map(move |mut binding| {
                if let Some(term) = expression_fn(&binding) {
                    let val_id = encode_term(term);
                    binding.push(EncodedBinding {
                        var: encoded_to,
                        val: val_id,
                    });
                }
                binding
            }))
        }
        PlanNode::LeftJoin {
            left,
            right,
            expression,
        } => {
            let left_results: Vec<Vec<EncodedBinding>> =
                evaluate_plan(left, triple_index).collect();
            let right_results: Vec<Vec<EncodedBinding>> =
                evaluate_plan(right, triple_index).collect();

            let mut joined = Vec::new();
            let filter_fn = expression.as_ref().map(|expr| eval_expression(expr));

            for l in left_results {
                let l_vars: std::collections::HashSet<usize> = l.iter().map(|b| b.var).collect();
                let mut matched_any = false;

                for r in &right_results {
                    let mut compatible = true;
                    let mut intersection = Vec::new();
                    for b_r in r {
                        if l_vars.contains(&b_r.var) {
                            intersection.push(b_r.var);
                            let b_l = l.iter().find(|b| b.var == b_r.var).unwrap();
                            if b_l.val != b_r.val {
                                compatible = false;
                                break;
                            }
                        }
                    }

                    if compatible {
                        let mut merged = l.clone();
                        for b_r in r {
                            if !intersection.contains(&b_r.var) {
                                merged.push(b_r.clone());
                            }
                        }

                        let pass = if let Some(ref f) = filter_fn {
                            f(&merged).and_then(|term| to_bool(&term)).unwrap_or(false)
                        } else {
                            true
                        };

                        if pass {
                            joined.push(merged);
                            matched_any = true;
                        }
                    }
                }

                if !matched_any {
                    joined.push(l);
                }
            }
            Box::new(joined.into_iter())
        }
        PlanNode::Union { left, right } => {
            let left_it = evaluate_plan(left, triple_index);
            let right_it = evaluate_plan(right, triple_index);
            Box::new(left_it.chain(right_it))
        }
        PlanNode::Minus { left, right } => {
            let left_results: Vec<Vec<EncodedBinding>> =
                evaluate_plan(left, triple_index).collect();
            let right_results: Vec<Vec<EncodedBinding>> =
                evaluate_plan(right, triple_index).collect();

            let mut remaining = Vec::new();
            for l in left_results {
                let l_vars: std::collections::HashSet<usize> = l.iter().map(|b| b.var).collect();
                let mut filter_out = false;

                for r in &right_results {
                    let mut compatible = true;
                    let mut shared_any = false;
                    for b_r in r {
                        if l_vars.contains(&b_r.var) {
                            shared_any = true;
                            let b_l = l.iter().find(|b| b.var == b_r.var).unwrap();
                            if b_l.val != b_r.val {
                                compatible = false;
                                break;
                            }
                        }
                    }

                    if compatible && shared_any {
                        filter_out = true;
                        break;
                    }
                }

                if !filter_out {
                    remaining.push(l);
                }
            }
            Box::new(remaining.into_iter())
        }
        PlanNode::Done => Box::new(empty()),
        PlanNode::Unit => Box::new(std::iter::once(Vec::new())),
    }
}

pub struct QueryResults {
    plan: PlanNode,
    iterator: Box<dyn Iterator<Item = Vec<EncodedBinding>>>,
}

impl Iterator for QueryResults {
    type Item = Vec<EncodedBinding>;

    fn next(&mut self) -> Option<Self::Item> {
        self.iterator.next()
    }
}

pub fn eval_query<'a>(query: &'a Query, _index: &'a TripleIndex) -> PlanNode {
    match query {
        spargebra::Query::Select {
            pattern,
            base_iri: _,
            ..
        } => extract_query_plan(pattern),
        spargebra::Query::Ask {
            pattern,
            base_iri: _,
            ..
        } => extract_query_plan(pattern),
        spargebra::Query::Construct {
            template,
            pattern,
            base_iri: _,
            ..
        } => {
            let inner = extract_query_plan(pattern);
            if template.is_empty() {
                return inner;
            }
            let mapping: Vec<usize> = template
                .iter()
                .flat_map(|tp| {
                    let mut vars = Vec::new();
                    if let spargebra::term::TermPattern::Variable(v) = &tp.subject {
                        vars.push(crate::Encoder::add(v.as_str().to_string()));
                    }
                    if let spargebra::term::NamedNodePattern::Variable(v) = &tp.predicate {
                        vars.push(crate::Encoder::add(v.as_str().to_string()));
                    }
                    if let spargebra::term::TermPattern::Variable(v) = &tp.object {
                        vars.push(crate::Encoder::add(v.as_str().to_string()));
                    }
                    vars
                })
                .collect::<std::collections::HashSet<_>>()
                .into_iter()
                .collect();
            PlanNode::Project {
                child: Box::new(inner),
                mapping,
            }
        }
        spargebra::Query::Describe {
            pattern,
            base_iri: _,
            ..
        } => extract_query_plan(pattern),
    }
}

#[cfg(test)]
#[path = "../sparql_test.rs"]
mod sparql_test;
