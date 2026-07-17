//! Bounded schedule analyzer: reads a `TemporalPlan` + its POWL partial-order
//! tape and derives structure — critical path, slack, parallelism, binding
//! resources, and capacity sensitivity — without introducing a new planner,
//! LP solver, or polytope representation. Everything here is either a direct
//! graph computation over the existing `pred_mask`/`succ_mask` DAG (bounded by
//! the 64-op tape cap) or a finite-difference re-run of the existing greedy
//! `find_temporal_plan`.
//!
//! This is deliberately *not* sensitivity over an optimal scheduler, and not
//! a feasible-region boundary/polytope. It explains the one schedule the
//! planner already found.

use crate::ground::GroundTemporalProblem;
use crate::powl_bridge::{temporal_plan_to_powl_tape, PowlOpSpec};
use crate::Pddl8Error;

/// L3 substage timing for one `analyze_schedule_instrumented` call — see
/// `docs/DFCM_BENCHMARK_ANALYSIS.md`'s resolution-ladder section.
///
/// `base_plan_ns` covers the initial `find_temporal_plan` call plus the
/// forward/backward-pass critical-path computation; `perturb_minus_ns`/
/// `perturb_plus_ns` cover the two `replan_with_perturbed_capacity` calls
/// for `resource_keys[0]` specifically (the ones `capacity_delta` reports);
/// remaining resource keys, if any, are folded into `sensitivity_compute_ns`
/// since the crown suite only ever exercises one key today (see
/// docs/DFCM_BENCHMARK_ANALYSIS.md's L5 deferral note).
#[derive(Debug, Clone, Copy, Default)]
pub struct AnalysisSubstageNs {
    pub resource_key_collect_ns: u128,
    pub base_plan_ns: u128,
    pub perturb_minus_ns: u128,
    pub perturb_plus_ns: u128,
    pub sensitivity_compute_ns: u128,
    pub result_build_ns: u128,
}

/// Per-resource finite-difference probe: does makespan change if this
/// resource's initial capacity moves by ±1?
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct CapacityDelta {
    /// Makespan with capacity − 1 (clamped at 0), or `None` if no plan was found at that capacity.
    pub minus_one_makespan: Option<f64>,
    /// Makespan at the resource's actual initial capacity.
    pub baseline_makespan: f64,
    /// Makespan with capacity + 1, or `None` if no plan was found at that capacity.
    pub plus_one_makespan: Option<f64>,
}

/// Bounded analysis of a single temporal plan's POWL tape (≤ 64 ops).
#[derive(Debug, Clone)]
pub struct ScheduleAnalysis64 {
    pub makespan: f64,
    /// Bitmask (bit i = op i) of ops with zero slack — the critical path.
    pub critical_path_mask: u64,
    /// Maximum number of ops with overlapping [start, end) intervals at any instant.
    pub max_parallelism: u8,
    /// Bitmask (bit i = resource_keys[i]) of resources whose capacity is binding —
    /// i.e. decreasing that resource's initial value by 1 changes the makespan.
    pub binding_resource_mask: u64,
    /// `latest_start[i] - earliest_start[i]` for each op, zero-padded past `op_count`.
    pub slack_by_op: [f64; 64],
    /// Number of real ops analyzed (≤ 64).
    pub op_count: usize,
    /// Capacity sensitivity for `resource_keys[0]`, if any resources were given.
    pub capacity_delta: Option<CapacityDelta>,
}

/// Analyze the schedule `gtp.find_temporal_plan()` produces, plus capacity
/// sensitivity for the named numeric-fluent resources (e.g. `"available-workers"`,
/// matching the key format `fn_key`/`eval_numeric` use for zero-param functions).
///
/// `resource_keys` is capped at 64 entries (bitmask width); extras are ignored.
pub fn analyze_schedule(
    gtp: &GroundTemporalProblem, resource_keys: &[String],
) -> Result<ScheduleAnalysis64, Pddl8Error> {
    analyze_schedule_instrumented(gtp, resource_keys).map(|(result, _substage)| result)
}

/// Same as `analyze_schedule`, but also returns L3 substage timing
/// (`AnalysisSubstageNs`). The extra `Instant::now()` checkpoints are cheap
/// relative to `analyze_schedule`'s ~3-6 total sub-calls (unlike
/// `execute_temporal_plan`'s per-step hot loop, which needed a separate
/// bench-only duplicate to avoid adding overhead) — `analyze_schedule`
/// delegates to this function directly rather than duplicating it.
pub fn analyze_schedule_instrumented(
    gtp: &GroundTemporalProblem, resource_keys: &[String],
) -> Result<(ScheduleAnalysis64, AnalysisSubstageNs), Pddl8Error> {
    use std::time::Instant;
    let mut substage = AnalysisSubstageNs::default();

    let t_base = Instant::now();
    let plan = gtp.find_temporal_plan().into_result()?;
    let ops = temporal_plan_to_powl_tape(&plan)?;
    // `temporal_plan_to_powl_tape` now refuses (rather than silently
    // truncating) any plan longer than `MAX_POWL_TAPE_STEPS` (64), so this
    // `.min(64)` is a provable no-op restating this function's own
    // documented 64-op bound, not a silent-truncation safety net.
    let n = ops.len().min(64);

    let (earliest_start, earliest_finish) = forward_pass(&ops, n);
    let makespan = earliest_finish.iter().cloned().fold(0.0_f64, f64::max);
    let (latest_start, _latest_finish) = backward_pass(&ops, n, makespan);

    let mut slack_by_op = [0.0_f64; 64];
    let mut critical_path_mask = 0u64;
    for i in 0..n {
        let slack = latest_start[i] - earliest_start[i];
        slack_by_op[i] = slack;
        if slack.abs() < 1e-9 {
            critical_path_mask |= 1u64 << i;
        }
    }
    let max_parallelism = max_parallelism(&earliest_start, &earliest_finish, n);
    substage.base_plan_ns = t_base.elapsed().as_nanos();

    let t_collect = Instant::now();
    let keys: Vec<&String> = resource_keys.iter().take(64).collect();
    substage.resource_key_collect_ns = t_collect.elapsed().as_nanos();

    let mut binding_resource_mask = 0u64;
    let mut capacity_delta = None;
    for (idx, key) in keys.into_iter().enumerate() {
        let t_minus = Instant::now();
        let minus = replan_with_perturbed_capacity(gtp, key, -1.0);
        substage.perturb_minus_ns += t_minus.elapsed().as_nanos();

        let t_plus = Instant::now();
        let plus = replan_with_perturbed_capacity(gtp, key, 1.0);
        substage.perturb_plus_ns += t_plus.elapsed().as_nanos();

        let t_sensitivity = Instant::now();
        let changed = match minus {
            Some(m) => (m - makespan).abs() > 1e-9,
            None => true, // infeasible at capacity-1 is itself evidence the resource binds
        };
        if changed {
            binding_resource_mask |= 1u64 << idx;
        }
        if idx == 0 {
            capacity_delta = Some(CapacityDelta {
                minus_one_makespan: minus,
                baseline_makespan: makespan,
                plus_one_makespan: plus,
            });
        }
        substage.sensitivity_compute_ns += t_sensitivity.elapsed().as_nanos();
    }

    let t_result = Instant::now();
    let result = ScheduleAnalysis64 {
        makespan,
        critical_path_mask,
        max_parallelism,
        binding_resource_mask,
        slack_by_op,
        op_count: n,
        capacity_delta,
    };
    substage.result_build_ns = t_result.elapsed().as_nanos();

    Ok((result, substage))
}

/// Earliest start/finish per op via a forward pass over `pred_mask`
/// (ops are already topologically ordered by index, since `pred_mask` for op
/// `i` only ever references indices `< i` — see `temporal_plan_to_powl_tape`).
fn forward_pass(ops: &[PowlOpSpec], n: usize) -> (Vec<f64>, Vec<f64>) {
    let mut earliest_start = vec![0.0_f64; n];
    let mut earliest_finish = vec![0.0_f64; n];
    for i in 0..n {
        let mut es = ops[i].start_time.unwrap_or(0.0);
        for (j, &finish) in earliest_finish.iter().enumerate().take(i) {
            if (ops[i].pred_mask >> j) & 1 == 1 {
                es = es.max(finish);
            }
        }
        earliest_start[i] = es;
        earliest_finish[i] = es + ops[i].duration.unwrap_or(0.0);
    }
    (earliest_start, earliest_finish)
}

/// Latest start/finish per op via a backward pass over `succ_mask`-implied
/// dependents (any op `k` whose `pred_mask` bit `i` is set depends on `i`).
fn backward_pass(ops: &[PowlOpSpec], n: usize, makespan: f64) -> (Vec<f64>, Vec<f64>) {
    let mut latest_finish = vec![makespan; n];
    let mut latest_start = vec![0.0_f64; n];
    for i in (0..n).rev() {
        let mut lf = makespan;
        for k in 0..n {
            if (ops[k].pred_mask >> i) & 1 == 1 {
                lf = lf.min(latest_start[k]);
            }
        }
        latest_finish[i] = lf;
        latest_start[i] = lf - ops[i].duration.unwrap_or(0.0);
    }
    (latest_start, latest_finish)
}

/// Maximum number of ops simultaneously in-flight, via a sweep over start/end events.
fn max_parallelism(earliest_start: &[f64], earliest_finish: &[f64], n: usize) -> u8 {
    let mut events: Vec<(f64, i32)> = Vec::with_capacity(n * 2);
    for i in 0..n {
        events.push((earliest_start[i], 1));
        events.push((earliest_finish[i], -1));
    }
    // Process ends before starts at the same instant: intervals are
    // half-open [start, end), so an op ending exactly when another starts
    // does not count as overlapping.
    events.sort_by(|a, b| a.0.partial_cmp(&b.0).unwrap().then(a.1.cmp(&b.1)));
    let mut cur = 0i32;
    let mut max_seen = 0i32;
    for (_, delta) in events {
        cur += delta;
        max_seen = max_seen.max(cur);
    }
    max_seen.max(0) as u8
}

/// Re-solve `find_temporal_plan` with `resource_key`'s initial value moved by
/// `delta` (clamped at 0), returning the resulting makespan, or `None` if no
/// plan was found at that capacity.
///
/// Uses `find_temporal_plan_with_fn_overrides` instead of cloning the whole
/// `GroundTemporalProblem` (grounded actions, conditions, atoms) just to
/// perturb one numeric fluent — only the small fn_values map gets cloned,
/// by `find_temporal_plan_with_fn_overrides` itself.
fn replan_with_perturbed_capacity(
    gtp: &GroundTemporalProblem, resource_key: &str, delta: f64,
) -> Option<f64> {
    let base = *gtp.initial_fn_values.get(resource_key).unwrap_or(&0.0);
    let mut overrides = std::collections::HashMap::with_capacity(1);
    overrides.insert(resource_key.to_string(), (base + delta).max(0.0));
    gtp.find_temporal_plan_with_fn_overrides(&overrides)
        .into_result()
        .ok()
        .map(|p| p.makespan)
}
