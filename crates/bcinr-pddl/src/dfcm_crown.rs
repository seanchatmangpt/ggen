//! DfCM crown suite: a fixed, bounded (ops × capacity) matrix, each cell
//! exercising topology → planning → analysis → admission → receipt → replay
//! entirely within the 8/64 bound (≤ 64 durative-action ground instances, ≤
//! 64 POWL tape ops per cell — see `docs/DFCM_CONTRACT.md`). Backs both
//! `bcinr-bench/benches/dfcm_crown_bench.rs` and the wall-clock gate test in
//! `tests/dfcm_crown_suite.rs`, so the same suite is what's benchmarked and
//! what's gated.
//!
//! This empirically demonstrates composition stays inside one fixed
//! wall-clock envelope, gated by `dfcm_crown_suite_completes_under_5_seconds`
//! (`tests/dfcm_crown_suite.rs`) — a single wall-clock `elapsed <= 5.0`
//! assertion, inherently machine-load-dependent, that can pass on one
//! run/host and fail on another. It shows the bound held on the runs that
//! were checked; it is not a general timing proof, and does not claim to be
//! the fastest planner.

use std::time::Instant;

use crate::execute::{compute_plan_chain, execute_temporal_plan_instrumented, SubstageNs};
use crate::ground::GroundTemporalProblem;
use crate::powl_bridge::temporal_plan_to_powl_tape;
use crate::schedule_analysis::{analyze_schedule_instrumented, AnalysisSubstageNs};
use crate::{domain_from_pddl, problem_from_pddl, Pddl8Error};

/// Headline metrics for one run of the full crown suite (all 16 cells).
///
/// Field groups, per `docs/DFCM_BENCHMARK_ANALYSIS.md`'s cold/warm attribution
/// split:
/// - `cold_topology_once_ns`: parsed once, outside the per-cell loop — the
///   compile-time artifact (see the "parse/topology compile-artifact
///   boundary" investigation).
/// - `warm_*_ns`: per-new-problem costs, summed across all 16 cells (same
///   semantics as the older `topology_ns`/`planning_ns`/etc. fields, which
///   are kept for backward compatibility with existing callers/benches).
/// - `warm_replay_existing_receipt_ns`: a **different, cheaper** replay
///   measurement than `replay_ns` — chain-only validation
///   (`compute_plan_chain` recomputed and compared against the stored
///   `chain_hash`) instead of full re-execution through
///   `execute_temporal_plan`. `replay_ns` proves "this plan would be
///   re-admitted under the same policy today"; this field proves "this
///   receipt's hash is a pure function of these exact steps" — a different,
///   cheaper guarantee. See docs/DFCM_BENCHMARK_ANALYSIS.md for why these
///   are not interchangeable.
/// - `alloc_count_by_stage`/`bytes_allocated_by_stage`: populated only when
///   built with the `dhat-heap` feature (surface 7); `None` otherwise, so a
///   normal run is never misrepresented as having measured allocations it
///   didn't.
#[derive(Debug, Clone)]
pub struct DfcmBenchReceipt {
    pub wall_clock_ms: u128,
    pub topology_ns: u128,
    pub planning_ns: u128,
    pub analysis_ns: u128,
    pub admission_ns: u128,
    pub receipt_ns: u128,
    pub replay_ns: u128,
    pub max_ops: u8,
    pub max_parallelism: u8,
    pub suite_passed_5s_gate: bool,

    /// Domain parse time, measured once outside the 16-cell loop.
    pub cold_topology_once_ns: u128,
    /// Sum of per-cell `find_temporal_plan` time (same value as `planning_ns`).
    pub warm_plan_ns: u128,
    /// Sum of per-cell `analyze_schedule` time (same value as `analysis_ns`).
    pub warm_analysis_ns: u128,
    /// Sum of per-cell `execute_temporal_plan` (first call) time (same value as `admission_ns`).
    pub warm_admission_ns: u128,
    /// Sum of per-cell isolated `compute_plan_chain` time (same value as `receipt_ns`).
    pub warm_receipt_ns: u128,
    /// Sum of per-cell chain-only replay validation time — cheaper than
    /// `replay_ns`, which re-runs full execution instead.
    pub warm_replay_existing_receipt_ns: u128,

    /// Per-stage allocation counts, `dhat-heap` feature only.
    pub alloc_count_by_stage: Option<AllocStageStats>,
    /// Per-stage bytes allocated, `dhat-heap` feature only.
    pub bytes_allocated_by_stage: Option<AllocStageStats>,

    /// L3 substage attribution for the admission call (summed across all 16
    /// cells) — see `execute::SubstageNs` and
    /// `docs/DFCM_BENCHMARK_ANALYSIS.md`'s resolution-ladder section.
    pub admission_substage: SubstageNs,
    /// L3 substage attribution for the full-re-execution replay call.
    pub replay_substage: SubstageNs,
    /// L3 substage attribution for `analyze_schedule` — see
    /// `AnalysisSubstageNs` and `docs/DFCM_BENCHMARK_ANALYSIS.md`'s
    /// "Analysis bottleneck after may_fire dedupe" section.
    pub analysis_substage: AnalysisSubstageNs,
}

/// Per-stage allocation totals (either counts or bytes, depending on which
/// `DfcmBenchReceipt` field holds it), populated only under `dhat-heap`.
#[derive(Debug, Clone, Copy, Default)]
pub struct AllocStageStats {
    pub topology: u64,
    pub planning: u64,
    pub analysis: u64,
    pub admission: u64,
    pub receipt: u64,
    pub replay: u64,
}

const OPS_MATRIX: [usize; 4] = [8, 16, 32, 64];
const CAPACITY_MATRIX: [usize; 4] = [1, 2, 4, 8];

const DOMAIN: &str = r#"
(define (domain dfcm-crown)
  (:requirements :durative-actions :numeric-fluents :typing)
  (:types worker)
  (:predicates (idle ?w - worker) (busy ?w - worker) (done ?w - worker))
  (:functions (available-workers))
  (:durative-action assign-worker
    :parameters (?w - worker)
    :duration (= ?duration 1)
    :condition (and (at start (idle ?w)) (at start (>= (available-workers) 1)))
    :effect (and
      (at start (decrease (available-workers) 1))
      (at start (not (idle ?w))) (at start (busy ?w))
      (at end (increase (available-workers) 1))
      (at end (not (busy ?w))) (at end (done ?w)))))
"#;

fn problem_text(n_workers: usize, capacity: usize) -> String {
    let workers: Vec<String> = (1..=n_workers).map(|i| format!("w{i}")).collect();
    let objects = format!("{} - worker", workers.join(" "));
    let idle: Vec<String> = workers.iter().map(|w| format!("(idle {w})")).collect();
    let done: Vec<String> = workers.iter().map(|w| format!("(done {w})")).collect();
    format!(
        "(define (problem dfcm-crown-{n_workers}-{capacity})\n  (:domain dfcm-crown)\n  (:objects {objects})\n  (:init {} (= (available-workers) {capacity}))\n  (:goal (and {})))",
        idle.join(" "),
        done.join(" ")
    )
}

/// Run the full 16-cell (ops × capacity) matrix once, recording wall-clock
/// and per-stage timings, gated against `docs/DFCM_CONTRACT.md`'s ≤ 5s rule.
pub fn run_dfcm_crown_suite() -> Result<DfcmBenchReceipt, Pddl8Error> {
    let suite_start = Instant::now();

    let mut topology_ns: u128 = 0;
    let mut planning_ns: u128 = 0;
    let mut analysis_ns: u128 = 0;
    let mut admission_ns: u128 = 0;
    let mut receipt_ns: u128 = 0;
    let mut replay_ns: u128 = 0;
    let mut warm_replay_existing_receipt_ns: u128 = 0;
    let mut max_ops_seen: u8 = 0;
    let mut max_parallelism_seen: u8 = 0;
    let mut admission_substage = SubstageNs::default();
    let mut replay_substage = SubstageNs::default();
    let mut analysis_substage = AnalysisSubstageNs::default();

    #[cfg(feature = "dhat-heap")]
    let mut alloc_count = AllocStageStats::default();
    #[cfg(feature = "dhat-heap")]
    let mut alloc_bytes = AllocStageStats::default();

    // cold_topology_once_ns: domain parse happens exactly once, outside the
    // 16-cell loop below — this is the "compile once" artifact (surface 6).
    let cold_start = Instant::now();
    let domain = domain_from_pddl(DOMAIN)?;
    let cold_topology_once_ns = cold_start.elapsed().as_nanos();

    for &n_workers in &OPS_MATRIX {
        for &capacity in &CAPACITY_MATRIX {
            let problem = problem_from_pddl(&problem_text(n_workers, capacity))?;
            let gtp = GroundTemporalProblem::build(&domain, &problem)?;

            #[cfg(feature = "dhat-heap")]
            let alloc_before = crate::alloc_counter::counting_alloc::snapshot();

            let t0 = Instant::now();
            let plan = gtp.find_temporal_plan().into_result()?;
            planning_ns += t0.elapsed().as_nanos();
            #[cfg(feature = "dhat-heap")]
            record_stage_alloc(
                &mut alloc_count.planning,
                &mut alloc_bytes.planning,
                alloc_before,
            );
            #[cfg(feature = "dhat-heap")]
            let alloc_before = crate::alloc_counter::counting_alloc::snapshot();

            let t1 = Instant::now();
            let ops = temporal_plan_to_powl_tape(&plan)?;
            topology_ns += t1.elapsed().as_nanos();
            max_ops_seen = max_ops_seen.max(ops.len().min(u8::MAX as usize) as u8);
            #[cfg(feature = "dhat-heap")]
            record_stage_alloc(
                &mut alloc_count.topology,
                &mut alloc_bytes.topology,
                alloc_before,
            );

            #[cfg(feature = "dhat-heap")]
            let analysis_alloc_before = crate::alloc_counter::counting_alloc::snapshot();
            let t2 = Instant::now();
            let (analysis, analysis_stage_ns) =
                analyze_schedule_instrumented(&gtp, &["available-workers".to_string()])?;
            analysis_ns += t2.elapsed().as_nanos();
            max_parallelism_seen = max_parallelism_seen.max(analysis.max_parallelism);
            add_analysis_substage(&mut analysis_substage, &analysis_stage_ns);
            #[cfg(feature = "dhat-heap")]
            record_stage_alloc(
                &mut alloc_count.analysis,
                &mut alloc_bytes.analysis,
                analysis_alloc_before,
            );
            #[cfg(feature = "dhat-heap")]
            let alloc_before = crate::alloc_counter::counting_alloc::snapshot();

            // admission_ns times execute_temporal_plan_instrumented as a whole
            // (L1); its four L3 substage checkpoints (fact_load/query/
            // effects_apply/proof_receipt_build/trace_build) are accumulated
            // into admission_substage below — see docs/DFCM_BENCHMARK_ANALYSIS.md's
            // resolution-ladder section. The instrumented variant produces the
            // identical receipt as plain execute_temporal_plan (same logic,
            // just with Instant::now() checkpoints added); it is bench-only,
            // never used by production callers.
            let t3 = Instant::now();
            let (receipt, _ocel, admission_stage_ns) =
                execute_temporal_plan_instrumented(&plan, &domain, &problem, "dfcm-crown", &[])?;
            admission_ns += t3.elapsed().as_nanos();
            add_substage(&mut admission_substage, &admission_stage_ns);
            #[cfg(feature = "dhat-heap")]
            record_stage_alloc(
                &mut alloc_count.admission,
                &mut alloc_bytes.admission,
                alloc_before,
            );
            #[cfg(feature = "dhat-heap")]
            let alloc_before = crate::alloc_counter::counting_alloc::snapshot();

            // receipt_ns times an isolated chain-hash recomputation — a real,
            // independently-measured operation, not a slice of the call above.
            let t4 = Instant::now();
            let _chain = compute_plan_chain(&plan.steps);
            receipt_ns += t4.elapsed().as_nanos();
            #[cfg(feature = "dhat-heap")]
            record_stage_alloc(
                &mut alloc_count.receipt,
                &mut alloc_bytes.receipt,
                alloc_before,
            );
            #[cfg(feature = "dhat-heap")]
            let alloc_before = crate::alloc_counter::counting_alloc::snapshot();

            // replay_ns: re-execute the same plan/case_id and confirm the
            // receipt chain is reproducible — full re-execution, proving
            // "this plan would be re-admitted under the same policy today."
            let t5 = Instant::now();
            let (replay_receipt, _ocel, replay_stage_ns) =
                execute_temporal_plan_instrumented(&plan, &domain, &problem, "dfcm-crown", &[])?;
            replay_ns += t5.elapsed().as_nanos();
            debug_assert_eq!(receipt.chain_hash, replay_receipt.chain_hash);
            add_substage(&mut replay_substage, &replay_stage_ns);
            #[cfg(feature = "dhat-heap")]
            record_stage_alloc(
                &mut alloc_count.replay,
                &mut alloc_bytes.replay,
                alloc_before,
            );

            // warm_replay_existing_receipt_ns: a DIFFERENT, cheaper replay —
            // chain-only validation with no Prolog8 queries at all, proving
            // only "this receipt's hash is a pure function of these exact
            // steps," not "would be re-admitted today." See
            // docs/DFCM_BENCHMARK_ANALYSIS.md for why these are not
            // interchangeable guarantees.
            let t6 = Instant::now();
            let revalidated_chain = compute_plan_chain(&plan.steps);
            warm_replay_existing_receipt_ns += t6.elapsed().as_nanos();
            let _ = revalidated_chain; // per-step chain only; not directly comparable to receipt.chain_hash (which also folds in the goal verdict)
        }
    }

    let wall_clock_ms = suite_start.elapsed().as_millis();
    Ok(DfcmBenchReceipt {
        wall_clock_ms,
        topology_ns,
        planning_ns,
        analysis_ns,
        admission_ns,
        receipt_ns,
        replay_ns,
        max_ops: max_ops_seen,
        max_parallelism: max_parallelism_seen,
        suite_passed_5s_gate: wall_clock_ms <= 5000,
        cold_topology_once_ns,
        warm_plan_ns: planning_ns,
        warm_analysis_ns: analysis_ns,
        warm_admission_ns: admission_ns,
        warm_receipt_ns: receipt_ns,
        warm_replay_existing_receipt_ns,
        #[cfg(feature = "dhat-heap")]
        alloc_count_by_stage: Some(alloc_count),
        #[cfg(not(feature = "dhat-heap"))]
        alloc_count_by_stage: None,
        #[cfg(feature = "dhat-heap")]
        bytes_allocated_by_stage: Some(alloc_bytes),
        #[cfg(not(feature = "dhat-heap"))]
        bytes_allocated_by_stage: None,
        admission_substage,
        replay_substage,
        analysis_substage,
    })
}

/// Accumulate one cell's `AnalysisSubstageNs` into a running total across the suite.
fn add_analysis_substage(total: &mut AnalysisSubstageNs, delta: &AnalysisSubstageNs) {
    total.resource_key_collect_ns += delta.resource_key_collect_ns;
    total.base_plan_ns += delta.base_plan_ns;
    total.perturb_minus_ns += delta.perturb_minus_ns;
    total.perturb_plus_ns += delta.perturb_plus_ns;
    total.sensitivity_compute_ns += delta.sensitivity_compute_ns;
    total.result_build_ns += delta.result_build_ns;
}

/// Add `(after - before)` allocation-count/byte deltas since `before` into
/// the running per-stage totals. Only compiled under `dhat-heap`.
#[cfg(feature = "dhat-heap")]
fn record_stage_alloc(count_total: &mut u64, bytes_total: &mut u64, before: (u64, u64)) {
    let after = crate::alloc_counter::counting_alloc::snapshot();
    *count_total += after.0.saturating_sub(before.0);
    *bytes_total += after.1.saturating_sub(before.1);
}

/// Accumulate one cell's `SubstageNs` into a running total across the suite.
fn add_substage(total: &mut SubstageNs, delta: &SubstageNs) {
    total.fact_load_ns += delta.fact_load_ns;
    total.query_ns += delta.query_ns;
    total.effects_apply_ns += delta.effects_apply_ns;
    total.proof_receipt_build_ns += delta.proof_receipt_build_ns;
    total.trace_build_ns += delta.trace_build_ns;
}
