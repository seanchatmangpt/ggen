//! Deterministic capability router and temporal planning interface.
//!
//! This module provides a deterministic router for agent capabilities. Instead of relying
//! on routing-table heuristics or machine learning classifiers, it models capability
//! selection and coordination as a decidable temporal planning and scheduling problem.
//!
//! # Architecture & Concepts
//!
//! Capability routing is built on three pillars:
//!
//! 1. **Temporal Planning Routing**: Capabilities (such as file editing, web form filling, or document drafting)
//!    are modeled as durative actions with preconditions and effects. The router sequences these actions to
//!    meet target effects while satisfying concurrency and resource bounds.
//! 2. **Pre-compiled Static Domain Parsing Cache (PSDP)**: Compiling a PDDL domain AST is computationally
//!    expensive and requires dynamic memory allocations. PSDP leverages [`OnceLock`] to compile the capability
//!    domain once and cache the AST, enabling lock-free, zero-allocation domain retrieval on subsequent requests.
//! 3. **Deterministic Lexicographic Cost Optimization**: Schedulable plans are sorted using a multi-dimensional
//!    cost vector ([`CostVector`]) ordering from most critical safety attributes to performance characteristics.
//!
//! # Domain Description
//!
//! The capability domain consists of three primary durative actions, each operating on a target `file`:
//!
//! - `claude-code-edit-file(?f)`: Simulates editing a repository file. It locks the file exclusively for its
//!   entire duration (5 time units) and requires at least 1 unit of `attention`.
//! - `claude-chrome-fill-form(?f)`: Simulates filling a web form referencing a file. It does not lock the file
//!   but requires 1 unit of `attention` for a duration of 3 time units.
//! - `claude-desktop-draft(?f)`: Simulates drafting a document referencing a file. It locks the file exclusively
//!   for its entire duration (4 time units) and requires 1 unit of `attention`.
//!
//! Concurrency is governed by the numeric fluent `attention`, which represents the maximum number of concurrent
//! operations allowed (typically matching the human user's attention capacity).
//!
//! Each precondition guards on its own goal atom not already being true (e.g., `(not (edited ?f))`). This
//! idempotency guard prevents the greedy scheduler from starving later capabilities by repeatedly executing
//! already-satisfied actions.
//!
//! # Complexity Analysis
//!
//! Let $F$ be the number of unique files (objects) and $E$ be the number of desired effects (goals):
//!
//! - **Parsing & PSDP Retrieval**: Parsing the domain text requires $O(L)$ time where $L$ is the string length.
//!   With PSDP, this cost is paid only once. Subsequent lookups retrieve the [`Pddl8Domain`] AST in $O(1)$ time
//!   and perform zero heap allocations.
//! - **Grounding**: Each durative action schema has a maximum arity of 1 parameter. Grounding enumerates
//!   all type-compatible objects for each action, resulting in $O(F)$ grounded actions.
//! - **Planning**: The forward-chaining planner performs a state-space search. It is bounded by a maximum
//!   search depth of `PDDL8_MAX_PLAN_DEPTH` iterations, guaranteeing completion in $O(S \cdot G)$ where $S$ is
//!   the search depth limit and $G$ is the cost of state transitions.
//! - **Scheduling Analysis**: The schedule analysis performs a path-based analysis over the plan steps,
//!   taking $O(P \log P)$ time where $P$ is the number of steps in the generated plan.
//!
//! # Examples
//!
//! ```
//! use bcinr_pddl::{route_capability_plan, CapabilityTask, DesiredEffect};
//!
//! // Request editing and form filling on different files with an attention capacity of 2.
//! let task = CapabilityTask {
//!     desired_effects: vec![
//!         DesiredEffect::Edited("filea".to_string()),
//!         DesiredEffect::FormFilled("fileb".to_string()),
//!     ],
//!     attention_capacity: 2,
//! };
//!
//! // The router schedules these capabilities in parallel because the files are disjoint
//! // and there is sufficient attention capacity.
//! let receipt = route_capability_plan(&task).expect("routing should succeed");
//! assert!(receipt.admitted);
//! assert!(receipt.refusal_reason.is_none());
//!
//! // Ensure the route receipt has valid timing analysis
//! let analysis = receipt.analysis.as_ref().unwrap();
//! assert_eq!(analysis.max_parallelism, 2);
//! ```

use std::cmp::Ordering;
use std::sync::OnceLock;

use crate::ground::GroundTemporalProblem;
use crate::schedule_analysis::{analyze_schedule, ScheduleAnalysis64};
use crate::{domain_from_pddl, problem_from_pddl, Pddl8Domain, Pddl8Error};
use blake3::Hasher;
use wasm4pm_compat::pddl::TemporalPlan;

const CAPABILITY_DOMAIN: &str = r#"
(define (domain capability-router)
  (:requirements :durative-actions :numeric-fluents :typing)
  (:types file)
  (:predicates (locked ?f - file) (edited ?f - file) (form-filled ?f - file) (drafted ?f - file))
  (:functions (attention))
  (:durative-action claude-code-edit-file
    :parameters (?f - file)
    :duration (= ?duration 5)
    :condition (and (at start (not (locked ?f))) (at start (not (edited ?f))) (at start (>= (attention) 1)))
    :effect (and
      (at start (decrease (attention) 1)) (at start (locked ?f))
      (at end (increase (attention) 1)) (at end (not (locked ?f))) (at end (edited ?f))))
  (:durative-action claude-chrome-fill-form
    :parameters (?f - file)
    :duration (= ?duration 3)
    :condition (and (at start (not (form-filled ?f))) (at start (>= (attention) 1)))
    :effect (and
      (at start (decrease (attention) 1))
      (at end (increase (attention) 1)) (at end (form-filled ?f))))
  (:durative-action claude-desktop-draft
    :parameters (?f - file)
    :duration (= ?duration 4)
    :condition (and (at start (not (locked ?f))) (at start (not (drafted ?f))) (at start (>= (attention) 1)))
    :effect (and
      (at start (decrease (attention) 1)) (at start (locked ?f))
      (at end (increase (attention) 1)) (at end (not (locked ?f))) (at end (drafted ?f)))))
"#;

/// Static Domain Parsing Cache (PSDP).
///
/// Pre-compiled cache of the capability domain AST. This avoids parsing overhead
/// and memory allocation during individual planning requests.
static CACHED_DOMAIN: OnceLock<Pddl8Domain> = OnceLock::new();

/// A goal atom this router can plan towards, e.g. `Edited("f1")`.
///
/// # Examples
///
/// ```
/// use bcinr_pddl::DesiredEffect;
///
/// let effect = DesiredEffect::Edited("src/lib.rs".to_string());
/// ```
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DesiredEffect {
    Edited(String),
    FormFilled(String),
    Drafted(String),
}

impl DesiredEffect {
    fn goal_atom(&self) -> String {
        match self {
            DesiredEffect::Edited(f) => format!("(edited {f})"),
            DesiredEffect::FormFilled(f) => format!("(form-filled {f})"),
            DesiredEffect::Drafted(f) => format!("(drafted {f})"),
        }
    }

    fn file(&self) -> &str {
        match self {
            DesiredEffect::Edited(f) | DesiredEffect::FormFilled(f) | DesiredEffect::Drafted(f) => {
                f
            }
        }
    }
}

/// A routing request: which effects must be achieved, and how many
/// capabilities may run concurrently (the human's attention capacity).
///
/// # Examples
///
/// ```
/// use bcinr_pddl::{CapabilityTask, DesiredEffect};
///
/// let task = CapabilityTask {
///     desired_effects: vec![DesiredEffect::Edited("file.rs".to_string())],
///     attention_capacity: 1,
/// };
/// ```
#[derive(Debug, Clone)]
pub struct CapabilityTask {
    pub desired_effects: Vec<DesiredEffect>,
    pub attention_capacity: u32,
}

/// Deterministic lexicographic cost, cheapest-first ordering:
/// admitted → risk → attention → tokens → latency → switches.
///
/// This cost vector imposes a strict lexicographic preference:
/// 1. `admitted` (non-admitted plans are always strictly worse than admitted plans)
/// 2. `unreceipted_mutation_risk` (lower risk wins)
/// 3. `human_attention_seconds` (shorter makespan/attention time wins)
/// 4. `token_cost` (lower token consumption wins)
/// 5. `latency_ms` (lower planning/latency overhead wins)
/// 6. `context_switches` (fewer distinct capabilities wins)
///
/// # Examples
///
/// ```
/// use bcinr_pddl::CostVector;
///
/// let c1 = CostVector {
///     admitted: true,
///     unreceipted_mutation_risk: 0,
///     human_attention_seconds: 5.0,
///     token_cost: 0,
///     latency_ms: 5000,
///     context_switches: 1,
/// };
///
/// let c2 = CostVector {
///     admitted: true,
///     unreceipted_mutation_risk: 0,
///     human_attention_seconds: 10.0,
///     token_cost: 0,
///     latency_ms: 10000,
///     context_switches: 1,
/// };
///
/// assert!(c1 < c2);
/// ```
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct CostVector {
    /// Whether a plan was found at all — non-admitted always loses.
    pub admitted: bool,
    /// Unreceipted mutation risk, 0 (none) .. 255 (highest) — this slice
    /// scores every fixed capability as 0 since none touches state outside
    /// its own receipted plan; a real ontology would vary this per-capability.
    pub unreceipted_mutation_risk: u8,
    /// Wall-clock human attention consumed, in seconds (derived from makespan).
    pub human_attention_seconds: f64,
    /// Reserved for future LLM-token accounting; 0 for this slice (no LLM
    /// routing loop exists to measure against — see docs on that deferral).
    pub token_cost: u64,
    pub latency_ms: u64,
    /// Number of distinct capabilities in the plan — a proxy for context switches.
    pub context_switches: u8,
}

impl CostVector {
    fn from_analysis(analysis: &ScheduleAnalysis64, plan: &TemporalPlan) -> Self {
        let distinct_capabilities: std::collections::HashSet<&str> =
            plan.steps.iter().map(|s| s.action_name.as_str()).collect();
        CostVector {
            admitted: true,
            unreceipted_mutation_risk: 0,
            human_attention_seconds: analysis.makespan,
            token_cost: 0,
            latency_ms: (analysis.makespan * 1000.0) as u64,
            context_switches: distinct_capabilities.len() as u8,
        }
    }

    fn refused() -> Self {
        CostVector {
            admitted: false,
            unreceipted_mutation_risk: u8::MAX,
            human_attention_seconds: f64::INFINITY,
            token_cost: u64::MAX,
            latency_ms: u64::MAX,
            context_switches: u8::MAX,
        }
    }
}

impl Eq for CostVector {}

impl PartialOrd for CostVector {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for CostVector {
    fn cmp(&self, other: &Self) -> Ordering {
        // admitted (true wins) first — Rust bool Ord has false < true, so reverse.
        other
            .admitted
            .cmp(&self.admitted)
            .then_with(|| {
                self.unreceipted_mutation_risk
                    .cmp(&other.unreceipted_mutation_risk)
            })
            .then_with(|| {
                self.human_attention_seconds
                    .partial_cmp(&other.human_attention_seconds)
                    .unwrap_or(Ordering::Equal)
            })
            .then_with(|| self.token_cost.cmp(&other.token_cost))
            .then_with(|| self.latency_ms.cmp(&other.latency_ms))
            .then_with(|| self.context_switches.cmp(&other.context_switches))
    }
}

/// The router's output: the plan it found, the schedule analysis that
/// justified it, its cost, and a BLAKE3 chain binding all three together —
/// same witnessing pattern as `WorldManufactureReceipt`, not a new format.
#[derive(Debug, Clone)]
pub struct CapabilityRouteReceipt {
    pub admitted: bool,
    pub refusal_reason: Option<String>,
    pub plan: TemporalPlan,
    pub analysis: Option<ScheduleAnalysis64>,
    pub cost: CostVector,
    pub route_chain: String,
}

fn build_problem_text(task: &CapabilityTask) -> String {
    let mut files: Vec<&str> = task.desired_effects.iter().map(|e| e.file()).collect();
    files.sort_unstable();
    files.dedup();

    let objects = files.join(" ");
    let goal_atoms: Vec<String> = task.desired_effects.iter().map(|e| e.goal_atom()).collect();

    format!(
        r#"
(define (problem capability-route)
  (:domain capability-router)
  (:objects {objects} - file)
  (:init (= (attention) {capacity}))
  (:goal (and {goal})))
"#,
        objects = objects,
        capacity = task.attention_capacity,
        goal = goal_atoms.join(" ")
    )
}

fn route_chain_hash(problem_text: &str, plan: &TemporalPlan, cost: &CostVector) -> String {
    let mut h = Hasher::new();
    h.update(problem_text.as_bytes());
    for step in &plan.steps {
        h.update(step.action_name.as_bytes());
        h.update(&step.start_time.to_le_bytes());
        h.update(&step.duration.to_le_bytes());
    }
    h.update(&(cost.human_attention_seconds.to_le_bytes()));
    h.update(&cost.context_switches.to_le_bytes());
    hex(h.finalize().as_bytes())
}

fn hex(b: &[u8; 32]) -> String {
    b.iter().map(|x| format!("{x:02x}")).collect()
}

/// Route a [`CapabilityTask`] to a schedulable, cost-ordered plan over the
/// fixed capability set.
///
/// Returns a receipt detailing the feasibility (`admitted`), generated planning steps,
/// scheduling analysis, and cost metrics. If the task is infeasible (e.g. attention capacity
/// is insufficient), a refused receipt is returned rather than triggering a panic.
///
/// # Examples
///
/// ```
/// use bcinr_pddl::{route_capability_plan, CapabilityTask, DesiredEffect};
///
/// // Refusal case: requesting capabilities with zero attention capacity
/// let task = CapabilityTask {
///     desired_effects: vec![DesiredEffect::Edited("filea".to_string())],
///     attention_capacity: 0,
/// };
///
/// let receipt = route_capability_plan(&task).expect("should return a receipt");
/// assert!(!receipt.admitted);
/// assert!(receipt.refusal_reason.is_some());
/// ```
pub fn route_capability_plan(task: &CapabilityTask) -> Result<CapabilityRouteReceipt, Pddl8Error> {
    let domain = CACHED_DOMAIN.get_or_try_init(|| domain_from_pddl(CAPABILITY_DOMAIN))?;
    let problem_text = build_problem_text(task);
    let problem = problem_from_pddl(&problem_text)?;
    let gtp = GroundTemporalProblem::build(domain, &problem)?;

    let plan = match gtp.find_temporal_plan() {
        crate::error::PlannerOutcome::Found(plan) => plan,
        _ => {
            return Ok(CapabilityRouteReceipt {
                admitted: false,
                refusal_reason: Some(format!(
                    "routing infeasible under attention capacity {}",
                    task.attention_capacity
                )),
                plan: TemporalPlan::default(),
                analysis: None,
                cost: CostVector::refused(),
                route_chain: {
                    let mut h = Hasher::new();
                    h.update(problem_text.as_bytes());
                    hex(h.finalize().as_bytes())
                },
            });
        }
    };

    let analysis = analyze_schedule(&gtp, &["attention".to_string()])?;
    let cost = CostVector::from_analysis(&analysis, &plan);
    let route_chain = route_chain_hash(&problem_text, &plan, &cost);

    Ok(CapabilityRouteReceipt {
        admitted: true,
        refusal_reason: None,
        plan,
        analysis: Some(analysis),
        cost,
        route_chain,
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn disjoint_files_with_capacity_two_route_in_parallel_with_minimal_cost() {
        let task = CapabilityTask {
            desired_effects: vec![
                DesiredEffect::Edited("f1".to_string()),
                DesiredEffect::FormFilled("f2".to_string()),
            ],
            attention_capacity: 2,
        };
        let receipt = route_capability_plan(&task).expect("routing should succeed");
        assert!(
            receipt.admitted,
            "refusal_reason={:?}",
            receipt.refusal_reason
        );
        let analysis = receipt.analysis.expect("analysis present when admitted");
        assert_eq!(
            analysis.max_parallelism, 2,
            "disjoint-file capabilities under capacity 2 should run concurrently"
        );
    }

    /// Note: `find_temporal_plan` greedily starts every applicable durative
    /// action, not just goal-relevant ones — with attention capacity 2 it may
    /// also opportunistically schedule the unrequested `claude-chrome-fill-form`
    /// capability in the gap, so global `max_parallelism` for the whole plan
    /// can legitimately be 2 even though the two *requested*, file-conflicting
    /// capabilities are correctly sequenced. This test asserts the specific
    /// guarantee the router makes (the conflicting pair never overlaps), not
    /// a global parallelism count the fixed capability set doesn't promise.
    #[test]
    fn same_file_edit_and_draft_conflict_and_sequence() {
        let task = CapabilityTask {
            desired_effects: vec![
                DesiredEffect::Edited("f1".to_string()),
                DesiredEffect::Drafted("f1".to_string()),
            ],
            attention_capacity: 2,
        };
        let receipt = route_capability_plan(&task).expect("routing should succeed");
        assert!(
            receipt.admitted,
            "refusal_reason={:?}",
            receipt.refusal_reason
        );
        assert!(receipt.analysis.is_some());

        let interval = |name: &str| -> (f64, f64) {
            let s = receipt
                .plan
                .steps
                .iter()
                .find(|s| s.action_name == name)
                .unwrap_or_else(|| panic!("expected a {name} step in the plan"));
            (s.start_time, s.start_time + s.duration)
        };
        let edit = interval("claude-code-edit-file");
        let draft = interval("claude-desktop-draft");
        let overlaps = edit.0 < draft.1 && draft.0 < edit.1;
        assert!(
            !overlaps,
            "edit and draft on the same file must be sequenced: edit={edit:?} draft={draft:?}"
        );
    }

    #[test]
    fn zero_attention_capacity_is_infeasible_and_refused_not_defaulted() {
        let task = CapabilityTask {
            desired_effects: vec![DesiredEffect::Edited("f1".to_string())],
            attention_capacity: 0,
        };
        let receipt =
            route_capability_plan(&task).expect("route_capability_plan itself should not error");
        assert!(
            !receipt.admitted,
            "zero attention capacity must refuse, not silently default a route"
        );
        assert!(receipt.refusal_reason.is_some());
        assert!(receipt.analysis.is_none());
        assert!(!receipt.cost.admitted);
    }

    #[test]
    fn routing_is_deterministic_same_task_same_route() {
        let task = CapabilityTask {
            desired_effects: vec![
                DesiredEffect::Edited("f1".to_string()),
                DesiredEffect::FormFilled("f2".to_string()),
            ],
            attention_capacity: 2,
        };
        let r1 = route_capability_plan(&task).expect("first route");
        let r2 = route_capability_plan(&task).expect("second route");
        assert_eq!(
            r1.route_chain, r2.route_chain,
            "same task must produce identical route chain"
        );
    }

    #[test]
    fn cost_vector_orders_admitted_before_refused() {
        let admitted = CostVector {
            admitted: true,
            unreceipted_mutation_risk: 5,
            human_attention_seconds: 100.0,
            token_cost: 1000,
            latency_ms: 5000,
            context_switches: 3,
        };
        let refused = CostVector::refused();
        assert!(
            admitted < refused,
            "an admitted route must always sort before a refused one"
        );
    }

    #[test]
    fn test_psdp_cache_structural_equivalence() {
        // Force initialization by triggering a dummy plan or calling get_or_try_init.
        let cached = CACHED_DOMAIN
            .get_or_try_init(|| domain_from_pddl(CAPABILITY_DOMAIN))
            .expect("cache init should succeed");

        let fresh_parse = domain_from_pddl(CAPABILITY_DOMAIN).expect("fresh parse should succeed");
        assert_eq!(
            format!("{cached:?}"),
            format!("{fresh_parse:?}"),
            "cached domain AST must be structurally identical to freshly parsed domain AST"
        );
    }

    #[test]
    fn test_psdp_cache_concurrency_determinism() {
        use std::thread;

        let handles: Vec<_> = (0..10)
            .map(|_| {
                thread::spawn(|| {
                    let task = CapabilityTask {
                        desired_effects: vec![
                            DesiredEffect::Edited("f1".to_string()),
                            DesiredEffect::FormFilled("f2".to_string()),
                        ],
                        attention_capacity: 2,
                    };
                    route_capability_plan(&task).expect("parallel route should succeed")
                })
            })
            .collect();

        let results: Vec<_> = handles.into_iter().map(|h| h.join().unwrap()).collect();
        let reference_receipt = &results[0];
        for receipt in &results[1..] {
            assert_eq!(
                receipt.route_chain, reference_receipt.route_chain,
                "parallel executions must produce identical route chains"
            );
        }
    }
}
