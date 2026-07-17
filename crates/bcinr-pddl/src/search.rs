//! Exact/exploit search rails, a fairness scheduler, and the MFW portfolio
//! that ties them together.
//!
//! # The exact/exploit split
//!
//! Only [`ExactSearchRail::step`]'s outcome enum has an `Exhausted` variant —
//! [`ExploitSearchRail::step`]'s does not. This is deliberate, not an
//! oversight: an exploit rail is heuristic and incomplete by construction
//! (see [`QLensRail`] below — greedy best-first, no backtracking), so it can
//! never *prove* a search space is exhausted; the best it can honestly say
//! is `Idle` ("I have nothing more to try right now"). Only
//! [`ExactBfsRail`], wrapping real BFS, is allowed to claim exhaustion.
//!
//! # `ExactBfsRail` is a wrapper, not a true step-wise BFS — stated plainly
//!
//! The mission brief explicitly allows this shortcut if a genuinely
//! step-wise (suspend/resume mid-frontier-expansion) refactor of
//! `GroundProblem::find_plan`'s BFS is too invasive for this phase's time
//! budget — it was. [`ExactBfsRail::step`] runs the **entire** existing
//! whole-run BFS (`GroundProblem::find_plan`, reused verbatim, not
//! reimplemented) on its first call and caches the result; every subsequent
//! `step()` just replays that cached outcome. This means
//! [`MfwPortfolio::solve`] does not actually get incremental exact progress
//! interleaved with exploit ticks *in this specific implementation* — the
//! very first time the scheduler selects `Exact`, the search either
//! completes or the whole exact side is done. The trait/scheduler/portfolio
//! machinery itself is generic and would interleave correctly with a truly
//! step-wise `ExactSearchRail`; only this one adapter takes the shortcut,
//! and it says so here rather than letting `MfwPortfolio::solve`'s doc
//! comment imply real interleaving happened.

use std::collections::BTreeSet;

use bcinr_mfw_ir::{BoundHit, ExhaustionWitness, PlannerOutcome};
use wasm4pm_compat::pddl::{Pddl8GroundAtom, Pddl8Tape};

use crate::ground::GroundProblem;
use crate::mfw::{q_lens, PositiveDistribution, PositiveMass, QValue};

// ---------------------------------------------------------------------
// Exact rail
// ---------------------------------------------------------------------

/// One step's outcome from an [`ExactSearchRail`]. Only this trait's step
/// outcome may claim `Exhausted` — see the module doc comment.
#[derive(Debug, Clone)]
pub enum ExactStepOutcome {
    /// Search continued; no terminal outcome yet.
    Progress,
    /// A plan was found.
    Found(Pddl8Tape),
    /// Search exhausted its frontier without reaching the goal — a proof no
    /// plan exists within the grounded action set.
    Exhausted(ExhaustionWitness),
    /// A structural bound was hit before a value could be found.
    Bounded(BoundHit),
}

/// A search rail that is allowed to *prove* its search space exhausted.
///
/// This is the exact/exploit split's load-bearing half: only an
/// `ExactSearchRail`'s [`ExactStepOutcome::Exhausted`] may be read as "no
/// plan exists" — see the module doc comment for why
/// [`ExploitSearchRail`] has no equivalent variant at all, not merely an
/// unused one.
pub trait ExactSearchRail {
    fn step(&mut self) -> ExactStepOutcome;
}

/// Wraps `GroundProblem::find_plan`'s existing whole-run BFS as a
/// step-callable `ExactSearchRail`. See the module doc comment for the
/// explicit, honest "wrapper, not true step-wise BFS" caveat.
pub struct ExactBfsRail<'a> {
    problem: &'a GroundProblem,
    result: Option<ExactStepOutcome>,
}

impl<'a> ExactBfsRail<'a> {
    pub fn new(problem: &'a GroundProblem) -> Self {
        Self {
            problem,
            result: None,
        }
    }
}

impl ExactSearchRail for ExactBfsRail<'_> {
    fn step(&mut self) -> ExactStepOutcome {
        if self.result.is_none() {
            let outcome = match self.problem.find_plan() {
                PlannerOutcome::Found(tape) => ExactStepOutcome::Found(tape),
                PlannerOutcome::Exhausted(w) => ExactStepOutcome::Exhausted(w),
                PlannerOutcome::Bounded(b) => ExactStepOutcome::Bounded(b),
                // `GroundProblem::find_plan` never actually constructs
                // `Unsupported`/`Inconsistent` today (grep-confirmed against
                // `ground/mod.rs`) — `ExactStepOutcome` has no slot for
                // either, so treat them as a zero-exploration exhaustion
                // rather than panicking if that ever changes upstream.
                PlannerOutcome::Unsupported(_) | PlannerOutcome::Inconsistent(_) => {
                    ExactStepOutcome::Exhausted(ExhaustionWitness {
                        search_profile: bcinr_mfw_ir::SearchProfileId(0),
                        explored_states: 0,
                        frontier_empty: true,
                        digest: bcinr_mfw_ir::Digest::ZERO,
                    })
                }
            };
            self.result = Some(outcome);
        }
        self.result.clone().expect("just set above")
    }
}

// ---------------------------------------------------------------------
// Exploit rail
// ---------------------------------------------------------------------

/// One step's outcome from an [`ExploitSearchRail`]. No `Exhausted` variant
/// — see the module doc comment.
#[derive(Debug, Clone)]
pub enum ExploitStepOutcome {
    /// Search continued; no candidate yet.
    Progress,
    /// A candidate plan was found — **not** exact-verified. `MfwPortfolio`
    /// treats this as a hint, never as a proof the search is complete.
    Candidate(Pddl8Tape),
    /// A structural bound was hit.
    Bounded(BoundHit),
    /// Nothing left to try (dead end, cycle, or goal-adjacent frontier
    /// exhausted heuristically) — deliberately *not* a claim that no plan
    /// exists, only that this rail has nothing more to offer.
    Idle,
}

/// A heuristic search rail that can never claim exhaustion.
///
/// The counterpart to [`ExactSearchRail`]: [`ExploitStepOutcome`] has no
/// `Exhausted` variant at all (not merely an unused one), because an
/// exploit rail is incomplete by construction and can only honestly say
/// [`ExploitStepOutcome::Idle`] ("nothing more to try right now") — never
/// "no plan exists." See the module doc comment for the full exact/exploit
/// split rationale.
pub trait ExploitSearchRail {
    fn step(&mut self) -> ExploitStepOutcome;
}

/// A real, non-trivial greedy best-first exploit rail: at each step, scores
/// every currently-applicable ground action by how many additional goal
/// atoms its effects would satisfy, normalizes those scores via
/// [`crate::mfw::q_lens`], and advances along the highest-weighted action.
/// No backtracking — a dead end or revisited state ends the walk (`Idle`),
/// honestly, rather than silently looping or claiming completeness.
///
/// # Complexity
///
/// One [`ExploitSearchRail::step`] call is O(A * k), where A is the
/// ground-action count (`problem.actions.len()`) and k is the average
/// precondition/effect-list size: the applicability filter and the
/// per-action scoring pass are each O(A * k), and [`crate::mfw::q_lens`]
/// is O(A) on top of that. `MfwPortfolio::solve` can tick this rail up to
/// `max_ticks` times, so the walk's total cost scales with both the
/// ground-action count and the number of ticks it survives before hitting
/// a dead end.
pub struct QLensRail<'a> {
    problem: &'a GroundProblem,
    current_state: BTreeSet<Pddl8GroundAtom>,
    plan_so_far: Vec<usize>,
    visited: BTreeSet<Vec<Pddl8GroundAtom>>,
    q: QValue,
    done: bool,
}

impl<'a> QLensRail<'a> {
    pub fn new(problem: &'a GroundProblem, q: QValue) -> Self {
        let mut visited = BTreeSet::new();
        visited.insert(problem.initial_state.iter().cloned().collect::<Vec<_>>());
        Self {
            problem,
            current_state: problem.initial_state.clone(),
            plan_so_far: Vec::new(),
            visited,
            q,
            done: false,
        }
    }
}

impl ExploitSearchRail for QLensRail<'_> {
    fn step(&mut self) -> ExploitStepOutcome {
        if self.done {
            return ExploitStepOutcome::Idle;
        }
        let goal_set: BTreeSet<Pddl8GroundAtom> = self.problem.goal.iter().cloned().collect();
        if goal_set.iter().all(|g| self.current_state.contains(g)) {
            self.done = true;
            let plan = self
                .plan_so_far
                .iter()
                .map(|&i| self.problem.actions[i].clone())
                .collect();
            return ExploitStepOutcome::Candidate(Pddl8Tape::from_plan(plan));
        }

        let applicable: Vec<usize> = self
            .problem
            .actions
            .iter()
            .enumerate()
            .filter(|(_, a)| {
                a.preconditions
                    .iter()
                    .all(|p| self.current_state.contains(p))
            })
            .map(|(i, _)| i)
            .collect();
        if applicable.is_empty() {
            self.done = true;
            return ExploitStepOutcome::Idle;
        }

        // Score: (# goal atoms this action would newly satisfy) + 1, so
        // every applicable action gets a strictly positive mass — an action
        // that satisfies zero goal atoms right now is still a legitimate
        // (if unpromising) candidate for q_lens to weigh, not an invalid
        // entry to exclude outright.
        let masses: Vec<(usize, PositiveMass)> = applicable
            .iter()
            .filter_map(|&i| {
                let action = &self.problem.actions[i];
                let newly_satisfied = action
                    .add_effects
                    .iter()
                    .filter(|e| goal_set.contains(*e) && !self.current_state.contains(*e))
                    .count();
                PositiveMass::new((newly_satisfied + 1) as f64)
                    .ok()
                    .map(|m| (i, m))
            })
            .collect();
        let Ok(dist) = PositiveDistribution::new(masses) else {
            self.done = true;
            return ExploitStepOutcome::Idle;
        };
        let Ok(weighted) = q_lens(self.q, &dist) else {
            self.done = true;
            return ExploitStepOutcome::Idle;
        };
        let &(best_idx, _) = weighted
            .entries()
            .iter()
            .max_by(|(_, a), (_, b)| a.get().partial_cmp(&b.get()).unwrap())
            .expect("dist is non-empty by construction (PositiveDistribution::new refuses empty)");

        let action = &self.problem.actions[best_idx];
        let mut next = self.current_state.clone();
        for d in &action.del_effects {
            next.remove(d);
        }
        for a in &action.add_effects {
            next.insert(a.clone());
        }
        let sorted: Vec<Pddl8GroundAtom> = next.iter().cloned().collect();
        if !self.visited.insert(sorted) {
            // Greedy walked back into an already-visited state: this rail
            // has no backtracking, so treat it as a dead end rather than
            // looping forever.
            self.done = true;
            return ExploitStepOutcome::Idle;
        }
        self.current_state = next;
        self.plan_so_far.push(best_idx);
        ExploitStepOutcome::Progress
    }
}

// ---------------------------------------------------------------------
// Fairness scheduler
// ---------------------------------------------------------------------

/// Which rail [`FairRailScheduler::select`] chose.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RailSelection {
    Exact,
    /// Index into the caller's exploit-rail list.
    Exploit(usize),
}

/// A round-robin-among-exploit-rails scheduler with a hard fairness floor:
/// the exact rail is selected at least once every `max_gap` selections,
/// regardless of how many exploit rails there are or what they return.
#[derive(Debug, Clone)]
pub struct FairRailScheduler {
    max_gap: usize,
    ticks_since_exact: usize,
    num_exploit_rails: usize,
    next_exploit: usize,
}

impl FairRailScheduler {
    pub fn new(max_gap: usize, num_exploit_rails: usize) -> Self {
        Self {
            max_gap,
            ticks_since_exact: 0,
            num_exploit_rails,
            next_exploit: 0,
        }
    }

    pub fn ticks_since_exact(&self) -> usize {
        self.ticks_since_exact
    }

    /// Choose the next rail to tick. Forces `Exact` (and resets
    /// `ticks_since_exact` to `0`) whenever there are no exploit rails at
    /// all, or `ticks_since_exact >= max_gap` — this is the fairness
    /// invariant [`tests::exact_rail_is_never_starved_beyond_max_gap`]
    /// checks directly. Otherwise round-robins among exploit rails and
    /// increments `ticks_since_exact`.
    pub fn select(&mut self) -> RailSelection {
        if self.num_exploit_rails == 0 || self.ticks_since_exact >= self.max_gap {
            self.ticks_since_exact = 0;
            RailSelection::Exact
        } else {
            let idx = self.next_exploit;
            self.next_exploit = (self.next_exploit + 1) % self.num_exploit_rails;
            self.ticks_since_exact += 1;
            RailSelection::Exploit(idx)
        }
    }
}

// ---------------------------------------------------------------------
// Portfolio
// ---------------------------------------------------------------------

/// [`MfwPortfolio::solve`]'s outcome. Every non-`Found` variant carries the
/// heuristic `Candidate` plans collected along the way (from exploit rails)
/// — never presented as verified, only as what was found before the exact
/// rail settled the question.
#[derive(Debug, Clone)]
pub enum PortfolioOutcome {
    /// The exact rail found (and therefore proved) a plan.
    Found(Pddl8Tape),
    /// The exact rail proved no plan exists.
    Exhausted(ExhaustionWitness, Vec<Pddl8Tape>),
    /// The exact rail hit a structural bound before resolving.
    Bounded(BoundHit, Vec<Pddl8Tape>),
    /// `max_ticks` elapsed before the exact rail reached a terminal
    /// outcome (only possible with a genuinely step-wise `ExactSearchRail`
    /// — `ExactBfsRail` always resolves on its first `step()`, so this
    /// variant is unreachable with that specific adapter, reachable with
    /// any truly incremental one).
    TickBudgetExhausted(Vec<Pddl8Tape>),
}

/// Ties one [`ExactSearchRail`], zero or more [`ExploitSearchRail`]s, and a
/// [`FairRailScheduler`] together into one bounded search loop.
pub struct MfwPortfolio<E: ExactSearchRail, X: ExploitSearchRail> {
    exact: E,
    exploit: Vec<X>,
    scheduler: FairRailScheduler,
    max_ticks: usize,
}

impl<E: ExactSearchRail, X: ExploitSearchRail> MfwPortfolio<E, X> {
    pub fn new(exact: E, exploit: Vec<X>, max_gap: usize, max_ticks: usize) -> Self {
        let scheduler = FairRailScheduler::new(max_gap, exploit.len());
        Self {
            exact,
            exploit,
            scheduler,
            max_ticks,
        }
    }

    /// Tick the scheduler-selected rail up to `max_ticks` times. Ends the
    /// moment the exact rail reports a terminal outcome (`Found`,
    /// `Exhausted`, or `Bounded`) — only the exact rail can end the search;
    /// exploit `Candidate`s are collected, never treated as terminal.
    pub fn solve(&mut self) -> PortfolioOutcome {
        let mut candidates = Vec::new();
        for _ in 0..self.max_ticks {
            match self.scheduler.select() {
                RailSelection::Exact => match self.exact.step() {
                    ExactStepOutcome::Found(tape) => return PortfolioOutcome::Found(tape),
                    ExactStepOutcome::Exhausted(w) => {
                        return PortfolioOutcome::Exhausted(w, candidates)
                    }
                    ExactStepOutcome::Bounded(b) => {
                        return PortfolioOutcome::Bounded(b, candidates)
                    }
                    ExactStepOutcome::Progress => {}
                },
                RailSelection::Exploit(idx) => {
                    if let Some(rail) = self.exploit.get_mut(idx) {
                        if let ExploitStepOutcome::Candidate(tape) = rail.step() {
                            candidates.push(tape);
                        }
                    }
                }
            }
        }
        PortfolioOutcome::TickBudgetExhausted(candidates)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parse::{domain_from_pddl, problem_from_pddl};

    const DOMAIN: &str = "(define (domain d) (:predicates (p) (q) (r)) \
                           (:action a1 :parameters () :precondition (p) :effect (q)) \
                           (:action a2 :parameters () :precondition (q) :effect (r)))";
    const PROBLEM: &str = "(define (problem pr) (:domain d) (:init (p)) (:goal (r)))";

    #[test]
    fn exact_bfs_rail_finds_a_plan_on_first_step() {
        let domain = domain_from_pddl(DOMAIN).unwrap();
        let problem = problem_from_pddl(PROBLEM).unwrap();
        let gp = GroundProblem::build(&domain, &problem, None).unwrap();
        let mut rail = ExactBfsRail::new(&gp);
        match rail.step() {
            ExactStepOutcome::Found(tape) => assert_eq!(tape.ops.len(), 2),
            other => panic!("expected Found, got {other:?}"),
        }
        // Second step replays the cached result — proving the "wrapper, not
        // step-wise" caveat is real, not just documented.
        match rail.step() {
            ExactStepOutcome::Found(tape) => assert_eq!(tape.ops.len(), 2),
            other => panic!("expected Found again, got {other:?}"),
        }
    }

    #[test]
    fn exact_bfs_rail_reports_exhausted_witness_for_an_infeasible_problem() {
        let domain = domain_from_pddl(DOMAIN).unwrap();
        let infeasible = "(define (problem pr2) (:domain d) (:init) (:goal (r)))";
        let problem = problem_from_pddl(infeasible).unwrap();
        let gp = GroundProblem::build(&domain, &problem, None).unwrap();
        let mut rail = ExactBfsRail::new(&gp);
        match rail.step() {
            ExactStepOutcome::Exhausted(w) => assert!(w.frontier_empty),
            other => panic!("expected Exhausted, got {other:?}"),
        }
    }

    #[test]
    fn qlens_rail_makes_real_progress_and_finds_a_candidate() {
        let domain = domain_from_pddl(DOMAIN).unwrap();
        let problem = problem_from_pddl(PROBLEM).unwrap();
        let gp = GroundProblem::build(&domain, &problem, None).unwrap();
        let mut rail = QLensRail::new(&gp, QValue::new(1.0).unwrap());
        // Hop 1: applies a1 (p -> q); goal (r) not yet checked-true this
        // call (the goal check happens at the *start* of the next step()).
        let first = rail.step();
        assert!(
            matches!(first, ExploitStepOutcome::Progress),
            "expected Progress on the first hop, got {first:?}"
        );
        // Hop 2: both a1 (still applicable — it never deletes p) and a2 (q
        // -> r, now applicable) are candidates; a2's add_effects intersect
        // the goal so q_lens must weight it higher and QLensRail applies it.
        let second = rail.step();
        assert!(
            matches!(second, ExploitStepOutcome::Progress),
            "expected Progress on the second hop, got {second:?}"
        );
        // Hop 3: the goal check at the start of this step() now sees (r)
        // satisfied by the plan built from the previous two hops.
        let third = rail.step();
        match third {
            ExploitStepOutcome::Candidate(tape) => assert_eq!(tape.ops.len(), 2),
            other => panic!("expected Candidate after two real hops, got {other:?}"),
        }
    }

    #[test]
    fn qlens_rail_reports_idle_not_exhausted_on_a_dead_end() {
        // No action ever satisfies (r) and (p) is never re-achievable —
        // a genuine dead end for a no-backtracking greedy rail.
        let dead_end_domain = "(define (domain d) (:predicates (p) (q)) \
             (:action a :parameters () :precondition (p) :effect (and (not (p)) (q))))";
        let dead_end_problem = "(define (problem pr) (:domain d) (:init (p)) (:goal (r)))";
        let domain = domain_from_pddl(dead_end_domain).unwrap();
        // (r) is never declared reachable; GroundProblem::build only fails
        // on EmptyGrounding, not on an unreachable goal atom, so this still
        // builds — it just never gets scheduled toward (r).
        let problem = problem_from_pddl(dead_end_problem).unwrap();
        let gp = GroundProblem::build(&domain, &problem, None).unwrap();
        let mut rail = QLensRail::new(&gp, QValue::new(1.0).unwrap());
        let _ = rail.step(); // consumes the one applicable action
        let outcome = rail.step();
        assert!(
            matches!(outcome, ExploitStepOutcome::Idle),
            "expected Idle (not Exhausted — only the exact rail may claim that), got {outcome:?}"
        );
    }

    /// The fairness invariant: across many selections, with exploit rails
    /// present the whole time (i.e. never dropping to zero, which would
    /// force Exact unconditionally for a different reason), the exact rail
    /// is never starved beyond `max_gap` consecutive exploit selections.
    #[test]
    fn exact_rail_is_never_starved_beyond_max_gap() {
        let max_gap = 3;
        let mut sched = FairRailScheduler::new(max_gap, 2);
        let mut since_last_exact = 0usize;
        for _ in 0..200 {
            match sched.select() {
                RailSelection::Exact => since_last_exact = 0,
                RailSelection::Exploit(idx) => {
                    assert!(idx < 2, "exploit index out of range");
                    since_last_exact += 1;
                    assert!(
                        since_last_exact <= max_gap,
                        "exact rail starved: {since_last_exact} consecutive exploit ticks > \
                         max_gap={max_gap}"
                    );
                }
            }
        }
    }

    #[test]
    fn ticks_since_exact_resets_on_exact_selection() {
        // max_gap=2: exact fires once ticks_since_exact reaches 2, i.e.
        // after two exploit selections, not one.
        let mut sched = FairRailScheduler::new(2, 1);
        assert_eq!(sched.select(), RailSelection::Exploit(0));
        assert_eq!(sched.ticks_since_exact(), 1);
        assert_eq!(sched.select(), RailSelection::Exploit(0));
        assert_eq!(sched.ticks_since_exact(), 2);
        assert_eq!(sched.select(), RailSelection::Exact);
        assert_eq!(sched.ticks_since_exact(), 0);
    }

    #[test]
    fn scheduler_forces_exact_when_there_are_no_exploit_rails() {
        let mut sched = FairRailScheduler::new(10, 0);
        for _ in 0..5 {
            assert_eq!(sched.select(), RailSelection::Exact);
        }
    }

    #[test]
    fn portfolio_solve_finds_a_plan_via_the_exact_rail() {
        let domain = domain_from_pddl(DOMAIN).unwrap();
        let problem = problem_from_pddl(PROBLEM).unwrap();
        let gp = GroundProblem::build(&domain, &problem, None).unwrap();
        let exact = ExactBfsRail::new(&gp);
        let exploit = vec![QLensRail::new(&gp, QValue::new(1.0).unwrap())];
        let mut portfolio = MfwPortfolio::new(exact, exploit, 2, 10);
        match portfolio.solve() {
            PortfolioOutcome::Found(tape) => assert_eq!(tape.ops.len(), 2),
            other => panic!("expected Found, got {other:?}"),
        }
    }

    #[test]
    fn portfolio_solve_reports_exhausted_with_no_candidates_for_infeasible_problem() {
        let domain = domain_from_pddl(DOMAIN).unwrap();
        let infeasible = "(define (problem pr2) (:domain d) (:init) (:goal (r)))";
        let problem = problem_from_pddl(infeasible).unwrap();
        let gp = GroundProblem::build(&domain, &problem, None).unwrap();
        let exact = ExactBfsRail::new(&gp);
        let exploit: Vec<QLensRail> = vec![];
        let mut portfolio = MfwPortfolio::new(exact, exploit, 2, 10);
        match portfolio.solve() {
            PortfolioOutcome::Exhausted(_, candidates) => assert!(candidates.is_empty()),
            other => panic!("expected Exhausted, got {other:?}"),
        }
    }
}
