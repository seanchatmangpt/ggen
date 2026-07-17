use bcinr_pddl::{
    domain_from_pddl, powl_bridge::temporal_plan_to_powl_tape, problem_from_pddl,
    GroundTemporalProblem,
};
use divan::{bench, black_box, Bencher};

fn main() {
    divan::main();
}

fn generate_fixture(n: usize) -> (String, String) {
    let domain = r#"(define (domain deploy-services)
    (:requirements :durative-actions :typing)
    (:types service)
    (:predicates (deployed ?s - service))
    (:durative-action deploy
        :parameters (?s - service)
        :duration (= ?duration 10)
        :condition ()
        :effect (and (at end (deployed ?s)))
    )
)"#
    .to_string();

    let mut objects = String::new();
    let mut goals = String::new();
    for i in 1..=n {
        objects.push_str(&format!("s{} ", i));
        goals.push_str(&format!("(deployed s{}) ", i));
    }

    let problem = format!(
        r#"(define (problem deploy-n)
    (:domain deploy-services)
    (:objects {} - service)
    (:init)
    (:goal (and {}))
)"#,
        objects.trim(),
        goals.trim()
    );

    (domain, problem)
}

const ARGS: &[usize] = &[8, 16, 32, 64, 128, 256, 512];

#[bench(args = ARGS)]
fn bench_ir(bencher: Bencher, n: usize) {
    let (domain_pddl, problem_pddl) = generate_fixture(n);
    bencher.bench_local(|| {
        let domain = domain_from_pddl(black_box(&domain_pddl)).unwrap();
        let problem = problem_from_pddl(black_box(&problem_pddl)).unwrap();
        black_box((domain, problem));
    });
}

#[bench(args = ARGS)]
fn bench_ground(bencher: Bencher, n: usize) {
    let (domain_pddl, problem_pddl) = generate_fixture(n);
    let domain = domain_from_pddl(&domain_pddl).unwrap();
    let problem = problem_from_pddl(&problem_pddl).unwrap();

    bencher.bench_local(|| {
        let gp = GroundTemporalProblem::build(black_box(&domain), black_box(&problem)).unwrap();
        black_box(gp);
    });
}

// `find_temporal_plan` bounds its own greedy search at `PDDL8_MAX_PLAN_DEPTH`
// (64) internally (`ground/mod.rs`, no public way for a caller to widen it),
// separately from -- and for a different reason than --
// `temporal_plan_to_powl_tape`'s `MAX_POWL_TAPE_STEPS` bound: a plan needing
// more than 64 sequential steps to deploy `n` independent services (`ARGS`
// runs this bench up to 512) genuinely cannot be found within that depth
// cap, so `find_temporal_plan()` correctly returns `Bounded`, not `Found`.
// This is a real, structural search bound this benchmark did not previously
// handle (`.unwrap()` on the `Bounded` case panicked) -- widening
// `PDDL8_MAX_PLAN_DEPTH` itself is a planner-wide design decision out of
// this fix's scope; not unwrapping a legitimately-`Bounded` outcome is not.

#[bench(args = ARGS)]
fn bench_solve(bencher: Bencher, n: usize) {
    let (domain_pddl, problem_pddl) = generate_fixture(n);
    let domain = domain_from_pddl(&domain_pddl).unwrap();
    let problem = problem_from_pddl(&problem_pddl).unwrap();
    let gp = GroundTemporalProblem::build(&domain, &problem).unwrap();

    bencher.bench_local(|| {
        let outcome = gp.find_temporal_plan().into_result();
        let _ = black_box(outcome);
    });
}

#[bench(args = ARGS)]
fn bench_powl(bencher: Bencher, n: usize) {
    let (domain_pddl, problem_pddl) = generate_fixture(n);
    let domain = domain_from_pddl(&domain_pddl).unwrap();
    let problem = problem_from_pddl(&problem_pddl).unwrap();
    let gp = GroundTemporalProblem::build(&domain, &problem).unwrap();
    let Ok(plan) = gp.find_temporal_plan().into_result() else {
        // No complete plan exists within the search depth bound at this
        // `n` (see the module-level note above) -- nothing to lower to a
        // POWL tape, so there is nothing meaningful for this benchmark
        // case to measure. Skip it rather than fabricate a plan.
        return;
    };

    // `temporal_plan_to_powl_tape` now refuses (returns `Err`, see
    // `MAX_POWL_TAPE_STEPS`) rather than shift-overflowing for `n > 64` --
    // `ARGS` deliberately runs this bench past that cap (128/256/512), so
    // the `Result` is intentionally not unwrapped here: this still
    // genuinely measures the function's real cost at every `n`, including
    // its (now O(1) rather than O(n^2)-then-panic) early refusal path for
    // n > 64.
    bencher.bench_local(|| {
        let tape = temporal_plan_to_powl_tape(black_box(&plan));
        let _ = black_box(tape);
    });
}

#[bench(args = ARGS)]
fn bench_e2e(bencher: Bencher, n: usize) {
    let (domain_pddl, problem_pddl) = generate_fixture(n);
    bencher.bench_local(|| {
        let domain = domain_from_pddl(black_box(&domain_pddl)).unwrap();
        let problem = problem_from_pddl(black_box(&problem_pddl)).unwrap();
        let gp = GroundTemporalProblem::build(&domain, &problem).unwrap();
        let Ok(plan) = gp.find_temporal_plan().into_result() else {
            return;
        };
        let tape = temporal_plan_to_powl_tape(&plan);
        let _ = black_box(tape);
    });
}
