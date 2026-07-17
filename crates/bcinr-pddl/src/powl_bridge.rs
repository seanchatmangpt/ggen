//! Converts a TemporalPlan into a POWL v2 tape description.
//!
//! The result can be fed to bcinr-powl's compiler to produce an executable Powl64Op tape.

use wasm4pm_compat::pddl::TemporalPlan;

use crate::error::Pddl8Error;

/// `pred_mask`/`succ_mask` are `u64` bitmasks (bit `i` = tape slot `i`), so
/// this representation cannot address a step index past 63 — see
/// [`temporal_plan_to_powl_tape`]'s own doc comment for the refusal this
/// bound enforces, and `crate::schedule_analysis::ScheduleAnalysis64`
/// (which documents the same 64-op cap independently) for a downstream
/// consumer that already assumed this bound before it was actually
/// enforced here.
pub const MAX_POWL_TAPE_STEPS: usize = 64;

/// A single op in the POWL tape description.
#[derive(Debug, Clone)]
pub struct PowlOpSpec {
    pub kind: PowlOpKind,
    pub label: String,
    pub pred_mask: u64, // bitmask of preceding ops that must complete
    pub succ_mask: u64, // bitmask of this op (1 << index)
    pub start_time: Option<f64>,
    pub duration: Option<f64>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PowlOpKind {
    Activity,
    PartialOrderGate,
    ChoiceGate,
}

/// Convert a TemporalPlan to a POWL tape spec.
///
/// Rules:
/// - Each step becomes an Activity op
/// - Steps with overlapping time intervals get pred_mask = 0 (can execute concurrently)
/// - Steps with sequential dependencies get pred_mask = 1 << predecessor_index
/// - Steps that must all precede a later step are wrapped in a PartialOrderGate
///
/// # Bounded, not exhaustive: refuses plans longer than [`MAX_POWL_TAPE_STEPS`]
///
/// `pred_mask`/`succ_mask` are `u64` — each bit addresses one tape slot, so
/// this representation has a hard capacity of `MAX_POWL_TAPE_STEPS` (64)
/// steps. Before this check existed, `1u64 << j`/`1u64 << i` for a step
/// index ≥ 64 was a shift-amount overflow: this panicked in a debug build
/// (`overflow-checks`, which is how `make test` runs) and would have
/// silently wrapped in a release build, producing a *wrong* (not merely
/// missing) precedence mask for the affected steps — `pred_mask` would then
/// misrepresent admitted precedence rather than erroring, exactly the
/// "bounded != exhausted" hazard this workspace's own discipline warns
/// against. Refusing outright, rather than silently truncating to the
/// first 64 steps, matters because a caller (e.g.
/// `crate::schedule_analysis::analyze_schedule`) computing a critical path
/// or makespan over a silently-truncated tape would present a partial
/// analysis as if it covered the whole plan.
///
/// # Complexity
/// O(n²) in `plan.steps.len()` (all-pairs precedence + transitive-reduction
/// passes), bounded by `MAX_POWL_TAPE_STEPS` — never worse than O(64²) once
/// this refusal is in place.
pub fn temporal_plan_to_powl_tape(plan: &TemporalPlan) -> Result<Vec<PowlOpSpec>, Pddl8Error> {
    let steps = &plan.steps;
    let n = steps.len();
    if n > MAX_POWL_TAPE_STEPS {
        return Err(Pddl8Error::BoundExceeded {
            what: "powl_bridge tape steps (pred_mask/succ_mask are u64-bit-indexed)",
            limit: MAX_POWL_TAPE_STEPS,
            got: n,
        });
    }
    let mut ops = Vec::with_capacity(n);

    for (i, step) in steps.iter().enumerate() {
        // Compute pred_mask: OR of bits for all steps that MUST finish before this one starts.
        // A step j must finish before step i starts if:
        //   step_j.start_time + step_j.duration <= step_i.start_time  (sequential dependency)
        //   AND they are not concurrent (no time overlap)
        let mut pred_mask: u64 = 0;
        for (j, prev) in steps.iter().enumerate() {
            if j >= i {
                continue;
            }
            let prev_end = prev.start_time + prev.duration;
            // prev must finish before step i starts (strict sequential dependency)
            if prev_end <= step.start_time + 1e-9 {
                // Check if there is no other step between them that already covers this ordering
                pred_mask |= 1u64 << j;
            }
        }

        ops.push(PowlOpSpec {
            kind: PowlOpKind::Activity,
            label: format!("{}({})", step.action_name, step.args.join(",")),
            pred_mask,
            succ_mask: 1u64 << i,
            start_time: Some(step.start_time),
            duration: Some(step.duration),
        });
    }

    // Reduce pred_masks: remove transitive dependencies (direct predecessors only)
    // A pred j is transitive if there exists k such that j < k < i and pred[i] has bit j AND pred[k] has bit j
    for i in 0..n {
        let mut direct_mask = ops[i].pred_mask;
        for k in 0..i {
            if (ops[i].pred_mask >> k) & 1 == 1 {
                // k is a predecessor of i; remove any predecessors of k that are also in i's pred_mask
                direct_mask &= !ops[k].pred_mask;
            }
        }
        ops[i].pred_mask = direct_mask;
    }

    Ok(ops)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn step(
        action_name: &str,
        start_time: f64,
        duration: f64,
    ) -> wasm4pm_compat::pddl::TemporalPlanStep {
        wasm4pm_compat::pddl::TemporalPlanStep {
            action_name: action_name.to_string(),
            args: vec![],
            start_time,
            duration,
        }
    }

    #[test]
    fn a_plan_within_the_step_cap_lowers_successfully() {
        let plan = TemporalPlan {
            steps: vec![step("a", 0.0, 1.0), step("b", 1.0, 1.0)],
            makespan: 2.0,
            metric_value: None,
        };
        let ops = temporal_plan_to_powl_tape(&plan).unwrap();
        assert_eq!(ops.len(), 2);
        assert_eq!(ops[0].succ_mask, 1u64);
        assert_eq!(ops[1].succ_mask, 1u64 << 1);
        assert_eq!(ops[1].pred_mask, 1u64);
    }

    #[test]
    fn a_plan_at_exactly_the_step_cap_still_lowers_successfully() {
        let steps: Vec<_> = (0..MAX_POWL_TAPE_STEPS)
            .map(|i| step("a", i as f64, 1.0))
            .collect();
        let plan = TemporalPlan {
            steps,
            makespan: MAX_POWL_TAPE_STEPS as f64,
            metric_value: None,
        };
        let ops = temporal_plan_to_powl_tape(&plan).unwrap();
        assert_eq!(ops.len(), MAX_POWL_TAPE_STEPS);
        assert_eq!(ops[63].succ_mask, 1u64 << 63);
    }

    /// The exact adversarial case this fix closes: before it, this call
    /// would panic (`attempt to shift left with overflow`) in a debug
    /// build rather than returning a typed refusal.
    #[test]
    fn a_plan_past_the_step_cap_is_refused_not_panicked_or_silently_truncated() {
        let steps: Vec<_> = (0..(MAX_POWL_TAPE_STEPS + 1))
            .map(|i| step("a", i as f64, 1.0))
            .collect();
        let plan = TemporalPlan {
            steps,
            makespan: (MAX_POWL_TAPE_STEPS + 1) as f64,
            metric_value: None,
        };
        match temporal_plan_to_powl_tape(&plan) {
            Err(Pddl8Error::BoundExceeded { limit, got, .. }) => {
                assert_eq!(limit, MAX_POWL_TAPE_STEPS);
                assert_eq!(got, MAX_POWL_TAPE_STEPS + 1);
            }
            other => panic!("expected BoundExceeded, got {other:?}"),
        }
    }
}
