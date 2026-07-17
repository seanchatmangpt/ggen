//! Generic bound-tracking primitives shared by every bounded search stage.
//!
//! `GroundedPlanningEpoch` itself (`initial_state`, `goal`, `actions`,
//! `numeric_state`) is PDDL-shaped and is left to `bcinr-pddl` to define —
//! this module only defines the pieces that don't need to know PDDL's
//! types: the bound configuration (`EpochBounds`) and a small recursion
//! budget meter (`DescentMeter`) that any bounded-recursion routine
//! (grounding, search, projection) can reuse.

use crate::outcome::{BoundHit, BoundKind};

/// Structural bounds a grounded planning epoch must respect. Every field
/// corresponds one-to-one with a `BoundKind` variant that can be hit while
/// respecting it (`GroundActions`, `PlanDepth`, `SearchSteps`,
/// `PartitionBoxes`).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct EpochBounds {
    pub max_ground_actions: usize,
    pub max_plan_depth: usize,
    pub max_search_steps: u64,
    pub max_partition_boxes: usize,
}

/// A simple recursion budget meter: `descend()` refuses (returns
/// `Err(BoundHit)`, kind `RecursiveDescent`) once `depth` would exceed
/// `budget`, otherwise increments `depth` and returns the new value.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct DescentMeter {
    pub budget: usize,
    pub depth: usize,
}

impl DescentMeter {
    /// A fresh meter at depth 0 with the given recursion budget.
    pub const fn new(budget: usize) -> Self {
        Self { budget, depth: 0 }
    }

    /// Attempt one more level of recursion. Returns `Ok(new_depth)` and
    /// increments `depth` if the budget allows it; otherwise leaves `depth`
    /// unchanged and returns `Err(BoundHit { kind: RecursiveDescent, .. })`.
    pub fn descend(&mut self) -> Result<usize, BoundHit> {
        if self.depth >= self.budget {
            return Err(BoundHit {
                kind: BoundKind::RecursiveDescent,
                limit: self.budget as u64,
                observed: self.depth as u64,
            });
        }
        self.depth += 1;
        Ok(self.depth)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn descend_increments_up_to_budget() {
        let mut meter = DescentMeter::new(3);
        assert_eq!(meter.descend(), Ok(1));
        assert_eq!(meter.descend(), Ok(2));
        assert_eq!(meter.descend(), Ok(3));
    }

    #[test]
    fn descend_refuses_past_budget() {
        let mut meter = DescentMeter::new(1);
        assert_eq!(meter.descend(), Ok(1));
        let err = meter.descend().unwrap_err();
        assert_eq!(err.kind, BoundKind::RecursiveDescent);
        assert_eq!(err.limit, 1);
        assert_eq!(err.observed, 1);
        // Depth is not corrupted by the refused attempt.
        assert_eq!(meter.depth, 1);
    }

    #[test]
    fn zero_budget_refuses_immediately() {
        let mut meter = DescentMeter::new(0);
        assert!(meter.descend().is_err());
    }
}
