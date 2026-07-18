/// Plan Generation and Execution
/// Creates and executes multi-step action plans

/// Plan step (re-exported from orchestrator)
pub use crate::orchestrator::PlanStep;

/// Plan execution result (re-exported from orchestrator)
pub use crate::orchestrator::ExecutionResult;

/// Plan generator
pub struct PlanGenerator;

impl PlanGenerator {
    /// Generate a plan from priorities (used internally)
    pub fn generate_plan() -> Vec<PlanStep> {
        vec![
            PlanStep {
                id: "step-1".to_string(),
                priority: crate::orchestrator::LifeDomain::Learning,
                description: "Enroll in online course (Learning priority)".to_string(),
                tool_id: Some("course-finder".to_string()),
                tool_args: None,
                dependencies: Vec::new(),
                timeout_ms: 5000,
            },
            PlanStep {
                id: "step-2".to_string(),
                priority: crate::orchestrator::LifeDomain::Health,
                description: "Schedule workouts 4x/week (Health priority)".to_string(),
                tool_id: Some("workout".to_string()),
                tool_args: None,
                dependencies: vec!["step-1".to_string()],
                timeout_ms: 5000,
            },
            PlanStep {
                id: "step-3".to_string(),
                priority: crate::orchestrator::LifeDomain::Leisure,
                description: "Plan weekend vacation (Leisure priority)".to_string(),
                tool_id: Some("travel-planner".to_string()),
                tool_args: None,
                dependencies: vec!["step-2".to_string()],
                timeout_ms: 5000,
            },
            PlanStep {
                id: "step-4".to_string(),
                priority: crate::orchestrator::LifeDomain::Learning,
                description: "Build study schedule around work".to_string(),
                tool_id: None,
                tool_args: None,
                dependencies: vec!["step-3".to_string()],
                timeout_ms: 3000,
            },
            PlanStep {
                id: "step-5".to_string(),
                priority: crate::orchestrator::LifeDomain::Health,
                description: "Create meal prep routine".to_string(),
                tool_id: Some("meal-planner".to_string()),
                tool_args: None,
                dependencies: vec!["step-4".to_string()],
                timeout_ms: 4000,
            },
            PlanStep {
                id: "step-6".to_string(),
                priority: crate::orchestrator::LifeDomain::Career,
                description: "Schedule career coaching session".to_string(),
                tool_id: Some("career-coach".to_string()),
                tool_args: None,
                dependencies: vec!["step-5".to_string()],
                timeout_ms: 5000,
            },
            PlanStep {
                id: "step-7".to_string(),
                priority: crate::orchestrator::LifeDomain::Learning,
                description: "Review plan success metrics".to_string(),
                tool_id: None,
                tool_args: None,
                dependencies: vec!["step-6".to_string()],
                timeout_ms: 2000,
            },
        ]
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_plan_generation() {
        let plan = PlanGenerator::generate_plan();
        assert_eq!(plan.len(), 7);
        assert_eq!(plan[0].id, "step-1");
    }

    #[test]
    fn test_plan_dependencies() {
        let plan = PlanGenerator::generate_plan();
        assert!(plan[1].dependencies.contains(&"step-1".to_string()));
        assert!(plan[2].dependencies.contains(&"step-2".to_string()));
    }
}
