use crate::{WorkOrder, Constraint, AcceptanceTest, AcceptanceCriterion};
use thiserror::Error;
use validator::Validate;

/// Validation errors for work orders and packets
#[derive(Debug, Error)]
pub enum ValidationError {
    /// Invalid work order structure
    #[error("Invalid work order: {0}")]
    InvalidWorkOrder(String),

    /// Structural validation failed
    #[error("Validation failed: {0}")]
    ValidationFailed(#[from] validator::ValidationErrors),

    /// Business rule violated
    #[error("Business rule violation: {0}")]
    BusinessRuleViolation(String),

    /// Constraint violated
    #[error("Constraint violation: {0}")]
    ConstraintViolation(String),
}

/// Validate a work order at the boundary
///
/// # Errors
///
/// Returns `ValidationError` if validation fails
pub fn validate_work_order(work_order: &WorkOrder) -> Result<(), ValidationError> {
    // Structural validation using validator
    work_order.validate()?;

    // Business rule validation
    validate_business_rules(work_order)?;

    // Constraint validation
    validate_constraints(&work_order.constraints)?;

    // Acceptance test validation
    validate_acceptance_test(&work_order.acceptance_test)?;

    // Dependency validation
    validate_dependencies(&work_order.dependencies)?;

    Ok(())
}

fn validate_business_rules(work_order: &WorkOrder) -> Result<(), ValidationError> {
    // Objective must not be empty or whitespace only
    if work_order.objective.trim().is_empty() {
        return Err(ValidationError::BusinessRuleViolation(
            String::from("Objective cannot be empty or whitespace"),
        ));
    }

    // Owner must not be empty or whitespace only
    if work_order.owner.trim().is_empty() {
        return Err(ValidationError::BusinessRuleViolation(
            String::from("Owner cannot be empty or whitespace"),
        ));
    }

    // Reversible work orders must have rollback steps
    if work_order.reversibility.reversible && work_order.reversibility.rollback_steps.is_empty() {
        return Err(ValidationError::BusinessRuleViolation(
            String::from("Reversible work orders must define rollback steps"),
        ));
    }

    // If backup is required, work order must be reversible
    if work_order.reversibility.backup_required && !work_order.reversibility.reversible {
        return Err(ValidationError::BusinessRuleViolation(
            String::from("Backup requirement implies reversibility"),
        ));
    }

    // Tags must not exceed reasonable limit
    if work_order.tags.len() > 20 {
        return Err(ValidationError::BusinessRuleViolation(
            String::from("Too many tags (max 20)"),
        ));
    }

    Ok(())
}

fn validate_constraints(constraints: &[Constraint]) -> Result<(), ValidationError> {
    for constraint in constraints {
        constraint.validate()?;

        // Constraint description must not be empty or whitespace
        if constraint.description.trim().is_empty() {
            return Err(ValidationError::ConstraintViolation(
                String::from("Constraint description cannot be empty or whitespace"),
            ));
        }
    }

    // Check for duplicate constraints
    let unique_count = constraints.iter().collect::<std::collections::HashSet<_>>().len();
    if unique_count != constraints.len() {
        return Err(ValidationError::ConstraintViolation(
            String::from("Duplicate constraints detected"),
        ));
    }

    Ok(())
}

fn validate_acceptance_test(test: &AcceptanceTest) -> Result<(), ValidationError> {
    test.validate()?;

    // Acceptance test description must not be empty
    if test.description.trim().is_empty() {
        return Err(ValidationError::ValidationFailed(
            validator::ValidationErrors::new(),
        ));
    }

    // Must have at least one criterion
    if test.criteria.is_empty() {
        return Err(ValidationError::BusinessRuleViolation(
            String::from("Acceptance test must have at least one criterion"),
        ));
    }

    // Validate each criterion
    for criterion in &test.criteria {
        validate_acceptance_criterion(criterion)?;
    }

    // At least one required criterion must exist
    if !test.criteria.iter().any(|c| c.required) {
        return Err(ValidationError::BusinessRuleViolation(
            String::from("Acceptance test must have at least one required criterion"),
        ));
    }

    Ok(())
}

fn validate_acceptance_criterion(criterion: &AcceptanceCriterion) -> Result<(), ValidationError> {
    criterion.validate()?;

    if criterion.criterion.trim().is_empty() {
        return Err(ValidationError::BusinessRuleViolation(
            String::from("Acceptance criterion cannot be empty or whitespace"),
        ));
    }

    Ok(())
}

fn validate_dependencies(dependencies: &[String]) -> Result<(), ValidationError> {
    for dep in dependencies {
        if dep.trim().is_empty() {
            return Err(ValidationError::BusinessRuleViolation(
                String::from("Dependencies cannot contain empty or whitespace strings"),
            ));
        }
    }

    // Check for duplicate dependencies
    let unique_count = dependencies.iter().collect::<std::collections::HashSet<_>>().len();
    if unique_count != dependencies.len() {
        return Err(ValidationError::BusinessRuleViolation(
            String::from("Duplicate dependencies detected"),
        ));
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{WorkOrder, Constraint, ConstraintType, AcceptanceTest, AcceptanceCriterion, TestType, ReversibilityPolicy};

    #[test]
    fn test_valid_work_order() {
        let wo = WorkOrder::new(
            String::from("Valid objective"),
            String::from("valid@owner.com"),
        )
        .ok()
        .unwrap();

        assert!(validate_work_order(&wo).is_ok());
    }

    #[test]
    fn test_empty_objective() {
        let result = WorkOrder::new(String::new(), String::from("owner"));
        assert!(result.is_err());
    }

    #[test]
    fn test_whitespace_objective() {
        let result = WorkOrder::new(String::from("   "), String::from("owner"));
        assert!(result.is_err());
    }

    #[test]
    fn test_empty_owner() {
        let result = WorkOrder::new(String::from("objective"), String::new());
        assert!(result.is_err());
    }

    #[test]
    fn test_reversible_without_rollback() {
        let wo = WorkOrder::new(
            String::from("Task"),
            String::from("owner"),
        )
        .ok()
        .unwrap();

        let policy = ReversibilityPolicy {
            reversible: true,
            rollback_steps: Vec::new(),
            backup_required: false,
            verification_required: false,
        };

        let result = wo.with_reversibility(policy);
        assert!(result.is_err());
    }

    #[test]
    fn test_backup_without_reversibility() {
        let wo = WorkOrder::new(
            String::from("Task"),
            String::from("owner"),
        )
        .ok()
        .unwrap();

        let policy = ReversibilityPolicy {
            reversible: false,
            rollback_steps: Vec::new(),
            backup_required: true,
            verification_required: false,
        };

        let result = wo.with_reversibility(policy);
        assert!(result.is_err());
    }

    #[test]
    fn test_duplicate_constraints() {
        let wo = WorkOrder::new(
            String::from("Task"),
            String::from("owner"),
        )
        .ok()
        .unwrap();

        let constraint = Constraint {
            description: String::from("Same constraint"),
            constraint_type: ConstraintType::Time,
            enforced: true,
        };

        let result = wo
            .with_constraint(constraint.clone())
            .ok()
            .unwrap()
            .with_constraint(constraint);

        assert!(result.is_err());
    }

    #[test]
    fn test_empty_acceptance_criteria() {
        let test = AcceptanceTest {
            description: String::from("Test"),
            criteria: Vec::new(),
            test_type: TestType::Manual,
        };

        assert!(validate_acceptance_test(&test).is_err());
    }

    #[test]
    fn test_no_required_criteria() {
        let test = AcceptanceTest {
            description: String::from("Test"),
            criteria: vec![AcceptanceCriterion {
                criterion: String::from("Optional criterion"),
                required: false,
                measurable: true,
            }],
            test_type: TestType::Manual,
        };

        assert!(validate_acceptance_test(&test).is_err());
    }

    #[test]
    fn test_duplicate_dependencies() {
        let wo = WorkOrder::new(
            String::from("Task"),
            String::from("owner"),
        )
        .ok()
        .unwrap();

        let result = wo.with_dependencies(vec![
            String::from("dep1"),
            String::from("dep2"),
            String::from("dep1"),
        ]);

        assert!(result.is_err());
    }

    #[test]
    fn test_empty_dependency() {
        let wo = WorkOrder::new(
            String::from("Task"),
            String::from("owner"),
        )
        .ok()
        .unwrap();

        let result = wo.with_dependencies(vec![String::from("dep1"), String::new()]);
        assert!(result.is_err());
    }

    #[test]
    fn test_constraint_description_validation() {
        let constraint = Constraint {
            description: String::from("   "),
            constraint_type: ConstraintType::Quality,
            enforced: true,
        };

        assert!(validate_constraints(&[constraint]).is_err());
    }

    #[test]
    fn test_acceptance_criterion_validation() {
        let criterion = AcceptanceCriterion {
            criterion: String::from("   "),
            required: true,
            measurable: true,
        };

        assert!(validate_acceptance_criterion(&criterion).is_err());
    }
}
