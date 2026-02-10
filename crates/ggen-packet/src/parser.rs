use crate::{
    AcceptanceCriterion, AcceptanceTest, Constraint, ConstraintType, Priority,
    ReversibilityPolicy, TestType, WorkOrder, WorkOrderId, WorkOrderStatus,
};
use serde_json::Value;
use std::collections::HashSet;
use thiserror::Error;

/// Parser errors
#[derive(Debug, Error)]
pub enum ParseError {
    /// JSON parsing error
    #[error("JSON parse error: {0}")]
    JsonError(#[from] serde_json::Error),

    /// Required field missing
    #[error("Missing required field: {0}")]
    MissingField(String),

    /// Invalid field value
    #[error("Invalid field value: {0}")]
    InvalidValue(String),

    /// Validation error after parsing
    #[error("Validation error: {0}")]
    ValidationError(#[from] crate::validation::ValidationError),
}

/// Parse work order from JSON string
///
/// # Errors
///
/// Returns `ParseError` if parsing or validation fails
pub fn parse_work_order(json: &str) -> Result<WorkOrder, ParseError> {
    let value: Value = serde_json::from_str(json)?;
    parse_work_order_from_value(&value)
}

/// Parse work order from JSON value
///
/// # Errors
///
/// Returns `ParseError` if parsing or validation fails
pub fn parse_work_order_from_value(value: &Value) -> Result<WorkOrder, ParseError> {
    let objective = value
        .get("objective")
        .and_then(Value::as_str)
        .ok_or_else(|| ParseError::MissingField(String::from("objective")))?
        .to_string();

    let owner = value
        .get("owner")
        .and_then(Value::as_str)
        .ok_or_else(|| ParseError::MissingField(String::from("owner")))?
        .to_string();

    let mut work_order = WorkOrder::new(objective, owner)?;

    // Parse optional fields
    if let Some(constraints) = value.get("constraints").and_then(Value::as_array) {
        for constraint_value in constraints {
            let constraint = parse_constraint(constraint_value)?;
            work_order = work_order.with_constraint(constraint)?;
        }
    }

    if let Some(acceptance_test) = value.get("acceptance_test") {
        let test = parse_acceptance_test(acceptance_test)?;
        work_order = work_order.with_acceptance_test(test)?;
    }

    if let Some(reversibility) = value.get("reversibility") {
        let policy = parse_reversibility_policy(reversibility)?;
        work_order = work_order.with_reversibility(policy)?;
    }

    if let Some(dependencies) = value.get("dependencies").and_then(Value::as_array) {
        let deps: Vec<String> = dependencies
            .iter()
            .filter_map(Value::as_str)
            .map(String::from)
            .collect();
        work_order = work_order.with_dependencies(deps)?;
    }

    if let Some(priority_str) = value.get("priority").and_then(Value::as_str) {
        let priority = parse_priority(priority_str)?;
        work_order = work_order.with_priority(priority)?;
    }

    if let Some(tags) = value.get("tags").and_then(Value::as_array) {
        let tag_set: HashSet<String> = tags
            .iter()
            .filter_map(Value::as_str)
            .map(String::from)
            .collect();
        work_order = work_order.with_tags(tag_set)?;
    }

    if let Some(status_str) = value.get("status").and_then(Value::as_str) {
        let status = parse_status(status_str)?;
        work_order.status = status;
    }

    if let Some(id_str) = value.get("id").and_then(Value::as_str) {
        if let Ok(uuid) = id_str.parse() {
            work_order.id = WorkOrderId::from_uuid(uuid);
        }
    }

    Ok(work_order)
}

fn parse_constraint(value: &Value) -> Result<Constraint, ParseError> {
    let description = value
        .get("description")
        .and_then(Value::as_str)
        .ok_or_else(|| ParseError::MissingField(String::from("constraint.description")))?
        .to_string();

    let constraint_type_str = value
        .get("type")
        .and_then(Value::as_str)
        .ok_or_else(|| ParseError::MissingField(String::from("constraint.type")))?;

    let constraint_type = parse_constraint_type(constraint_type_str)?;

    let enforced = value
        .get("enforced")
        .and_then(Value::as_bool)
        .unwrap_or(true);

    Ok(Constraint {
        description,
        constraint_type,
        enforced,
    })
}

fn parse_constraint_type(s: &str) -> Result<ConstraintType, ParseError> {
    match s.to_lowercase().as_str() {
        "time" => Ok(ConstraintType::Time),
        "resource" => Ok(ConstraintType::Resource),
        "quality" => Ok(ConstraintType::Quality),
        "safety" => Ok(ConstraintType::Safety),
        "compliance" => Ok(ConstraintType::Compliance),
        "budget" => Ok(ConstraintType::Budget),
        "dependency" => Ok(ConstraintType::Dependency),
        _ => Err(ParseError::InvalidValue(format!(
            "Unknown constraint type: {s}"
        ))),
    }
}

fn parse_acceptance_test(value: &Value) -> Result<AcceptanceTest, ParseError> {
    let description = value
        .get("description")
        .and_then(Value::as_str)
        .ok_or_else(|| ParseError::MissingField(String::from("acceptance_test.description")))?
        .to_string();

    let criteria_array = value
        .get("criteria")
        .and_then(Value::as_array)
        .ok_or_else(|| ParseError::MissingField(String::from("acceptance_test.criteria")))?;

    let criteria: Result<Vec<AcceptanceCriterion>, ParseError> = criteria_array
        .iter()
        .map(parse_acceptance_criterion)
        .collect();

    let test_type_str = value
        .get("test_type")
        .and_then(Value::as_str)
        .unwrap_or("manual");

    let test_type = parse_test_type(test_type_str)?;

    Ok(AcceptanceTest {
        description,
        criteria: criteria?,
        test_type,
    })
}

fn parse_acceptance_criterion(value: &Value) -> Result<AcceptanceCriterion, ParseError> {
    let criterion = value
        .get("criterion")
        .and_then(Value::as_str)
        .ok_or_else(|| ParseError::MissingField(String::from("criterion.criterion")))?
        .to_string();

    let required = value
        .get("required")
        .and_then(Value::as_bool)
        .unwrap_or(true);

    let measurable = value
        .get("measurable")
        .and_then(Value::as_bool)
        .unwrap_or(false);

    Ok(AcceptanceCriterion {
        criterion,
        required,
        measurable,
    })
}

fn parse_test_type(s: &str) -> Result<TestType, ParseError> {
    match s.to_lowercase().as_str() {
        "manual" => Ok(TestType::Manual),
        "automated" => Ok(TestType::Automated),
        "hybrid" => Ok(TestType::Hybrid),
        _ => Err(ParseError::InvalidValue(format!("Unknown test type: {s}"))),
    }
}

fn parse_reversibility_policy(value: &Value) -> Result<ReversibilityPolicy, ParseError> {
    let reversible = value
        .get("reversible")
        .and_then(Value::as_bool)
        .unwrap_or(false);

    let rollback_steps = value
        .get("rollback_steps")
        .and_then(Value::as_array)
        .map(|arr| {
            arr.iter()
                .filter_map(Value::as_str)
                .map(String::from)
                .collect()
        })
        .unwrap_or_default();

    let backup_required = value
        .get("backup_required")
        .and_then(Value::as_bool)
        .unwrap_or(false);

    let verification_required = value
        .get("verification_required")
        .and_then(Value::as_bool)
        .unwrap_or(false);

    Ok(ReversibilityPolicy {
        reversible,
        rollback_steps,
        backup_required,
        verification_required,
    })
}

fn parse_priority(s: &str) -> Result<Priority, ParseError> {
    match s.to_lowercase().as_str() {
        "critical" => Ok(Priority::Critical),
        "high" => Ok(Priority::High),
        "normal" => Ok(Priority::Normal),
        "low" => Ok(Priority::Low),
        _ => Err(ParseError::InvalidValue(format!("Unknown priority: {s}"))),
    }
}

fn parse_status(s: &str) -> Result<WorkOrderStatus, ParseError> {
    match s.to_lowercase().as_str() {
        "pending" => Ok(WorkOrderStatus::Pending),
        "validated" => Ok(WorkOrderStatus::Validated),
        "inprogress" | "in_progress" => Ok(WorkOrderStatus::InProgress),
        "blocked" => Ok(WorkOrderStatus::Blocked),
        "completed" => Ok(WorkOrderStatus::Completed),
        "cancelled" => Ok(WorkOrderStatus::Cancelled),
        "failed" => Ok(WorkOrderStatus::Failed),
        _ => Err(ParseError::InvalidValue(format!("Unknown status: {s}"))),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_minimal_work_order() {
        let json = r#"{
            "objective": "Complete task",
            "owner": "team@example.com"
        }"#;

        let result = parse_work_order(json);
        assert!(result.is_ok());

        let wo = result.ok().unwrap();
        assert_eq!(wo.objective, "Complete task");
        assert_eq!(wo.owner, "team@example.com");
    }

    #[test]
    fn test_parse_full_work_order() {
        let json = r#"{
            "objective": "Deploy feature X",
            "owner": "devops@example.com",
            "priority": "high",
            "status": "validated",
            "tags": ["deployment", "backend"],
            "constraints": [
                {
                    "description": "Must complete within 2 hours",
                    "type": "time",
                    "enforced": true
                }
            ],
            "acceptance_test": {
                "description": "Deployment verification",
                "criteria": [
                    {
                        "criterion": "All services healthy",
                        "required": true,
                        "measurable": true
                    }
                ],
                "test_type": "automated"
            },
            "reversibility": {
                "reversible": true,
                "rollback_steps": ["Restore previous version"],
                "backup_required": true,
                "verification_required": true
            },
            "dependencies": ["feature-y", "feature-z"]
        }"#;

        let result = parse_work_order(json);
        assert!(result.is_ok());

        let wo = result.ok().unwrap();
        assert_eq!(wo.objective, "Deploy feature X");
        assert_eq!(wo.priority, Priority::High);
        assert_eq!(wo.status, WorkOrderStatus::Validated);
        assert_eq!(wo.constraints.len(), 1);
        assert_eq!(wo.dependencies.len(), 2);
        assert!(wo.reversibility.reversible);
    }

    #[test]
    fn test_parse_missing_objective() {
        let json = r#"{"owner": "team@example.com"}"#;
        let result = parse_work_order(json);
        assert!(result.is_err());
    }

    #[test]
    fn test_parse_missing_owner() {
        let json = r#"{"objective": "Task"}"#;
        let result = parse_work_order(json);
        assert!(result.is_err());
    }

    #[test]
    fn test_parse_invalid_priority() {
        let json = r#"{
            "objective": "Task",
            "owner": "owner",
            "priority": "super-urgent"
        }"#;
        let result = parse_work_order(json);
        assert!(result.is_err());
    }

    #[test]
    fn test_parse_invalid_constraint_type() {
        let json = r#"{
            "objective": "Task",
            "owner": "owner",
            "constraints": [
                {
                    "description": "Test",
                    "type": "invalid_type",
                    "enforced": true
                }
            ]
        }"#;
        let result = parse_work_order(json);
        assert!(result.is_err());
    }

    #[test]
    fn test_parse_constraint_types() {
        assert!(parse_constraint_type("time").is_ok());
        assert!(parse_constraint_type("resource").is_ok());
        assert!(parse_constraint_type("quality").is_ok());
        assert!(parse_constraint_type("safety").is_ok());
        assert!(parse_constraint_type("compliance").is_ok());
        assert!(parse_constraint_type("budget").is_ok());
        assert!(parse_constraint_type("dependency").is_ok());
    }

    #[test]
    fn test_parse_priorities() {
        assert_eq!(parse_priority("critical").ok().unwrap(), Priority::Critical);
        assert_eq!(parse_priority("high").ok().unwrap(), Priority::High);
        assert_eq!(parse_priority("normal").ok().unwrap(), Priority::Normal);
        assert_eq!(parse_priority("low").ok().unwrap(), Priority::Low);
    }

    #[test]
    fn test_parse_statuses() {
        assert_eq!(parse_status("pending").ok().unwrap(), WorkOrderStatus::Pending);
        assert_eq!(parse_status("validated").ok().unwrap(), WorkOrderStatus::Validated);
        assert_eq!(parse_status("inprogress").ok().unwrap(), WorkOrderStatus::InProgress);
        assert_eq!(parse_status("in_progress").ok().unwrap(), WorkOrderStatus::InProgress);
        assert_eq!(parse_status("blocked").ok().unwrap(), WorkOrderStatus::Blocked);
        assert_eq!(parse_status("completed").ok().unwrap(), WorkOrderStatus::Completed);
        assert_eq!(parse_status("cancelled").ok().unwrap(), WorkOrderStatus::Cancelled);
        assert_eq!(parse_status("failed").ok().unwrap(), WorkOrderStatus::Failed);
    }
}
