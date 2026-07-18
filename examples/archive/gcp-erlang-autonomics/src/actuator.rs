//! Actuator - safe action execution engine
//!
//! This module implements the **Execute** phase of MAPE-K:
//! - Safe action execution with pre-flight checks
//! - Rollback capability with receipt-based audit
//! - Resource isolation (action failures don't cascade)
//! - Async/await for resource efficiency

use crate::receipt::ReceiptLedger;
use thiserror::Error;
use serde::{Deserialize, Serialize};
use std::time::Duration;

/// Actuator execution errors
#[derive(Debug, Error)]
pub enum ActuatorError {
    #[error("Action execution timeout after {duration_secs}s")]
    ExecutionTimeout { duration_secs: u64 },

    #[error("Action execution failed: {0}")]
    ExecutionFailed(String),

    #[error("Rollback failed: {reason}")]
    RollbackFailed { reason: String },

    #[error("Invalid action: {0}")]
    InvalidAction(String),

    #[error("Receipt not found: {receipt_id}")]
    ReceiptNotFound { receipt_id: String },

    #[error("Action already executing")]
    AlreadyExecuting,
}

/// Supported actions
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Action {
    /// Throttle requests (percentage 0-100)
    Throttle(u32),
    /// Pause service temporarily
    Pause,
    /// Rollback to previous revision
    Rollback(String),
    /// Shed load (percentage 0-100)
    Shed(u32),
}

impl Action {
    /// Validate action semantics
    pub fn validate(&self) -> Result<(), ActuatorError> {
        match self {
            Action::Throttle(pct) => {
                if *pct > 100 {
                    return Err(ActuatorError::InvalidAction(
                        "Throttle percentage must be 0-100".to_string(),
                    ));
                }
                Ok(())
            }
            Action::Pause => Ok(()),
            Action::Rollback(rev) => {
                if rev.is_empty() {
                    return Err(ActuatorError::InvalidAction(
                        "Rollback revision cannot be empty".to_string(),
                    ));
                }
                Ok(())
            }
            Action::Shed(pct) => {
                if *pct > 100 {
                    return Err(ActuatorError::InvalidAction(
                        "Shed percentage must be 0-100".to_string(),
                    ));
                }
                Ok(())
            }
        }
    }
}

/// Result of action execution
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ActionReceipt {
    pub id: String,
    pub action: Action,
    pub status: ActionStatus,
    pub duration_ms: u64,
    pub executed_at: chrono::DateTime<chrono::Utc>,
}

/// Action execution status
#[derive(Debug, Clone, Copy, Serialize, Deserialize, Eq, PartialEq)]
pub enum ActionStatus {
    /// Action executed successfully
    Success,
    /// Action failed (error logged)
    Failed,
    /// Action executing (not terminal)
    InProgress,
}

/// Actuator service
pub struct Actuator;

// Simple tracking for active actions (in-memory for this example)
static ACTIVE_ACTIONS: std::sync::OnceLock<std::sync::Mutex<std::collections::HashMap<String, ActionReceipt>>> =
    std::sync::OnceLock::new();

fn get_active_actions() -> std::sync::MutexGuard<'static, std::collections::HashMap<String, ActionReceipt>> {
    ACTIVE_ACTIONS
        .get_or_init(|| std::sync::Mutex::new(std::collections::HashMap::new()))
        .lock()
        .unwrap()
}

impl Actuator {
    /// Execute action with safety checks and timeout
    ///
    /// ## Safety guarantees
    /// - Pre-flight validation via Action::validate()
    /// - Timeout enforcement (default 30s)
    /// - Async execution (non-blocking)
    /// - Receipt ledger entry for audit trail
    ///
    /// ## Returns
    /// ActionReceipt with execution ID for future reference/rollback
    pub async fn execute(action: Action) -> Result<ActionReceipt, ActuatorError> {
        // Validate action
        action.validate()?;

        let receipt_id = uuid::Uuid::new_v4().to_string();
        let start = std::time::Instant::now();

        // Simulate action execution with timeout
        let result = tokio::time::timeout(
            Duration::from_secs(30),
            Self::execute_action(&action, &receipt_id),
        )
        .await;

        let status = match result {
            Ok(Ok(())) => ActionStatus::Success,
            Ok(Err(_)) => ActionStatus::Failed,
            Err(_) => {
                return Err(ActuatorError::ExecutionTimeout {
                    duration_secs: 30,
                })
            }
        };

        let duration_ms = start.elapsed().as_millis() as u64;

        let receipt = ActionReceipt {
            id: receipt_id.clone(),
            action: action.clone(),
            status,
            duration_ms,
            executed_at: chrono::Utc::now(),
        };

        // Record receipt in ledger
        ReceiptLedger::emit(&format!("{:?}", action), &format!("{:?}", status))
            .await
            .ok(); // Log errors but don't fail action

        // Store for potential rollback
        {
            let mut actions = get_active_actions();
            actions.insert(receipt_id, receipt.clone());
        }

        tracing::info!(
            action_id = %receipt.id,
            status = ?receipt.status,
            duration_ms = receipt.duration_ms,
            "Action executed"
        );

        Ok(receipt)
    }

    /// Rollback action using receipt
    ///
    /// ## Preconditions
    /// - Receipt must exist in ledger
    /// - Original action must be reversible
    ///
    /// ## Returns
    /// Ok(()) on successful rollback
    pub async fn rollback(receipt_id: &str) -> Result<(), ActuatorError> {
        // Extract receipt and drop lock before await
        let receipt = {
            let actions = get_active_actions();
            actions
                .get(receipt_id)
                .ok_or_else(|| ActuatorError::ReceiptNotFound {
                    receipt_id: receipt_id.to_string(),
                })?
                .clone()
        };

        // Determine rollback action
        let rollback_action = match &receipt.action {
            Action::Throttle(_) => Action::Throttle(0),
            Action::Pause => {
                return Err(ActuatorError::RollbackFailed {
                    reason: "Cannot rollback Pause action".to_string(),
                })
            }
            Action::Rollback(rev) => {
                return Err(ActuatorError::RollbackFailed {
                    reason: format!("Cannot double-rollback from revision {}", rev),
                })
            }
            Action::Shed(_) => Action::Shed(0),
        };

        // Execute rollback (simulate) - no lock held during await
        tokio::time::timeout(
            Duration::from_secs(10),
            Self::execute_action(&rollback_action, receipt_id),
        )
        .await
        .map_err(|_| ActuatorError::ExecutionTimeout {
            duration_secs: 10,
        })??;

        // Re-acquire lock to remove the receipt
        {
            let mut actions = get_active_actions();
            actions.remove(receipt_id); // Actually needs mut for remove
        }

        tracing::info!(receipt_id = receipt_id, "Action rolled back");

        Ok(())
    }

    /// Internal: Execute action against GCP/Kubernetes/etc (simulated)
    async fn execute_action(action: &Action, _receipt_id: &str) -> Result<(), ActuatorError> {
        match action {
            Action::Throttle(pct) => {
                tracing::debug!(percentage = pct, "Executing Throttle");
                // Simulate: Call to LB to apply throttle rule
                tokio::time::sleep(Duration::from_millis(100)).await;
                Ok(())
            }
            Action::Pause => {
                tracing::debug!("Executing Pause");
                // Simulate: Call to orchestrator to pause pods
                tokio::time::sleep(Duration::from_millis(200)).await;
                Ok(())
            }
            Action::Rollback(rev) => {
                tracing::debug!(revision = rev, "Executing Rollback");
                // Simulate: Trigger ArgoCD or deployment rollback
                tokio::time::sleep(Duration::from_millis(500)).await;
                Ok(())
            }
            Action::Shed(pct) => {
                tracing::debug!(percentage = pct, "Executing Shed");
                // Simulate: Route traffic away from this cluster
                tokio::time::sleep(Duration::from_millis(150)).await;
                Ok(())
            }
        }
    }

    /// Get receipt history (for debugging/audit)
    pub fn get_receipt(receipt_id: &str) -> Option<ActionReceipt> {
        get_active_actions().get(receipt_id).cloned()
    }

    /// List all active receipts
    pub fn active_receipts() -> Vec<ActionReceipt> {
        get_active_actions()
            .values()
            .cloned()
            .collect()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_execute_throttle_action() {
        // Arrange
        let action = Action::Throttle(50);

        // Act
        let result = Actuator::execute(action).await;

        // Assert
        assert!(result.is_ok());
        let receipt = result.unwrap();
        assert_eq!(receipt.status, ActionStatus::Success);
        assert!(receipt.duration_ms > 0);
    }

    #[tokio::test]
    async fn test_execute_invalid_throttle_percentage() {
        // Arrange
        let action = Action::Throttle(150); // Invalid

        // Act
        let result = Actuator::execute(action).await;

        // Assert
        assert!(matches!(result, Err(ActuatorError::InvalidAction(_))));
    }

    #[tokio::test]
    async fn test_execute_shed_action() {
        // Arrange
        let action = Action::Shed(25);

        // Act
        let result = Actuator::execute(action).await;

        // Assert
        assert!(result.is_ok());
        let receipt = result.unwrap();
        assert_eq!(receipt.status, ActionStatus::Success);
    }

    #[tokio::test]
    async fn test_rollback_throttle() {
        // Arrange
        let action = Action::Throttle(50);
        let receipt = Actuator::execute(action).await.unwrap();

        // Act
        let rollback_result = Actuator::rollback(&receipt.id).await;

        // Assert
        assert!(rollback_result.is_ok());
        assert!(Actuator::get_receipt(&receipt.id).is_none());
    }

    #[tokio::test]
    async fn test_rollback_nonexistent_receipt() {
        // Act
        let result = Actuator::rollback("nonexistent-receipt").await;

        // Assert
        assert!(matches!(result, Err(ActuatorError::ReceiptNotFound { .. })));
    }

    #[tokio::test]
    async fn test_action_validation() {
        // Arrange & Act
        let valid = Action::Throttle(50).validate();
        let invalid_high = Action::Throttle(150).validate();
        let invalid_shed = Action::Shed(101).validate();

        // Assert
        assert!(valid.is_ok());
        assert!(invalid_high.is_err());
        assert!(invalid_shed.is_err());
    }

    #[tokio::test]
    async fn test_active_receipts_tracking() {
        // Arrange & Act
        let action1 = Action::Throttle(50);
        let action2 = Action::Shed(30);

        let _receipt1 = Actuator::execute(action1).await.unwrap();
        let _receipt2 = Actuator::execute(action2).await.unwrap();

        let active = Actuator::active_receipts();

        // Assert
        assert!(active.len() >= 2);
    }
}
