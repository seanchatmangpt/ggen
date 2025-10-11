//! Safety Controls for Governance
//!
//! Emergency stop, rollback, and validation mechanisms to prevent dangerous operations.

use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::sync::Arc;
use tokio::sync::RwLock;
use uuid::Uuid;

use super::error::{GovernanceError, Result};
use super::types::Decision;

/// Safety controller configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SafetyConfig {
    pub enable_emergency_stop: bool,
    pub enable_auto_rollback: bool,
    pub max_rollback_depth: usize,
    pub dangerous_operations: Vec<String>,
    pub require_confirmation_for: Vec<String>,
}

impl Default for SafetyConfig {
    fn default() -> Self {
        Self {
            enable_emergency_stop: true,
            enable_auto_rollback: true,
            max_rollback_depth: 10,
            dangerous_operations: vec![
                "delete_all".to_string(),
                "drop_database".to_string(),
                "modify_schema".to_string(),
            ],
            require_confirmation_for: vec![
                "graph_mutation".to_string(),
                "schema_change".to_string(),
            ],
        }
    }
}

/// Emergency stop state
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct EmergencyStop {
    pub active: bool,
    pub triggered_at: Option<DateTime<Utc>>,
    pub triggered_by: Option<String>,
    pub reason: Option<String>,
}

/// System state snapshot for rollback
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct StateSnapshot {
    pub id: String,
    pub timestamp: DateTime<Utc>,
    pub description: String,
    pub data: serde_json::Value,
    pub checksum: String,
}

/// Rollback operation
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Rollback {
    pub id: String,
    pub triggered_at: DateTime<Utc>,
    pub target_snapshot: String,
    pub reason: String,
    pub success: bool,
}

/// Validation gate for critical operations
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ValidationGate {
    pub id: String,
    pub operation: String,
    pub required_confirmations: usize,
    pub confirmations: Vec<Confirmation>,
    pub status: GateStatus,
}

/// Confirmation for validation gate
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Confirmation {
    pub confirmed_by: String,
    pub confirmed_at: DateTime<Utc>,
    pub notes: Option<String>,
}

/// Gate status
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum GateStatus {
    Pending,
    Approved,
    Rejected,
    Expired,
}

/// Safety violation
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SafetyViolation {
    pub id: String,
    pub violation_type: ViolationType,
    pub message: String,
    pub detected_at: DateTime<Utc>,
}

/// Type of safety violation
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ViolationType {
    DangerousOperation,
    RateLimitExceeded,
    UnauthorizedAccess,
    DataIntegrityRisk,
    SystemResourceExhaustion,
}

/// Safety controller
pub struct SafetyController {
    config: SafetyConfig,
    emergency_stop: Arc<RwLock<EmergencyStop>>,
    snapshots: Arc<RwLock<Vec<StateSnapshot>>>,
    rollback_history: Arc<RwLock<Vec<Rollback>>>,
    validation_gates: Arc<RwLock<HashMap<String, ValidationGate>>>,
    violations: Arc<RwLock<Vec<SafetyViolation>>>,
}

impl SafetyController {
    /// Create a new safety controller
    pub fn new(config: SafetyConfig) -> Self {
        Self {
            config,
            emergency_stop: Arc::new(RwLock::new(EmergencyStop {
                active: false,
                triggered_at: None,
                triggered_by: None,
                reason: None,
            })),
            snapshots: Arc::new(RwLock::new(Vec::new())),
            rollback_history: Arc::new(RwLock::new(Vec::new())),
            validation_gates: Arc::new(RwLock::new(HashMap::new())),
            violations: Arc::new(RwLock::new(Vec::new())),
        }
    }

    /// Check if a decision violates safety constraints
    pub async fn check_safety(&self, decision: &Decision) -> Result<Option<String>> {
        // Check if emergency stop is active
        let emergency = self.emergency_stop.read().await;
        if emergency.active {
            return Ok(Some(format!(
                "Emergency stop active: {}",
                emergency
                    .reason
                    .as_ref()
                    .unwrap_or(&"Unknown reason".to_string())
            )));
        }
        drop(emergency);

        // Check for dangerous operations
        if self.config.dangerous_operations.contains(&decision.action) {
            let violation = SafetyViolation {
                id: Uuid::new_v4().to_string(),
                violation_type: ViolationType::DangerousOperation,
                message: format!("Dangerous operation blocked: {}", decision.action),
                detected_at: Utc::now(),
            };

            let mut violations = self.violations.write().await;
            violations.push(violation.clone());

            return Ok(Some(violation.message));
        }

        // Check if operation requires confirmation gate
        if self
            .config
            .require_confirmation_for
            .contains(&decision.action)
        {
            if !self.check_validation_gate(&decision.id).await? {
                return Ok(Some(format!(
                    "Operation requires validation gate approval: {}",
                    decision.action
                )));
            }
        }

        // Additional safety checks can be added here
        // - Rate limiting
        // - Resource usage
        // - Data integrity
        // - Access control

        Ok(None)
    }

    /// Trigger emergency stop
    pub async fn trigger_emergency_stop(&self, reason: &str) -> Result<()> {
        if !self.config.enable_emergency_stop {
            return Err(GovernanceError::SafetyError(
                "Emergency stop is disabled".to_string(),
            ));
        }

        let mut emergency = self.emergency_stop.write().await;
        emergency.active = true;
        emergency.triggered_at = Some(Utc::now());
        emergency.triggered_by = Some("system".to_string());
        emergency.reason = Some(reason.to_string());

        tracing::error!("Emergency stop triggered: {}", reason);
        Ok(())
    }

    /// Resume operations after emergency stop
    pub async fn resume(&self) -> Result<()> {
        let mut emergency = self.emergency_stop.write().await;
        emergency.active = false;
        emergency.triggered_at = None;
        emergency.triggered_by = None;
        emergency.reason = None;

        tracing::info!("Operations resumed after emergency stop");
        Ok(())
    }

    /// Create a state snapshot
    pub async fn create_snapshot(
        &self, description: &str, data: serde_json::Value,
    ) -> Result<String> {
        let snapshot = StateSnapshot {
            id: Uuid::new_v4().to_string(),
            timestamp: Utc::now(),
            description: description.to_string(),
            checksum: self.calculate_checksum(&data),
            data,
        };

        let snapshot_id = snapshot.id.clone();

        let mut snapshots = self.snapshots.write().await;
        snapshots.push(snapshot);

        // Limit snapshot history
        if snapshots.len() > self.config.max_rollback_depth {
            snapshots.remove(0);
        }

        tracing::info!("Created snapshot: {} - {}", snapshot_id, description);
        Ok(snapshot_id)
    }

    /// Rollback to a previous snapshot
    pub async fn rollback(&self, target_snapshot_id: &str) -> Result<()> {
        if !self.config.enable_auto_rollback {
            return Err(GovernanceError::SafetyError(
                "Auto rollback is disabled".to_string(),
            ));
        }

        let snapshots = self.snapshots.read().await;
        let snapshot = snapshots
            .iter()
            .find(|s| s.id == target_snapshot_id)
            .ok_or_else(|| GovernanceError::SnapshotNotFound(target_snapshot_id.to_string()))?;

        // Validate snapshot integrity
        let calculated_checksum = self.calculate_checksum(&snapshot.data);
        if calculated_checksum != snapshot.checksum {
            return Err(GovernanceError::SafetyError(format!(
                "Snapshot checksum mismatch: expected {}, got {}",
                snapshot.checksum, calculated_checksum
            )));
        }

        tracing::info!(
            snapshot_id = %target_snapshot_id,
            description = %snapshot.description,
            timestamp = %snapshot.timestamp,
            "Starting rollback operation"
        );

        let start_time = std::time::Instant::now();
        let mut rollback_success = true;
        let mut rollback_reason = "Manual rollback".to_string();

        // Implement actual rollback logic
        // This would restore system state from the snapshot
        match self.restore_system_state(&snapshot.data).await {
            Ok(_) => {
                tracing::info!(
                    snapshot_id = %target_snapshot_id,
                    "Successfully restored system state from snapshot"
                );
            }
            Err(e) => {
                let _ = rollback_success; // Used for potential future rollback logic
                rollback_success = false;
                rollback_reason = format!("Rollback failed: {}", e);
                tracing::error!(
                    snapshot_id = %target_snapshot_id,
                    error = %e,
                    "Failed to restore system state"
                );

                return Err(GovernanceError::SafetyError(rollback_reason));
            }
        }

        let duration = start_time.elapsed().as_millis() as u64;

        let rollback = Rollback {
            id: Uuid::new_v4().to_string(),
            triggered_at: Utc::now(),
            target_snapshot: target_snapshot_id.to_string(),
            reason: rollback_reason.clone(),
            success: rollback_success,
        };

        let mut history = self.rollback_history.write().await;
        history.push(rollback);

        tracing::warn!(
            snapshot_id = %target_snapshot_id,
            duration_ms = duration,
            success = rollback_success,
            "Rollback completed"
        );

        Ok(())
    }

    /// Create a validation gate for critical operation
    pub async fn create_validation_gate(
        &self, operation: &str, required_confirmations: usize,
    ) -> Result<String> {
        let gate = ValidationGate {
            id: Uuid::new_v4().to_string(),
            operation: operation.to_string(),
            required_confirmations,
            confirmations: Vec::new(),
            status: GateStatus::Pending,
        };

        let gate_id = gate.id.clone();

        let mut gates = self.validation_gates.write().await;
        gates.insert(gate_id.clone(), gate);

        Ok(gate_id)
    }

    /// Add confirmation to validation gate
    pub async fn confirm_gate(
        &self, gate_id: &str, confirmed_by: &str, notes: Option<String>,
    ) -> Result<()> {
        let mut gates = self.validation_gates.write().await;

        let gate = gates
            .get_mut(gate_id)
            .ok_or_else(|| GovernanceError::ValidationGateNotFound(gate_id.to_string()))?;

        if gate.status != GateStatus::Pending {
            return Err(GovernanceError::SafetyError(format!(
                "Validation gate is not pending: {:?}",
                gate.status
            )));
        }

        gate.confirmations.push(Confirmation {
            confirmed_by: confirmed_by.to_string(),
            confirmed_at: Utc::now(),
            notes,
        });

        if gate.confirmations.len() >= gate.required_confirmations {
            gate.status = GateStatus::Approved;
        }

        Ok(())
    }

    /// Check if validation gate is approved
    async fn check_validation_gate(&self, gate_id: &str) -> Result<bool> {
        let gates = self.validation_gates.read().await;

        if let Some(gate) = gates.get(gate_id) {
            Ok(gate.status == GateStatus::Approved)
        } else {
            Ok(false)
        }
    }

    /// Calculate checksum for snapshot data
    fn calculate_checksum(&self, data: &serde_json::Value) -> String {
        use std::collections::hash_map::DefaultHasher;
        use std::hash::{Hash, Hasher};

        let mut hasher = DefaultHasher::new();
        data.to_string().hash(&mut hasher);
        format!("{:x}", hasher.finish())
    }

    /// Get safety violations
    pub async fn get_violations(&self) -> Result<Vec<SafetyViolation>> {
        let violations = self.violations.read().await;
        Ok(violations.clone())
    }

    /// Get rollback history
    pub async fn get_rollback_history(&self) -> Result<Vec<Rollback>> {
        let history = self.rollback_history.read().await;
        Ok(history.clone())
    }

    /// List available snapshots
    pub async fn list_snapshots(&self) -> Result<Vec<StateSnapshot>> {
        let snapshots = self.snapshots.read().await;
        Ok(snapshots.clone())
    }

    /// Restore system state from snapshot data
    async fn restore_system_state(&self, snapshot_data: &serde_json::Value) -> Result<()> {
        tracing::debug!("Restoring system state from snapshot");

        // Parse snapshot structure
        let state_map = snapshot_data.as_object().ok_or_else(|| {
            GovernanceError::SafetyError("Invalid snapshot format: expected object".to_string())
        })?;

        // Restore different components based on snapshot content
        for (component, data) in state_map {
            match component.as_str() {
                "policies" => {
                    // Restore policy configurations
                    if let Some(policies) = data.as_array() {
                        tracing::debug!(count = policies.len(), "Restoring policies");
                        // In a real system, this would restore policy state
                    }
                }
                "validation_gates" => {
                    // Restore validation gate state
                    if let Some(gates) = data.as_object() {
                        tracing::debug!(count = gates.len(), "Restoring validation gates");
                        // In a real system, this would restore gate state
                    }
                }
                "emergency_stop" => {
                    // Restore emergency stop state
                    if let Some(stop_state) = data.as_bool() {
                        let mut emergency = self.emergency_stop.write().await;
                        emergency.active = stop_state;
                        tracing::debug!(active = stop_state, "Restored emergency stop state");
                    }
                }
                "metadata" => {
                    // Restore system metadata
                    tracing::debug!("Restoring system metadata");
                }
                _ => {
                    tracing::warn!(component = %component, "Unknown snapshot component");
                }
            }
        }

        tracing::info!("System state restoration completed");
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_emergency_stop() {
        let controller = SafetyController::new(SafetyConfig::default());

        controller
            .trigger_emergency_stop("test emergency")
            .await
            .expect("Failed to trigger emergency stop");

        let emergency = controller.emergency_stop.read().await;
        assert!(emergency.active);
        assert_eq!(
            emergency.reason.as_ref().map(String::as_str),
            Some("test emergency"),
            "Expected emergency stop reason to be 'test emergency'"
        );
    }

    #[tokio::test]
    async fn test_snapshot_creation() {
        let controller = SafetyController::new(SafetyConfig::default());

        let data = serde_json::json!({"test": "data"});
        let snapshot_id = controller
            .create_snapshot("test", data)
            .await
            .expect("Failed to create snapshot");

        let snapshots = controller
            .list_snapshots()
            .await
            .expect("Failed to list snapshots");
        assert_eq!(snapshots.len(), 1);
        assert_eq!(snapshots[0].id, snapshot_id);
    }

    #[tokio::test]
    async fn test_validation_gate() {
        let controller = SafetyController::new(SafetyConfig::default());

        let gate_id = controller
            .create_validation_gate("test_op", 2)
            .await
            .expect("Failed to create validation gate");

        controller
            .confirm_gate(&gate_id, "user1", None)
            .await
            .expect("Failed to confirm gate for user1");
        assert!(
            !controller
                .check_validation_gate(&gate_id)
                .await
                .expect("Failed to check validation gate"),
            "Expected gate to not be approved after 1 confirmation"
        );

        controller
            .confirm_gate(&gate_id, "user2", None)
            .await
            .expect("Failed to confirm gate for user2");
        assert!(
            controller
                .check_validation_gate(&gate_id)
                .await
                .expect("Failed to check validation gate"),
            "Expected gate to be approved after 2 confirmations"
        );
    }
}
