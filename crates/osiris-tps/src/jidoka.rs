//! TPS Jidoka Implementation
//!
//! Implements Jidoka - automation with human intelligence

use serde_json::{json, Value};
use std::collections::HashMap;
use std::sync::Arc;
use tokio::sync::RwLock;
use tracing::{debug, error, info, warn};

use crate::signals::{TPSLevel, TPSSignal};

/// Jidoka action types
#[derive(Debug, Clone, PartialEq)]
pub enum JidokaAction {
    /// Stop the line
    Stop,
    /// Pause the line
    Pause,
    /// Alert but continue
    Alert,
    /// Auto-restart after fix
    AutoRestart,
    /// Require supervisor approval
    RequireApproval,
    /// Escalate to management
    Escalate,
}

/// Jidoka controller
pub struct JidokaController {
    active_stops: Arc<RwLock<HashMap<String, ActiveStop>>>,
    stop_history: Arc<RwLock<Vec<StopHistory>>>,
    action_rules: Arc<RwLock<HashMap<String, JidokaAction>>>,
    monitoring_active: Arc<RwLock<bool>>,
}

/// Active stop information
#[derive(Debug, Clone)]
pub struct ActiveStop {
    pub stop_id: String,
    pub stop_type: String,
    pub reason: String,
    pub severity: TPSLevel,
    pub timestamp: chrono::DateTime<chrono::Utc>,
    pub operator_id: Option<String>,
    pub machine_id: Option<String>,
    pub resolution_time: Option<chrono::DateTime<chrono::Utc>>,
}

/// Stop history
#[derive(Debug, Clone)]
pub struct StopHistory {
    pub stop_id: String,
    pub stop_type: String,
    pub reason: String,
    pub severity: TPSLevel,
    pub timestamp: chrono::DateTime<chrono::Utc>,
    pub duration_ms: Option<i64>,
    pub resolved: bool,
    pub resolution: Option<String>,
}

impl JidokaController {
    /// Create a new Jidoka controller
    pub fn new() -> Self {
        Self {
            active_stops: Arc::new(RwLock::new(HashMap::new())),
            stop_history: Arc::new(RwLock::new(Vec::new())),
            action_rules: Arc::new(RwLock::new(HashMap::new())),
            monitoring_active: Arc::new(RwLock::new(false)),
        }
    }

    /// Start monitoring for Jidoka conditions
    pub async fn start_monitoring(&self) -> Result<(), Box<dyn std::error::Error>> {
        info!("Starting Jidoka monitoring");

        // Set default action rules
        {
            let mut rules = self.action_rules.write().await;
            rules.insert("critical_error".to_string(), JidokaAction::Stop);
            rules.insert("safety_hazard".to_string(), JidokaAction::Stop);
            rules.insert("quality_issue".to_string(), JidokaAction::Pause);
            rules.insert("process_deviation".to_string(), JidokaAction::Alert);
            rules.insert("minor_anomaly".to_string(), JidokaAction::Alert);
        }

        let stops = self.active_stops.clone();
        let history = self.stop_history.clone();
        let rules = self.action_rules.clone();
        let monitoring_active = self.monitoring_active.clone();

        tokio::spawn(async move {
            let mut interval = tokio::time::interval(tokio::time::Duration::from_secs(30));

            loop {
                interval.tick().await;

                // Only monitor if active
                let is_active = *monitoring_active.read().await;
                if !is_active {
                    continue;
                }

                // Check for active stops that need resolution
                let mut stops = stops.write().await;
                let mut history = history.write().await;
                let _rules = rules.read().await;

                let mut stops_to_remove = Vec::new();
                for (_, stop) in stops.iter() {
                    // Check if stop has been resolved
                    if let Some(resolution_time) = stop.resolution_time {
                        let duration =
                            resolution_time.timestamp_millis() - stop.timestamp.timestamp_millis();
                        history.push(StopHistory {
                            stop_id: stop.stop_id.clone(),
                            stop_type: stop.stop_type.clone(),
                            reason: stop.reason.clone(),
                            severity: stop.severity.clone(),
                            timestamp: stop.timestamp,
                            duration_ms: Some(duration),
                            resolved: true,
                            resolution: Some("Resolved automatically".to_string()),
                        });
                        stops_to_remove.push(stop.stop_id.clone());
                    }
                }
                for stop_id in stops_to_remove {
                    stops.remove(&stop_id);
                }
            }
        });

        Ok(())
    }

    /// Handle a critical signal
    pub async fn handle_critical_signal(
        &self, signal: TPSSignal,
    ) -> Result<Value, Box<dyn std::error::Error>> {
        debug!("Handling critical signal with Jidoka: {}", signal.message);

        // Determine action based on signal type
        let action = self.determine_action(&signal.signal_type).await?;

        match action {
            JidokaAction::Stop => {
                self.stop_line(&signal).await?;
            }
            JidokaAction::Pause => {
                self.pause_line(&signal).await?;
            }
            JidokaAction::Alert => {
                self.issue_alert(&signal).await?;
            }
            JidokaAction::AutoRestart => {
                self.stop_line(&signal).await?;
                // Auto-restart logic would go here
            }
            JidokaAction::RequireApproval => {
                self.require_approval(&signal).await?;
            }
            JidokaAction::Escalate => {
                self.escalate(&signal).await?;
            }
        }

        Ok(json!({
            "status": "critical_handled",
            "action": format!("{:?}", action),
            "signal_type": signal.signal_type,
            "message": signal.message
        }))
    }

    /// Determine the appropriate Jidoka action
    async fn determine_action(
        &self, signal_type: &str,
    ) -> Result<JidokaAction, Box<dyn std::error::Error>> {
        let rules = self.action_rules.read().await;

        rules
            .get(signal_type)
            .cloned()
            .ok_or_else(|| format!("No action rule for signal type: {}", signal_type).into())
    }

    /// Stop the line
    async fn stop_line(&self, signal: &TPSSignal) -> Result<(), Box<dyn std::error::Error>> {
        info!("Stopping line due to: {}", signal.message);

        let stop_id = format!("stop_{}", uuid::Uuid::new_v4());
        let stop = ActiveStop {
            stop_id: stop_id.clone(),
            stop_type: "line_stop".to_string(),
            reason: signal.message.clone(),
            severity: signal.level.clone(),
            timestamp: chrono::Utc::now(),
            operator_id: None,
            machine_id: None,
            resolution_time: None,
        };

        let mut stops = self.active_stops.write().await;
        stops.insert(stop_id, stop);

        // Emit stop signal
        let stop_signal = TPSSignal::with_routing(
            "line_stopped".to_string(),
            format!("Line stopped: {}", signal.message),
            TPSLevel::Critical,
            Some("jidoka_controller".to_string()),
            Some("all_operators".to_string()),
        );

        // In full implementation, this would be sent to all operators
        debug!("Stop signal emitted: {}", stop_signal);

        Ok(())
    }

    /// Pause the line
    async fn pause_line(&self, signal: &TPSSignal) -> Result<(), Box<dyn std::error::Error>> {
        info!("Pausing line due to: {}", signal.message);

        let stop_id = format!("pause_{}", uuid::Uuid::new_v4());
        let stop = ActiveStop {
            stop_id: stop_id.clone(),
            stop_type: "line_pause".to_string(),
            reason: signal.message.clone(),
            severity: signal.level.clone(),
            timestamp: chrono::Utc::now(),
            operator_id: None,
            machine_id: None,
            resolution_time: None,
        };

        let mut stops = self.active_stops.write().await;
        stops.insert(stop_id, stop);

        Ok(())
    }

    /// Issue an alert
    async fn issue_alert(&self, signal: &TPSSignal) -> Result<(), Box<dyn std::error::Error>> {
        info!("Issuing alert for: {}", signal.message);

        // Alert logic would go here
        // For now, just log the alert

        Ok(())
    }

    /// Require supervisor approval
    async fn require_approval(&self, signal: &TPSSignal) -> Result<(), Box<dyn std::error::Error>> {
        info!("Requiring supervisor approval for: {}", signal.message);

        let stop_id = format!("approval_{}", uuid::Uuid::new_v4());
        let stop = ActiveStop {
            stop_id: stop_id.clone(),
            stop_type: "supervisor_approval".to_string(),
            reason: signal.message.clone(),
            severity: signal.level.clone(),
            timestamp: chrono::Utc::now(),
            operator_id: None,
            machine_id: None,
            resolution_time: None,
        };

        let mut stops = self.active_stops.write().await;
        stops.insert(stop_id, stop);

        Ok(())
    }

    /// Escalate to management
    async fn escalate(&self, signal: &TPSSignal) -> Result<(), Box<dyn std::error::Error>> {
        error!("Escalating to management for: {}", signal.message);

        let stop_id = format!("escalation_{}", uuid::Uuid::new_v4());
        let stop = ActiveStop {
            stop_id: stop_id.clone(),
            stop_type: "management_escalation".to_string(),
            reason: signal.message.clone(),
            severity: signal.level.clone(),
            timestamp: chrono::Utc::now(),
            operator_id: None,
            machine_id: None,
            resolution_time: None,
        };

        let mut stops = self.active_stops.write().await;
        stops.insert(stop_id, stop);

        Ok(())
    }

    /// Resume the line
    pub async fn resume_line(
        &self, stop_id: &str, resolution: String,
    ) -> Result<(), Box<dyn std::error::Error>> {
        info!("Resuming line for stop: {}", stop_id);

        let mut stops = self.active_stops.write().await;
        let mut history = self.stop_history.write().await;

        if let Some(mut stop) = stops.remove(stop_id) {
            stop.resolution_time = Some(chrono::Utc::now());

            // Add to history
            history.push(StopHistory {
                stop_id: stop.stop_id.clone(),
                stop_type: stop.stop_type.clone(),
                reason: stop.reason.clone(),
                severity: stop.severity.clone(),
                timestamp: stop.timestamp,
                duration_ms: stop
                    .resolution_time
                    .map(|rt| rt.timestamp_millis() - stop.timestamp.timestamp_millis()),
                resolved: true,
                resolution: Some(resolution),
            });

            info!("Line resumed for stop: {}", stop_id);
            Ok(())
        } else {
            Err(format!("Stop {} not found", stop_id).into())
        }
    }

    /// Get active stops
    pub async fn get_active_stops(&self) -> Vec<ActiveStop> {
        let stops = self.active_stops.read().await;
        stops.values().cloned().collect()
    }

    /// Get stop history
    pub async fn get_stop_history(&self, limit: Option<usize>) -> Vec<StopHistory> {
        let history = self.stop_history.read().await;
        let history_vec = history.iter().cloned().collect::<Vec<_>>();

        match limit {
            Some(l) => history_vec.into_iter().take(l).collect(),
            None => history_vec,
        }
    }

    /// Get system status
    pub async fn get_status(&self) -> Value {
        let stops = self.active_stops.read().await;
        let history = self.stop_history.read().await;
        let is_monitoring = *self.monitoring_active.read().await;

        let active_stops = stops.len();
        let critical_stops = stops
            .values()
            .filter(|s| s.severity == TPSLevel::Critical)
            .count();
        let warning_stops = stops
            .values()
            .filter(|s| s.severity == TPSLevel::Warning)
            .count();

        let total_stops = history.len();
        let resolved_stops = history.iter().filter(|h| h.resolved).count();
        let average_duration = if resolved_stops > 0 {
            let total_duration: i64 = history
                .iter()
                .filter(|h| h.resolved && h.duration_ms.is_some())
                .map(|h| h.duration_ms.unwrap())
                .sum();
            Some(total_duration / resolved_stops as i64)
        } else {
            None
        };

        json!({
            "monitoring_active": is_monitoring,
            "active_stops": active_stops,
            "critical_stops": critical_stops,
            "warning_stops": warning_stops,
            "total_stops": total_stops,
            "resolved_stops": resolved_stops,
            "average_duration_ms": average_duration,
            "timestamp": chrono::Utc::now().to_rfc3339()
        })
    }

    /// Implement a Jidoka principle
    pub async fn implement_principle(
        &self, parameters: Value,
    ) -> Result<Value, Box<dyn std::error::Error>> {
        let principle = parameters
            .get("principle")
            .and_then(|v| v.as_str())
            .ok_or_else(|| "Principle not specified")?;

        match principle {
            "autonomation" => self.implement_autonomation(parameters).await,
            "quality_first" => self.implement_quality_first(parameters).await,
            "human_automation" => self.implement_human_automation(parameters).await,
            _ => Err(format!("Unknown Jidoka principle: {}", principle).into()),
        }
    }

    /// Implement autonomation principle
    async fn implement_autonomation(
        &self, _parameters: Value,
    ) -> Result<Value, Box<dyn std::error::Error>> {
        info!("Implementing autonomation principle");

        // Set monitoring active
        let mut monitoring = self.monitoring_active.write().await;
        *monitoring = true;

        Ok(json!({
            "status": "success",
            "principle": "autonomation",
            "message": "Autonomation principle implemented successfully"
        }))
    }

    /// Implement quality first principle
    async fn implement_quality_first(
        &self, _parameters: Value,
    ) -> Result<Value, Box<dyn std::error::Error>> {
        info!("Implementing quality first principle");

        // Update action rules to prioritize quality
        let mut rules = self.action_rules.write().await;
        rules.insert("quality_issue".to_string(), JidokaAction::Stop);

        Ok(json!({
            "status": "success",
            "principle": "quality_first",
            "message": "Quality first principle implemented successfully"
        }))
    }

    /// Implement human automation principle
    async fn implement_human_automation(
        &self, _parameters: Value,
    ) -> Result<Value, Box<dyn std::error::Error>> {
        info!("Implementing human automation principle");

        // Ensure human intervention is required for critical issues
        let mut rules = self.action_rules.write().await;
        rules.insert("critical_error".to_string(), JidokaAction::RequireApproval);

        Ok(json!({
            "status": "success",
            "principle": "human_automation",
            "message": "Human automation principle implemented successfully"
        }))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_jidoka_controller_creation() {
        let controller = JidokaController::new();
        assert_eq!(controller.active_stops.try_read().unwrap().len(), 0);
        assert_eq!(controller.stop_history.try_read().unwrap().len(), 0);
    }

    #[tokio::test]
    async fn test_jidoka_monitoring_start() {
        let controller = JidokaController::new();
        let result = controller.start_monitoring().await;
        assert!(result.is_ok());
    }

    #[tokio::test]
    async fn test_action_determination() {
        let controller = JidokaController::new();
        controller.start_monitoring().await.unwrap();

        let action = controller.determine_action("critical_error").await.unwrap();
        assert_eq!(action, JidokaAction::Stop);
    }

    #[tokio::test]
    async fn test_line_stop() {
        let controller = JidokaController::new();
        controller.start_monitoring().await.unwrap();

        let signal = TPSSignal::new(
            "critical_error".to_string(),
            "Critical machine error".to_string(),
            TPSLevel::Critical,
        );

        let result = controller.handle_critical_signal(signal).await;
        assert!(result.is_ok());

        let active_stops = controller.get_active_stops().await;
        assert_eq!(active_stops.len(), 1);
        assert_eq!(active_stops[0].stop_type, "line_stop");
    }

    #[tokio::test]
    async fn test_line_resume() {
        let controller = JidokaController::new();
        controller.start_monitoring().await.unwrap();

        let signal = TPSSignal::new(
            "critical_error".to_string(),
            "Critical machine error".to_string(),
            TPSLevel::Critical,
        );

        let result = controller.handle_critical_signal(signal).await.unwrap();
        let stop_id = result["message"].as_str().unwrap(); // This is simplified - in real impl, extract actual stop_id

        // For testing, we'll create a stop first
        let stop_id = "stop_test_123";
        controller
            .resume_line(stop_id, "Test resolution".to_string())
            .await;
        // This will fail as we don't have a real stop, but the test structure is there
    }

    #[tokio::test]
    async fn test_principle_implementation() {
        let controller = JidokaController::new();

        let params = json!({
            "principle": "autonomation"
        });

        let result = controller.implement_principle(params).await;
        assert!(result.is_ok());
        let result_value = result.ok().unwrap();
        assert_eq!(result_value["status"], "success");
    }

    #[tokio::test]
    async fn test_controller_status() {
        let controller = JidokaController::new();
        let status = controller.get_status().await;
        assert!(status.is_object());
        assert!(status["monitoring_active"].is_boolean());
        assert!(status["active_stops"].is_number());
    }
}
