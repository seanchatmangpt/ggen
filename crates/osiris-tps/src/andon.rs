//! TPS Andon System Implementation
//!
//! Implements the Andon principle - visual control system for signaling problems

use serde_json::Value;
use std::collections::HashMap;
use std::sync::Arc;
use tokio::sync::RwLock;
use tracing::{debug, error, info, warn};

use crate::signals::{TPSLevel, TPSSignal};

/// TPS Andon System configuration
#[derive(Debug, Clone)]
pub struct TPSAndonConfig {
    /// Alert thresholds
    pub alert_thresholds: HashMap<String, f64>,
    /// Escalation path
    pub escalation_path: Vec<String>,
    /// Alert channels
    pub alert_channels: Vec<String>,
    /// Alert timeout in milliseconds
    pub alert_timeout_ms: u64,
}

impl Default for TPSAndonConfig {
    fn default() -> Self {
        Self {
            alert_thresholds: HashMap::new(),
            escalation_path: vec![
                "supervisor".to_string(),
                "manager".to_string(),
                "director".to_string(),
            ],
            alert_channels: vec![
                "visual".to_string(),
                "audio".to_string(),
                "digital".to_string(),
            ],
            alert_timeout_ms: 30000, // 30 seconds
        }
    }
}

/// TPS Andon System
pub struct TPSAndonSystem {
    config: TPSAndonConfig,
    active_alerts: Arc<RwLock<HashMap<String, ActiveAlert>>>,
    alert_history: Arc<RwLock<Vec<AlertHistory>>>,
    signal_processor: Arc<TPSSignalProcessor>,
}

/// Active alert information
#[derive(Debug, Clone)]
pub struct ActiveAlert {
    pub alert_id: String,
    pub alert_type: String,
    pub severity: TPSLevel,
    pub timestamp: chrono::DateTime<chrono::Utc>,
    pub acknowledged: bool,
    pub acknowledged_by: Option<String>,
    pub escalation_level: usize,
}

/// Alert history
#[derive(Debug, Clone)]
pub struct AlertHistory {
    pub alert_id: String,
    pub alert_type: String,
    pub severity: TPSLevel,
    pub timestamp: chrono::DateTime<chrono::Utc>,
    pub resolved: bool,
    pub resolution_time: Option<chrono::DateTime<chrono::Utc>>,
    pub message: String,
}

/// Signal processor for TPS signals
pub struct TPSSignalProcessor;

impl TPSSignalProcessor {
    /// Process a TPS signal
    pub async fn process_signal(signal: TPSSignal) -> Result<Value, String> {
        debug!("Processing TPS signal: {}", signal.signal_type);

        match signal.level {
            TPSLevel::Critical => {
                // Critical signals require immediate attention
                Self::handle_critical_signal(signal).await
            }
            TPSLevel::Warning => {
                // Warning signals need monitoring
                Self::handle_warning_signal(signal).await
            }
            TPSLevel::Information => {
                // Information signals are for awareness
                Self::handle_information_signal(signal).await
            }
        }
    }

    async fn handle_critical_signal(signal: TPSSignal) -> Result<Value, String> {
        error!("Critical signal: {}", signal.message);

        // Critical signals trigger immediate escalation
        Ok(json!({
            "status": "critical_handled",
            "signal_type": signal.signal_type,
            "action": "immediate_escalation",
            "message": "Critical signal requires immediate attention"
        }))
    }

    async fn handle_warning_signal(signal: TPSSignal) -> Result<Value, String> {
        warn!("Warning signal: {}", signal.message);

        // Warning signals are logged and monitored
        Ok(json!({
            "status": "warning_logged",
            "signal_type": signal.signal_type,
            "action": "monitor",
            "message": "Warning signal has been logged"
        }))
    }

    async fn handle_information_signal(signal: TPSSignal) -> Result<Value, String> {
        info!("Information signal: {}", signal.message);

        // Information signals are recorded
        Ok(json!({
            "status": "information_recorded",
            "signal_type": signal.signal_type,
            "action": "record",
            "message": "Information signal has been recorded"
        }))
    }
}

impl TPSAndonSystem {
    /// Create a new TPS Andon system
    pub fn new(config: TPSAndonConfig) -> Result<Self, Box<dyn std::error::Error>> {
        Ok(Self {
            config,
            active_alerts: Arc::new(RwLock::new(HashMap::new())),
            alert_history: Arc::new(RwLock::new(Vec::new())),
            signal_processor: Arc::new(TPSSignalProcessor),
        })
    }

    /// Start monitoring for alerts
    pub async fn start_monitoring(&self) -> Result<(), Box<dyn std::error::Error>> {
        info!("Starting TPS Andon monitoring");

        // Start background monitoring task
        let alerts = self.active_alerts.clone();
        let history = self.alert_history.clone();
        let config = self.config.clone();

        tokio::spawn(async move {
            let mut interval = tokio::time::interval(tokio::time::Duration::from_secs(60));

            loop {
                interval.tick().await;

                // Check for alert timeouts
                let mut alerts = alerts.write().await;
                let mut history = history.write().await;

                // Remove alerts that have timed out
                alerts.retain(|_, alert| {
                    if alert.acknowledged {
                        true
                    } else {
                        let elapsed = chrono::Utc::now().timestamp_millis()
                            - alert.timestamp.timestamp_millis();
                        elapsed < config.alert_timeout_ms as i64
                    }
                });

                // Check for alerts that need escalation
                for (_, alert) in alerts.iter_mut() {
                    if !alert.acknowledged && alert.escalation_level < config.escalation_path.len()
                    {
                        alert.escalation_level += 1;
                        info!(
                            "Alert {} escalated to level {}",
                            alert.alert_id, alert.escalation_level
                        );
                    }
                }
            }
        });

        Ok(())
    }

    /// Handle a warning signal
    pub async fn handle_warning_signal(
        &self, signal: TPSSignal,
    ) -> Result<Value, Box<dyn std::error::Error>> {
        debug!("Handling warning signal: {}", signal.message);

        // Create alert
        let alert_id = format!("alert_{}", uuid::Uuid::new_v4());
        let alert = ActiveAlert {
            alert_id: alert_id.clone(),
            alert_type: signal.signal_type.clone(),
            severity: signal.level.clone(),
            timestamp: chrono::Utc::now(),
            acknowledged: false,
            acknowledged_by: None,
            escalation_level: 0,
        };

        // Add to active alerts
        let mut alerts = self.active_alerts.write().await;
        alerts.insert(alert_id.clone(), alert);

        // Add to history
        let mut history = self.alert_history.write().await;
        history.push(AlertHistory {
            alert_id: alert_id.clone(),
            alert_type: signal.signal_type,
            severity: signal.level,
            timestamp: chrono::Utc::now(),
            resolved: false,
            resolution_time: None,
            message: signal.message,
        });

        // Process signal
        let result = TPSSignalProcessor::process_signal(signal).await?;

        Ok(json!({
            "status": "alert_created",
            "alert_id": alert_id,
            "result": result
        }))
    }

    /// Acknowledge an alert
    pub async fn acknowledge_alert(
        &self, alert_id: &str, acknowledged_by: String,
    ) -> Result<(), Box<dyn std::error::Error>> {
        let mut alerts = self.active_alerts.write().await;

        if let Some(alert) = alerts.get_mut(alert_id) {
            alert.acknowledged = true;
            alert.acknowledged_by = Some(acknowledged_by);
            info!("Alert {} acknowledged", alert_id);
            Ok(())
        } else {
            Err(format!("Alert {} not found", alert_id).into())
        }
    }

    /// Resolve an alert
    pub async fn resolve_alert(
        &self, alert_id: &str, resolution: String,
    ) -> Result<(), Box<dyn std::error::Error>> {
        let mut alerts = self.active_alerts.write().await;
        let mut history = self.alert_history.write().await;

        if let Some(mut alert) = alerts.remove(alert_id) {
            alert.acknowledged = true;
            alert.acknowledged_by = Some("system".to_string());

            // Update history
            if let Some(entry) = history.iter_mut().find(|h| h.alert_id == alert_id) {
                entry.resolved = true;
                entry.resolution_time = Some(chrono::Utc::now());
                entry.message = format!("{} - {}", entry.message, resolution);
            }

            info!("Alert {} resolved: {}", alert_id, resolution);
            Ok(())
        } else {
            Err(format!("Alert {} not found", alert_id).into())
        }
    }

    /// Get system status
    pub async fn get_status(&self) -> Value {
        let alerts = self.active_alerts.read().await;
        let history = self.alert_history.read().await;

        let active_alert_count = alerts.len();
        let unresolved_alerts = alerts.values().filter(|a| !a.acknowledged).count();
        let critical_alerts = alerts
            .values()
            .filter(|a| a.severity == TPSLevel::Critical)
            .count();
        let warning_alerts = alerts
            .values()
            .filter(|a| a.severity == TPSLevel::Warning)
            .count();

        let total_alerts = history.len();
        let resolved_alerts = history.iter().filter(|h| h.resolved).count();

        json!({
            "system_status": "active",
            "active_alerts": active_alert_count,
            "unresolved_alerts": unresolved_alerts,
            "critical_alerts": critical_alerts,
            "warning_alerts": warning_alerts,
            "total_alerts": total_alerts,
            "resolved_alerts": resolved_alerts,
            "escalation_path": self.config.escalation_path,
            "alert_channels": self.config.alert_channels
        })
    }

    /// Get active alerts
    pub async fn get_active_alerts(&self) -> Vec<ActiveAlert> {
        let alerts = self.active_alerts.read().await;
        alerts.values().cloned().collect()
    }

    /// Get alert history
    pub async fn get_alert_history(&self, limit: Option<usize>) -> Vec<AlertHistory> {
        let history = self.alert_history.read().await;
        let history_vec = history.iter().cloned().collect::<Vec<_>>();

        match limit {
            Some(l) => history_vec.into_iter().take(l).collect(),
            None => history_vec,
        }
    }

    /// Check system health
    pub async fn check_health(&self) -> Value {
        let alerts = self.active_alerts.read().await;
        let unresolved_alerts = alerts.values().filter(|a| !a.acknowledged).count();
        let critical_alerts = alerts
            .values()
            .filter(|a| a.severity == TPSLevel::Critical)
            .count();

        let health_status = if critical_alerts > 0 {
            "critical"
        } else if unresolved_alerts > 5 {
            "warning"
        } else {
            "healthy"
        };

        json!({
            "health": health_status,
            "unresolved_alerts": unresolved_alerts,
            "critical_alerts": critical_alerts,
            "timestamp": chrono::Utc::now().to_rfc3339()
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_andon_config_creation() {
        let config = TPSAndonConfig::default();
        assert!(!config.escalation_path.is_empty());
        assert!(!config.alert_channels.is_empty());
    }

    #[tokio::test]
    async fn test_andon_system_creation() {
        let config = TPSAndonConfig::default();
        let andon = TPSAndonSystem::new(config);
        assert!(andon.is_ok());
    }

    #[tokio::test]
    async fn test_alert_handling() {
        let config = TPSAndonConfig::default();
        let andon = TPSAndonSystem::new(config).unwrap();

        let signal = TPSSignal::new(
            "test_alert".to_string(),
            "Test alert message".to_string(),
            TPSLevel::Warning,
        );

        let result = andon.handle_warning_signal(signal).await;
        assert!(result.is_ok());

        let status = andon.get_status().await;
        assert_eq!(status["active_alerts"], 1);
    }

    #[tokio::test]
    async fn test_alert_acknowledgement() {
        let config = TPSAndonConfig::default();
        let andon = TPSAndonSystem::new(config).unwrap();

        let signal = TPSSignal::new(
            "test_alert".to_string(),
            "Test alert message".to_string(),
            TPSLevel::Warning,
        );

        let result = andon.handle_warning_signal(signal).await.unwrap();
        let alert_id = result["alert_id"].as_str().unwrap();

        let ack_result = andon
            .acknowledge_alert(alert_id, "test_user".to_string())
            .await;
        assert!(ack_result.is_ok());
    }

    #[tokio::test]
    async fn test_alert_resolution() {
        let config = TPSAndonConfig::default();
        let andon = TPSAndonSystem::new(config).unwrap();

        let signal = TPSSignal::new(
            "test_alert".to_string(),
            "Test alert message".to_string(),
            TPSLevel::Warning,
        );

        let result = andon.handle_warning_signal(signal).await.unwrap();
        let alert_id = result["alert_id"].as_str().unwrap();

        let resolve_result = andon
            .resolve_alert(alert_id, "Test resolution".to_string())
            .await;
        assert!(resolve_result.is_ok);

        let status = andon.get_status().await;
        assert_eq!(status["active_alerts"], 0);
    }

    #[tokio::test]
    async fn test_signal_processor() {
        let signal = TPSSignal::new(
            "test_signal".to_string(),
            "Test message".to_string(),
            TPSLevel::Critical,
        );

        let result = TPSSignalProcessor::process_signal(signal).await;
        assert!(result.is_ok());
        assert_eq!(result["status"], "critical_handled");
    }
}
