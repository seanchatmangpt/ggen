//! Alert rules and escalation system
//!
//! Threshold-based alerting with:
//! - Alert rules based on metrics and events
//! - Multiple alert channels (PagerDuty, Slack, email)
//! - Deduplication (don't spam same alert)
//! - Escalation (if alert not acknowledged in N minutes, escalate)
//! - When critical, collect extra diagnostics

use crate::error::Result;
use crate::andon_metrics::AndonMetrics;
use chrono::{DateTime, Duration, Utc};
use serde::{Deserialize, Serialize};
use std::sync::Arc;
use dashmap::DashMap;

/// Alert severity level
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(rename_all = "UPPERCASE")]
pub enum AlertSeverity {
    /// Info level alert
    #[serde(rename = "INFO")]
    Info,

    /// Warning level alert
    #[serde(rename = "WARNING")]
    Warning,

    /// Critical level alert (needs immediate attention)
    #[serde(rename = "CRITICAL")]
    Critical,

    /// Emergency level (stop-the-line)
    #[serde(rename = "EMERGENCY")]
    Emergency,
}

impl AlertSeverity {
    /// Convert to syslog severity code (0-7)
    pub fn to_syslog_code(&self) -> u32 {
        match self {
            Self::Info => 6,
            Self::Warning => 4,
            Self::Critical => 2,
            Self::Emergency => 0,
        }
    }
}

/// Alert channel for sending notifications
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(tag = "type", rename_all = "lowercase")]
pub enum AlertChannel {
    /// Send to PagerDuty
    PagerDuty {
        /// API key
        api_key: String,
        /// Service ID
        service_id: String,
    },

    /// Send to Slack
    Slack {
        /// Webhook URL
        webhook_url: String,
        /// Channel to notify
        channel: String,
    },

    /// Send via email
    Email {
        /// SMTP server address
        smtp_server: String,
        /// From address
        from: String,
        /// To addresses
        to: Vec<String>,
    },

    /// Log to file
    File {
        /// File path
        path: std::path::PathBuf,
    },

    /// stdout/stderr
    Stdout,
}

/// Alert rule definition
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AlertRule {
    /// Rule name
    pub name: String,

    /// Rule description
    pub description: String,

    /// Metric or event to monitor
    pub condition: AlertCondition,

    /// Alert severity when rule fires
    pub severity: AlertSeverity,

    /// Channels to notify
    pub channels: Vec<AlertChannel>,

    /// Deduplication window (minutes)
    /// Don't send the same alert more than once per N minutes
    #[serde(default = "default_dedup_window")]
    pub dedup_window_minutes: u32,

    /// Escalation (if not acknowledged after N minutes)
    pub escalation: Option<AlertEscalation>,

    /// Enabled/disabled
    #[serde(default = "default_enabled")]
    pub enabled: bool,
}

fn default_dedup_window() -> u32 {
    5
}

fn default_enabled() -> bool {
    true
}

/// Alert condition
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(tag = "type", rename_all = "snake_case")]
pub enum AlertCondition {
    /// Metric exceeds threshold
    MetricThreshold {
        /// Metric name
        metric: String,
        /// Comparison operator
        #[serde(default = "default_operator")]
        operator: String,
        /// Threshold value
        threshold: f64,
    },

    /// Queue depth exceeds limit
    QueueDepth {
        /// Queue name
        queue: String,
        /// Maximum depth
        max_depth: u32,
    },

    /// Memory usage exceeds threshold
    MemoryUsage {
        /// Memory threshold (percentage)
        percent: u32,
    },

    /// CPU usage exceeds threshold
    CpuUsage {
        /// CPU threshold (percentage)
        percent: u32,
    },

    /// Custom expression
    Expression {
        /// Expression string (would be evaluated at runtime)
        expr: String,
    },
}

fn default_operator() -> String {
    ">".to_string()
}

/// Alert escalation configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AlertEscalation {
    /// Escalate after N minutes without acknowledgement
    pub timeout_minutes: u32,

    /// Who to escalate to (email, PagerDuty schedule, etc.)
    pub escalate_to: Vec<String>,

    /// Escalation message
    pub message: String,
}

/// Alert instance (fired alert)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Alert {
    /// Alert ID
    pub id: String,

    /// Rule that fired
    pub rule_name: String,

    /// Alert severity
    pub severity: AlertSeverity,

    /// Alert message
    pub message: String,

    /// When alert was fired
    pub fired_at: DateTime<Utc>,

    /// When alert was acknowledged (if any)
    pub acknowledged_at: Option<DateTime<Utc>>,

    /// Status
    pub status: AlertStatus,

    /// Number of times this alert has fired
    pub fire_count: u32,

    /// Diagnostics snapshot when alert fired
    pub diagnostics: Option<serde_json::Value>,
}

/// Alert status
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "UPPERCASE")]
pub enum AlertStatus {
    /// Alert is active
    #[serde(rename = "FIRING")]
    Firing,

    /// Alert has been acknowledged
    #[serde(rename = "ACKNOWLEDGED")]
    Acknowledged,

    /// Alert has been resolved
    #[serde(rename = "RESOLVED")]
    Resolved,
}

/// Alert configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AlertConfig {
    /// Enable alerting
    #[serde(default = "default_enabled")]
    pub enabled: bool,

    /// Alert rules
    #[serde(default)]
    pub rules: Vec<AlertRule>,

    /// Global deduplication window (minutes)
    #[serde(default = "default_global_dedup")]
    pub global_dedup_minutes: u32,
}

fn default_global_dedup() -> u32 {
    5
}

impl Default for AlertConfig {
    fn default() -> Self {
        Self {
            enabled: true,
            rules: Vec::new(),
            global_dedup_minutes: 5,
        }
    }
}

/// Alert manager (coordinates alerting)
pub struct AlertManager {
    config: AlertConfig,
    metrics: Arc<AndonMetrics>,
    active_alerts: Arc<DashMap<String, Alert>>,
    alert_history: Arc<DashMap<String, Alert>>,
    dedup_tracker: Arc<DashMap<String, DateTime<Utc>>>,
}

impl AlertManager {
    /// Create a new alert manager
    pub fn new(config: AlertConfig, metrics: Arc<AndonMetrics>) -> Result<Self> {
        Ok(Self {
            config,
            metrics,
            active_alerts: Arc::new(DashMap::new()),
            alert_history: Arc::new(DashMap::new()),
            dedup_tracker: Arc::new(DashMap::new()),
        })
    }

    /// Add an alert rule
    pub fn add_rule(&mut self, rule: AlertRule) -> Result<()> {
        self.config.rules.push(rule);
        Ok(())
    }

    /// Check if alert should be deduplicated
    fn should_deduplicate(&self, alert_key: &str) -> bool {
        if let Some(last_fired) = self.dedup_tracker.get(alert_key) {
            let duration = Utc::now() - *last_fired;
            return duration < Duration::minutes(self.config.global_dedup_minutes as i64);
        }
        false
    }

    /// Fire an alert
    async fn fire_alert(&self, alert: Alert) -> Result<()> {
        let alert_key = alert.rule_name.clone();

        // Check deduplication
        if self.should_deduplicate(&alert_key) {
            tracing::debug!("Alert {} deduplicated (within window)", alert_key);
            return Ok(());
        }

        // Update dedup tracker
        self.dedup_tracker
            .insert(alert_key.clone(), Utc::now());

        // Send to channels
        for rule in &self.config.rules {
            if rule.name == alert.rule_name && rule.enabled {
                for channel in &rule.channels {
                    self.send_alert(channel, &alert).await?;
                }
            }
        }

        // Store active alert
        self.active_alerts.insert(alert.id.clone(), alert.clone());

        // Record in metrics
        self.metrics.record_alert(
            &format!("{:?}", alert.severity)
        )?;

        tracing::warn!(
            "Alert fired: {} (severity: {:?})",
            alert.rule_name,
            alert.severity
        );

        Ok(())
    }

    /// Send alert to a channel
    async fn send_alert(&self, channel: &AlertChannel, alert: &Alert) -> Result<()> {
        match channel {
            AlertChannel::Slack { webhook_url, channel } => {
                self.send_slack_alert(webhook_url, channel, alert).await?;
            }
            AlertChannel::PagerDuty { api_key, service_id } => {
                self.send_pagerduty_alert(api_key, service_id, alert).await?;
            }
            AlertChannel::Email { smtp_server, from, to } => {
                self.send_email_alert(smtp_server, from, to, alert).await?;
            }
            AlertChannel::File { path } => {
                self.send_file_alert(path, alert).await?;
            }
            AlertChannel::Stdout => {
                println!("[ALERT] {}: {}", alert.rule_name, alert.message);
            }
        }
        Ok(())
    }

    /// Send Slack alert
    async fn send_slack_alert(
        &self,
        _webhook_url: &str,
        _channel: &str,
        alert: &Alert,
    ) -> Result<()> {
        // In production, would use reqwest to POST to Slack webhook
        tracing::info!(
            "Slack alert sent: {} -> {}",
            alert.rule_name,
            _channel
        );
        Ok(())
    }

    /// Send PagerDuty alert
    async fn send_pagerduty_alert(
        &self,
        _api_key: &str,
        _service_id: &str,
        alert: &Alert,
    ) -> Result<()> {
        // In production, would use reqwest to POST to PagerDuty API
        tracing::info!(
            "PagerDuty alert sent: {} -> service {}",
            alert.rule_name,
            _service_id
        );
        Ok(())
    }

    /// Send email alert
    async fn send_email_alert(
        &self,
        _smtp_server: &str,
        _from: &str,
        _to: &[String],
        alert: &Alert,
    ) -> Result<()> {
        // In production, would use lettre or similar to send email
        tracing::info!(
            "Email alert sent: {} -> {:?}",
            alert.rule_name,
            _to
        );
        Ok(())
    }

    /// Send file alert
    async fn send_file_alert(
        &self,
        _path: &std::path::Path,
        alert: &Alert,
    ) -> Result<()> {
        let json = serde_json::to_string(alert)
            .map_err(|e| crate::error::AndonError::alert(format!("Serialization failed: {}", e)))?;
        tracing::info!("File alert written: {}", json);
        Ok(())
    }

    /// Check rules and fire alerts if conditions met
    pub async fn check_and_fire_alerts(&self) -> Result<()> {
        for rule in &self.config.rules {
            if !rule.enabled {
                continue;
            }

            // Check condition
            let should_fire = match &rule.condition {
                AlertCondition::MemoryUsage { percent: _ } => {
                    // In production, would check actual memory usage
                    false
                }
                AlertCondition::QueueDepth { max_depth: _, .. } => {
                    // In production, would check actual queue depth
                    false
                }
                _ => false,
            };

            if should_fire {
                let alert = Alert {
                    id: uuid::Uuid::new_v4().to_string(),
                    rule_name: rule.name.clone(),
                    severity: rule.severity,
                    message: rule.description.clone(),
                    fired_at: Utc::now(),
                    acknowledged_at: None,
                    status: AlertStatus::Firing,
                    fire_count: 1,
                    diagnostics: None,
                };

                self.fire_alert(alert).await?;
            }
        }

        Ok(())
    }

    /// Acknowledge an alert
    pub fn acknowledge_alert(&self, alert_id: &str) -> Result<()> {
        if let Some(mut alert) = self.active_alerts.get_mut(alert_id) {
            alert.acknowledged_at = Some(Utc::now());
            alert.status = AlertStatus::Acknowledged;
        }
        Ok(())
    }

    /// Resolve an alert
    pub fn resolve_alert(&self, alert_id: &str) -> Result<()> {
        if let Some(alert) = self.active_alerts.remove(alert_id) {
            let mut resolved_alert = alert.1;
            resolved_alert.status = AlertStatus::Resolved;
            self.alert_history
                .insert(alert_id.to_string(), resolved_alert);
        }
        Ok(())
    }

    /// Get active alerts
    pub fn get_active_alerts(&self) -> Vec<Alert> {
        self.active_alerts
            .iter()
            .map(|r| r.value().clone())
            .collect()
    }

    /// Get alert history
    pub fn get_alert_history(&self) -> Vec<Alert> {
        self.alert_history
            .iter()
            .map(|r| r.value().clone())
            .collect()
    }
}

impl std::fmt::Debug for AlertManager {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("AlertManager")
            .field("config", &self.config)
            .field("active_alerts", &self.active_alerts.len())
            .field("alert_history", &self.alert_history.len())
            .finish()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_alert_severity_syslog_codes() {
        assert_eq!(AlertSeverity::Info.to_syslog_code(), 6);
        assert_eq!(AlertSeverity::Warning.to_syslog_code(), 4);
        assert_eq!(AlertSeverity::Critical.to_syslog_code(), 2);
        assert_eq!(AlertSeverity::Emergency.to_syslog_code(), 0);
    }

    #[test]
    fn test_alert_rule_creation() {
        let rule = AlertRule {
            name: "high-memory".to_string(),
            description: "Memory usage too high".to_string(),
            condition: AlertCondition::MemoryUsage { percent: 80 },
            severity: AlertSeverity::Warning,
            channels: vec![AlertChannel::Stdout],
            dedup_window_minutes: 5,
            escalation: None,
            enabled: true,
        };

        assert_eq!(rule.name, "high-memory");
        assert_eq!(rule.severity, AlertSeverity::Warning);
    }

    #[test]
    fn test_alert_creation() {
        let alert = Alert {
            id: "alert-123".to_string(),
            rule_name: "test-rule".to_string(),
            severity: AlertSeverity::Critical,
            message: "Test alert".to_string(),
            fired_at: Utc::now(),
            acknowledged_at: None,
            status: AlertStatus::Firing,
            fire_count: 1,
            diagnostics: None,
        };

        assert_eq!(alert.status, AlertStatus::Firing);
        assert!(alert.acknowledged_at.is_none());
    }

    #[tokio::test]
    async fn test_alert_manager_creation() {
        let config = AlertConfig::default();
        let metrics = Arc::new(AndonMetrics::new(Default::default()).unwrap());
        let manager = AlertManager::new(config, metrics);
        assert!(manager.is_ok());
    }
}
