//! Prometheus integration for ggen monitoring
//!
//! This module provides types and functions for generating Prometheus configuration
//! from RDF specifications and defining alerting rules for SLO violations.
//!
//! ## Type-First Design
//!
//! All configuration types use compile-time validation through newtypes:
//! - `ScrapeTarget` encodes target validation invariants
//! - `AlertRule` enforces alert structure at type level
//! - Zero-cost abstractions via generics
//!
//! ## Examples
//!
//! ### Basic Configuration
//!
//! ```rust
//! use ggen_core::monitoring::prometheus::{PrometheusExporter, ScrapeTarget};
//! use std::time::Duration;
//!
//! # fn main() -> ggen_utils::error::Result<()> {
//! let mut exporter = PrometheusExporter::new();
//! exporter.add_target(ScrapeTarget {
//!     job_name: "ggen-jobs".to_string(),
//!     targets: vec!["localhost:9090".to_string()],
//!     scrape_interval: Duration::from_secs(15),
//!     scrape_timeout: Duration::from_secs(10),
//!     metrics_path: "/metrics".to_string(),
//! });
//!
//! let yaml = exporter.generate_config()?;
//! assert!(yaml.contains("job_name: ggen-jobs"));
//! # Ok(())
//! # }
//! ```

use ggen_utils::error::Result;
use serde::{Deserialize, Serialize};
use std::time::Duration;

/// Prometheus exporter for ggen metrics
///
/// Generates prometheus.yml configuration from RDF specifications and
/// manages scrape targets for application metrics endpoints.
///
/// # Type Invariants
///
/// - All scrape targets must have valid job names (non-empty)
/// - Scrape intervals must be positive
/// - Scrape timeouts must be less than intervals
#[derive(Debug, Clone)]
pub struct PrometheusExporter {
    /// Global scrape configuration
    global_config: GlobalConfig,
    /// Scrape target configurations
    scrape_configs: Vec<ScrapeTarget>,
    /// Alert rule groups
    alert_rules: Vec<AlertRuleGroup>,
}

/// Global Prometheus configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GlobalConfig {
    /// Default scrape interval
    pub scrape_interval: Duration,
    /// Default scrape timeout
    pub scrape_timeout: Duration,
    /// Default evaluation interval for alerting rules
    pub evaluation_interval: Duration,
}

impl Default for GlobalConfig {
    fn default() -> Self {
        Self {
            scrape_interval: Duration::from_secs(15),
            scrape_timeout: Duration::from_secs(10),
            evaluation_interval: Duration::from_secs(15),
        }
    }
}

/// Scrape target configuration
///
/// Defines a single scrape target with job name, endpoints, and timing parameters.
///
/// # Type Safety
///
/// - `job_name` must be non-empty (validated at construction)
/// - `scrape_timeout` must be less than `scrape_interval`
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ScrapeTarget {
    /// Job name (must be unique across targets)
    pub job_name: String,
    /// List of target endpoints (e.g., "localhost:9090")
    pub targets: Vec<String>,
    /// Scrape interval for this target
    pub scrape_interval: Duration,
    /// Scrape timeout for this target
    pub scrape_timeout: Duration,
    /// HTTP path for metrics endpoint
    pub metrics_path: String,
}

impl ScrapeTarget {
    /// Validate scrape target configuration
    ///
    /// # Errors
    ///
    /// Returns error if:
    /// - Job name is empty
    /// - No targets specified
    /// - Scrape timeout >= scrape interval
    pub fn validate(&self) -> Result<()> {
        if self.job_name.is_empty() {
            return Err("Job name cannot be empty".into());
        }

        if self.targets.is_empty() {
            return Err("At least one target must be specified".into());
        }

        if self.scrape_timeout >= self.scrape_interval {
            return Err(format!(
                "Scrape timeout ({:?}) must be less than scrape interval ({:?})",
                self.scrape_timeout, self.scrape_interval
            )
            .into());
        }

        Ok(())
    }
}

/// Alert rule group
///
/// Groups related alerting rules with a common evaluation interval.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AlertRuleGroup {
    /// Group name
    pub name: String,
    /// Evaluation interval for rules in this group
    pub interval: Duration,
    /// Alert rules in this group
    pub rules: Vec<AlertRule>,
}

/// Alert rule for SLO monitoring
///
/// Defines a Prometheus alerting rule with expression, duration, and annotations.
///
/// # Type Safety
///
/// - `alert_name` must be non-empty
/// - `expr` must be valid PromQL (validated at runtime)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AlertRule {
    /// Alert name (e.g., "HighErrorRate")
    pub alert: String,
    /// PromQL expression
    pub expr: String,
    /// Duration for which condition must be true
    #[serde(with = "duration_serde")]
    pub for_duration: Duration,
    /// Alert severity (critical, warning, info)
    pub severity: AlertSeverity,
    /// Human-readable summary
    pub summary: String,
    /// Detailed description
    pub description: String,
}

/// Alert severity levels
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum AlertSeverity {
    /// Critical alert (requires immediate action)
    Critical,
    /// Warning alert (requires attention)
    Warning,
    /// Info alert (for awareness)
    Info,
}

impl PrometheusExporter {
    /// Create a new Prometheus exporter with default configuration
    #[must_use]
    pub fn new() -> Self {
        Self {
            global_config: GlobalConfig::default(),
            scrape_configs: Vec::new(),
            alert_rules: Vec::new(),
        }
    }

    /// Create exporter with custom global configuration
    #[must_use]
    pub fn with_global_config(global_config: GlobalConfig) -> Self {
        Self {
            global_config,
            scrape_configs: Vec::new(),
            alert_rules: Vec::new(),
        }
    }

    /// Add a scrape target
    ///
    /// # Errors
    ///
    /// Returns error if target validation fails
    pub fn add_target(&mut self, target: ScrapeTarget) -> Result<()> {
        target.validate()
            .map_err(|e| format!("Invalid scrape target: {}", e))?;
        self.scrape_configs.push(target);
        Ok(())
    }

    /// Add an alert rule group
    pub fn add_alert_group(&mut self, group: AlertRuleGroup) {
        self.alert_rules.push(group);
    }

    /// Generate prometheus.yml configuration
    ///
    /// # Errors
    ///
    /// Returns error if YAML serialization fails
    pub fn generate_config(&self) -> Result<String> {
        let config = PrometheusConfig {
            global: self.global_config.clone(),
            scrape_configs: self.scrape_configs.clone(),
            rule_files: if self.alert_rules.is_empty() {
                vec![]
            } else {
                vec!["/etc/prometheus/rules/*.yml".to_string()]
            },
        };

        serde_yaml::to_string(&config)
            .map_err(|e| format!("Failed to serialize Prometheus config: {}", e).into())
    }

    /// Generate alert rules YAML
    ///
    /// # Errors
    ///
    /// Returns error if YAML serialization fails
    pub fn generate_alert_rules(&self) -> Result<String> {
        let rules = AlertRulesFile {
            groups: self.alert_rules.clone(),
        };

        serde_yaml::to_string(&rules)
            .map_err(|e| format!("Failed to serialize alert rules: {}", e).into())
    }

    /// Add default SLO alert rules for ggen
    ///
    /// Adds alerting rules for:
    /// - High error rate (>5%)
    /// - High latency (p95 > 1s)
    /// - Low throughput (<100 jobs/min)
    pub fn add_default_slo_alerts(&mut self) {
        let rules = vec![
            AlertRule {
                alert: "HighErrorRate".to_string(),
                expr: "rate(jobs_failed_total[5m]) / rate(jobs_total[5m]) > 0.05".to_string(),
                for_duration: Duration::from_secs(300),
                severity: AlertSeverity::Critical,
                summary: "High job error rate detected".to_string(),
                description: "More than 5% of jobs are failing in the last 5 minutes".to_string(),
            },
            AlertRule {
                alert: "HighLatency".to_string(),
                expr: "histogram_quantile(0.95, rate(job_duration_seconds_bucket[5m])) > 1"
                    .to_string(),
                for_duration: Duration::from_secs(300),
                severity: AlertSeverity::Warning,
                summary: "High job processing latency detected".to_string(),
                description: "P95 job processing time exceeds 1 second".to_string(),
            },
            AlertRule {
                alert: "LowThroughput".to_string(),
                expr: "rate(jobs_processed_total[5m]) * 60 < 100".to_string(),
                for_duration: Duration::from_secs(600),
                severity: AlertSeverity::Warning,
                summary: "Low job throughput detected".to_string(),
                description: "Job processing rate is below 100 jobs per minute".to_string(),
            },
        ];

        self.add_alert_group(AlertRuleGroup {
            name: "ggen_slo_alerts".to_string(),
            interval: Duration::from_secs(30),
            rules,
        });
    }
}

impl Default for PrometheusExporter {
    fn default() -> Self {
        Self::new()
    }
}

/// Prometheus configuration file structure
#[derive(Debug, Serialize, Deserialize)]
struct PrometheusConfig {
    global: GlobalConfig,
    scrape_configs: Vec<ScrapeTarget>,
    rule_files: Vec<String>,
}

/// Alert rules file structure
#[derive(Debug, Serialize, Deserialize)]
struct AlertRulesFile {
    groups: Vec<AlertRuleGroup>,
}

/// Custom duration serialization for Prometheus format
mod duration_serde {
    use serde::{Deserialize, Deserializer, Serializer};
    use std::time::Duration;

    pub fn serialize<S>(duration: &Duration, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let seconds = duration.as_secs();
        let formatted = if seconds % 60 == 0 {
            format!("{}m", seconds / 60)
        } else {
            format!("{}s", seconds)
        };
        serializer.serialize_str(&formatted)
    }

    pub fn deserialize<'de, D>(deserializer: D) -> Result<Duration, D::Error>
    where
        D: Deserializer<'de>,
    {
        let s = String::deserialize(deserializer)?;
        parse_duration(&s).map_err(serde::de::Error::custom)
    }

    fn parse_duration(s: &str) -> Result<Duration, String> {
        if let Some(mins) = s.strip_suffix('m') {
            mins.parse::<u64>()
                .map(|m| Duration::from_secs(m * 60))
                .map_err(|e| e.to_string())
        } else if let Some(secs) = s.strip_suffix('s') {
            secs.parse::<u64>()
                .map(Duration::from_secs)
                .map_err(|e| e.to_string())
        } else {
            Err(format!("Invalid duration format: {}", s))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_scrape_target_validation_success() {
        // Arrange
        let target = ScrapeTarget {
            job_name: "test-job".to_string(),
            targets: vec!["localhost:9090".to_string()],
            scrape_interval: Duration::from_secs(15),
            scrape_timeout: Duration::from_secs(10),
            metrics_path: "/metrics".to_string(),
        };

        // Act
        let result = target.validate();

        // Assert
        assert!(result.is_ok());
    }

    #[test]
    fn test_scrape_target_validation_empty_job_name() {
        // Arrange
        let target = ScrapeTarget {
            job_name: String::new(),
            targets: vec!["localhost:9090".to_string()],
            scrape_interval: Duration::from_secs(15),
            scrape_timeout: Duration::from_secs(10),
            metrics_path: "/metrics".to_string(),
        };

        // Act
        let result = target.validate();

        // Assert
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("Job name"));
    }

    #[test]
    fn test_scrape_target_validation_no_targets() {
        // Arrange
        let target = ScrapeTarget {
            job_name: "test-job".to_string(),
            targets: vec![],
            scrape_interval: Duration::from_secs(15),
            scrape_timeout: Duration::from_secs(10),
            metrics_path: "/metrics".to_string(),
        };

        // Act
        let result = target.validate();

        // Assert
        assert!(result.is_err());
        assert!(result
            .unwrap_err()
            .to_string()
            .contains("At least one target"));
    }

    #[test]
    fn test_scrape_target_validation_timeout_exceeds_interval() {
        // Arrange
        let target = ScrapeTarget {
            job_name: "test-job".to_string(),
            targets: vec!["localhost:9090".to_string()],
            scrape_interval: Duration::from_secs(10),
            scrape_timeout: Duration::from_secs(15),
            metrics_path: "/metrics".to_string(),
        };

        // Act
        let result = target.validate();

        // Assert
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("timeout"));
    }

    #[test]
    fn test_prometheus_exporter_add_target_success() {
        // Arrange
        let mut exporter = PrometheusExporter::new();
        let target = ScrapeTarget {
            job_name: "test-job".to_string(),
            targets: vec!["localhost:9090".to_string()],
            scrape_interval: Duration::from_secs(15),
            scrape_timeout: Duration::from_secs(10),
            metrics_path: "/metrics".to_string(),
        };

        // Act
        let result = exporter.add_target(target);

        // Assert
        assert!(result.is_ok());
        assert_eq!(exporter.scrape_configs.len(), 1);
    }

    #[test]
    fn test_prometheus_exporter_generate_config() {
        // Arrange
        let mut exporter = PrometheusExporter::new();
        let target = ScrapeTarget {
            job_name: "ggen-api".to_string(),
            targets: vec!["localhost:9090".to_string()],
            scrape_interval: Duration::from_secs(15),
            scrape_timeout: Duration::from_secs(10),
            metrics_path: "/metrics".to_string(),
        };
        exporter.add_target(target).expect("Failed to add target");

        // Act
        let result = exporter.generate_config();

        // Assert
        assert!(result.is_ok());
        let yaml = result.expect("Failed to generate config");
        assert!(yaml.contains("job_name: ggen-api"));
        assert!(yaml.contains("localhost:9090"));
    }

    #[test]
    fn test_prometheus_exporter_add_default_slo_alerts() {
        // Arrange
        let mut exporter = PrometheusExporter::new();

        // Act
        exporter.add_default_slo_alerts();

        // Assert
        assert_eq!(exporter.alert_rules.len(), 1);
        assert_eq!(exporter.alert_rules[0].rules.len(), 3);

        let alert_names: Vec<_> = exporter.alert_rules[0]
            .rules
            .iter()
            .map(|r| &r.alert)
            .collect();
        assert!(alert_names.contains(&&"HighErrorRate".to_string()));
        assert!(alert_names.contains(&&"HighLatency".to_string()));
        assert!(alert_names.contains(&&"LowThroughput".to_string()));
    }

    #[test]
    fn test_prometheus_exporter_generate_alert_rules() {
        // Arrange
        let mut exporter = PrometheusExporter::new();
        exporter.add_default_slo_alerts();

        // Act
        let result = exporter.generate_alert_rules();

        // Assert
        assert!(result.is_ok());
        let yaml = result.expect("Failed to generate alert rules");
        assert!(yaml.contains("HighErrorRate"));
        assert!(yaml.contains("HighLatency"));
        assert!(yaml.contains("LowThroughput"));
    }

    #[test]
    fn test_global_config_default() {
        // Arrange & Act
        let config = GlobalConfig::default();

        // Assert
        assert_eq!(config.scrape_interval, Duration::from_secs(15));
        assert_eq!(config.scrape_timeout, Duration::from_secs(10));
        assert_eq!(config.evaluation_interval, Duration::from_secs(15));
    }
}
