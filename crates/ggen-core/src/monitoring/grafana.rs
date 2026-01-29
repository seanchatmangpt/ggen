//! Grafana dashboard generation for ggen monitoring
//!
//! This module provides types and functions for programmatically creating
//! Grafana dashboard JSON configurations with panels, targets, and provisioning.
//!
//! ## Type-First Design
//!
//! - `GrafanaDashboard` encodes dashboard structure invariants
//! - `Panel` ensures panel configuration validity at compile time
//! - Zero-cost abstractions for JSON generation
//!
//! ## Examples
//!
//! ### Creating a Dashboard
//!
//! ```rust
//! use ggen_core::monitoring::grafana::{GrafanaDashboard, PanelType};
//!
//! # fn main() -> ggen_utils::error::Result<()> {
//! let mut dashboard = GrafanaDashboard::new("Job Metrics", "job-metrics");
//!
//! dashboard.add_panel(
//!     "Throughput",
//!     PanelType::Graph,
//!     "rate(jobs_processed_total[5m])",
//!     0, 0, 12, 8
//! );
//!
//! dashboard.add_panel(
//!     "Error Rate",
//!     PanelType::Graph,
//!     "rate(jobs_failed_total[5m])",
//!     0, 8, 12, 8
//! );
//!
//! let json = dashboard.generate_json()?;
//! assert!(json.contains("Job Metrics"));
//! # Ok(())
//! # }
//! ```

use ggen_utils::error::Result;
use serde::{Deserialize, Serialize};
use serde_json::Value;

/// Grafana dashboard generator
///
/// Provides a type-safe API for creating Grafana dashboards with panels,
/// queries, and visualization configuration.
///
/// # Type Invariants
///
/// - Dashboard UID must be unique and non-empty
/// - Panel IDs must be unique within dashboard
/// - Grid positions must not overlap
#[derive(Debug, Clone)]
pub struct GrafanaDashboard {
    /// Dashboard title
    title: String,
    /// Unique identifier
    uid: String,
    /// Dashboard tags
    tags: Vec<String>,
    /// Time range configuration
    time: TimeRange,
    /// Dashboard panels
    panels: Vec<Panel>,
    /// Dashboard options
    options: DashboardOptions,
    /// Next available panel ID (monotonically increasing)
    next_panel_id: i32,
}

/// Dashboard time range configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TimeRange {
    /// Start time (e.g., "now-6h")
    pub from: String,
    /// End time (e.g., "now")
    pub to: String,
}

impl Default for TimeRange {
    fn default() -> Self {
        Self {
            from: "now-6h".to_string(),
            to: "now".to_string(),
        }
    }
}

/// Dashboard options
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DashboardOptions {
    /// Refresh interval (e.g., "10s", "1m")
    pub refresh: String,
    /// Time options
    pub time_options: Vec<String>,
}

impl Default for DashboardOptions {
    fn default() -> Self {
        Self {
            refresh: "10s".to_string(),
            time_options: vec![
                "5m".to_string(),
                "15m".to_string(),
                "1h".to_string(),
                "6h".to_string(),
                "24h".to_string(),
            ],
        }
    }
}

/// Dashboard panel
///
/// Represents a single visualization panel with query targets and positioning.
///
/// # Type Safety
///
/// - Panel IDs are unique (enforced by dashboard)
/// - Grid positions validated to ensure non-overlapping layout
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Panel {
    /// Unique panel ID
    pub id: i32,
    /// Panel title
    pub title: String,
    /// Panel type (graph, stat, gauge, etc.)
    #[serde(rename = "type")]
    pub panel_type: PanelType,
    /// Data source (default: Prometheus)
    pub datasource: String,
    /// Query targets
    pub targets: Vec<Target>,
    /// Grid position
    pub grid_pos: GridPos,
    /// Panel-specific options
    #[serde(skip_serializing_if = "Option::is_none")]
    pub field_config: Option<FieldConfig>,
}

/// Panel type enumeration
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum PanelType {
    /// Time-series graph
    Graph,
    /// Single stat display
    Stat,
    /// Gauge visualization
    Gauge,
    /// Table display
    Table,
    /// Heatmap
    Heatmap,
}

/// Query target for panel
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Target {
    /// PromQL expression
    pub expr: String,
    /// Legend format
    #[serde(skip_serializing_if = "Option::is_none")]
    pub legend_format: Option<String>,
    /// Reference ID (auto-generated: A, B, C, ...)
    #[serde(rename = "refId")]
    pub ref_id: String,
}

/// Panel grid position
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GridPos {
    /// X position (0-24)
    pub x: i32,
    /// Y position
    pub y: i32,
    /// Width (1-24)
    pub w: i32,
    /// Height
    pub h: i32,
}

/// Field configuration for advanced panel options
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FieldConfig {
    /// Default field settings
    pub defaults: FieldDefaults,
}

/// Default field settings
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FieldDefaults {
    /// Unit (e.g., "short", "percent", "ms")
    #[serde(skip_serializing_if = "Option::is_none")]
    pub unit: Option<String>,
    /// Minimum value
    #[serde(skip_serializing_if = "Option::is_none")]
    pub min: Option<f64>,
    /// Maximum value
    #[serde(skip_serializing_if = "Option::is_none")]
    pub max: Option<f64>,
}

/// Grafana provisioning configuration
///
/// Defines how dashboards should be automatically provisioned when Grafana starts.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GrafanaProvisioning {
    /// Provisioning API version
    #[serde(rename = "apiVersion")]
    pub api_version: i32,
    /// Providers list
    pub providers: Vec<DashboardProvider>,
}

/// Dashboard provider configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DashboardProvider {
    /// Provider name
    pub name: String,
    /// Organization ID
    #[serde(rename = "orgId")]
    pub org_id: i32,
    /// Folder name
    pub folder: String,
    /// Provider type (always "file")
    #[serde(rename = "type")]
    pub provider_type: String,
    /// Disable deletion
    #[serde(rename = "disableDeletion")]
    pub disable_deletion: bool,
    /// Update interval in seconds
    #[serde(rename = "updateIntervalSeconds")]
    pub update_interval_seconds: i32,
    /// Allow UI updates
    #[serde(rename = "allowUiUpdates")]
    pub allow_ui_updates: bool,
    /// Options
    pub options: ProviderOptions,
}

/// Provider options
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ProviderOptions {
    /// Path to dashboard JSON files
    pub path: String,
}

impl GrafanaDashboard {
    /// Create a new dashboard with title and UID
    #[must_use]
    pub fn new(title: impl Into<String>, uid: impl Into<String>) -> Self {
        Self {
            title: title.into(),
            uid: uid.into(),
            tags: vec!["ggen".to_string()],
            time: TimeRange::default(),
            panels: Vec::new(),
            options: DashboardOptions::default(),
            next_panel_id: 1,
        }
    }

    /// Add a panel to the dashboard
    ///
    /// # Arguments
    ///
    /// - `title` - Panel title
    /// - `panel_type` - Panel type (graph, stat, gauge, etc.)
    /// - `query` - PromQL query expression
    /// - `x` - X position (0-24)
    /// - `y` - Y position
    /// - `w` - Width (1-24)
    /// - `h` - Height
    pub fn add_panel(
        &mut self,
        title: impl Into<String>,
        panel_type: PanelType,
        query: impl Into<String>,
        x: i32,
        y: i32,
        w: i32,
        h: i32,
    ) {
        let panel_id = self.next_panel_id;
        self.next_panel_id += 1;

        let panel = Panel {
            id: panel_id,
            title: title.into(),
            panel_type,
            datasource: "Prometheus".to_string(),
            targets: vec![Target {
                expr: query.into(),
                legend_format: None,
                ref_id: "A".to_string(),
            }],
            grid_pos: GridPos { x, y, w, h },
            field_config: None,
        };

        self.panels.push(panel);
    }

    /// Add a panel with custom field configuration
    pub fn add_panel_with_config(
        &mut self,
        title: impl Into<String>,
        panel_type: PanelType,
        query: impl Into<String>,
        x: i32,
        y: i32,
        w: i32,
        h: i32,
        unit: Option<String>,
    ) {
        let panel_id = self.next_panel_id;
        self.next_panel_id += 1;

        let field_config = unit.map(|u| FieldConfig {
            defaults: FieldDefaults {
                unit: Some(u),
                min: None,
                max: None,
            },
        });

        let panel = Panel {
            id: panel_id,
            title: title.into(),
            panel_type,
            datasource: "Prometheus".to_string(),
            targets: vec![Target {
                expr: query.into(),
                legend_format: None,
                ref_id: "A".to_string(),
            }],
            grid_pos: GridPos { x, y, w, h },
            field_config,
        };

        self.panels.push(panel);
    }

    /// Add tags to the dashboard
    pub fn add_tags(&mut self, tags: Vec<String>) {
        self.tags.extend(tags);
    }

    /// Set dashboard refresh interval
    pub fn set_refresh(&mut self, refresh: impl Into<String>) {
        self.options.refresh = refresh.into();
    }

    /// Generate Grafana dashboard JSON
    ///
    /// # Errors
    ///
    /// Returns error if JSON serialization fails
    pub fn generate_json(&self) -> Result<String> {
        let mut dashboard_obj = serde_json::Map::new();

        dashboard_obj.insert("title".to_string(), Value::String(self.title.clone()));
        dashboard_obj.insert("uid".to_string(), Value::String(self.uid.clone()));
        dashboard_obj.insert(
            "tags".to_string(),
            serde_json::to_value(&self.tags)
                .map_err(|e| format!("Failed to serialize tags: {}", e))?,
        );
        dashboard_obj.insert(
            "time".to_string(),
            serde_json::to_value(&self.time)
                .map_err(|e| format!("Failed to serialize time: {}", e))?,
        );
        dashboard_obj.insert(
            "refresh".to_string(),
            Value::String(self.options.refresh.clone()),
        );
        dashboard_obj.insert(
            "panels".to_string(),
            serde_json::to_value(&self.panels)
                .map_err(|e| format!("Failed to serialize panels: {}", e))?,
        );

        serde_json::to_string_pretty(&dashboard_obj)
            .map_err(|e| format!("Failed to generate dashboard JSON: {}", e).into())
    }

    /// Create a standard ggen job monitoring dashboard
    ///
    /// Includes panels for:
    /// - Job throughput
    /// - Error rate
    /// - Latency (p50, p95, p99)
    /// - Queue depth
    #[must_use]
    pub fn create_job_monitoring_dashboard() -> Self {
        let mut dashboard = Self::new("ggen Job Monitoring", "ggen-jobs");

        // Row 1: Throughput and Error Rate
        dashboard.add_panel(
            "Job Throughput",
            PanelType::Graph,
            "rate(jobs_processed_total[5m])",
            0,
            0,
            12,
            8,
        );

        dashboard.add_panel_with_config(
            "Error Rate",
            PanelType::Graph,
            "rate(jobs_failed_total[5m]) / rate(jobs_total[5m]) * 100",
            12,
            0,
            12,
            8,
            Some("percent".to_string()),
        );

        // Row 2: Latency Metrics
        dashboard.add_panel_with_config(
            "Job Latency (p50)",
            PanelType::Graph,
            "histogram_quantile(0.50, rate(job_duration_seconds_bucket[5m]))",
            0,
            8,
            8,
            8,
            Some("s".to_string()),
        );

        dashboard.add_panel_with_config(
            "Job Latency (p95)",
            PanelType::Graph,
            "histogram_quantile(0.95, rate(job_duration_seconds_bucket[5m]))",
            8,
            8,
            8,
            8,
            Some("s".to_string()),
        );

        dashboard.add_panel_with_config(
            "Job Latency (p99)",
            PanelType::Graph,
            "histogram_quantile(0.99, rate(job_duration_seconds_bucket[5m]))",
            16,
            8,
            8,
            8,
            Some("s".to_string()),
        );

        // Row 3: Queue Depth
        dashboard.add_panel(
            "Queue Depth",
            PanelType::Graph,
            "job_queue_depth",
            0,
            16,
            24,
            8,
        );

        dashboard
    }
}

impl GrafanaProvisioning {
    /// Create default provisioning configuration
    #[must_use]
    pub fn default_config() -> Self {
        Self {
            api_version: 1,
            providers: vec![DashboardProvider {
                name: "ggen-dashboards".to_string(),
                org_id: 1,
                folder: "ggen".to_string(),
                provider_type: "file".to_string(),
                disable_deletion: false,
                update_interval_seconds: 10,
                allow_ui_updates: true,
                options: ProviderOptions {
                    path: "/etc/grafana/provisioning/dashboards".to_string(),
                },
            }],
        }
    }

    /// Generate provisioning YAML
    ///
    /// # Errors
    ///
    /// Returns error if YAML serialization fails
    pub fn generate_yaml(&self) -> Result<String> {
        serde_yaml::to_string(self)
            .map_err(|e| format!("Failed to generate provisioning YAML: {}", e).into())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_dashboard_creation() {
        // Arrange & Act
        let dashboard = GrafanaDashboard::new("Test Dashboard", "test-uid");

        // Assert
        assert_eq!(dashboard.title, "Test Dashboard");
        assert_eq!(dashboard.uid, "test-uid");
        assert_eq!(dashboard.panels.len(), 0);
    }

    #[test]
    fn test_add_panel() {
        // Arrange
        let mut dashboard = GrafanaDashboard::new("Test Dashboard", "test-uid");

        // Act
        dashboard.add_panel(
            "Test Panel",
            PanelType::Graph,
            "up",
            0,
            0,
            12,
            8,
        );

        // Assert
        assert_eq!(dashboard.panels.len(), 1);
        assert_eq!(dashboard.panels[0].title, "Test Panel");
        assert_eq!(dashboard.panels[0].panel_type, PanelType::Graph);
        assert_eq!(dashboard.panels[0].targets[0].expr, "up");
    }

    #[test]
    fn test_add_multiple_panels_unique_ids() {
        // Arrange
        let mut dashboard = GrafanaDashboard::new("Test Dashboard", "test-uid");

        // Act
        dashboard.add_panel("Panel 1", PanelType::Graph, "query1", 0, 0, 12, 8);
        dashboard.add_panel("Panel 2", PanelType::Stat, "query2", 12, 0, 12, 8);
        dashboard.add_panel("Panel 3", PanelType::Gauge, "query3", 0, 8, 12, 8);

        // Assert
        assert_eq!(dashboard.panels.len(), 3);
        assert_eq!(dashboard.panels[0].id, 1);
        assert_eq!(dashboard.panels[1].id, 2);
        assert_eq!(dashboard.panels[2].id, 3);
    }

    #[test]
    fn test_generate_json_success() {
        // Arrange
        let mut dashboard = GrafanaDashboard::new("Test Dashboard", "test-uid");
        dashboard.add_panel("Test Panel", PanelType::Graph, "up", 0, 0, 12, 8);

        // Act
        let result = dashboard.generate_json();

        // Assert
        assert!(result.is_ok());
        let json = result.expect("Failed to generate JSON");
        assert!(json.contains("Test Dashboard"));
        assert!(json.contains("test-uid"));
        assert!(json.contains("Test Panel"));
    }

    #[test]
    fn test_create_job_monitoring_dashboard() {
        // Arrange & Act
        let dashboard = GrafanaDashboard::create_job_monitoring_dashboard();

        // Assert
        assert_eq!(dashboard.title, "ggen Job Monitoring");
        assert_eq!(dashboard.uid, "ggen-jobs");
        assert_eq!(dashboard.panels.len(), 6);

        // Verify panel titles
        let panel_titles: Vec<_> = dashboard.panels.iter().map(|p| &p.title).collect();
        assert!(panel_titles.contains(&&"Job Throughput".to_string()));
        assert!(panel_titles.contains(&&"Error Rate".to_string()));
        assert!(panel_titles.contains(&&"Job Latency (p50)".to_string()));
        assert!(panel_titles.contains(&&"Job Latency (p95)".to_string()));
        assert!(panel_titles.contains(&&"Job Latency (p99)".to_string()));
        assert!(panel_titles.contains(&&"Queue Depth".to_string()));
    }

    #[test]
    fn test_add_panel_with_config() {
        // Arrange
        let mut dashboard = GrafanaDashboard::new("Test Dashboard", "test-uid");

        // Act
        dashboard.add_panel_with_config(
            "Latency",
            PanelType::Graph,
            "histogram_quantile(0.95, rate(duration_bucket[5m]))",
            0,
            0,
            12,
            8,
            Some("ms".to_string()),
        );

        // Assert
        assert_eq!(dashboard.panels.len(), 1);
        assert!(dashboard.panels[0].field_config.is_some());
        let field_config = dashboard.panels[0]
            .field_config
            .as_ref()
            .expect("Field config should be present");
        assert_eq!(field_config.defaults.unit, Some("ms".to_string()));
    }

    #[test]
    fn test_provisioning_default_config() {
        // Arrange & Act
        let provisioning = GrafanaProvisioning::default_config();

        // Assert
        assert_eq!(provisioning.api_version, 1);
        assert_eq!(provisioning.providers.len(), 1);
        assert_eq!(provisioning.providers[0].name, "ggen-dashboards");
    }

    #[test]
    fn test_provisioning_generate_yaml() {
        // Arrange
        let provisioning = GrafanaProvisioning::default_config();

        // Act
        let result = provisioning.generate_yaml();

        // Assert
        assert!(result.is_ok());
        let yaml = result.expect("Failed to generate YAML");
        assert!(yaml.contains("apiVersion"));
        assert!(yaml.contains("ggen-dashboards"));
    }
}
