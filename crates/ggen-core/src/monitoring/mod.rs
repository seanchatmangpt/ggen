//! Monitoring infrastructure for ggen
//!
//! This module provides observability and monitoring capabilities for the ggen system,
//! including Prometheus metrics export and Grafana dashboard generation.
//!
//! ## Features
//!
//! - **Prometheus Integration**: Automatic generation of prometheus.yml configuration from RDF specs
//! - **Grafana Dashboards**: Programmatic creation of JSON dashboards for metrics visualization
//! - **SLO Monitoring**: Alert rules for service level objective violations
//! - **Docker Compose Integration**: Auto-provisioning of monitoring stack
//!
//! ## Modules
//!
//! - [`prometheus`] - Prometheus configuration generation and metrics export
//! - [`grafana`] - Grafana dashboard JSON generation and provisioning
//!
//! ## Examples
//!
//! ### Generating Prometheus Configuration
//!
//! ```rust
//! use ggen_core::monitoring::prometheus::{PrometheusExporter, ScrapeTarget};
//! use std::time::Duration;
//!
//! # fn main() -> ggen_utils::error::Result<()> {
//! let mut exporter = PrometheusExporter::new();
//! exporter.add_target(ScrapeTarget {
//!     job_name: "ggen-api".to_string(),
//!     targets: vec!["localhost:9090".to_string()],
//!     scrape_interval: Duration::from_secs(15),
//!     scrape_timeout: Duration::from_secs(10),
//!     metrics_path: "/metrics".to_string(),
//! });
//!
//! let yaml_config = exporter.generate_config()?;
//! # Ok(())
//! # }
//! ```
//!
//! ### Creating Grafana Dashboard
//!
//! ```rust
//! use ggen_core::monitoring::grafana::{GrafanaDashboard, PanelType};
//!
//! # fn main() -> ggen_utils::error::Result<()> {
//! let mut dashboard = GrafanaDashboard::new("ggen Monitoring", "ggen-mon");
//! dashboard.add_panel(
//!     "Throughput",
//!     PanelType::Graph,
//!     "rate(jobs_processed_total[5m])",
//!     0, 0, 12, 8
//! );
//!
//! let json_config = dashboard.generate_json()?;
//! # Ok(())
//! # }
//! ```

pub mod grafana;
pub mod prometheus;

// Re-export commonly used types for convenience
pub use grafana::{
    GrafanaDashboard, GrafanaProvisioning, Panel, PanelType, Target, DashboardOptions,
};
pub use prometheus::{
    AlertRule, AlertRuleGroup, PrometheusExporter, ScrapeTarget,
};
