//! Observability Dashboard for Governance
//!
//! Provides real-time metrics, health monitoring, and system status visualization through:
//! - Metrics collection and aggregation
//! - Data visualization and reporting
//! - Dashboard API for external integrations

pub mod api;
pub mod metrics;
pub mod visualization;

// Re-export main types for backward compatibility
pub use api::{DashboardApi, ExportFormat};
pub use metrics::{
    Dashboard, DashboardConfig, HealthStatus, MetricsSnapshot, ResourceUsage, SystemStatus,
};
pub use visualization::{TimescaleMetrics, TimeSeriesPoint};
