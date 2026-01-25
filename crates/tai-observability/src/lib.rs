//! TAI Observability: Production-Grade Observability for Rust Applications
//!
//! This crate provides comprehensive observability solutions for Rust applications
//! with GCP Cloud integration:
//!
//! - **Cloud Profiler**: CPU, heap, and thread profiling with flame graph generation
//! - **Cloud Trace**: Distributed tracing with request flow visualization
//! - **Cloud Monitoring**: Custom metrics with alerting rules
//! - **Continuous Profiling**: Automatic production profiling with regression detection
//!
//! ## Features
//!
//! - `gcp`: GCP Cloud integration (default)
//! - `profiling`: CPU and heap profiling
//! - `monitoring`: Cloud Monitoring metrics
//! - `tracing`: Cloud Trace distributed tracing
//! - `full`: All features enabled
//!
//! ## Quick Start
//!
//! ```no_run
//! use tai_observability::cloud_profiler::CloudProfiler;
//! use tai_observability::cloud_trace::CloudTrace;
//! use tai_observability::gcp_monitoring::CloudMonitoring;
//! use std::sync::Arc;
//!
//! #[tokio::main]
//! async fn main() {
//!     // Initialize services
//!     let profiler = Arc::new(CloudProfiler::new("my-project".to_string(), 100));
//!     let tracer = CloudTrace::new("my-project".to_string(), 0.01); // 1% sampling in prod
//!     let monitoring = CloudMonitoring::new("my-project".to_string());
//!
//!     // Start profiling
//!     profiler.start_cpu_profile("profile-1".to_string()).await.ok();
//!
//!     // Start trace
//!     let trace_id = tracer.start_trace("http-request".to_string()).await.ok();
//!
//!     // Do work...
//!
//!     // Finish operations
//!     let profile = profiler.finish_cpu_profile().await.ok();
//!     tracer.finish_trace(trace_id.as_deref().unwrap_or("")).await.ok();
//! }
//! ```

// Public module exports
pub mod cloud_profiler;
pub mod cloud_trace;
pub mod continuous_profiling;
pub mod error;
pub mod gcp_monitoring;

// Re-export commonly used types
pub use cloud_profiler::{CloudProfiler, CpuProfile, ProfileSample, StackFrame};
pub use cloud_trace::{CloudTrace, TraceSpan, SpanStatus, SamplingDecision};
pub use continuous_profiling::{ContinuousProfilingService, RegressionAlert, AlertSeverity};
pub use error::{ObservabilityError, Result};
pub use gcp_monitoring::{CloudMonitoring, MetricDescriptor, MetricType, MetricUnit};

/// Observability system version
pub const VERSION: &str = env!("CARGO_PKG_VERSION");

/// Initialize tracing for observability operations
pub fn init_tracing() {
    use tracing_subscriber::layer::SubscriberExt;
    use tracing_subscriber::util::SubscriberInitExt;

    tracing_subscriber::registry()
        .with(tracing_subscriber::fmt::layer())
        .with(tracing_subscriber::EnvFilter::from_default_env())
        .init();
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_version() {
        assert!(!VERSION.is_empty());
    }

    #[test]
    fn test_imports() {
        // Verify all public exports are accessible
        let _ = std::any::type_name::<CloudProfiler>();
        let _ = std::any::type_name::<CloudTrace>();
        let _ = std::any::type_name::<CloudMonitoring>();
        let _ = std::any::type_name::<ContinuousProfilingService>();
    }
}
