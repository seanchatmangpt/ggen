#![doc = include_str!("../README.md")]
#![warn(
    missing_docs,
    missing_debug_implementations,
    rust_2018_idioms,
    unreachable_pub
)]
#![forbid(unsafe_code)]

//! # ggen-tps-andon: Toyota Production System Visibility & Alerting
//!
//! Production-grade Andon system implementing TPS principles:
//! - **Problem Visibility**: Structured logging, metrics, distributed tracing
//! - **Automatic Alerting**: Threshold-based alerts with escalation
//! - **Diagnostics**: Runtime health checks and performance monitoring
//! - **Stop-the-Line**: Critical alerts trigger immediate action
//!
//! ## Core Components
//!
//! - [`AndonLogger`]: Structured JSON logging with Lager-like handlers and sampling
//! - [`AndonMetrics`]: Prometheus metrics (counters, gauges, histograms)
//! - [`AndonTracer`]: OpenTelemetry distributed tracing with span context
//! - [`AndonObserver`]: Runtime diagnostics (memory, processes, file descriptors)
//! - [`AndonAlert`]: Alert rules with threshold-based triggering and escalation
//!
//! ## TPS Andon Philosophy
//!
//! In Toyota manufacturing, Andon is a visual control system where workers can signal problems.
//! When a problem is detected, a cord is pulled, triggering:
//! 1. **Visibility**: A light illuminates above the station showing the problem location
//! 2. **Immediate Response**: Team members respond to fix the issue
//! 3. **Stop-the-Line**: Production halts if the problem isn't fixed quickly
//! 4. **Root Cause Analysis**: Why did this happen? Prevent recurrence.
//!
//! In software systems, Andon means:
//! - **Every failure is visible** (structured logs, metrics, traces)
//! - **Thresholds trigger alerts** (not just humans noticing)
//! - **Critical issues stop processing** (jidoka - autonomic response)
//! - **No silent failures** (observability is mandatory, not optional)

pub mod andon_alert;
pub mod andon_logger;
pub mod andon_metrics;
pub mod andon_observer;
pub mod andon_tracer;
pub mod error;
pub mod signal;

pub use andon_alert::{
    AlertChannel, AlertCondition, AlertConfig, AlertManager, AlertRule, AlertSeverity,
};
pub use andon_logger::{AndonLogger, LogConfig, LogLevel, LogSink};
pub use andon_metrics::{AndonMetrics, MetricConfig};
pub use andon_observer::{AndonObserver, ObserverConfig};
pub use andon_tracer::{AndonTracer, SpanStatus, TracerConfig};
pub use error::{AndonError, Result};
pub use signal::{AndonSignal, SignalColor};

use std::sync::Arc;
use tracing::info;

/// TPS Andon system configuration
#[derive(Debug, Clone)]
pub struct AndonConfig {
    /// Logger configuration
    pub logger: LogConfig,
    /// Metrics configuration
    pub metrics: MetricConfig,
    /// Tracer configuration
    pub tracer: TracerConfig,
    /// Observer configuration
    pub observer: ObserverConfig,
    /// Alert configuration
    pub alert: AlertConfig,
}

impl Default for AndonConfig {
    fn default() -> Self {
        Self {
            logger: LogConfig::default(),
            metrics: MetricConfig::default(),
            tracer: TracerConfig::default(),
            observer: ObserverConfig::default(),
            alert: AlertConfig::default(),
        }
    }
}

/// Complete TPS Andon system (gen_event pattern equivalent)
///
/// This is the main entry point for all Andon functionality. It coordinates:
/// - Structured logging with sampling and routing
/// - Prometheus metrics with threshold monitoring
/// - OpenTelemetry distributed tracing
/// - Runtime diagnostics and health checks
/// - Alert rules and escalation
#[derive(Clone)]
pub struct AndonSystem {
    logger: Arc<AndonLogger>,
    metrics: Arc<AndonMetrics>,
    tracer: Arc<AndonTracer>,
    observer: Arc<AndonObserver>,
    alert: Arc<AlertManager>,
}

impl AndonSystem {
    /// Initialize a complete Andon system
    #[tracing::instrument(skip(config))]
    pub async fn new(config: AndonConfig) -> Result<Self> {
        info!("Initializing TPS Andon system");

        let logger = Arc::new(AndonLogger::new(config.logger)?);
        let metrics = Arc::new(AndonMetrics::new(config.metrics)?);
        let tracer = Arc::new(AndonTracer::new(config.tracer)?);
        let observer = Arc::new(AndonObserver::new(config.observer)?);
        let alert = Arc::new(AlertManager::new(config.alert, metrics.clone())?);

        info!("Andon system initialized successfully");

        Ok(Self {
            logger,
            metrics,
            tracer,
            observer,
            alert,
        })
    }

    /// Get reference to logger
    pub fn logger(&self) -> &AndonLogger {
        &self.logger
    }

    /// Get reference to metrics
    pub fn metrics(&self) -> &AndonMetrics {
        &self.metrics
    }

    /// Get reference to tracer
    pub fn tracer(&self) -> &AndonTracer {
        &self.tracer
    }

    /// Get reference to observer
    pub fn observer(&self) -> &AndonObserver {
        &self.observer
    }

    /// Get reference to alert manager
    pub fn alert(&self) -> &AlertManager {
        &self.alert
    }

    /// Start background health checks and diagnostics
    #[tracing::instrument(skip(self))]
    pub async fn start_health_checks(&self) -> Result<()> {
        info!("Starting background health checks");

        let observer = self.observer.clone();
        let metrics = self.metrics.clone();

        tokio::spawn(async move {
            loop {
                tokio::time::sleep(tokio::time::Duration::from_secs(60)).await;

                if let Err(e) = observer.run_diagnostics().await {
                    tracing::warn!("Diagnostics failed: {}", e);
                }

                if let Err(e) = metrics.check_health().await {
                    tracing::warn!("Metrics health check failed: {}", e);
                }
            }
        });

        Ok(())
    }

    /// Stop the Andon system (cleanup)
    #[tracing::instrument(skip(self))]
    pub async fn shutdown(&self) -> Result<()> {
        info!("Shutting down Andon system");
        self.logger.flush().await?;
        self.metrics.shutdown().await?;
        self.tracer.shutdown().await?;
        Ok(())
    }

    /// Signal a problem (pull the Andon cord)
    ///
    /// This is the main interface for reporting problems in the system.
    /// It will:
    /// 1. Log the problem at WARNING or CRITICAL level
    /// 2. Update metrics
    /// 3. Create a trace span
    /// 4. Trigger alerts if thresholds are crossed
    #[tracing::instrument(skip(self, signal))]
    pub async fn signal_problem(&self, signal: AndonSignal) -> Result<()> {
        let color = signal.color;
        let level = match color {
            SignalColor::Red => LogLevel::Critical,
            SignalColor::Yellow => LogLevel::Warning,
            SignalColor::Green => LogLevel::Info,
        };

        self.logger.log(level, &signal.message).await?;
        self.metrics.record_signal(&signal).await?;

        // Trigger alerts if necessary
        if matches!(color, SignalColor::Red | SignalColor::Yellow) {
            self.alert.check_and_fire_alerts().await?;
        }

        Ok(())
    }
}

impl std::fmt::Debug for AndonSystem {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("AndonSystem")
            .field("logger", &"AndonLogger")
            .field("metrics", &"AndonMetrics")
            .field("tracer", &"AndonTracer")
            .field("observer", &"AndonObserver")
            .field("alert", &"AlertManager")
            .finish()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_andon_system_initialization() {
        let config = AndonConfig::default();
        let system = AndonSystem::new(config).await;
        assert!(system.is_ok());
    }

    #[tokio::test]
    async fn test_andon_system_signal_problem() {
        let config = AndonConfig::default();
        let system = AndonSystem::new(config).await.unwrap();

        let signal = AndonSignal::yellow("Test queue overflow");
        let result = system.signal_problem(signal).await;
        assert!(result.is_ok());
    }
}
