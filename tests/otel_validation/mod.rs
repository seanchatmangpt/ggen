//! OpenTelemetry validation framework for README capabilities
//!
//! This module provides trace-based validation that all README capabilities
//! work correctly end-to-end using OpenTelemetry instrumentation.

use anyhow::{Context, Result};
use ggen_core::telemetry::{init_telemetry, shutdown_telemetry, TelemetryConfig};
use opentelemetry::{global, trace::Tracer, KeyValue};
use std::collections::HashMap;
use std::sync::{Arc, Mutex};
use std::time::Duration;
use tracing::{info, instrument, warn};

pub mod capabilities;
pub mod collectors;
pub mod validators;

/// Trace collector for validation
#[derive(Debug, Clone)]
pub struct TraceCollector {
    spans: Arc<Mutex<Vec<SpanRecord>>>,
    metrics: Arc<Mutex<HashMap<String, f64>>>,
}

#[derive(Debug, Clone)]
pub struct SpanRecord {
    pub name: String,
    pub duration_ms: f64,
    pub attributes: HashMap<String, String>,
    pub status: SpanStatus,
}

#[derive(Debug, Clone, PartialEq)]
pub enum SpanStatus {
    Ok,
    Error,
}

impl TraceCollector {
    pub fn new() -> Self {
        Self {
            spans: Arc::new(Mutex::new(Vec::new())),
            metrics: Arc::new(Mutex::new(HashMap::new())),
        }
    }

    pub fn record_span(&self, span: SpanRecord) {
        if let Ok(mut spans) = self.spans.lock() {
            spans.push(span);
        }
    }

    pub fn record_metric(&self, name: String, value: f64) {
        if let Ok(mut metrics) = self.metrics.lock() {
            metrics.insert(name, value);
        }
    }

    pub fn get_spans(&self) -> Vec<SpanRecord> {
        self.spans.lock().unwrap().clone()
    }

    pub fn get_metrics(&self) -> HashMap<String, f64> {
        self.metrics.lock().unwrap().clone()
    }

    pub fn find_span(&self, name: &str) -> Option<SpanRecord> {
        self.spans
            .lock()
            .unwrap()
            .iter()
            .find(|s| s.name == name)
            .cloned()
    }

    pub fn assert_span_exists(&self, name: &str) -> Result<()> {
        self.find_span(name)
            .context(format!("Expected span '{}' not found", name))?;
        Ok(())
    }

    pub fn assert_span_success(&self, name: &str) -> Result<()> {
        let span = self
            .find_span(name)
            .context(format!("Span '{}' not found", name))?;

        if span.status != SpanStatus::Ok {
            anyhow::bail!("Span '{}' did not complete successfully", name);
        }
        Ok(())
    }

    pub fn assert_duration_under(&self, name: &str, max_ms: f64) -> Result<()> {
        let span = self
            .find_span(name)
            .context(format!("Span '{}' not found", name))?;

        if span.duration_ms > max_ms {
            anyhow::bail!(
                "Span '{}' took {}ms (expected <{}ms)",
                name,
                span.duration_ms,
                max_ms
            );
        }
        Ok(())
    }

    pub fn clear(&self) {
        if let Ok(mut spans) = self.spans.lock() {
            spans.clear();
        }
        if let Ok(mut metrics) = self.metrics.lock() {
            metrics.clear();
        }
    }
}

impl Default for TraceCollector {
    fn default() -> Self {
        Self::new()
    }
}

/// Validation context for tests
pub struct ValidationContext {
    pub collector: TraceCollector,
    pub config: TelemetryConfig,
}

impl ValidationContext {
    pub fn new() -> Self {
        Self {
            collector: TraceCollector::new(),
            config: TelemetryConfig {
                endpoint: "http://localhost:4318".to_string(),
                service_name: "ggen-validation".to_string(),
                sample_ratio: 1.0,
                console_output: false,
            },
        }
    }

    pub fn init(&self) -> Result<()> {
        init_telemetry(self.config.clone())?;
        Ok(())
    }

    pub fn shutdown(&self) {
        shutdown_telemetry();
    }
}

impl Default for ValidationContext {
    fn default() -> Self {
        Self::new()
    }
}

/// Validation result for a single capability
#[derive(Debug)]
pub struct ValidationResult {
    pub capability: String,
    pub success: bool,
    pub duration_ms: f64,
    pub errors: Vec<String>,
    pub spans_validated: usize,
    pub metrics_validated: usize,
}

impl ValidationResult {
    pub fn success(capability: String, duration_ms: f64, spans: usize, metrics: usize) -> Self {
        Self {
            capability,
            success: true,
            duration_ms,
            errors: Vec::new(),
            spans_validated: spans,
            metrics_validated: metrics,
        }
    }

    pub fn failure(capability: String, duration_ms: f64, errors: Vec<String>) -> Self {
        Self {
            capability,
            success: false,
            duration_ms,
            errors,
            spans_validated: 0,
            metrics_validated: 0,
        }
    }
}

/// Overall validation report
#[derive(Debug)]
pub struct ValidationReport {
    pub results: Vec<ValidationResult>,
    pub total_duration_ms: f64,
    pub success_rate: f64,
}

impl ValidationReport {
    pub fn new(results: Vec<ValidationResult>, total_duration_ms: f64) -> Self {
        let total = results.len() as f64;
        let successes = results.iter().filter(|r| r.success).count() as f64;
        let success_rate = if total > 0.0 {
            (successes / total) * 100.0
        } else {
            0.0
        };

        Self {
            results,
            total_duration_ms,
            success_rate,
        }
    }

    pub fn print_summary(&self) {
        println!("\n═══════════════════════════════════════════════════════");
        println!("      OpenTelemetry Validation Report");
        println!("═══════════════════════════════════════════════════════");
        println!(
            "\nOverall: {:.1}% success ({}/{} capabilities)",
            self.success_rate,
            self.results.iter().filter(|r| r.success).count(),
            self.results.len()
        );
        println!("Total Duration: {:.2}ms\n", self.total_duration_ms);

        println!("Capability Details:");
        println!("───────────────────────────────────────────────────────");

        for result in &self.results {
            let status = if result.success {
                "✅ PASS"
            } else {
                "❌ FAIL"
            };
            println!(
                "{} {} ({:.2}ms)",
                status, result.capability, result.duration_ms
            );

            if result.success {
                println!(
                    "   Validated: {} spans, {} metrics",
                    result.spans_validated, result.metrics_validated
                );
            } else {
                println!("   Errors:");
                for error in &result.errors {
                    println!("     - {}", error);
                }
            }
            println!();
        }

        println!("═══════════════════════════════════════════════════════\n");
    }

    pub fn to_markdown(&self) -> String {
        let mut md = String::new();
        md.push_str("# OpenTelemetry Validation Report\n\n");

        md.push_str(&format!(
            "**Overall Success Rate:** {:.1}% ({}/{})\n",
            self.success_rate,
            self.results.iter().filter(|r| r.success).count(),
            self.results.len()
        ));
        md.push_str(&format!(
            "**Total Duration:** {:.2}ms\n\n",
            self.total_duration_ms
        ));

        md.push_str("## Capability Validation Results\n\n");
        md.push_str("| Capability | Status | Duration (ms) | Spans | Metrics | Errors |\n");
        md.push_str("|------------|--------|---------------|-------|---------|--------|\n");

        for result in &self.results {
            let status = if result.success {
                "✅ PASS"
            } else {
                "❌ FAIL"
            };
            let errors = if result.errors.is_empty() {
                "None".to_string()
            } else {
                format!("{} error(s)", result.errors.len())
            };

            md.push_str(&format!(
                "| {} | {} | {:.2} | {} | {} | {} |\n",
                result.capability,
                status,
                result.duration_ms,
                result.spans_validated,
                result.metrics_validated,
                errors
            ));
        }

        md.push_str("\n## Detailed Errors\n\n");
        for result in &self.results {
            if !result.errors.is_empty() {
                md.push_str(&format!("### {}\n\n", result.capability));
                for error in &result.errors {
                    md.push_str(&format!("- {}\n", error));
                }
                md.push_str("\n");
            }
        }

        md
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_trace_collector() {
        let collector = TraceCollector::new();

        let span = SpanRecord {
            name: "test_span".to_string(),
            duration_ms: 100.0,
            attributes: HashMap::new(),
            status: SpanStatus::Ok,
        };

        collector.record_span(span);
        assert_eq!(collector.get_spans().len(), 1);

        collector.assert_span_exists("test_span").unwrap();
        collector.assert_span_success("test_span").unwrap();
        collector.assert_duration_under("test_span", 200.0).unwrap();
    }

    #[test]
    fn test_validation_result() {
        let result = ValidationResult::success("test".to_string(), 100.0, 5, 3);
        assert!(result.success);
        assert_eq!(result.spans_validated, 5);
        assert_eq!(result.metrics_validated, 3);
    }

    #[test]
    fn test_validation_report() {
        let results = vec![
            ValidationResult::success("cap1".to_string(), 100.0, 5, 3),
            ValidationResult::failure("cap2".to_string(), 200.0, vec!["Error 1".to_string()]),
        ];

        let report = ValidationReport::new(results, 300.0);
        assert_eq!(report.success_rate, 50.0);
        assert_eq!(report.total_duration_ms, 300.0);
    }
}
