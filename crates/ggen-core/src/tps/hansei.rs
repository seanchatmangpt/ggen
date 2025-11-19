//! Hansei - Reflection and continuous improvement
//!
//! This module implements metrics collection and retrospective analysis,
//! following the TPS principle of "reflection" for continuous improvement.

use ggen_utils::error::{Error, Result};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::path::PathBuf;

/// A single generation event for metrics
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GenerationEvent {
    /// When the generation occurred
    pub timestamp: chrono::DateTime<chrono::Utc>,
    /// Duration in milliseconds
    pub duration_ms: u64,
    /// Number of files generated
    pub files_generated: usize,
    /// Number of errors encountered
    pub errors: usize,
    /// Number of warnings
    pub warnings: usize,
    /// Total lines of code generated
    pub code_lines: usize,
    /// Template set used
    pub templates: Vec<String>,
    /// Ontology version
    pub ontology_version: String,
}

impl GenerationEvent {
    /// Create a new generation event
    pub fn new(
        duration_ms: u64,
        files_generated: usize,
        errors: usize,
        warnings: usize,
        code_lines: usize,
        templates: Vec<String>,
        ontology_version: String,
    ) -> Self {
        Self {
            timestamp: chrono::Utc::now(),
            duration_ms,
            files_generated,
            errors,
            warnings,
            code_lines,
            templates,
            ontology_version,
        }
    }

    /// Check if this was a successful generation
    pub fn is_success(&self) -> bool {
        self.errors == 0
    }
}

/// Trends over time
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Trends {
    /// Average generation time in milliseconds
    pub avg_generation_time_ms: f64,
    /// Average files per generation
    pub avg_files_per_generation: f64,
    /// Error rate (0.0-1.0)
    pub error_rate: f64,
    /// Code growth rate (lines per day)
    pub code_growth_rate: f64,
    /// Most frequently changed templates
    pub top_templates: Vec<(String, usize)>,
}

/// Metrics collector for generation statistics
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MetricsCollector {
    /// All recorded events
    events: Vec<GenerationEvent>,
}

impl MetricsCollector {
    /// Create a new metrics collector
    pub fn new() -> Self {
        Self {
            events: Vec::new(),
        }
    }

    /// Load from JSON file
    pub fn load(path: &PathBuf) -> Result<Self> {
        let content = std::fs::read_to_string(path)
            .map_err(|e| Error::new(&format!("Failed to read metrics: {}", e)))?;
        serde_json::from_str(&content)
            .map_err(|e| Error::new(&format!("Failed to parse metrics: {}", e)))
    }

    /// Save to JSON file
    pub fn save(&self, path: &PathBuf) -> Result<()> {
        let content = serde_json::to_string_pretty(self)
            .map_err(|e| Error::new(&format!("Failed to serialize metrics: {}", e)))?;

        // Create parent directory if needed
        if let Some(parent) = path.parent() {
            std::fs::create_dir_all(parent)
                .map_err(|e| Error::new(&format!("Failed to create metrics directory: {}", e)))?;
        }

        std::fs::write(path, content)
            .map_err(|e| Error::new(&format!("Failed to write metrics: {}", e)))
    }

    /// Record a generation event
    pub fn record(&mut self, event: GenerationEvent) {
        self.events.push(event);
    }

    /// Get trends for the last N days
    pub fn get_trends(&self, days: u32) -> Trends {
        let cutoff = chrono::Utc::now() - chrono::Duration::days(days as i64);
        let recent_events: Vec<_> = self
            .events
            .iter()
            .filter(|e| e.timestamp >= cutoff)
            .collect();

        if recent_events.is_empty() {
            return Trends {
                avg_generation_time_ms: 0.0,
                avg_files_per_generation: 0.0,
                error_rate: 0.0,
                code_growth_rate: 0.0,
                top_templates: Vec::new(),
            };
        }

        let total_time: u64 = recent_events.iter().map(|e| e.duration_ms).sum();
        let total_files: usize = recent_events.iter().map(|e| e.files_generated).sum();
        let total_errors: usize = recent_events.iter().map(|e| e.errors).sum();
        let total_code_lines: usize = recent_events.iter().map(|e| e.code_lines).sum();

        let count = recent_events.len() as f64;

        // Calculate template frequency
        let mut template_counts: HashMap<String, usize> = HashMap::new();
        for event in &recent_events {
            for template in &event.templates {
                *template_counts.entry(template.clone()).or_insert(0) += 1;
            }
        }

        let mut top_templates: Vec<_> = template_counts.into_iter().collect();
        top_templates.sort_by(|a, b| b.1.cmp(&a.1));
        top_templates.truncate(10);

        Trends {
            avg_generation_time_ms: total_time as f64 / count,
            avg_files_per_generation: total_files as f64 / count,
            error_rate: total_errors as f64 / count,
            code_growth_rate: total_code_lines as f64 / days as f64,
            top_templates,
        }
    }

    /// Get statistics summary
    pub fn stats(&self) -> MetricsStats {
        let total = self.events.len();
        let successful = self.events.iter().filter(|e| e.is_success()).count();
        let total_lines: usize = self.events.iter().map(|e| e.code_lines).sum();
        let total_files: usize = self.events.iter().map(|e| e.files_generated).sum();

        MetricsStats {
            total_generations: total,
            successful_generations: successful,
            success_rate: if total > 0 {
                (successful as f64 / total as f64) * 100.0
            } else {
                0.0
            },
            total_lines_generated: total_lines,
            total_files_generated: total_files,
        }
    }

    /// Get events in a date range
    pub fn events_in_range(
        &self,
        start: chrono::DateTime<chrono::Utc>,
        end: chrono::DateTime<chrono::Utc>,
    ) -> Vec<&GenerationEvent> {
        self.events
            .iter()
            .filter(|e| e.timestamp >= start && e.timestamp <= end)
            .collect()
    }
}

impl Default for MetricsCollector {
    fn default() -> Self {
        Self::new()
    }
}

/// Overall metrics statistics
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MetricsStats {
    /// Total number of generations
    pub total_generations: usize,
    /// Number of successful generations
    pub successful_generations: usize,
    /// Success rate percentage
    pub success_rate: f64,
    /// Total lines of code generated
    pub total_lines_generated: usize,
    /// Total files generated
    pub total_files_generated: usize,
}

/// Code quality metrics
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct QualityMetrics {
    /// Cyclomatic complexity (average)
    pub avg_complexity: f64,
    /// Test coverage percentage
    pub test_coverage: f64,
    /// Type coverage percentage
    pub type_coverage: f64,
    /// Number of linter warnings
    pub lint_warnings: usize,
    /// Number of security issues
    pub security_issues: usize,
}

impl QualityMetrics {
    /// Create quality metrics with default values
    pub fn default_metrics() -> Self {
        Self {
            avg_complexity: 0.0,
            test_coverage: 0.0,
            type_coverage: 0.0,
            lint_warnings: 0,
            security_issues: 0,
        }
    }

    /// Check if quality meets threshold
    pub fn meets_threshold(&self) -> bool {
        self.avg_complexity < 10.0
            && self.test_coverage > 80.0
            && self.type_coverage > 95.0
            && self.security_issues == 0
    }
}

/// Sprint retrospective report
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SprintReport {
    /// Sprint period
    pub period: DateRange,
    /// Total generations in sprint
    pub total_generations: usize,
    /// Total files generated
    pub total_files_generated: usize,
    /// Average generation time
    pub avg_generation_time_ms: f64,
    /// Error rate
    pub error_rate: f64,
    /// Top generated components
    pub top_components: Vec<String>,
    /// Improvements implemented
    pub improvements_implemented: Vec<String>,
    /// Suggested improvements
    pub improvements_suggested: Vec<String>,
}

/// Date range for reports
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DateRange {
    /// Start date
    pub start: chrono::DateTime<chrono::Utc>,
    /// End date
    pub end: chrono::DateTime<chrono::Utc>,
}

impl DateRange {
    /// Create a new date range
    pub fn new(start: chrono::DateTime<chrono::Utc>, end: chrono::DateTime<chrono::Utc>) -> Self {
        Self { start, end }
    }

    /// Create a date range for the last N days
    pub fn last_days(days: i64) -> Self {
        let end = chrono::Utc::now();
        let start = end - chrono::Duration::days(days);
        Self { start, end }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use chicago_tdd_tools::test;

    test!(test_generation_event, {
        let event = GenerationEvent::new(
            1000, // 1 second
            5,    // 5 files
            0,    // no errors
            2,    // 2 warnings
            500,  // 500 lines
            vec!["template1.rs".to_string()],
            "1.0.0".to_string(),
        );

        assert!(event.is_success());
        assert_eq!(event.files_generated, 5);

        Ok(())
    });

    test!(test_metrics_collector, {
        let mut collector = MetricsCollector::new();

        collector.record(GenerationEvent::new(
            1000,
            5,
            0,
            0,
            500,
            vec!["template1.rs".to_string()],
            "1.0.0".to_string(),
        ));

        collector.record(GenerationEvent::new(
            2000,
            3,
            1,
            0,
            300,
            vec!["template2.rs".to_string()],
            "1.0.0".to_string(),
        ));

        let stats = collector.stats();
        assert_eq!(stats.total_generations, 2);
        assert_eq!(stats.successful_generations, 1);
        assert_eq!(stats.success_rate, 50.0);

        Ok(())
    });

    test!(test_trends_calculation, {
        let mut collector = MetricsCollector::new();

        // Add some events
        for i in 0..5 {
            collector.record(GenerationEvent::new(
                1000 + i * 100,
                5,
                0,
                0,
                500,
                vec!["template1.rs".to_string()],
                "1.0.0".to_string(),
            ));
        }

        let trends = collector.get_trends(30);
        assert!(trends.avg_generation_time_ms > 0.0);
        assert_eq!(trends.avg_files_per_generation, 5.0);
        assert_eq!(trends.error_rate, 0.0);

        Ok(())
    });

    test!(test_quality_metrics, {
        let metrics = QualityMetrics {
            avg_complexity: 8.0,
            test_coverage: 85.0,
            type_coverage: 96.0,
            lint_warnings: 5,
            security_issues: 0,
        };

        assert!(metrics.meets_threshold());

        Ok(())
    });
}
