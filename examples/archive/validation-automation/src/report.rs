//! Validation report generation
//!
//! Generates comprehensive validation reports in multiple formats:
//! - Markdown (human-readable)
//! - JSON (machine-readable)
//! - HTML (web-friendly)

use crate::error::ValidationResult;
use chrono::{DateTime, Utc};
use colored::Colorize;
use std::io::Write;

/// Complete validation report
#[derive(Debug, Clone)]
pub struct ValidationReport {
    /// Results from each validation approach
    pub results: Vec<ValidationResult>,

    /// Total time taken
    pub total_duration_ms: u128,

    /// When the report was generated
    pub timestamp: DateTime<Utc>,
}

impl ValidationReport {
    /// Generate a markdown report
    pub async fn generate_markdown(&self, path: &str) -> std::io::Result<()> {
        let mut file = std::fs::File::create(path)?;

        writeln!(file, "# Validation Report")?;
        writeln!(file)?;
        writeln!(file, "**Generated:** {}", self.timestamp.format("%Y-%m-%d %H:%M:%S UTC"))?;
        writeln!(file, "**Duration:** {}ms", self.total_duration_ms)?;
        writeln!(file)?;
        writeln!(file, "---")?;
        writeln!(file)?;

        // Summary section
        writeln!(file, "## Summary")?;
        writeln!(file)?;
        writeln!(file, "| Approach | Status | Score | Duration |")?;
        writeln!(file, "|----------|--------|-------|----------|")?;

        for result in &self.results {
            let status = if result.passed { "✅ PASS" } else { "❌ FAIL" };
            writeln!(
                file,
                "| {} | {} | {:.1}% | {}ms |",
                result.approach, status, result.score * 100.0, result.duration_ms
            )?;
        }

        writeln!(file)?;

        // Detailed results
        writeln!(file, "## Detailed Results")?;
        writeln!(file)?;

        for result in &self.results {
            writeln!(file, "### {}", result.approach)?;
            writeln!(file)?;
            writeln!(file, "- **Status:** {}", if result.passed { "Passed" } else { "Failed" })?;
            writeln!(file, "- **Score:** {:.1}%", result.score * 100.0)?;
            writeln!(file, "- **Assessment:** {}", result.assessment())?;
            writeln!(file, "- **Duration:** {}ms", result.duration_ms)?;
            writeln!(file)?;

            // Recommendations
            if !result.recommendations.is_empty() {
                writeln!(file, "**Recommendations:**")?;
                for rec in &result.recommendations {
                    writeln!(file, "- {}", rec)?;
                }
                writeln!(file)?;
            }

            // Details (JSON)
            writeln!(file, "**Details:**")?;
            writeln!(file, "```json")?;
            writeln!(file, "{}", serde_json::to_string_pretty(&result.details).unwrap())?;
            writeln!(file, "```")?;
            writeln!(file)?;
        }

        file.flush()?;
        Ok(())
    }

    /// Generate an HTML report
    pub async fn generate_html(&self, path: &str) -> std::io::Result<()> {
        let mut file = std::fs::File::create(path)?;

        writeln!(file, "<!DOCTYPE html>")?;
        writeln!(file, "<html lang=\"en\">")?;
        writeln!(file, "<head>")?;
        writeln!(file, "    <meta charset=\"UTF-8\">")?;
        writeln!(file, "    <meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">")?;
        writeln!(file, "    <title>Validation Report</title>")?;
        writeln!(file, "    <style>")?;
        writeln!(file, "        body {{ font-family: system-ui, sans-serif; max-width: 1200px; margin: 0 auto; padding: 20px; }}")?;
        writeln!(file, "        h1 {{ color: #333; border-bottom: 2px solid #007bff; padding-bottom: 10px; }}")?;
        writeln!(file, "        h2 {{ color: #555; margin-top: 30px; }}")?;
        writeln!(file, "        table {{ border-collapse: collapse; width: 100%; margin: 20px 0; }}")?;
        writeln!(file, "        th, td {{ border: 1px solid #ddd; padding: 12px; text-align: left; }}")?;
        writeln!(file, "        th {{ background-color: #007bff; color: white; }}")?;
        writeln!(file, "        tr:nth-child(even) {{ background-color: #f9f9f9; }}")?;
        writeln!(file, "        .pass {{ color: #28a745; font-weight: bold; }}")?;
        writeln!(file, "        .fail {{ color: #dc3545; font-weight: bold; }}")?;
        writeln!(file, "        .score {{ font-weight: bold; }}")?;
        writeln!(file, "        .recommendations {{ background-color: #fff3cd; padding: 15px; border-left: 4px solid #ffc107; margin: 10px 0; }}")?;
        writeln!(file, "        pre {{ background-color: #f5f5f5; padding: 15px; border-radius: 5px; overflow-x: auto; }}")?;
        writeln!(file, "    </style>")?;
        writeln!(file, "</head>")?;
        writeln!(file, "<body>")?;
        writeln!(file, "    <h1>Validation Report</h1>")?;
        writeln!(file)?;
        writeln!(file, "    <p><strong>Generated:</strong> {}</p>", self.timestamp.format("%Y-%m-%d %H:%M:%S UTC"))?;
        writeln!(file, "    <p><strong>Duration:</strong> {}ms</p>", self.total_duration_ms)?;
        writeln!(file)?;
        writeln!(file, "    <h2>Summary</h2>")?;
        writeln!(file, "    <table>")?;
        writeln!(file, "        <tr>")?;
        writeln!(file, "            <th>Approach</th>")?;
        writeln!(file, "            <th>Status</th>")?;
        writeln!(file, "            <th>Score</th>")?;
        writeln!(file, "            <th>Duration</th>")?;
        writeln!(file, "        </tr>")?;

        for result in &self.results {
            let status_class = if result.passed { "pass" } else { "fail" };
            let status_text = if result.passed { "✅ PASS" } else { "❌ FAIL" };
            writeln!(file, "        <tr>")?;
            writeln!(file, "            <td>{}</td>", result.approach)?;
            writeln!(file, "            <td class=\"{}\">{}</td>", status_class, status_text)?;
            writeln!(file, "            <td class=\"score\">{:.1}%</td>", result.score * 100.0)?;
            writeln!(file, "            <td>{}ms</td>", result.duration_ms)?;
            writeln!(file, "        </tr>")?;
        }

        writeln!(file, "    </table>")?;
        writeln!(file)?;

        // Detailed results
        writeln!(file, "    <h2>Detailed Results</h2>")?;

        for result in &self.results {
            writeln!(file, "    <h3>{}</h3>", result.approach)?;
            writeln!(file, "    <ul>")?;
            writeln!(file, "        <li><strong>Status:</strong> {}</li>", if result.passed { "Passed" } else { "Failed" })?;
            writeln!(file, "        <li><strong>Score:</strong> {:.1}%</li>", result.score * 100.0)?;
            writeln!(file, "        <li><strong>Assessment:</strong> {}</li>", result.assessment())?;
            writeln!(file, "        <li><strong>Duration:</strong> {}ms</li>", result.duration_ms)?;
            writeln!(file, "    </ul>")?;

            if !result.recommendations.is_empty() {
                writeln!(file, "    <div class=\"recommendations\">")?;
                writeln!(file, "        <strong>Recommendations:</strong>")?;
                writeln!(file, "        <ul>")?;
                for rec in &result.recommendations {
                    writeln!(file, "            <li>{}</li>", rec)?;
                }
                writeln!(file, "        </ul>")?;
                writeln!(file, "    </div>")?;
            }

            writeln!(file, "    <pre>{}</pre>", serde_json::to_string_pretty(&result.details).unwrap())?;
            writeln!(file)?;
        }

        writeln!(file, "</body>")?;
        writeln!(file, "</html>")?;

        file.flush()?;
        Ok(())
    }

    /// Generate a JSON report
    pub async fn generate_json(&self, path: &str) -> std::io::Result<()> {
        let mut file = std::fs::File::create(path)?;

        let report_json = serde_json::json!({
            "timestamp": self.timestamp.to_rfc3339(),
            "duration_ms": self.total_duration_ms,
            "results": self.results
        });

        writeln!(file, "{}", serde_json::to_string_pretty(&report_json).unwrap())?;

        file.flush()?;
        Ok(())
    }

    /// Print a summary to the terminal
    pub fn print_summary(&self) {
        println!();
        println!("{}", "=== Validation Summary ===".bold());
        println!();
        println!("Duration: {}ms", self.total_duration_ms);
        println!();

        for result in &self.results {
            let status = if result.passed {
                "✅".green().to_string()
            } else {
                "❌".red().to_string()
            };

            println!(
                "{} {} - {} ({}%)",
                status,
                result.approach,
                result.colored_assessment(),
                (result.score * 100.0) as u32
            );
        }

        println!();
    }

    /// Get overall pass/fail status
    pub fn overall_passed(&self) -> bool {
        self.results.iter().all(|r| r.passed)
    }

    /// Calculate average score across all approaches
    pub fn average_score(&self) -> f64 {
        if self.results.is_empty() {
            return 0.0;
        }
        self.results.iter().map(|r| r.score).sum::<f64>() / self.results.len() as f64
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::error::ValidationResult;

    #[tokio::test]
    async fn test_report_generation() {
        let results = vec![
            ValidationResult::new(
                "Test",
                true,
                0.9,
                serde_json::json!({"test": "data"}),
                vec!["Good".to_string()],
                1000,
            ),
        ];

        let report = ValidationReport {
            results,
            total_duration_ms: 1000,
            timestamp: Utc::now(),
        };

        assert!(report.overall_passed());
        assert_eq!(report.average_score(), 0.9);
    }
}
