//! Export functionality for evaluation results
//!
//! Provides CSV and JSON export capabilities for evaluation results.

use super::types::EvaluationResult;
use crate::dspy::ModuleError;
use serde_json::json;
use std::fs::File;
use std::path::Path;

/// Export evaluation result to CSV
///
/// # Arguments
/// * `result` - Evaluation result to export
/// * `path` - File path for CSV output
///
/// # Returns
/// Result indicating success or failure
pub fn export_to_csv(result: &EvaluationResult, path: impl AsRef<Path>) -> Result<(), ModuleError> {
    let file = File::create(path)
        .map_err(|e| ModuleError::Other(format!("Failed to create CSV file: {}", e)))?;
    let mut writer = csv::Writer::from_writer(file);

    // Write header
    writer.write_record(&["index", "score", "error", "duration_ms"])
        .map_err(|e| ModuleError::Other(format!("Failed to write CSV header: {}", e)))?;

    // Write data rows
    for (idx, point) in result.results.iter().enumerate() {
        let duration_ms = point
            .duration
            .map(|d| d.as_millis().to_string())
            .unwrap_or_else(|| "".to_string());

        let error_str = point.error.as_deref().unwrap_or("");

        writer.write_record(&[
            &idx.to_string(),
            &point.score.to_string(),
            error_str,
            &duration_ms,
        ])
        .map_err(|e| ModuleError::Other(format!("Failed to write CSV row: {}", e)))?;
    }

    writer.flush()
        .map_err(|e| ModuleError::Other(format!("Failed to flush CSV writer: {}", e)))?;
    Ok(())
}

/// Export evaluation result to JSON
///
/// # Arguments
/// * `result` - Evaluation result to export
/// * `path` - File path for JSON output
///
/// # Returns
/// Result indicating success or failure
pub fn export_to_json(result: &EvaluationResult, path: impl AsRef<Path>) -> Result<(), ModuleError> {
    let file = File::create(path)
        .map_err(|e| ModuleError::Other(format!("Failed to create JSON file: {}", e)))?;

    // Build JSON structure
    let json_data = json!({
        "score": result.score,
        "successful": result.successful,
        "failed": result.failed,
        "elapsed_secs": result.elapsed.as_secs_f64(),
        "results": result.results.iter().enumerate().map(|(idx, point)| {
            json!({
                "index": idx,
                "score": point.score,
                "error": point.error,
                "duration_ms": point.duration.map(|d| d.as_millis()),
                "prediction": point.prediction,
            })
        }).collect::<Vec<_>>()
    });

    serde_json::to_writer_pretty(file, &json_data)
        .map_err(|e| ModuleError::Other(format!("Failed to write JSON: {}", e)))?;
    Ok(())
}

/// Display results in a formatted table
///
/// # Arguments
/// * `result` - Evaluation result to display
/// * `num_rows` - Number of rows to display
pub fn display_table(result: &EvaluationResult, num_rows: usize) {
    use prettytable::{Cell, Row, Table};

    let mut table = Table::new();

    // Header
    table.add_row(Row::new(vec![
        Cell::new("Index"),
        Cell::new("Score"),
        Cell::new("Status"),
        Cell::new("Duration (ms)"),
    ]));

    // Data rows
    for (idx, point) in result.results.iter().enumerate().take(num_rows) {
        let status = if point.is_success() {
            "OK"
        } else {
            "FAIL"
        };

        let duration = point
            .duration
            .map(|d| d.as_millis().to_string())
            .unwrap_or_else(|| "-".to_string());

        table.add_row(Row::new(vec![
            Cell::new(&idx.to_string()),
            Cell::new(&format!("{:.2}", point.score)),
            Cell::new(status),
            Cell::new(&duration),
        ]));
    }

    table.printstd();

    // Summary
    println!();
    println!("Overall Score: {:.2}%", result.score);
    println!(
        "Success Rate: {:.2}% ({}/{})",
        result.success_rate() * 100.0,
        result.successful,
        result.results.len()
    );
    println!("Total Time: {:.2}s", result.elapsed.as_secs_f64());
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::dspy::Example;
    use std::collections::HashMap;
    use std::time::Duration;
    use tempfile::TempDir;

    fn create_test_result() -> EvaluationResult {
        let example1 = Example::new(HashMap::new(), HashMap::new());
        let example2 = Example::new(HashMap::new(), HashMap::new());

        let point1 =
            EvaluationPoint::new(example1, HashMap::new(), 1.0).with_duration(Duration::from_millis(100));
        let point2 = EvaluationPoint::failed(example2, "Test error".to_string())
            .with_duration(Duration::from_millis(50));

        EvaluationResult::new(vec![point1, point2], Duration::from_secs(1))
    }

    #[test]
    fn test_export_to_csv() {
        let result = create_test_result();
        let temp_dir = TempDir::new().unwrap();
        let csv_path = temp_dir.path().join("test.csv");

        export_to_csv(&result, &csv_path).unwrap();

        assert!(csv_path.exists());

        // Read and verify
        let content = std::fs::read_to_string(&csv_path).unwrap();
        assert!(content.contains("index,score,error,duration_ms"));
        assert!(content.contains("0,1,"));
        assert!(content.contains("1,0,Test error"));
    }

    #[test]
    fn test_export_to_json() {
        let result = create_test_result();
        let temp_dir = TempDir::new().unwrap();
        let json_path = temp_dir.path().join("test.json");

        export_to_json(&result, &json_path).unwrap();

        assert!(json_path.exists());

        // Read and verify
        let content = std::fs::read_to_string(&json_path).unwrap();
        let parsed: serde_json::Value = serde_json::from_str(&content).unwrap();

        assert_eq!(parsed["successful"], 1);
        assert_eq!(parsed["failed"], 1);
        assert!(parsed["results"].is_array());
        assert_eq!(parsed["results"].as_array().unwrap().len(), 2);
    }

    #[test]
    fn test_display_table() {
        let result = create_test_result();

        // This test just ensures the function doesn't panic
        // Can't easily test the output without capturing stdout
        display_table(&result, 10);
    }

    #[test]
    fn test_export_to_csv_empty_result() {
        let result = EvaluationResult::new(vec![], Duration::from_secs(0));
        let temp_dir = TempDir::new().unwrap();
        let csv_path = temp_dir.path().join("empty.csv");

        export_to_csv(&result, &csv_path).unwrap();

        assert!(csv_path.exists());

        let content = std::fs::read_to_string(&csv_path).unwrap();
        assert!(content.contains("index,score,error,duration_ms"));
    }

    #[test]
    fn test_export_to_json_empty_result() {
        let result = EvaluationResult::new(vec![], Duration::from_secs(0));
        let temp_dir = TempDir::new().unwrap();
        let json_path = temp_dir.path().join("empty.json");

        export_to_json(&result, &json_path).unwrap();

        assert!(json_path.exists());

        let content = std::fs::read_to_string(&json_path).unwrap();
        let parsed: serde_json::Value = serde_json::from_str(&content).unwrap();

        assert_eq!(parsed["successful"], 0);
        assert_eq!(parsed["failed"], 0);
    }
}
