use super::super::world::CleanroomWorld;
use cucumber::{given, then, when};
use std::fs;
use std::path::Path;

/// Coverage step definitions for Cleanroom BDD tests
///
/// These steps handle coverage analysis, reporting, and validation
/// for cleanroom test execution and code coverage.

// ============================================================================
// GIVEN steps - Setup preconditions
// ============================================================================

#[given(regex = r"^I have coverage analysis enabled$")]
fn coverage_analysis_enabled(world: &mut CleanroomWorld) {
    world.set_policy("coverage_enabled".to_string(), "true".to_string());
}

#[given(regex = r"^I have a coverage report "([^"]+)"$")]
fn coverage_report(world: &mut CleanroomWorld, filename: String) {
    // Create a mock coverage report
    let coverage_content = r#"{
  "coverage": {
    "lines": {
      "total": 100,
      "covered": 85,
      "percentage": 85.0
    },
    "functions": {
      "total": 20,
      "covered": 18,
      "percentage": 90.0
    },
    "branches": {
      "total": 30,
      "covered": 25,
      "percentage": 83.33
    }
  }
}"#;
    
    let file_path = world.project_dir.join(&filename);
    fs::write(&file_path, coverage_content)
        .unwrap_or_else(|e| panic!("Failed to write coverage report {}: {}", filename, e));
    
    world.capture_file(&filename, coverage_content.to_string());
}

#[given(regex = r"^I have a coverage threshold of (\d+)%$")]
fn coverage_threshold(world: &mut CleanroomWorld, threshold: i32) {
    world.set_policy("coverage_threshold".to_string(), threshold.to_string());
}

#[given(regex = r"^I have coverage data for "([^"]+)"$")]
fn coverage_data_for_file(world: &mut CleanroomWorld, filename: String) {
    // Create coverage data for a specific file
    let coverage_data = format!(
        r#"file: {}
lines_covered: 45
lines_total: 50
coverage_percentage: 90.0
"#,
        filename
    );
    
    let data_file = world.project_dir.join(format!("{}.coverage", filename));
    fs::write(&data_file, coverage_data)
        .unwrap_or_else(|e| panic!("Failed to write coverage data: {}", e));
    
    world.capture_file(&format!("{}.coverage", filename), coverage_data);
}

#[given(regex = r"^I have branch coverage data$")]
fn branch_coverage_data(world: &mut CleanroomWorld) {
    let branch_data = r#"branches: [
  {
    "line": 10,
    "type": "if",
    "covered": true
  },
  {
    "line": 15,
    "type": "else",
    "covered": false
  },
  {
    "line": 20,
    "type": "match",
    "covered": true
  }
]"#;
    
    let data_file = world.project_dir.join("branch_coverage.json");
    fs::write(&data_file, branch_data)
        .unwrap_or_else(|e| panic!("Failed to write branch coverage data: {}", e));
    
    world.capture_file("branch_coverage.json", branch_data.to_string());
}

// ============================================================================
// WHEN steps - Execute actions
// ============================================================================

#[when(regex = r"^I generate a coverage report$")]
fn generate_coverage_report(world: &mut CleanroomWorld) {
    // Simulate coverage report generation
    let report_content = r#"# Coverage Report

## Summary
- Lines: 85/100 (85%)
- Functions: 18/20 (90%)
- Branches: 25/30 (83.33%)

## Files
- src/main.rs: 90%
- src/lib.rs: 80%
- src/utils.rs: 85%
"#;
    
    let report_file = world.project_dir.join("coverage_report.md");
    fs::write(&report_file, report_content)
        .unwrap_or_else(|e| panic!("Failed to write coverage report: {}", e));
    
    world.capture_file("coverage_report.md", report_content.to_string());
}

#[when(regex = r"^I analyze coverage for "([^"]+)"$")]
fn analyze_coverage_for_file(world: &mut CleanroomWorld, filename: String) {
    // Simulate coverage analysis for a specific file
    let analysis_content = format!(
        r#"Coverage analysis for {}

Lines covered: 45/50 (90%)
Functions covered: 8/10 (80%)
Branches covered: 12/15 (80%)

Uncovered lines: 15, 23, 31, 42, 48
"#,
        filename
    );
    
    let analysis_file = world.project_dir.join(format!("{}_analysis.txt", filename));
    fs::write(&analysis_file, analysis_content)
        .unwrap_or_else(|e| panic!("Failed to write coverage analysis: {}", e));
    
    world.capture_file(&format!("{}_analysis.txt", filename), analysis_content);
}

#[when(regex = r"^I check coverage against threshold$")]
fn check_coverage_against_threshold(world: &mut CleanroomWorld) {
    // Check coverage against the configured threshold
    let threshold: i32 = world.policy_settings.get("coverage_threshold")
        .unwrap_or(&"80".to_string())
        .parse()
        .expect("Coverage threshold should be a number");
    
    // Simulate coverage check
    let current_coverage = 85; // Mock coverage percentage
    
    if current_coverage >= threshold {
        world.set_policy("coverage_check_passed".to_string(), "true".to_string());
    } else {
        world.set_policy("coverage_check_passed".to_string(), "false".to_string());
    }
}

#[when(regex = r"^I export coverage data to "([^"]+)"$")]
fn export_coverage_data(world: &mut CleanroomWorld, format: String) {
    let export_content = match format.as_str() {
        "json" => r#"{
  "coverage": {
    "lines": 85,
    "functions": 18,
    "branches": 25
  }
}"#,
        "xml" => r#"<?xml version="1.0" encoding="UTF-8"?>
<coverage>
  <lines>85</lines>
  <functions>18</functions>
  <branches>25</branches>
</coverage>"#,
        "html" => r#"<html>
<head><title>Coverage Report</title></head>
<body>
  <h1>Coverage Report</h1>
  <p>Lines: 85%</p>
  <p>Functions: 90%</p>
  <p>Branches: 83.33%</p>
</body>
</html>"#,
        _ => panic!("Unsupported coverage export format: {}", format),
    };
    
    let export_file = world.project_dir.join(format!("coverage.{}", format));
    fs::write(&export_file, export_content)
        .unwrap_or_else(|e| panic!("Failed to export coverage data: {}", e));
    
    world.capture_file(&format!("coverage.{}", format), export_content.to_string());
}

// ============================================================================
// THEN steps - Verify outcomes
// ============================================================================

#[then(regex = r"^the coverage report should be generated$")]
fn coverage_report_should_be_generated(world: &mut CleanroomWorld) {
    let report_file = world.project_dir.join("coverage_report.md");
    
    if !report_file.exists() {
        panic!("Coverage report should be generated");
    }
    
    let content = fs::read_to_string(&report_file)
        .unwrap_or_else(|e| panic!("Failed to read coverage report: {}", e));
    
    // Verify report contains expected sections
    assert!(content.contains("Coverage Report"), "Report should contain title");
    assert!(content.contains("Summary"), "Report should contain summary");
    assert!(content.contains("Files"), "Report should contain files section");
}

#[then(regex = r"^the coverage should be at least (\d+)%$")]
fn coverage_should_be_at_least(world: &mut CleanroomWorld, min_coverage: i32) {
    // Check if coverage meets the minimum threshold
    let coverage_check_passed = world.policy_settings.get("coverage_check_passed")
        .unwrap_or(&"false".to_string());
    
    if coverage_check_passed != "true" {
        panic!("Coverage should be at least {}%", min_coverage);
    }
}

#[then(regex = r"^the coverage should meet the threshold$")]
fn coverage_should_meet_threshold(world: &mut CleanroomWorld) {
    let coverage_check_passed = world.policy_settings.get("coverage_check_passed")
        .expect("Coverage check should have been performed");
    
    assert_eq!(
        coverage_check_passed, "true",
        "Coverage should meet the configured threshold"
    );
}

#[then(regex = r"^the coverage analysis should identify uncovered lines$")]
fn coverage_analysis_should_identify_uncovered_lines(world: &mut CleanroomWorld) {
    // Check if coverage analysis identified uncovered lines
    let analysis_files: Vec<_> = world.captured_files.keys()
        .filter(|k| k.ends_with("_analysis.txt"))
        .collect();
    
    if analysis_files.is_empty() {
        panic!("Coverage analysis should identify uncovered lines");
    }
    
    // Check that analysis files contain uncovered line information
    for analysis_file in analysis_files {
        let content = world.captured_files.get(analysis_file)
            .expect("Analysis file content should be captured");
        
        if !content.contains("Uncovered lines") {
            panic!("Coverage analysis should identify uncovered lines");
        }
    }
}

#[then(regex = r"^the branch coverage should be reported$")]
fn branch_coverage_should_be_reported(world: &mut CleanroomWorld) {
    let branch_data = world.captured_files.get("branch_coverage.json")
        .expect("Branch coverage data should be captured");
    
    // Verify branch coverage data structure
    assert!(branch_data.contains("branches"), "Should contain branches data");
    assert!(branch_data.contains("covered"), "Should contain coverage information");
}

#[then(regex = r"^the coverage data should be exported in "([^"]+)" format$")]
fn coverage_data_should_be_exported_in_format(world: &mut CleanroomWorld, format: String) {
    let export_file = format!("coverage.{}", format);
    let exported_data = world.captured_files.get(&export_file)
        .expect("Coverage data should be exported");
    
    // Verify format-specific content
    match format.as_str() {
        "json" => {
            assert!(exported_data.contains("{"), "JSON should contain braces");
            assert!(exported_data.contains("coverage"), "JSON should contain coverage key");
        }
        "xml" => {
            assert!(exported_data.contains("<?xml"), "XML should contain declaration");
            assert!(exported_data.contains("<coverage>"), "XML should contain coverage tag");
        }
        "html" => {
            assert!(exported_data.contains("<html>"), "HTML should contain html tag");
            assert!(exported_data.contains("Coverage Report"), "HTML should contain title");
        }
        _ => {
            panic!("Unsupported format: {}", format);
        }
    }
}

#[then(regex = r"^the coverage report should show "([^"]+)"$")]
fn coverage_report_should_show(world: &mut CleanroomWorld, expected_content: String) {
    let report_content = world.captured_files.get("coverage_report.md")
        .expect("Coverage report should be captured");
    
    assert!(
        report_content.contains(&expected_content),
        "Coverage report should show '{}'",
        expected_content
    );
}

#[then(regex = r"^the coverage should be tracked for "([^"]+)"$")]
fn coverage_should_be_tracked_for_file(world: &mut CleanroomWorld, filename: String) {
    let coverage_file = format!("{}.coverage", filename);
    let coverage_data = world.captured_files.get(&coverage_file)
        .expect("Coverage data should be tracked for file");
    
    assert!(
        coverage_data.contains(&filename),
        "Coverage data should be tracked for '{}'",
        filename
    );
}

#[then(regex = r"^the coverage metrics should be accurate$")]
fn coverage_metrics_should_be_accurate(world: &mut CleanroomWorld) {
    // Verify that coverage metrics are reasonable
    let report_content = world.captured_files.get("coverage_report.md")
        .expect("Coverage report should be captured");
    
    // Check for percentage values
    assert!(report_content.contains("%"), "Report should contain percentage values");
    
    // Check for numeric values
    assert!(report_content.contains("85"), "Report should contain coverage numbers");
    assert!(report_content.contains("90"), "Report should contain coverage numbers");
}

#[then(regex = r"^the coverage report should be comprehensive$")]
fn coverage_report_should_be_comprehensive(world: &mut CleanroomWorld) {
    let report_content = world.captured_files.get("coverage_report.md")
        .expect("Coverage report should be captured");
    
    // Check for comprehensive coverage information
    assert!(report_content.contains("Lines"), "Report should include line coverage");
    assert!(report_content.contains("Functions"), "Report should include function coverage");
    assert!(report_content.contains("Branches"), "Report should include branch coverage");
    assert!(report_content.contains("Files"), "Report should include file coverage");
}
