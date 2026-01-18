//! Golden Test Framework
//!
//! Provides utilities for golden/baseline testing to detect regressions.

use serde::{Deserialize, Serialize};
use serde_json::Value;
use std::collections::HashMap;
use std::fs;
use std::path::{Path, PathBuf};

/// Golden test configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GoldenTest {
    pub name: String,
    pub inputs: HashMap<String, Value>,
    pub expected_outputs: HashMap<String, Value>,
    pub metadata: HashMap<String, String>,
}

impl GoldenTest {
    /// Create a new golden test
    pub fn new(
        name: impl Into<String>,
        inputs: HashMap<String, Value>,
        expected_outputs: HashMap<String, Value>,
    ) -> Self {
        Self {
            name: name.into(),
            inputs,
            expected_outputs,
            metadata: HashMap::new(),
        }
    }

    /// Add metadata field
    pub fn with_metadata(mut self, key: impl Into<String>, value: impl Into<String>) -> Self {
        self.metadata.insert(key.into(), value.into());
        self
    }

    /// Load from file
    pub fn load(path: &Path) -> Result<Self, std::io::Error> {
        let content = fs::read_to_string(path)?;
        serde_json::from_str(&content).map_err(|e| {
            std::io::Error::new(std::io::ErrorKind::InvalidData, e.to_string())
        })
    }

    /// Save to file
    pub fn save(&self, path: &Path) -> Result<(), std::io::Error> {
        let content = serde_json::to_string_pretty(self)?;
        if let Some(parent) = path.parent() {
            fs::create_dir_all(parent)?;
        }
        fs::write(path, content)
    }
}

/// Golden test runner for managing collections of golden tests
pub struct GoldenTestRunner {
    tests: Vec<GoldenTest>,
    base_dir: PathBuf,
}

impl GoldenTestRunner {
    /// Create a new golden test runner
    pub fn new(base_dir: impl Into<PathBuf>) -> Self {
        Self {
            tests: Vec::new(),
            base_dir: base_dir.into(),
        }
    }

    /// Add a test to the runner
    pub fn add_test(&mut self, test: GoldenTest) {
        self.tests.push(test);
    }

    /// Load all tests from directory
    pub fn load_all(&mut self) -> Result<(), std::io::Error> {
        self.tests.clear();

        if !self.base_dir.exists() {
            return Ok(());
        }

        for entry in fs::read_dir(&self.base_dir)? {
            let entry = entry?;
            let path = entry.path();

            if path.extension().and_then(|s| s.to_str()) == Some("json") {
                match GoldenTest::load(&path) {
                    Ok(test) => self.tests.push(test),
                    Err(e) => eprintln!("Warning: Failed to load {}: {}", path.display(), e),
                }
            }
        }

        Ok(())
    }

    /// Save all tests to directory
    pub fn save_all(&self) -> Result<(), std::io::Error> {
        fs::create_dir_all(&self.base_dir)?;

        for test in &self.tests {
            let filename = format!("{}.json", sanitize_filename(&test.name));
            let path = self.base_dir.join(filename);
            test.save(&path)?;
        }

        Ok(())
    }

    /// Get number of tests
    pub fn test_count(&self) -> usize {
        self.tests.len()
    }

    /// Get test by name
    pub fn get_test(&self, name: &str) -> Option<&GoldenTest> {
        self.tests.iter().find(|t| t.name == name)
    }

    /// Compare actual outputs with golden test
    pub fn compare(
        &self,
        test_name: &str,
        actual_outputs: &HashMap<String, Value>,
    ) -> ComparisonResult {
        match self.get_test(test_name) {
            Some(test) => compare_outputs(&test.expected_outputs, actual_outputs),
            None => ComparisonResult::TestNotFound,
        }
    }
}

/// Result of comparing actual vs expected outputs
#[derive(Debug, PartialEq, Eq)]
pub enum ComparisonResult {
    /// Outputs match exactly
    Match,

    /// Outputs differ
    Mismatch {
        missing_fields: Vec<String>,
        extra_fields: Vec<String>,
        different_values: Vec<String>,
    },

    /// Test not found
    TestNotFound,
}

impl ComparisonResult {
    pub fn is_match(&self) -> bool {
        matches!(self, ComparisonResult::Match)
    }
}

/// Compare two output maps
fn compare_outputs(
    expected: &HashMap<String, Value>,
    actual: &HashMap<String, Value>,
) -> ComparisonResult {
    let mut missing_fields = Vec::new();
    let mut extra_fields = Vec::new();
    let mut different_values = Vec::new();

    // Check for missing and different fields
    for (key, expected_value) in expected {
        match actual.get(key) {
            None => missing_fields.push(key.clone()),
            Some(actual_value) if actual_value != expected_value => {
                different_values.push(key.clone())
            }
            _ => {}
        }
    }

    // Check for extra fields
    for key in actual.keys() {
        if !expected.contains_key(key) {
            extra_fields.push(key.clone());
        }
    }

    if missing_fields.is_empty() && extra_fields.is_empty() && different_values.is_empty() {
        ComparisonResult::Match
    } else {
        ComparisonResult::Mismatch {
            missing_fields,
            extra_fields,
            different_values,
        }
    }
}

/// Save golden test data
pub fn save_golden(
    name: &str,
    inputs: &HashMap<String, Value>,
    outputs: &HashMap<String, Value>,
    path: &Path,
) -> Result<(), std::io::Error> {
    let test = GoldenTest::new(name, inputs.clone(), outputs.clone());
    test.save(path)
}

/// Compare with golden test data
pub fn compare_with_golden(
    path: &Path,
    actual_outputs: &HashMap<String, Value>,
) -> Result<ComparisonResult, std::io::Error> {
    let test = GoldenTest::load(path)?;
    Ok(compare_outputs(&test.expected_outputs, actual_outputs))
}

/// Sanitize filename by replacing invalid characters
fn sanitize_filename(name: &str) -> String {
    name.chars()
        .map(|c| if c.is_alphanumeric() || c == '-' || c == '_' {
            c
        } else {
            '_'
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;
    use tempfile::TempDir;

    #[test]
    fn test_golden_test_creation() {
        let inputs = HashMap::from([("question".to_string(), json!("Q"))]);
        let outputs = HashMap::from([("answer".to_string(), json!("A"))]);

        let test = GoldenTest::new("test1", inputs.clone(), outputs.clone());

        assert_eq!(test.name, "test1");
        assert_eq!(test.inputs, inputs);
        assert_eq!(test.expected_outputs, outputs);
    }

    #[test]
    fn test_golden_test_with_metadata() {
        let test = GoldenTest::new("test1", HashMap::new(), HashMap::new())
            .with_metadata("version", "1.0")
            .with_metadata("author", "test");

        assert_eq!(test.metadata.get("version"), Some(&"1.0".to_string()));
        assert_eq!(test.metadata.get("author"), Some(&"test".to_string()));
    }

    #[test]
    fn test_golden_test_save_load() {
        let temp_dir = TempDir::new().unwrap();
        let path = temp_dir.path().join("test.json");

        let inputs = HashMap::from([("q".to_string(), json!("Question"))]);
        let outputs = HashMap::from([("a".to_string(), json!("Answer"))]);
        let test = GoldenTest::new("test1", inputs, outputs);

        // Save
        test.save(&path).unwrap();
        assert!(path.exists());

        // Load
        let loaded = GoldenTest::load(&path).unwrap();
        assert_eq!(loaded.name, test.name);
        assert_eq!(loaded.inputs, test.inputs);
        assert_eq!(loaded.expected_outputs, test.expected_outputs);
    }

    #[test]
    fn test_golden_test_runner() {
        let temp_dir = TempDir::new().unwrap();
        let mut runner = GoldenTestRunner::new(temp_dir.path());

        let test = GoldenTest::new(
            "test1",
            HashMap::from([("input".to_string(), json!("value"))]),
            HashMap::from([("output".to_string(), json!("result"))]),
        );

        runner.add_test(test);
        assert_eq!(runner.test_count(), 1);

        // Save all
        runner.save_all().unwrap();

        // Load all
        let mut runner2 = GoldenTestRunner::new(temp_dir.path());
        runner2.load_all().unwrap();
        assert_eq!(runner2.test_count(), 1);
    }

    #[test]
    fn test_compare_outputs_match() {
        let outputs1 = HashMap::from([
            ("key1".to_string(), json!("value1")),
            ("key2".to_string(), json!("value2")),
        ]);
        let outputs2 = outputs1.clone();

        let result = compare_outputs(&outputs1, &outputs2);
        assert!(result.is_match());
    }

    #[test]
    fn test_compare_outputs_missing_field() {
        let expected = HashMap::from([
            ("key1".to_string(), json!("value1")),
            ("key2".to_string(), json!("value2")),
        ]);
        let actual = HashMap::from([("key1".to_string(), json!("value1"))]);

        let result = compare_outputs(&expected, &actual);
        match result {
            ComparisonResult::Mismatch { missing_fields, .. } => {
                assert_eq!(missing_fields, vec!["key2"]);
            }
            _ => panic!("Expected mismatch"),
        }
    }

    #[test]
    fn test_compare_outputs_extra_field() {
        let expected = HashMap::from([("key1".to_string(), json!("value1"))]);
        let actual = HashMap::from([
            ("key1".to_string(), json!("value1")),
            ("key2".to_string(), json!("value2")),
        ]);

        let result = compare_outputs(&expected, &actual);
        match result {
            ComparisonResult::Mismatch { extra_fields, .. } => {
                assert_eq!(extra_fields, vec!["key2"]);
            }
            _ => panic!("Expected mismatch"),
        }
    }

    #[test]
    fn test_compare_outputs_different_value() {
        let expected = HashMap::from([("key".to_string(), json!("value1"))]);
        let actual = HashMap::from([("key".to_string(), json!("value2"))]);

        let result = compare_outputs(&expected, &actual);
        match result {
            ComparisonResult::Mismatch {
                different_values, ..
            } => {
                assert_eq!(different_values, vec!["key"]);
            }
            _ => panic!("Expected mismatch"),
        }
    }

    #[test]
    fn test_save_and_compare_with_golden() {
        let temp_dir = TempDir::new().unwrap();
        let path = temp_dir.path().join("test.json");

        let inputs = HashMap::from([("q".to_string(), json!("Question"))]);
        let outputs = HashMap::from([("a".to_string(), json!("Answer"))]);

        // Save
        save_golden("test", &inputs, &outputs, &path).unwrap();

        // Compare with matching outputs
        let result = compare_with_golden(&path, &outputs).unwrap();
        assert!(result.is_match());

        // Compare with different outputs
        let different = HashMap::from([("a".to_string(), json!("Different"))]);
        let result = compare_with_golden(&path, &different).unwrap();
        assert!(!result.is_match());
    }

    #[test]
    fn test_sanitize_filename() {
        assert_eq!(sanitize_filename("test-file_123"), "test-file_123");
        assert_eq!(sanitize_filename("test file.txt"), "test_file_txt");
        assert_eq!(sanitize_filename("test@#$%"), "test____");
    }

    #[test]
    fn test_runner_compare() {
        let temp_dir = TempDir::new().unwrap();
        let mut runner = GoldenTestRunner::new(temp_dir.path());

        let test = GoldenTest::new(
            "test1",
            HashMap::new(),
            HashMap::from([("key".to_string(), json!("value"))]),
        );
        runner.add_test(test);

        // Compare matching
        let actual = HashMap::from([("key".to_string(), json!("value"))]);
        let result = runner.compare("test1", &actual);
        assert!(result.is_match());

        // Compare not found
        let result = runner.compare("nonexistent", &actual);
        assert_eq!(result, ComparisonResult::TestNotFound);
    }
}
