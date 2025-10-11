// Test Library - London School TDD Helpers and Mocks
// Provides mock traits, test helpers, and behavior verification utilities

use mockall::predicate::*;
use mockall::*;
use serde_json::{json, Value};

// Re-export commonly used test utilities
pub use proptest::prelude::*;
pub use proptest::strategy::Strategy;
pub use test_case::test_case;

/// Mock trait for ggen-core Registry operations
#[automock]
pub trait RegistryTrait: Send + Sync {
    fn list_packages(&self) -> Result<Vec<PackageInfo>, String>;
    fn search_packages(&self, query: &str, fuzzy: bool) -> Result<Vec<PackageInfo>, String>;
    fn get_package_info(&self, id: &str) -> Result<PackageInfo, String>;
    fn get_installed_packages(&self) -> Result<Vec<PackageInfo>, String>;
}

/// Mock trait for filesystem operations
#[automock]
pub trait FileSystemTrait: Send + Sync {
    fn read_file(&self, path: &str) -> Result<String, String>;
    fn write_file(&self, path: &str, content: &str) -> Result<(), String>;
    fn file_exists(&self, path: &str) -> bool;
    fn create_dir(&self, path: &str) -> Result<(), String>;
}

/// Mock trait for template engine
#[automock]
pub trait TemplateEngineTrait: Send + Sync {
    fn validate_template(&self, content: &str) -> Result<ValidationResult, String>;
    fn render_template(&self, template: &str, vars: &Value) -> Result<String, String>;
    fn extract_variables(&self, template: &str) -> Vec<String>;
}

/// Mock trait for graph operations
#[automock]
pub trait GraphStoreTrait: Send + Sync {
    fn execute_sparql(
        &self, query: &str, graph: Option<&'static str>,
    ) -> Result<QueryResult, String>;
    fn load_rdf(
        &self, file: &str, format: &str, graph: Option<&'static str>,
    ) -> Result<usize, String>;
    fn export_graph(
        &self, output: &str, format: &str, graph: Option<&'static str>,
    ) -> Result<usize, String>;
}

/// Test data structures
#[derive(Clone, Debug, PartialEq)]
pub struct PackageInfo {
    pub id: String,
    pub name: String,
    pub description: String,
    pub category: Option<String>,
    pub tags: Vec<String>,
    pub version: String,
    pub author: Option<String>,
    pub license: Option<String>,
    pub stars: u32,
    pub downloads: u32,
    pub updated_at: chrono::DateTime<chrono::Utc>,
    pub homepage: Option<String>,
    pub repository: Option<String>,
    pub dependencies: Vec<String>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ValidationResult {
    pub valid: bool,
    pub errors: Vec<String>,
    pub warnings: Vec<String>,
    pub variables: Vec<String>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct QueryResult {
    pub bindings: Vec<Value>,
    pub count: usize,
    pub execution_time_ms: u64,
}

// Test Helper Functions

/// Create a sample package for testing
pub fn sample_package(id: &str) -> PackageInfo {
    PackageInfo {
        id: id.to_string(),
        name: format!("test-package-{}", id),
        description: "A test package".to_string(),
        category: Some("testing".to_string()),
        tags: vec!["test".to_string(), "mock".to_string()],
        version: "1.0.0".to_string(),
        author: Some("Test Author".to_string()),
        license: Some("MIT".to_string()),
        stars: 42,
        downloads: 1000,
        updated_at: chrono::Utc::now(),
        homepage: Some("https://example.com".to_string()),
        repository: Some("https://github.com/test/repo".to_string()),
        dependencies: vec![],
    }
}

/// Create multiple sample packages
pub fn sample_packages(count: usize) -> Vec<PackageInfo> {
    (0..count)
        .map(|i| sample_package(&format!("pkg{}", i)))
        .collect()
}

/// Assert that a JSON value contains expected fields
pub fn assert_json_contains(actual: &Value, expected_fields: &[&str]) {
    for field in expected_fields {
        assert!(
            actual.get(field).is_some(),
            "JSON missing expected field: {}",
            field
        );
    }
}

/// Assert that a result is successful and contains expected data
pub fn assert_success_with_data(result: &Value, expected_fields: &[&str]) {
    assert_eq!(result.get("success").and_then(|v| v.as_bool()), Some(true));
    if let Some(data) = result.get("data") {
        assert_json_contains(data, expected_fields);
    } else {
        panic!("Result missing 'data' field");
    }
}

/// BDD-style test assertion helpers
pub mod bdd {
    use super::*;

    /// Given: Setup test preconditions
    pub fn given_package_exists(mock: &mut MockRegistryTrait, package: PackageInfo) {
        let package_clone = package.clone();
        mock.expect_get_package_info()
            .with(eq(package.id.clone()))
            .returning(move |_| Ok(package_clone.clone()));
    }

    /// When: Perform the action being tested
    pub async fn when_searching_packages(
        mock: &mut MockRegistryTrait, query: &str, results: Vec<PackageInfo>,
    ) {
        mock.expect_search_packages()
            .with(eq(query.to_string()), always())
            .returning(move |_, _| Ok(results.clone()));
    }

    /// Then: Verify the expected outcome
    pub fn then_should_contain_package(results: &[Value], package_id: &str) {
        assert!(
            results.iter().any(|r| {
                r.get("id")
                    .and_then(|v| v.as_str())
                    .map(|id| id == package_id)
                    .unwrap_or(false)
            }),
            "Results should contain package with id: {}",
            package_id
        );
    }
}

// Property-based testing helpers
pub mod properties {
    use super::*;

    /// Strategy for generating valid package IDs
    pub fn package_id_strategy() -> impl Strategy<Value = String> {
        "[a-z][a-z0-9-]{2,20}".prop_map(|s| s)
    }

    /// Strategy for generating valid template names
    pub fn template_name_strategy() -> impl Strategy<Value = String> {
        "[a-z][a-z0-9-]{2,30}".prop_map(|s| s)
    }

    /// Strategy for generating SPARQL queries
    pub fn sparql_query_strategy() -> impl Strategy<Value = String> {
        proptest::collection::vec("[A-Z]+", 1..5)
            .prop_map(|words| format!("SELECT * WHERE {{ ?s ?p ?o }}"))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_sample_package_creation() {
        let pkg = sample_package("test1");
        assert_eq!(pkg.id, "test1");
        assert_eq!(pkg.name, "test-package-test1");
        assert!(pkg.stars > 0);
    }

    #[test]
    fn test_sample_packages_creates_multiple() {
        let packages = sample_packages(5);
        assert_eq!(packages.len(), 5);
        assert_eq!(packages[0].id, "pkg0");
        assert_eq!(packages[4].id, "pkg4");
    }

    #[test]
    fn test_assert_json_contains() {
        let json = json!({
            "name": "test",
            "value": 42,
            "active": true
        });

        assert_json_contains(&json, &["name", "value", "active"]);
    }

    #[test]
    #[should_panic(expected = "JSON missing expected field")]
    fn test_assert_json_contains_fails_on_missing_field() {
        let json = json!({ "name": "test" });
        assert_json_contains(&json, &["name", "missing"]);
    }

    #[test]
    fn test_mock_registry_behavior() {
        let mut mock_registry = MockRegistryTrait::new();
        let package = sample_package("test");

        bdd::given_package_exists(&mut mock_registry, package.clone());

        let result = mock_registry.get_package_info("test");
        assert!(result.is_ok());
        assert_eq!(result.unwrap().id, "test");
    }
}
