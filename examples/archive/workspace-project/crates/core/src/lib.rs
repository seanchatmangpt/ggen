use serde::{Deserialize, Serialize};
use uuid::Uuid;
use std::collections::{HashMap, HashSet};

pub mod coordination;

/// Project metadata
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Project {
    pub id: Uuid,
    pub name: String,
    pub description: String,
    pub active: bool,
}

impl Project {
    pub fn new(name: String, description: String) -> Self {
        Self {
            id: Uuid::new_v4(),
            name,
            description,
            active: true,
        }
    }
}

/// Workspace crate information
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub struct Crate {
    pub name: String,
    pub path: String,
    pub dependencies: Vec<String>,
}

impl Crate {
    pub fn new(name: String, path: String) -> Self {
        Self {
            name,
            path,
            dependencies: Vec::new(),
        }
    }

    pub fn with_dependencies(mut self, deps: Vec<String>) -> Self {
        self.dependencies = deps;
        self
    }
}

/// Build result for a single crate
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BuildResult {
    pub crate_name: String,
    pub success: bool,
    pub duration_ms: u64,
    pub error: Option<String>,
}

impl BuildResult {
    pub fn success(crate_name: String, duration_ms: u64) -> Self {
        Self {
            crate_name,
            success: true,
            duration_ms,
            error: None,
        }
    }

    pub fn failure(crate_name: String, duration_ms: u64, error: String) -> Self {
        Self {
            crate_name,
            success: false,
            duration_ms,
            error: Some(error),
        }
    }
}

/// Test result for a single crate
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TestResult {
    pub crate_name: String,
    pub passed: usize,
    pub failed: usize,
    pub skipped: usize,
    pub duration_ms: u64,
    pub coverage_percentage: f32,
}

impl TestResult {
    pub fn new(
        crate_name: String,
        passed: usize,
        failed: usize,
        skipped: usize,
        duration_ms: u64,
        coverage_percentage: f32,
    ) -> Self {
        Self {
            crate_name,
            passed,
            failed,
            skipped,
            duration_ms,
            coverage_percentage,
        }
    }

    pub fn all_passed(&self) -> bool {
        self.failed == 0
    }

    pub fn total_tests(&self) -> usize {
        self.passed + self.failed + self.skipped
    }
}

/// Workspace metrics aggregation
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct WorkspaceMetrics {
    pub total_crates: usize,
    pub total_build_time_ms: u64,
    pub total_test_time_ms: u64,
    pub total_tests: usize,
    pub total_passed: usize,
    pub total_failed: usize,
    pub average_coverage: f32,
}

impl WorkspaceMetrics {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn success_rate(&self) -> f32 {
        if self.total_tests == 0 {
            0.0
        } else {
            (self.total_passed as f32 / self.total_tests as f32) * 100.0
        }
    }

    pub fn aggregate_from_results(test_results: &[TestResult]) -> Self {
        let mut metrics = WorkspaceMetrics::new();
        metrics.total_crates = test_results.len();

        for result in test_results {
            metrics.total_test_time_ms += result.duration_ms;
            metrics.total_tests += result.total_tests();
            metrics.total_passed += result.passed;
            metrics.total_failed += result.failed;
        }

        metrics.average_coverage = if test_results.is_empty() {
            0.0
        } else {
            test_results.iter().map(|r| r.coverage_percentage).sum::<f32>()
                / test_results.len() as f32
        };

        metrics
    }
}

/// Workspace coordinator for orchestrating builds
pub struct Coordinator {
    crates: HashMap<String, Crate>,
    build_results: Vec<BuildResult>,
    test_results: Vec<TestResult>,
}

impl Coordinator {
    pub fn new() -> Self {
        Self {
            crates: HashMap::new(),
            build_results: Vec::new(),
            test_results: Vec::new(),
        }
    }

    /// Add a crate to the workspace
    pub fn add_crate(&mut self, crate_info: Crate) {
        self.crates.insert(crate_info.name.clone(), crate_info);
    }

    /// Determine build order respecting dependencies
    pub fn determine_build_order(&self) -> Result<Vec<String>, String> {
        let mut order = Vec::new();
        let mut remaining: HashSet<String> = self.crates.keys().cloned().collect();
        let mut visited = HashSet::new();

        while !remaining.is_empty() {
            let mut found = false;

            for crate_name in &remaining.clone() {
                if let Some(crate_info) = self.crates.get(crate_name) {
                    // Check if all dependencies are already visited
                    let deps_satisfied = crate_info
                        .dependencies
                        .iter()
                        .all(|dep| visited.contains(dep) || !self.crates.contains_key(dep));

                    if deps_satisfied {
                        order.push(crate_name.clone());
                        visited.insert(crate_name.clone());
                        remaining.remove(crate_name);
                        found = true;
                        break;
                    }
                }
            }

            if !found {
                return Err("Circular dependency detected".to_string());
            }
        }

        Ok(order)
    }

    /// Record build result
    pub fn record_build(&mut self, result: BuildResult) {
        self.build_results.push(result);
    }

    /// Record test result
    pub fn record_test(&mut self, result: TestResult) {
        self.test_results.push(result);
    }

    /// Get all build results
    pub fn build_results(&self) -> &[BuildResult] {
        &self.build_results
    }

    /// Get all test results
    pub fn test_results(&self) -> &[TestResult] {
        &self.test_results
    }

    /// Check if all builds succeeded
    pub fn all_builds_succeeded(&self) -> bool {
        self.build_results.iter().all(|r| r.success)
    }

    /// Check if all tests passed
    pub fn all_tests_passed(&self) -> bool {
        self.test_results.iter().all(|r| r.all_passed())
    }

    /// Aggregate workspace metrics
    pub fn aggregate_metrics(&self) -> WorkspaceMetrics {
        WorkspaceMetrics::aggregate_from_results(&self.test_results)
    }

    /// Get crate count
    pub fn crate_count(&self) -> usize {
        self.crates.len()
    }
}

impl Default for Coordinator {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_project_creation() {
        let project = Project::new("Test".to_string(), "Desc".to_string());
        assert_eq!(project.name, "Test");
        assert!(project.active);
    }

    #[test]
    fn test_crate_creation() {
        let krate = Crate::new("test-crate".to_string(), "crates/test".to_string());
        assert_eq!(krate.name, "test-crate");
        assert!(krate.dependencies.is_empty());
    }

    #[test]
    fn test_crate_with_dependencies() {
        let krate = Crate::new("cli".to_string(), "crates/cli".to_string())
            .with_dependencies(vec!["core".to_string()]);
        assert_eq!(krate.dependencies.len(), 1);
        assert!(krate.dependencies.contains(&"core".to_string()));
    }

    #[test]
    fn test_build_result_success() {
        let result = BuildResult::success("test-crate".to_string(), 1500);
        assert!(result.success);
        assert!(result.error.is_none());
        assert_eq!(result.duration_ms, 1500);
    }

    #[test]
    fn test_build_result_failure() {
        let result = BuildResult::failure(
            "test-crate".to_string(),
            2000,
            "Compilation error".to_string(),
        );
        assert!(!result.success);
        assert!(result.error.is_some());
    }

    #[test]
    fn test_test_result_creation() {
        let result = TestResult::new("core".to_string(), 50, 0, 2, 3000, 85.5);
        assert_eq!(result.passed, 50);
        assert_eq!(result.failed, 0);
        assert!(result.all_passed());
        assert_eq!(result.total_tests(), 52);
    }

    #[test]
    fn test_workspace_metrics_success_rate() {
        let mut metrics = WorkspaceMetrics::new();
        metrics.total_tests = 100;
        metrics.total_passed = 90;
        metrics.total_failed = 10;
        assert_eq!(metrics.success_rate(), 90.0);
    }

    #[test]
    fn test_workspace_metrics_aggregate() {
        let results = vec![
            TestResult::new("core".to_string(), 30, 0, 0, 2000, 90.0),
            TestResult::new("cli".to_string(), 20, 0, 0, 1500, 80.0),
        ];
        let metrics = WorkspaceMetrics::aggregate_from_results(&results);
        assert_eq!(metrics.total_crates, 2);
        assert_eq!(metrics.total_tests, 50);
        assert_eq!(metrics.total_passed, 50);
        assert_eq!(metrics.average_coverage, 85.0);
    }

    #[test]
    fn test_coordinator_add_crate() {
        let mut coord = Coordinator::new();
        let krate = Crate::new("test".to_string(), "crates/test".to_string());
        coord.add_crate(krate.clone());
        assert_eq!(coord.crate_count(), 1);
    }

    #[test]
    fn test_coordinator_build_order_simple() {
        let mut coord = Coordinator::new();
        let core = Crate::new("core".to_string(), "crates/core".to_string());
        let cli = Crate::new("cli".to_string(), "crates/cli".to_string())
            .with_dependencies(vec!["core".to_string()]);

        coord.add_crate(core);
        coord.add_crate(cli);

        let order = coord.determine_build_order().unwrap();
        assert_eq!(order.len(), 2);
        assert_eq!(order[0], "core");
        assert_eq!(order[1], "cli");
    }

    #[test]
    fn test_coordinator_build_order_complex() {
        let mut coord = Coordinator::new();
        let core = Crate::new("core".to_string(), "crates/core".to_string());
        let utils = Crate::new("utils".to_string(), "crates/utils".to_string());
        let cli = Crate::new("cli".to_string(), "crates/cli".to_string())
            .with_dependencies(vec!["core".to_string(), "utils".to_string()]);
        let web = Crate::new("web".to_string(), "crates/web".to_string())
            .with_dependencies(vec!["core".to_string()]);

        coord.add_crate(core);
        coord.add_crate(utils);
        coord.add_crate(cli);
        coord.add_crate(web);

        let order = coord.determine_build_order().unwrap();
        assert_eq!(order.len(), 4);

        // Verify order respects dependencies
        let core_idx = order.iter().position(|x| x == "core").unwrap();
        let utils_idx = order.iter().position(|x| x == "utils").unwrap();
        let cli_idx = order.iter().position(|x| x == "cli").unwrap();
        let web_idx = order.iter().position(|x| x == "web").unwrap();

        assert!(core_idx < cli_idx);
        assert!(utils_idx < cli_idx);
        assert!(core_idx < web_idx);
    }

    #[test]
    fn test_coordinator_circular_dependency() {
        let mut coord = Coordinator::new();
        let a = Crate::new("a".to_string(), "crates/a".to_string())
            .with_dependencies(vec!["b".to_string()]);
        let b = Crate::new("b".to_string(), "crates/b".to_string())
            .with_dependencies(vec!["a".to_string()]);

        coord.add_crate(a);
        coord.add_crate(b);

        let result = coord.determine_build_order();
        assert!(result.is_err());
    }

    #[test]
    fn test_coordinator_record_builds() {
        let mut coord = Coordinator::new();
        coord.record_build(BuildResult::success("core".to_string(), 1000));
        coord.record_build(BuildResult::success("cli".to_string(), 800));

        assert_eq!(coord.build_results().len(), 2);
        assert!(coord.all_builds_succeeded());
    }

    #[test]
    fn test_coordinator_record_tests() {
        let mut coord = Coordinator::new();
        coord.record_test(TestResult::new("core".to_string(), 30, 0, 0, 2000, 90.0));
        coord.record_test(TestResult::new("cli".to_string(), 20, 0, 0, 1500, 80.0));

        assert_eq!(coord.test_results().len(), 2);
        assert!(coord.all_tests_passed());

        let metrics = coord.aggregate_metrics();
        assert_eq!(metrics.total_crates, 2);
        assert_eq!(metrics.success_rate(), 100.0);
    }

    #[test]
    fn test_coordinator_partial_failure() {
        let mut coord = Coordinator::new();
        coord.record_test(TestResult::new("core".to_string(), 30, 0, 0, 2000, 90.0));
        coord.record_test(TestResult::new("cli".to_string(), 15, 5, 0, 1500, 80.0));

        assert!(!coord.all_tests_passed());
        let metrics = coord.aggregate_metrics();
        assert_eq!(metrics.success_rate(), 90.0);
    }
}
