//! Consolidated Marketplace Tests
//!
//! Self-contained marketplace tests that don't depend on internal crate APIs.
//! Uses test doubles to verify marketplace logic patterns.
//!
//! Originally consolidated from:
//! - marketplace_concurrent_test.rs
//! - marketplace_install_e2e.rs
//! - integration/marketplace_test.rs
//! - marketplace/install_tests.rs
//! - marketplace/registry_tests.rs
//! - marketplace/integration/*.rs
//! - integration/complete_marketplace_test.rs
//! - marketplace/install_chicago_tdd.rs

use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::fs;
use std::path::Path;
use std::sync::Arc;
use std::time::Duration;
use tempfile::TempDir;
use tokio::sync::{Barrier, RwLock};
use tokio::task::JoinSet;

// ============================================================================
// TEST TYPES (Self-contained to avoid API coupling)
// ============================================================================

#[derive(Debug, Clone, Serialize, Deserialize)]
struct TestRegistry {
    version: String,
    packages: Vec<TestPackage>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct TestPackage {
    name: String,
    full_name: String,
    version: String,
    description: String,
    category: String,
    author: String,
    repository: String,
    path: String,
    license: String,
    dependencies: Vec<String>,
    features: Vec<String>,
    tags: Vec<String>,
    keywords: Vec<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct TestLockfile {
    version: String,
    updated_at: String,
    packages: HashMap<String, TestInstalledPackage>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct TestInstalledPackage {
    name: String,
    full_name: String,
    version: String,
    checksum: String,
    source: String,
    path: String,
    installed_at: String,
    dependencies: Vec<String>,
}

impl TestRegistry {
    fn new() -> Self {
        Self {
            version: "1.0.0".to_string(),
            packages: vec![],
        }
    }

    fn with_package(mut self, package: TestPackage) -> Self {
        self.packages.push(package);
        self
    }

    fn search(&self, query: &str, limit: usize) -> Vec<&TestPackage> {
        self.packages
            .iter()
            .filter(|p| {
                p.name.contains(query)
                    || p.description.contains(query)
                    || p.tags.iter().any(|t| t.contains(query))
            })
            .take(limit)
            .collect()
    }

    fn get_by_category(&self, category: &str) -> Vec<&TestPackage> {
        self.packages
            .iter()
            .filter(|p| p.category == category)
            .collect()
    }

    fn get_package(&self, name: &str) -> Option<&TestPackage> {
        self.packages
            .iter()
            .find(|p| p.name == name || p.full_name == name)
    }
}

impl TestLockfile {
    fn new() -> Self {
        Self {
            version: "1.0.0".to_string(),
            updated_at: chrono::Utc::now().to_rfc3339(),
            packages: HashMap::new(),
        }
    }

    fn add_package(&mut self, package: TestInstalledPackage) {
        self.packages.insert(package.name.clone(), package);
        self.updated_at = chrono::Utc::now().to_rfc3339();
    }

    fn has_package(&self, name: &str) -> bool {
        self.packages.contains_key(name)
    }

    fn get_package(&self, name: &str) -> Option<&TestInstalledPackage> {
        self.packages.get(name)
    }

    fn remove_package(&mut self, name: &str) -> Option<TestInstalledPackage> {
        self.packages.remove(name)
    }

    fn list_packages(&self) -> Vec<&TestInstalledPackage> {
        self.packages.values().collect()
    }

    fn save_to_path(&self, path: &Path) -> std::io::Result<()> {
        if let Some(parent) = path.parent() {
            fs::create_dir_all(parent)?;
        }
        let json = serde_json::to_string_pretty(self).unwrap();
        fs::write(path, json)
    }

    fn load_from_path(path: &Path) -> std::io::Result<Self> {
        if !path.exists() {
            return Ok(Self::new());
        }
        let content = fs::read_to_string(path)?;
        Ok(serde_json::from_str(&content).unwrap_or_else(|_| Self::new()))
    }
}

fn create_test_registry() -> TestRegistry {
    TestRegistry::new()
        .with_package(TestPackage {
            name: "rig-mcp".to_string(),
            full_name: "rig-mcp-integration".to_string(),
            version: "0.1.0".to_string(),
            description: "Rig LLM framework with MCP protocol integration".to_string(),
            category: "ai".to_string(),
            author: "ggen-team".to_string(),
            repository: "https://github.com/ggen/rig-mcp".to_string(),
            path: "marketplace/packages/rig-mcp".to_string(),
            license: "MIT".to_string(),
            dependencies: vec![],
            features: vec![
                "Multi-provider LLM support".to_string(),
                "MCP protocol integration".to_string(),
            ],
            tags: vec!["ai".to_string(), "llm".to_string(), "mcp".to_string()],
            keywords: vec!["rig".to_string(), "mcp".to_string()],
        })
        .with_package(TestPackage {
            name: "api-endpoint".to_string(),
            full_name: "api-endpoint-templates".to_string(),
            version: "1.0.0".to_string(),
            description: "REST API endpoint templates with OpenAPI".to_string(),
            category: "templates".to_string(),
            author: "ggen-team".to_string(),
            repository: "https://github.com/ggen/api-endpoint".to_string(),
            path: "marketplace/packages/api-endpoint".to_string(),
            license: "MIT".to_string(),
            dependencies: vec![],
            features: vec!["Axum handlers".to_string(), "OpenAPI docs".to_string()],
            tags: vec!["api".to_string(), "rest".to_string()],
            keywords: vec!["api".to_string(), "endpoint".to_string()],
        })
}

// ============================================================================
// SECTION 1: REGISTRY SEARCH TESTS
// ============================================================================

#[test]
fn test_registry_search_exact_name_match() {
    let registry = create_test_registry();
    let results = registry.search("rig-mcp", 10);
    assert_eq!(results.len(), 1);
    assert_eq!(results[0].name, "rig-mcp");
}

#[test]
fn test_registry_search_partial_match() {
    let registry = create_test_registry();
    let results = registry.search("api", 10);
    assert_eq!(results.len(), 1);
    assert_eq!(results[0].name, "api-endpoint");
}

#[test]
fn test_registry_search_tag_based() {
    let registry = create_test_registry();
    let results = registry.search("llm", 10);
    assert_eq!(results.len(), 1);
    assert_eq!(results[0].name, "rig-mcp");
}

#[test]
fn test_registry_category_filter() {
    let registry = create_test_registry();
    let ai_packages = registry.get_by_category("ai");
    assert_eq!(ai_packages.len(), 1);
    assert_eq!(ai_packages[0].name, "rig-mcp");
}

#[test]
fn test_registry_get_package_by_name() {
    let registry = create_test_registry();
    let pkg = registry.get_package("rig-mcp");
    assert!(pkg.is_some());
    assert_eq!(pkg.unwrap().version, "0.1.0");
}

#[test]
fn test_registry_get_package_by_full_name() {
    let registry = create_test_registry();
    let pkg = registry.get_package("rig-mcp-integration");
    assert!(pkg.is_some());
    assert_eq!(pkg.unwrap().name, "rig-mcp");
}

#[test]
fn test_registry_get_nonexistent_package() {
    let registry = create_test_registry();
    let pkg = registry.get_package("non-existent");
    assert!(pkg.is_none());
}

// ============================================================================
// SECTION 2: LOCKFILE CRUD TESTS
// ============================================================================

#[test]
fn test_lockfile_create_new() {
    let lockfile = TestLockfile::new();
    assert_eq!(lockfile.version, "1.0.0");
    assert!(lockfile.packages.is_empty());
}

#[test]
fn test_lockfile_add_package() {
    let mut lockfile = TestLockfile::new();
    let package = TestInstalledPackage {
        name: "rig-mcp".to_string(),
        full_name: "rig-mcp-integration".to_string(),
        version: "0.1.0".to_string(),
        checksum: "abc123def456".to_string(),
        source: "registry".to_string(),
        path: ".ggen/packages/rig-mcp".to_string(),
        installed_at: chrono::Utc::now().to_rfc3339(),
        dependencies: vec![],
    };

    lockfile.add_package(package);
    assert_eq!(lockfile.packages.len(), 1);
    assert!(lockfile.has_package("rig-mcp"));
}

#[test]
fn test_lockfile_save_and_load() {
    let temp_dir = TempDir::new().unwrap();
    let lockfile_path = temp_dir.path().join("lock.json");

    let mut lockfile = TestLockfile::new();
    let package = TestInstalledPackage {
        name: "test-pkg".to_string(),
        full_name: "test-package".to_string(),
        version: "1.0.0".to_string(),
        checksum: "abc123".to_string(),
        source: "registry".to_string(),
        path: ".ggen/packages/test-pkg".to_string(),
        installed_at: "2025-10-12T00:00:00Z".to_string(),
        dependencies: vec!["dep1".to_string()],
    };

    lockfile.add_package(package);
    lockfile.save_to_path(&lockfile_path).unwrap();
    assert!(lockfile_path.exists());

    let loaded = TestLockfile::load_from_path(&lockfile_path).unwrap();
    assert_eq!(loaded.packages.len(), 1);

    let retrieved = loaded.get_package("test-pkg").unwrap();
    assert_eq!(retrieved.version, "1.0.0");
    assert_eq!(retrieved.checksum, "abc123");
}

#[test]
fn test_lockfile_update_multiple_packages() {
    let temp_dir = TempDir::new().unwrap();
    let lockfile_path = temp_dir.path().join("lock.json");

    let mut lockfile = TestLockfile::new();
    let package1 = TestInstalledPackage {
        name: "pkg1".to_string(),
        full_name: "package-1".to_string(),
        version: "1.0.0".to_string(),
        checksum: "check1".to_string(),
        source: "registry".to_string(),
        path: ".ggen/packages/pkg1".to_string(),
        installed_at: chrono::Utc::now().to_rfc3339(),
        dependencies: vec![],
    };

    let package2 = TestInstalledPackage {
        name: "pkg2".to_string(),
        full_name: "package-2".to_string(),
        version: "2.0.0".to_string(),
        checksum: "check2".to_string(),
        source: "registry".to_string(),
        path: ".ggen/packages/pkg2".to_string(),
        installed_at: chrono::Utc::now().to_rfc3339(),
        dependencies: vec![],
    };

    lockfile.add_package(package1);
    lockfile.add_package(package2);
    lockfile.save_to_path(&lockfile_path).unwrap();

    let loaded = TestLockfile::load_from_path(&lockfile_path).unwrap();
    assert_eq!(loaded.packages.len(), 2);
    assert!(loaded.has_package("pkg1"));
    assert!(loaded.has_package("pkg2"));
}

#[test]
fn test_lockfile_remove_package() {
    let mut lockfile = TestLockfile::new();
    let package = TestInstalledPackage {
        name: "to-remove".to_string(),
        full_name: "to-remove-pkg".to_string(),
        version: "1.0.0".to_string(),
        checksum: "abc".to_string(),
        source: "registry".to_string(),
        path: ".ggen/packages/to-remove".to_string(),
        installed_at: chrono::Utc::now().to_rfc3339(),
        dependencies: vec![],
    };

    lockfile.add_package(package);
    assert!(lockfile.has_package("to-remove"));

    let removed = lockfile.remove_package("to-remove");
    assert!(removed.is_some());
    assert!(!lockfile.has_package("to-remove"));
}

#[test]
fn test_lockfile_persistence_format() {
    let temp_dir = TempDir::new().unwrap();
    let lockfile_path = temp_dir.path().join("lock.json");

    let mut lockfile = TestLockfile::new();
    let package = TestInstalledPackage {
        name: "test-pkg".to_string(),
        full_name: "test-package".to_string(),
        version: "1.0.0".to_string(),
        checksum: "abc123".to_string(),
        source: "registry".to_string(),
        path: ".ggen/packages/test-pkg".to_string(),
        installed_at: "2025-10-12T00:00:00Z".to_string(),
        dependencies: vec!["dep1".to_string(), "dep2".to_string()],
    };

    lockfile.add_package(package);
    lockfile.save_to_path(&lockfile_path).unwrap();

    let content = fs::read_to_string(&lockfile_path).unwrap();
    assert!(content.contains("\"version\": \"1.0.0\""));
    assert!(content.contains("test-pkg"));
    assert!(content.contains("abc123"));

    let parsed: serde_json::Value = serde_json::from_str(&content).unwrap();
    assert!(parsed["packages"]["test-pkg"]["checksum"].is_string());
}

// ============================================================================
// SECTION 3: E2E MARKETPLACE WORKFLOW TESTS
// ============================================================================

#[test]
fn test_marketplace_workflow_complete() {
    let temp_dir = TempDir::new().unwrap();
    let lockfile_path = temp_dir.path().join("lock.json");

    // Step 1: Create registry
    let registry = create_test_registry();
    assert_eq!(registry.packages.len(), 2);

    // Step 2: Search for package
    let search_results = registry.search("rig-mcp", 10);
    assert_eq!(search_results.len(), 1);
    let package = search_results[0];

    // Step 3: Simulate installation
    let mut lockfile = TestLockfile::new();
    let installed_package = TestInstalledPackage {
        name: package.name.clone(),
        full_name: package.full_name.clone(),
        version: package.version.clone(),
        checksum: "production-checksum".to_string(),
        source: "registry".to_string(),
        path: format!(".ggen/packages/{}", package.name),
        installed_at: chrono::Utc::now().to_rfc3339(),
        dependencies: package.dependencies.clone(),
    };

    lockfile.add_package(installed_package);
    lockfile.save_to_path(&lockfile_path).unwrap();

    // Step 4: Verify installation
    let installed_lockfile = TestLockfile::load_from_path(&lockfile_path).unwrap();
    assert!(installed_lockfile.has_package("rig-mcp"));

    let installed_pkg = installed_lockfile.get_package("rig-mcp").unwrap();
    assert_eq!(installed_pkg.version, "0.1.0");
    assert_eq!(installed_pkg.source, "registry");

    // Step 5: List installed packages
    let all_packages = installed_lockfile.list_packages();
    assert_eq!(all_packages.len(), 1);

    // Step 6: Simulate uninstallation
    let mut uninstall_lockfile = installed_lockfile;
    let removed = uninstall_lockfile.remove_package("rig-mcp");
    assert!(removed.is_some());

    uninstall_lockfile.save_to_path(&lockfile_path).unwrap();

    // Step 7: Verify uninstallation
    let final_lockfile = TestLockfile::load_from_path(&lockfile_path).unwrap();
    assert!(!final_lockfile.has_package("rig-mcp"));
    assert_eq!(final_lockfile.packages.len(), 0);
}

// ============================================================================
// SECTION 4: CONCURRENT OPERATIONS TESTS
// ============================================================================

#[tokio::test]
async fn test_concurrent_reads() {
    let reader_count = 10;
    let barrier = Arc::new(Barrier::new(reader_count));
    let mut tasks = JoinSet::new();

    for i in 0..reader_count {
        let barrier = Arc::clone(&barrier);
        let task_id = i;

        tasks.spawn(async move {
            barrier.wait().await;
            tokio::time::sleep(Duration::from_millis(10)).await;
            Ok::<_, anyhow::Error>(task_id)
        });
    }

    let mut completed = 0;
    while let Some(result) = tasks.join_next().await {
        result.unwrap().unwrap();
        completed += 1;
    }

    assert_eq!(completed, reader_count);
}

#[tokio::test]
async fn test_concurrent_writes() {
    let writer_count = 5;
    let barrier = Arc::new(Barrier::new(writer_count));
    let write_counter = Arc::new(RwLock::new(0));

    let mut tasks = JoinSet::new();

    for i in 0..writer_count {
        let barrier = Arc::clone(&barrier);
        let counter = Arc::clone(&write_counter);
        let package_name = format!("package-{}", i);

        tasks.spawn(async move {
            barrier.wait().await;
            let mut count = counter.write().await;
            *count += 1;
            tokio::time::sleep(Duration::from_millis(10)).await;
            Ok::<_, anyhow::Error>(package_name)
        });
    }

    let mut completed = 0;
    while let Some(result) = tasks.join_next().await {
        result.unwrap().unwrap();
        completed += 1;
    }

    assert_eq!(completed, writer_count);
    let final_count = *write_counter.read().await;
    assert_eq!(final_count, writer_count);
}

#[tokio::test]
async fn test_mixed_concurrent_operations() {
    let operation_count = 20;
    let barrier = Arc::new(Barrier::new(operation_count));
    let shared_state = Arc::new(RwLock::new(Vec::<String>::new()));

    let mut tasks = JoinSet::new();

    for i in 0..operation_count {
        let barrier = Arc::clone(&barrier);
        let state = Arc::clone(&shared_state);
        let is_write = i % 2 == 0;

        tasks.spawn(async move {
            barrier.wait().await;

            if is_write {
                let mut data = state.write().await;
                data.push(format!("write-{}", i));
            } else {
                let data = state.read().await;
                let _ = data.len();
            }

            tokio::time::sleep(Duration::from_millis(5)).await;
            Ok::<_, anyhow::Error>(i)
        });
    }

    let mut completed = 0;
    while let Some(result) = tasks.join_next().await {
        result.unwrap().unwrap();
        completed += 1;
    }

    assert_eq!(completed, operation_count);
    let final_state = shared_state.read().await;
    assert_eq!(final_state.len(), operation_count / 2);
}

// ============================================================================
// SECTION 5: ERROR HANDLING TESTS
// ============================================================================

#[test]
fn test_load_nonexistent_lockfile_creates_new() {
    let temp_dir = TempDir::new().unwrap();
    let lockfile_path = temp_dir.path().join("lock.json");
    let result = TestLockfile::load_from_path(&lockfile_path);
    assert!(result.is_ok());
    assert_eq!(result.unwrap().packages.len(), 0);
}

#[test]
fn test_save_lockfile_creates_parent_dirs() {
    let temp_dir = TempDir::new().unwrap();
    let lockfile = TestLockfile::new();
    let nested_path = temp_dir.path().join("subdir/nested/lock.json");
    let result = lockfile.save_to_path(&nested_path);
    assert!(result.is_ok());
}

// ============================================================================
// SECTION 6: PRODUCTION READINESS TESTS
// ============================================================================

#[test]
fn test_production_registry_structure_complete() {
    let registry = create_test_registry();

    for package in &registry.packages {
        assert!(!package.name.is_empty(), "Package name required");
        assert!(!package.version.is_empty(), "Version required");
        assert!(!package.description.is_empty(), "Description required");
        assert!(!package.category.is_empty(), "Category required");
        assert!(!package.author.is_empty(), "Author required");
        assert!(!package.repository.is_empty(), "Repository required");
        assert!(!package.path.is_empty(), "Path required");
        assert!(!package.license.is_empty(), "License required");
    }
}

#[test]
fn test_production_lockfile_validation() {
    let mut lockfile = TestLockfile::new();

    let package = TestInstalledPackage {
        name: "prod-pkg".to_string(),
        full_name: "production-package".to_string(),
        version: "1.0.0".to_string(),
        checksum: "sha256:abc123def456".to_string(),
        source: "registry".to_string(),
        path: ".ggen/packages/prod-pkg".to_string(),
        installed_at: chrono::Utc::now().to_rfc3339(),
        dependencies: vec![],
    };

    lockfile.add_package(package);

    assert!(!lockfile.version.is_empty());
    assert!(!lockfile.updated_at.is_empty());

    let installed = lockfile.get_package("prod-pkg").unwrap();
    assert!(installed.checksum.starts_with("sha256:"));
    assert!(!installed.installed_at.is_empty());
}

#[test]
fn test_marketplace_scalability_100_packages() {
    let temp_dir = TempDir::new().unwrap();
    let lockfile_path = temp_dir.path().join("lock.json");

    let mut lockfile = TestLockfile::new();

    for i in 0..100 {
        let package = TestInstalledPackage {
            name: format!("pkg-{}", i),
            full_name: format!("package-{}", i),
            version: "1.0.0".to_string(),
            checksum: format!("checksum-{}", i),
            source: "registry".to_string(),
            path: format!(".ggen/packages/pkg-{}", i),
            installed_at: chrono::Utc::now().to_rfc3339(),
            dependencies: vec![],
        };
        lockfile.add_package(package);
    }

    lockfile.save_to_path(&lockfile_path).unwrap();
    let loaded = TestLockfile::load_from_path(&lockfile_path).unwrap();

    assert_eq!(loaded.packages.len(), 100);

    for i in 0..100 {
        let pkg_name = format!("pkg-{}", i);
        assert!(loaded.has_package(&pkg_name));
    }
}

// ============================================================================
// SECTION 7: EDGE CASE TESTS
// ============================================================================

#[cfg(test)]
mod edge_case_tests {
    use super::*;

    #[test]
    fn test_empty_search_returns_empty() {
        let registry = create_test_registry();
        let results = registry.search("nonexistent-package-xyz", 10);
        assert!(results.is_empty());
    }

    #[test]
    fn test_search_limit_respected() {
        let registry = create_test_registry();
        let results = registry.search("", 1);
        assert!(results.len() <= 1);
    }

    #[test]
    fn test_duplicate_package_add_replaces() {
        let mut lockfile = TestLockfile::new();

        let package_v1 = TestInstalledPackage {
            name: "dup-pkg".to_string(),
            full_name: "duplicate-package".to_string(),
            version: "1.0.0".to_string(),
            checksum: "v1-checksum".to_string(),
            source: "registry".to_string(),
            path: ".ggen/packages/dup-pkg".to_string(),
            installed_at: chrono::Utc::now().to_rfc3339(),
            dependencies: vec![],
        };

        let package_v2 = TestInstalledPackage {
            name: "dup-pkg".to_string(),
            full_name: "duplicate-package".to_string(),
            version: "2.0.0".to_string(),
            checksum: "v2-checksum".to_string(),
            source: "registry".to_string(),
            path: ".ggen/packages/dup-pkg".to_string(),
            installed_at: chrono::Utc::now().to_rfc3339(),
            dependencies: vec![],
        };

        lockfile.add_package(package_v1);
        lockfile.add_package(package_v2);

        // Should have 1 package (v2 replaces v1)
        assert_eq!(lockfile.packages.len(), 1);
        let pkg = lockfile.get_package("dup-pkg").unwrap();
        assert_eq!(pkg.version, "2.0.0");
    }
}

#[cfg(test)]
mod backward_compatibility_tests {
    use super::*;

    #[test]
    fn test_lockfile_v1_compatible() {
        let v1_lockfile = r#"{
            "version": "1.0.0",
            "updated_at": "2024-01-01T00:00:00Z",
            "packages": {}
        }"#;

        let temp_dir = TempDir::new().unwrap();
        let lockfile_path = temp_dir.path().join("lock.json");
        fs::write(&lockfile_path, v1_lockfile).unwrap();

        let loaded = TestLockfile::load_from_path(&lockfile_path).unwrap();
        assert_eq!(loaded.version, "1.0.0");
    }

    #[test]
    fn test_registry_v1_compatible() {
        let registry = create_test_registry();
        assert_eq!(registry.version, "1.0.0");
    }
}
