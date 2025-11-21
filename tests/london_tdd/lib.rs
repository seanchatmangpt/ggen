#![cfg(feature = "london_tdd")]
//! Common test utilities and mocks for London TDD tests
//!
//! This module provides reusable mocks and utilities for testing
//! ggen CLI capabilities using London School TDD principles.

use ggen_utils::error::Error as GgenError;
use mockall::predicate::*;
use mockall::*;
use std::path::PathBuf;

// Re-export common test dependencies
pub use fake::{Fake, Faker};
pub use tempfile::TempDir;

/// Mock filesystem for testing file operations
#[allow(dead_code)]
#[automock]
pub trait Filesystem: Send + Sync {
    fn read_file(&self, path: &str) -> Result<String, std::io::Error>;
    fn write_file(&self, path: &str, content: &str) -> Result<(), std::io::Error>;
    fn exists(&self, path: &str) -> bool;
    fn create_dir(&self, path: &str) -> Result<(), std::io::Error>;
    fn list_files(&self, path: &str) -> Result<Vec<String>, std::io::Error>;
}

/// Mock HTTP client for network requests
#[allow(dead_code)]
#[automock]
pub trait HttpClient: Send + Sync {
    fn get(&self, url: &str) -> Result<String, GgenError>;
    fn post(&self, url: &str, body: &str) -> Result<String, GgenError>;
    fn download_file(&self, url: &str, dest: &str) -> Result<(), GgenError>;
}

/// Mock LLM client for AI operations
#[allow(dead_code)]
#[automock]
pub trait LlmClient: Send + Sync {
    fn generate(&self, prompt: &str, model: &str) -> Result<String, GgenError>;
    fn stream_generate(&self, prompt: &str, model: &str) -> Result<Vec<String>, GgenError>;
}

/// Mock marketplace/registry client
#[allow(dead_code)]
#[automock]
pub trait MarketplaceClient: Send + Sync {
    fn search(&self, query: &str) -> Result<Vec<Package>, GgenError>;
    fn download(&self, package_id: &str) -> Result<Vec<u8>, GgenError>;
    fn list_categories(&self) -> Result<Vec<String>, GgenError>;
}

/// Mock GitHub API client
#[allow(dead_code)]
#[automock]
pub trait GitHubClient: Send + Sync {
    fn get_pages_status(&self, repo: &str) -> Result<PagesStatus, GgenError>;
    fn get_workflow_status(&self, repo: &str) -> Result<Vec<WorkflowRun>, GgenError>;
    fn trigger_workflow(&self, repo: &str, workflow: &str) -> Result<(), GgenError>;
}

/// Package information from marketplace
#[allow(dead_code)]
#[derive(Debug, Clone)]
pub struct Package {
    pub id: String,
    pub name: String,
    pub version: String,
    pub description: String,
    pub category: String,
}

/// GitHub Pages deployment status
#[allow(dead_code)]
#[derive(Debug, Clone)]
pub struct PagesStatus {
    pub status: String,
    pub html_url: String,
    pub source_branch: String,
}

/// GitHub workflow run information
#[allow(dead_code)]
#[derive(Debug, Clone)]
pub struct WorkflowRun {
    pub id: u64,
    pub name: String,
    pub status: String,
    pub conclusion: Option<String>,
}

/// Test helper to create a temporary directory
pub fn setup_test_dir() -> TempDir {
    #[allow(clippy::expect_used)]
    TempDir::new().expect("Failed to create temp dir")
}

/// Test helper to create a mock template file
pub fn create_mock_template(dir: &TempDir, name: &str, content: &str) -> PathBuf {
    let path = dir.path().join(name);
    #[allow(clippy::expect_used)]
    std::fs::write(&path, content).expect("Failed to write template");
    path
}

/// Test helper to create a mock config file
#[allow(dead_code)]
pub fn create_mock_config(dir: &TempDir, config: &str) -> PathBuf {
    let path = dir.path().join("config.toml");
    #[allow(clippy::expect_used)]
    std::fs::write(&path, config).expect("Failed to write config");
    path
}

/// OpenTelemetry test utilities
pub mod otel {
    use std::sync::{Arc, Mutex};

    /// Mock span for testing OpenTelemetry instrumentation
    #[allow(dead_code)]
    #[derive(Debug, Clone)]
    pub struct MockSpan {
        pub name: String,
        pub attributes: Vec<(String, String)>,
        pub events: Vec<String>,
        pub status: SpanStatus,
    }

    #[allow(dead_code)]
    #[derive(Debug, Clone, PartialEq)]
    pub enum SpanStatus {
        Unset,
        Ok,
        Error(String),
    }

    /// Mock tracer provider for testing
    #[derive(Clone)]
    pub struct MockTracerProvider {
        spans: Arc<Mutex<Vec<MockSpan>>>,
    }

    impl MockTracerProvider {
        pub fn new() -> Self {
            Self {
                spans: Arc::new(Mutex::new(Vec::new())),
            }
        }

        pub fn record_span(&self, span: MockSpan) {
            self.spans.lock().unwrap().push(span);
        }

        pub fn get_spans(&self) -> Vec<MockSpan> {
            self.spans.lock().unwrap().clone()
        }

        #[allow(dead_code)]
        pub fn clear(&self) {
            self.spans.lock().unwrap().clear();
        }

        pub fn find_span(&self, name: &str) -> Option<MockSpan> {
            self.spans
                .lock()
                .unwrap()
                .iter()
                .find(|s| s.name == name)
                .cloned()
        }
    }

    impl Default for MockTracerProvider {
        fn default() -> Self {
            Self::new()
        }
    }
}

/// Fake data generators for testing
pub mod generators {
    use super::*;

    pub fn fake_package() -> Package {
        Package {
            id: format!("io.ggen.{}", Faker.fake::<String>()),
            name: Faker.fake(),
            version: "1.0.0".to_string(),
            description: Faker.fake(),
            category: "rust".to_string(),
        }
    }

    #[allow(dead_code)]
    pub fn fake_packages(count: usize) -> Vec<Package> {
        (0..count).map(|_| fake_package()).collect()
    }

    #[allow(dead_code)]
    pub fn fake_pages_status() -> PagesStatus {
        PagesStatus {
            status: "built".to_string(),
            html_url: "https://example.github.io/repo".to_string(),
            source_branch: "main".to_string(),
        }
    }

    #[allow(dead_code)]
    pub fn fake_workflow_run() -> WorkflowRun {
        WorkflowRun {
            id: Faker.fake(),
            name: Faker.fake(),
            status: "completed".to_string(),
            conclusion: Some("success".to_string()),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_setup_test_dir() {
        let dir = setup_test_dir();
        assert!(dir.path().exists());
    }

    #[test]
    fn test_create_mock_template() {
        let dir = setup_test_dir();
        let path = create_mock_template(&dir, "test.tmpl", "content");
        assert!(path.exists());
        let content = std::fs::read_to_string(&path).unwrap();
        assert_eq!(content, "content");
    }

    #[test]
    fn test_fake_package_generation() {
        let pkg = generators::fake_package();
        assert!(pkg.id.starts_with("io.ggen."));
        assert!(!pkg.name.is_empty());
    }

    #[test]
    fn test_mock_tracer_provider() {
        let provider = otel::MockTracerProvider::new();
        let span = otel::MockSpan {
            name: "test".to_string(),
            attributes: vec![],
            events: vec![],
            status: otel::SpanStatus::Ok,
        };
        provider.record_span(span.clone());
        assert_eq!(provider.get_spans().len(), 1);
        assert_eq!(provider.find_span("test").unwrap().name, "test");
    }
}
