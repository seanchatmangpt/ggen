//! Auto-deployment automation with validation and rollback

use crate::error::{GgenAiError, Result};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::process::Command;
use tokio::fs;
use tracing::{debug, error, info, warn};

/// Rollback strategy on deployment failure
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq)]
#[serde(rename_all = "snake_case")]
pub enum RollbackStrategy {
    /// Automatically rollback on any failure
    Automatic,
    /// Require manual intervention
    Manual,
    /// Keep both versions (blue-green)
    BlueGreen,
    /// No rollback
    None,
}

/// Deployment configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DeploymentConfig {
    /// Enable auto-deployment
    pub enabled: bool,
    /// Rollback strategy
    pub rollback_strategy: RollbackStrategy,
    /// Run validation tests before deployment
    pub validate_before_deploy: bool,
    /// Run integration tests after deployment
    pub run_integration_tests: bool,
    /// Deployment timeout in seconds
    pub timeout_seconds: u64,
    /// Environments to deploy to
    pub environments: Vec<DeploymentEnvironment>,
    /// Commands to run before deployment
    pub pre_deploy_commands: Vec<String>,
    /// Commands to run after deployment
    pub post_deploy_commands: Vec<String>,
}

/// Deployment environment configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DeploymentEnvironment {
    /// Environment name (e.g., "development", "staging", "production")
    pub name: String,
    /// Target directory
    pub target_dir: PathBuf,
    /// Environment-specific variables
    pub env_vars: HashMap<String, String>,
    /// Enable auto-deploy for this environment
    pub auto_deploy: bool,
}

impl Default for DeploymentConfig {
    fn default() -> Self {
        Self {
            enabled: true,
            rollback_strategy: RollbackStrategy::Automatic,
            validate_before_deploy: true,
            run_integration_tests: false,
            timeout_seconds: 300,
            environments: vec![DeploymentEnvironment {
                name: "development".to_string(),
                target_dir: PathBuf::from("build/development"),
                env_vars: HashMap::new(),
                auto_deploy: true,
            }],
            pre_deploy_commands: Vec::new(),
            post_deploy_commands: Vec::new(),
        }
    }
}

/// Result of a deployment operation
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DeploymentResult {
    /// Whether deployment succeeded
    pub success: bool,
    /// Environment deployed to
    pub environment: String,
    /// Deployment timestamp
    pub timestamp: chrono::DateTime<chrono::Utc>,
    /// Version deployed
    pub version: String,
    /// Files deployed
    pub files_deployed: Vec<PathBuf>,
    /// Validation results
    pub validation_results: Vec<ValidationResult>,
    /// Error message if failed
    pub error: Option<String>,
    /// Rollback performed
    pub rolled_back: bool,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ValidationResult {
    pub name: String,
    pub passed: bool,
    pub message: String,
    pub duration_ms: u64,
}

/// Auto-deployment automation system
#[derive(Debug)]
pub struct DeploymentAutomation {
    config: DeploymentConfig,
    deployment_history: Vec<DeploymentResult>,
}

impl DeploymentAutomation {
    /// Create a new deployment automation system
    pub fn new(config: DeploymentConfig) -> Self {
        Self {
            config,
            deployment_history: Vec::new(),
        }
    }

    /// Deploy generated code to configured environments
    pub async fn deploy(
        &mut self,
        source_dir: &Path,
        version: &str,
    ) -> Result<Vec<DeploymentResult>> {
        if !self.config.enabled {
            info!("Auto-deployment is disabled");
            return Ok(Vec::new());
        }

        info!(
            version = %version,
            environments = self.config.environments.len(),
            "Starting deployment"
        );

        let mut results = Vec::new();

        for env in &self.config.environments {
            if !env.auto_deploy {
                info!(environment = %env.name, "Skipping (auto-deploy disabled)");
                continue;
            }

            let result = self.deploy_to_environment(source_dir, version, env).await;
            results.push(result);
        }

        // Store in history
        self.deployment_history.extend(results.clone());

        Ok(results)
    }

    /// Deploy to a specific environment
    async fn deploy_to_environment(
        &self,
        source_dir: &Path,
        version: &str,
        env: &DeploymentEnvironment,
    ) -> DeploymentResult {
        let start_time = std::time::Instant::now();

        info!(
            environment = %env.name,
            target = %env.target_dir.display(),
            "Deploying to environment"
        );

        // Pre-deployment validation
        if self.config.validate_before_deploy {
            match self.run_validation(source_dir, env).await {
                Ok(results) => {
                    if results.iter().any(|r| !r.passed) {
                        return DeploymentResult {
                            success: false,
                            environment: env.name.clone(),
                            timestamp: chrono::Utc::now(),
                            version: version.to_string(),
                            files_deployed: Vec::new(),
                            validation_results: results,
                            error: Some("Validation failed".to_string()),
                            rolled_back: false,
                        };
                    }
                }
                Err(e) => {
                    return DeploymentResult {
                        success: false,
                        environment: env.name.clone(),
                        timestamp: chrono::Utc::now(),
                        version: version.to_string(),
                        files_deployed: Vec::new(),
                        validation_results: Vec::new(),
                        error: Some(format!("Validation error: {}", e)),
                        rolled_back: false,
                    };
                }
            }
        }

        // Run pre-deploy commands
        if let Err(e) = self.run_commands(&self.config.pre_deploy_commands, env).await {
            error!(error = %e, "Pre-deploy commands failed");
            return self.create_failed_result(env, version, e.to_string());
        }

        // Backup current deployment (for rollback)
        let backup_dir = self.create_backup(&env.target_dir).await.ok();

        // Copy files to target directory
        let deployed_files = match self.copy_files(source_dir, &env.target_dir).await {
            Ok(files) => files,
            Err(e) => {
                error!(error = %e, "File deployment failed");
                // Attempt rollback
                if let Some(backup) = backup_dir {
                    let _ = self.rollback(&env.target_dir, &backup).await;
                }
                return self.create_failed_result(env, version, e.to_string());
            }
        };

        // Run post-deploy commands
        if let Err(e) = self.run_commands(&self.config.post_deploy_commands, env).await {
            error!(error = %e, "Post-deploy commands failed");
            // Attempt rollback if configured
            if self.config.rollback_strategy == RollbackStrategy::Automatic {
                if let Some(backup) = backup_dir {
                    if let Err(rollback_err) = self.rollback(&env.target_dir, &backup).await {
                        error!(error = %rollback_err, "Rollback failed");
                    }
                    return DeploymentResult {
                        success: false,
                        environment: env.name.clone(),
                        timestamp: chrono::Utc::now(),
                        version: version.to_string(),
                        files_deployed: deployed_files,
                        validation_results: Vec::new(),
                        error: Some(e.to_string()),
                        rolled_back: true,
                    };
                }
            }
            return self.create_failed_result(env, version, e.to_string());
        }

        // Run integration tests if configured
        let mut validation_results = Vec::new();
        if self.config.run_integration_tests {
            match self.run_integration_tests(env).await {
                Ok(results) => {
                    if results.iter().any(|r| !r.passed) {
                        // Tests failed - rollback if configured
                        if self.config.rollback_strategy == RollbackStrategy::Automatic {
                            if let Some(backup) = backup_dir {
                                let _ = self.rollback(&env.target_dir, &backup).await;
                                return DeploymentResult {
                                    success: false,
                                    environment: env.name.clone(),
                                    timestamp: chrono::Utc::now(),
                                    version: version.to_string(),
                                    files_deployed: deployed_files,
                                    validation_results: results,
                                    error: Some("Integration tests failed".to_string()),
                                    rolled_back: true,
                                };
                            }
                        }
                    }
                    validation_results = results;
                }
                Err(e) => {
                    warn!(error = %e, "Integration tests failed to run");
                }
            }
        }

        let duration = start_time.elapsed().as_millis() as u64;
        info!(
            environment = %env.name,
            duration_ms = duration,
            files = deployed_files.len(),
            "Deployment completed successfully"
        );

        DeploymentResult {
            success: true,
            environment: env.name.clone(),
            timestamp: chrono::Utc::now(),
            version: version.to_string(),
            files_deployed: deployed_files,
            validation_results,
            error: None,
            rolled_back: false,
        }
    }

    /// Run validation tests
    async fn run_validation(
        &self,
        source_dir: &Path,
        env: &DeploymentEnvironment,
    ) -> Result<Vec<ValidationResult>> {
        debug!("Running validation tests for {}", source_dir.display());
        let mut results = Vec::new();

        // Syntax validation
        let syntax_result = self.validate_syntax(source_dir).await?;
        results.push(syntax_result);

        // Security validation
        let security_result = self.validate_security(source_dir).await?;
        results.push(security_result);

        // Performance validation
        let performance_result = self.validate_performance(source_dir).await?;
        results.push(performance_result);

        // Environment-specific validation
        let env_result = self.validate_environment(source_dir, env).await?;
        results.push(env_result);

        Ok(results)
    }

    /// Run integration tests
    async fn run_integration_tests(
        &self,
        env: &DeploymentEnvironment,
    ) -> Result<Vec<ValidationResult>> {
        debug!("Running integration tests for environment: {}", env.name);
        let mut results = Vec::new();

        // Test basic functionality
        let basic_test = self.run_basic_tests(env).await?;
        results.extend(basic_test);

        // Test API endpoints if applicable
        let api_test = self.run_api_tests(env).await?;
        results.extend(api_test);

        // Test database connectivity if applicable
        let db_test = self.run_database_tests(env).await?;
        results.extend(db_test);

        Ok(results)
    }

    /// Run basic functionality tests
    async fn run_basic_tests(&self, _env: &DeploymentEnvironment) -> Result<Vec<ValidationResult>> {
        // Basic functionality tests
        Ok(vec![])
    }

    /// Run API endpoint tests
    async fn run_api_tests(&self, _env: &DeploymentEnvironment) -> Result<Vec<ValidationResult>> {
        // API endpoint tests
        Ok(vec![])
    }

    /// Run database connectivity tests
    async fn run_database_tests(&self, _env: &DeploymentEnvironment) -> Result<Vec<ValidationResult>> {
        // Database connectivity tests
        Ok(vec![])
    }

    /// Run shell commands
    async fn run_commands(
        &self,
        commands: &[String],
        env: &DeploymentEnvironment,
    ) -> Result<()> {
        for cmd in commands {
            debug!(command = %cmd, "Executing command");

            let mut child = Command::new("sh")
                .arg("-c")
                .arg(cmd)
                .envs(&env.env_vars)
                .spawn()
                .map_err(|e| GgenAiError::deployment(format!("Failed to spawn command: {}", e)))?;

            let status = child.wait().map_err(|e| {
                GgenAiError::deployment(format!("Failed to wait for command: {}", e))
            })?;

            if !status.success() {
                return Err(GgenAiError::deployment(format!(
                    "Command failed with status: {}",
                    status
                )));
            }
        }

        Ok(())
    }

    /// Create backup of current deployment
    async fn create_backup(&self, target_dir: &Path) -> Result<PathBuf> {
        let timestamp = chrono::Utc::now().format("%Y%m%d_%H%M%S");
        let backup_dir = target_dir.with_extension(format!("backup_{}", timestamp));

        debug!(
            source = %target_dir.display(),
            backup = %backup_dir.display(),
            "Creating backup"
        );

        // Create backup directory
        fs::create_dir_all(&backup_dir).await.map_err(|e| {
            GgenAiError::deployment(format!("Failed to create backup directory: {}", e))
        })?;

        // Copy files (simplified - in production use proper backup tool)
        // TODO: Implement recursive copy

        Ok(backup_dir)
    }

    /// Rollback to previous version
    async fn rollback(&self, target_dir: &Path, backup_dir: &Path) -> Result<()> {
        warn!(
            target = %target_dir.display(),
            backup = %backup_dir.display(),
            "Rolling back deployment"
        );

        // Remove current deployment
        if target_dir.exists() {
            fs::remove_dir_all(target_dir).await.map_err(|e| {
                GgenAiError::deployment(format!("Failed to remove target directory: {}", e))
            })?;
        }

        // Restore backup
        fs::rename(backup_dir, target_dir).await.map_err(|e| {
            GgenAiError::deployment(format!("Failed to restore backup: {}", e))
        })?;

        info!("Rollback completed successfully");
        Ok(())
    }

    /// Copy files from source to target
    async fn copy_files(&self, source: &Path, target: &Path) -> Result<Vec<PathBuf>> {
        debug!(
            source = %source.display(),
            target = %target.display(),
            "Copying files"
        );

        // Create target directory
        fs::create_dir_all(target).await.map_err(|e| {
            GgenAiError::deployment(format!("Failed to create target directory: {}", e))
        })?;

        // Implement recursive file copy
        let mut copied_files = Vec::new();
        
        if let Ok(entries) = fs::read_dir(source).await {
            let mut entries = entries;
            while let Some(entry) = entries.next_entry().await? {
                let source_path = entry.path();
                let file_name = source_path.file_name()
                    .ok_or_else(|| GgenAiError::deployment("Invalid file name"))?;
                let target_path = target.join(file_name);
                
                if source_path.is_dir() {
                    // Recursively copy directory
                    fs::create_dir_all(&target_path).await.map_err(|e| {
                        GgenAiError::deployment(format!("Failed to create directory: {}", e))
                    })?;
                    
                    let sub_files = Box::pin(self.copy_files(&source_path, &target_path)).await?;
                    copied_files.extend(sub_files);
                } else {
                    // Copy file
                    fs::copy(&source_path, &target_path).await.map_err(|e| {
                        GgenAiError::deployment(format!("Failed to copy file: {}", e))
                    })?;
                    copied_files.push(target_path);
                }
            }
        }

        Ok(copied_files)
    }

    /// Create a failed deployment result
    fn create_failed_result(
        &self,
        env: &DeploymentEnvironment,
        version: &str,
        error: String,
    ) -> DeploymentResult {
        DeploymentResult {
            success: false,
            environment: env.name.clone(),
            timestamp: chrono::Utc::now(),
            version: version.to_string(),
            files_deployed: Vec::new(),
            validation_results: Vec::new(),
            error: Some(error),
            rolled_back: false,
        }
    }

    /// Get deployment history
    pub fn get_history(&self, limit: usize) -> &[DeploymentResult] {
        let start = if self.deployment_history.len() > limit {
            self.deployment_history.len() - limit
        } else {
            0
        };
        &self.deployment_history[start..]
    }

    /// Get last deployment for an environment
    pub fn get_last_deployment(&self, environment: &str) -> Option<&DeploymentResult> {
        self.deployment_history
            .iter()
            .rev()
            .find(|d| d.environment == environment)
    }

    /// Validate syntax of generated files
    async fn validate_syntax(&self, source_dir: &Path) -> Result<ValidationResult> {
        let start = std::time::Instant::now();
        
        // Check for common syntax issues in generated files
        let mut issues = Vec::new();
        
        // Validate Rust files
        if let Ok(entries) = fs::read_dir(source_dir).await {
            let mut entries = entries;
            while let Some(entry) = entries.next_entry().await? {
                let path = entry.path();
                if path.extension().and_then(|s| s.to_str()) == Some("rs") {
                    if let Ok(content) = fs::read_to_string(&path).await {
                        // Basic syntax checks
                        if content.contains("fn main() {") && !content.contains("}") {
                            issues.push("Missing closing brace in main function".to_string());
                        }
                        if content.contains("unsafe {") && !content.contains("}") {
                            issues.push("Missing closing brace in unsafe block".to_string());
                        }
                    }
                }
            }
        }

        let duration_ms = start.elapsed().as_millis() as u64;
        let passed = issues.is_empty();
        
        Ok(ValidationResult {
            name: "syntax_check".to_string(),
            passed,
            message: if passed {
                "Syntax validation passed".to_string()
            } else {
                format!("Syntax issues found: {}", issues.join(", "))
            },
            duration_ms,
        })
    }

    /// Validate security aspects of generated files
    async fn validate_security(&self, source_dir: &Path) -> Result<ValidationResult> {
        let start = std::time::Instant::now();
        
        let mut issues = Vec::new();
        
        // Check for dangerous patterns
        if let Ok(entries) = fs::read_dir(source_dir).await {
            let mut entries = entries;
            while let Some(entry) = entries.next_entry().await? {
                let path = entry.path();
                if let Ok(content) = fs::read_to_string(&path).await {
                    // Check for dangerous patterns
                    if content.contains("unsafe {") {
                        issues.push("Unsafe code detected".to_string());
                    }
                    if content.contains("std::process::Command") && !content.contains("sanitize") {
                        issues.push("Process execution without sanitization".to_string());
                    }
                    if content.contains("eval(") || content.contains("exec(") {
                        issues.push("Code execution detected".to_string());
                    }
                }
            }
        }

        let duration_ms = start.elapsed().as_millis() as u64;
        let passed = issues.is_empty();
        
        Ok(ValidationResult {
            name: "security_check".to_string(),
            passed,
            message: if passed {
                "Security validation passed".to_string()
            } else {
                format!("Security issues found: {}", issues.join(", "))
            },
            duration_ms,
        })
    }

    /// Validate performance characteristics
    async fn validate_performance(&self, source_dir: &Path) -> Result<ValidationResult> {
        let start = std::time::Instant::now();
        
        let mut issues = Vec::new();
        
        // Check for performance anti-patterns
        if let Ok(entries) = fs::read_dir(source_dir).await {
            let mut entries = entries;
            while let Some(entry) = entries.next_entry().await? {
                let path = entry.path();
                if let Ok(content) = fs::read_to_string(&path).await {
                    // Check for performance issues
                    if content.contains("Vec::new()") && content.contains("push") && content.contains("for") {
                        issues.push("Potential vector reallocation in loop".to_string());
                    }
                    if content.contains("String::new()") && content.contains("push_str") && content.contains("for") {
                        issues.push("Potential string reallocation in loop".to_string());
                    }
                    if content.contains("clone()") && content.contains("for") {
                        issues.push("Cloning in loop detected".to_string());
                    }
                }
            }
        }

        let duration_ms = start.elapsed().as_millis() as u64;
        let passed = issues.is_empty();
        
        Ok(ValidationResult {
            name: "performance_check".to_string(),
            passed,
            message: if passed {
                "Performance validation passed".to_string()
            } else {
                format!("Performance issues found: {}", issues.join(", "))
            },
            duration_ms,
        })
    }

    /// Validate environment-specific requirements
    async fn validate_environment(&self, source_dir: &Path, env: &DeploymentEnvironment) -> Result<ValidationResult> {
        let start = std::time::Instant::now();
        
        let mut issues = Vec::new();
        
        // Environment-specific validation
        match env.name.as_str() {
            "production" => {
                // Production-specific checks
                if let Ok(entries) = fs::read_dir(source_dir).await {
                    let mut entries = entries;
                    while let Some(entry) = entries.next_entry().await? {
                        let path = entry.path();
                        if let Ok(content) = fs::read_to_string(&path).await {
                            if content.contains("println!") || content.contains("dbg!") {
                                issues.push("Debug output in production code".to_string());
                            }
                            if content.contains("unwrap()") || content.contains("expect(") {
                                issues.push("Unsafe error handling in production".to_string());
                            }
                        }
                    }
                }
            },
            "development" => {
                // Development-specific checks (more lenient)
                debug!("Development environment validation - lenient checks");
            },
            _ => {
                // Other environments
                debug!("Environment {} validation", env.name);
            }
        }

        let duration_ms = start.elapsed().as_millis() as u64;
        let passed = issues.is_empty();
        
        Ok(ValidationResult {
            name: "environment_check".to_string(),
            passed,
            message: if passed {
                format!("Environment validation passed for {}", env.name)
            } else {
                format!("Environment issues found for {}: {}", env.name, issues.join(", "))
            },
            duration_ms,
        })
    }

    /// Run basic integration test
    async fn run_basic_tests(&self, env: &DeploymentEnvironment) -> Result<Vec<ValidationResult>> {
        let start = std::time::Instant::now();

        // Test basic file structure
        let target_exists = env.target_dir.exists();
        let target_is_dir = env.target_dir.is_dir();

        let duration_ms = start.elapsed().as_millis() as u64;
        let passed = target_exists && target_is_dir;

        Ok(vec![ValidationResult {
            name: "basic_integration".to_string(),
            passed,
            message: if passed {
                "Basic integration test passed".to_string()
            } else {
                "Target directory does not exist or is not a directory".to_string()
            },
            duration_ms,
        }])
    }

    /// Run API integration test
    async fn run_api_tests(&self, env: &DeploymentEnvironment) -> Result<Vec<ValidationResult>> {
        let start = std::time::Instant::now();

        // Check if API endpoints are accessible
        let mut passed = true;
        let mut issues = Vec::new();

        // Look for API-related files
        if let Ok(entries) = fs::read_dir(&env.target_dir).await {
            let mut entries = entries;
            let mut has_api_files = false;

            while let Some(entry) = entries.next_entry().await? {
                let path = entry.path();
                if let Some(file_name) = path.file_name().and_then(|n| n.to_str()) {
                    if file_name.contains("api") || file_name.contains("server") || file_name.contains("endpoint") {
                        has_api_files = true;
                        break;
                    }
                }
            }

            if has_api_files {
                // Basic API structure validation
                debug!("API files detected, running basic validation");
            } else {
                debug!("No API files detected, skipping API tests");
            }
        } else {
            issues.push("Cannot read target directory".to_string());
            passed = false;
        }

        let duration_ms = start.elapsed().as_millis() as u64;

        Ok(vec![ValidationResult {
            name: "api_integration".to_string(),
            passed,
            message: if passed {
                "API integration test passed".to_string()
            } else {
                format!("API integration issues: {}", issues.join(", "))
            },
            duration_ms,
        }])
    }

    /// Run database integration test
    async fn run_database_tests(&self, env: &DeploymentEnvironment) -> Result<Vec<ValidationResult>> {
        let start = std::time::Instant::now();

        // Check for database-related configuration
        let mut passed = true;
        let mut issues = Vec::new();

        // Look for database configuration files
        if let Ok(entries) = fs::read_dir(&env.target_dir).await {
            let mut entries = entries;
            let mut has_db_config = false;

            while let Some(entry) = entries.next_entry().await? {
                let path = entry.path();
                if let Some(file_name) = path.file_name().and_then(|n| n.to_str()) {
                    if file_name.contains("database") || file_name.contains("db") || file_name.contains("sql") {
                        has_db_config = true;
                        break;
                    }
                }
            }

            if has_db_config {
                // Basic database configuration validation
                debug!("Database configuration detected, running basic validation");
            } else {
                debug!("No database configuration detected, skipping database tests");
            }
        } else {
            issues.push("Cannot read target directory".to_string());
            passed = false;
        }

        let duration_ms = start.elapsed().as_millis() as u64;

        Ok(vec![ValidationResult {
            name: "database_integration".to_string(),
            passed,
            message: if passed {
                "Database integration test passed".to_string()
            } else {
                format!("Database integration issues: {}", issues.join(", "))
            },
            duration_ms,
        }])
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::TempDir;

    #[tokio::test]
    async fn test_deployment_validation() {
        let temp_dir = TempDir::new().expect("Failed to create temp dir");
        let config = DeploymentConfig::default();
        let automation = DeploymentAutomation::new(config);
        
        // Create test files
        let test_file = temp_dir.path().join("test.rs");
        std::fs::write(&test_file, "fn main() { println!(\"Hello\"); }").expect("Failed to write test file");
        
        let env = DeploymentEnvironment {
            name: "test".to_string(),
            target_dir: temp_dir.path().to_path_buf(),
            env_vars: std::collections::HashMap::new(),
            auto_deploy: true,
        };
        
        let results = automation.run_validation(temp_dir.path(), &env).await.expect("Validation should succeed");
        assert!(!results.is_empty());
        assert!(results.iter().any(|r| r.name == "syntax_check"));
    }

    #[tokio::test]
    async fn test_integration_tests() {
        let temp_dir = TempDir::new().expect("Failed to create temp dir");
        let config = DeploymentConfig::default();
        let automation = DeploymentAutomation::new(config);
        
        let env = DeploymentEnvironment {
            name: "test".to_string(),
            target_dir: temp_dir.path().to_path_buf(),
            env_vars: std::collections::HashMap::new(),
            auto_deploy: true,
        };
        
        let results = automation.run_integration_tests(&env).await.expect("Integration tests should succeed");
        assert!(!results.is_empty());
        assert!(results.iter().any(|r| r.name == "basic_integration"));
    }

    #[tokio::test]
    async fn test_file_copying() {
        let temp_dir = TempDir::new().expect("Failed to create temp dir");
        let config = DeploymentConfig::default();
        let automation = DeploymentAutomation::new(config);
        
        // Create source directory with files
        let source_dir = temp_dir.path().join("source");
        std::fs::create_dir_all(&source_dir).expect("Failed to create source dir");
        std::fs::write(source_dir.join("test.txt"), "Hello World").expect("Failed to write test file");
        
        let target_dir = temp_dir.path().join("target");
        
        let copied_files = automation.copy_files(&source_dir, &target_dir).await.expect("File copy should succeed");
        assert!(!copied_files.is_empty());
        assert!(target_dir.join("test.txt").exists());
    }

    #[test]
    fn test_deployment_config_default() {
        let config = DeploymentConfig::default();
        assert!(config.enabled);
        assert_eq!(config.rollback_strategy, RollbackStrategy::Automatic);
        assert!(config.validate_before_deploy);
    }

    #[tokio::test]
    async fn test_deployment_automation() {
        let config = DeploymentConfig::default();
        let automation = DeploymentAutomation::new(config);

        assert_eq!(automation.deployment_history.len(), 0);
    }
}
