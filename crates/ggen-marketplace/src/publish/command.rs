//! T016: CLI Publish Command Implementation
//!
//! Implements the `ggen marketplace publish` command for publishing
//! packages to crates.io.
//!
//! ## Usage
//!
//! ```bash
//! ggen marketplace publish --package my-crate --target crates.io
//! ggen marketplace publish -p my-crate -t crates.io --dry-run
//! ```
//!
//! ## Features
//!
//! - Manifest validation before publish
//! - FMEA requirement enforcement
//! - Dry-run mode for testing
//! - Progress display
//! - Error handling with suggestions

use crate::error::{Error, Result};
use crate::publish::crates_client::{
    CratesClient, CratesClientConfig, CratesClientTrait, PublishResult,
};
use crate::publish::format::{ManifestValidator, ValidationResult};
use crate::publish::manifest::{CargoManifest, GpackManifest, ManifestConverter};
use std::path::{Path, PathBuf};
use tracing::{debug, error, info};

/// Publish command configuration
#[derive(Debug, Clone)]
pub struct PublishConfig {
    /// Package directory (containing gpack.yaml)
    pub package_path: PathBuf,
    /// Target registry (e.g., "crates.io")
    pub target: String,
    /// Whether to perform a dry run (validate only)
    pub dry_run: bool,
    /// Whether to require FMEA documentation
    pub require_fmea: bool,
    /// Allow publish even with warnings
    pub allow_warnings: bool,
    /// Custom registry URL (for testing)
    pub registry_url: Option<String>,
}

impl Default for PublishConfig {
    fn default() -> Self {
        Self {
            package_path: PathBuf::from("."),
            target: "crates.io".to_string(),
            dry_run: false,
            require_fmea: false,
            allow_warnings: false,
            registry_url: None,
        }
    }
}

impl PublishConfig {
    /// Create a new publish config for a package path
    pub fn new(package_path: impl Into<PathBuf>) -> Self {
        Self {
            package_path: package_path.into(),
            ..Default::default()
        }
    }

    /// Set the target registry
    pub fn with_target(mut self, target: impl Into<String>) -> Self {
        self.target = target.into();
        self
    }

    /// Enable dry-run mode
    pub fn with_dry_run(mut self, dry_run: bool) -> Self {
        self.dry_run = dry_run;
        self
    }

    /// Require FMEA documentation
    pub fn with_fmea_required(mut self, required: bool) -> Self {
        self.require_fmea = required;
        self
    }

    /// Allow warnings
    pub fn with_allow_warnings(mut self, allow: bool) -> Self {
        self.allow_warnings = allow;
        self
    }

    /// Set custom registry URL
    pub fn with_registry_url(mut self, url: impl Into<String>) -> Self {
        self.registry_url = Some(url.into());
        self
    }
}

/// Result of the publish command
#[derive(Debug, Clone)]
pub struct PublishCommandResult {
    /// Whether the command succeeded
    pub success: bool,
    /// Published crate name (if successful)
    pub crate_name: Option<String>,
    /// Published version (if successful)
    pub version: Option<String>,
    /// URL to the published crate (if successful)
    pub crate_url: Option<String>,
    /// Validation warnings
    pub warnings: Vec<String>,
    /// Error message (if failed)
    pub error: Option<String>,
    /// Whether this was a dry run
    pub dry_run: bool,
}

impl PublishCommandResult {
    /// Create a success result
    pub fn success(
        crate_name: String, version: String, crate_url: Option<String>, warnings: Vec<String>,
        dry_run: bool,
    ) -> Self {
        Self {
            success: true,
            crate_name: Some(crate_name),
            version: Some(version),
            crate_url,
            warnings,
            error: None,
            dry_run,
        }
    }

    /// Create a failure result
    pub fn failure(error: String, warnings: Vec<String>, dry_run: bool) -> Self {
        Self {
            success: false,
            crate_name: None,
            version: None,
            crate_url: None,
            warnings,
            error: Some(error),
            dry_run,
        }
    }

    /// Display the result to the user
    pub fn display(&self) {
        if self.dry_run {
            println!("=== DRY RUN ===");
        }

        if self.success {
            if let (Some(name), Some(version)) = (&self.crate_name, &self.version) {
                if self.dry_run {
                    println!("Would publish {} v{}", name, version);
                } else {
                    println!("Successfully published {} v{}", name, version);
                }
            }

            if let Some(url) = &self.crate_url {
                println!("View at: {}", url);
            }
        } else if let Some(err) = &self.error {
            eprintln!("Error: {}", err);
        }

        if !self.warnings.is_empty() {
            println!("\nWarnings:");
            for warning in &self.warnings {
                println!("  - {}", warning);
            }
        }
    }
}

/// Execute the publish command
pub async fn execute_publish(config: &PublishConfig) -> Result<PublishCommandResult> {
    info!(
        package_path = %config.package_path.display(),
        target = %config.target,
        dry_run = config.dry_run,
        "Starting publish command"
    );

    // Step 1: Load and parse the manifest
    let manifest_path = find_manifest_file(&config.package_path)?;
    debug!(manifest_path = %manifest_path.display(), "Found manifest file");

    let converter = ManifestConverter::new();
    let gpack_manifest = converter.load_yaml_file(&manifest_path)?;

    // Step 2: Validate the manifest
    let validator = if config.require_fmea {
        ManifestValidator::new().with_fmea_required()
    } else {
        ManifestValidator::new()
    };

    let validation = validator.validate(&gpack_manifest);

    // Collect warnings
    let warnings: Vec<String> = validation
        .warnings_only()
        .iter()
        .map(|w| format!("{}: {}", w.field, w.message))
        .collect();

    // Check for validation errors
    if !validation.is_valid {
        let errors: Vec<String> = validation
            .errors_only()
            .iter()
            .map(|e| format!("{}: {}", e.field, e.message))
            .collect();

        let error_msg = format!("Validation failed:\n{}", errors.join("\n"));
        error!("{}", error_msg);

        return Ok(PublishCommandResult::failure(error_msg, warnings, config.dry_run));
    }

    // Check for warnings if not allowed
    if !config.allow_warnings && !warnings.is_empty() {
        let warning_msg = format!(
            "Publish blocked due to warnings (use --allow-warnings to override):\n{}",
            warnings.join("\n")
        );
        return Ok(PublishCommandResult::failure(warning_msg, warnings, config.dry_run));
    }

    // Step 3: Convert to Cargo.toml format
    let cargo_manifest = converter.yaml_to_toml(&gpack_manifest)?;

    // Step 4: Create tarball (placeholder - in real implementation, this would package the crate)
    let tarball = create_tarball(&config.package_path, &cargo_manifest)?;

    // Step 5: If dry-run, stop here
    if config.dry_run {
        info!("Dry run complete - skipping actual publish");
        return Ok(PublishCommandResult::success(
            gpack_manifest.package.name.clone(),
            gpack_manifest.package.version.clone(),
            Some(format!(
                "https://crates.io/crates/{}/{}",
                gpack_manifest.package.name, gpack_manifest.package.version
            )),
            warnings,
            true,
        ));
    }

    // Step 6: Publish to registry
    let client_config = if let Some(ref url) = config.registry_url {
        CratesClientConfig::for_testing(url)
    } else {
        CratesClientConfig::default()
    };

    let client = CratesClient::new(client_config)?;
    let publish_result = client.publish(&cargo_manifest, &tarball).await?;

    if publish_result.success {
        info!(
            crate_name = %publish_result.crate_name,
            version = %publish_result.version,
            "Publish successful"
        );

        Ok(PublishCommandResult::success(
            publish_result.crate_name,
            publish_result.version,
            publish_result.crate_url,
            warnings,
            false,
        ))
    } else {
        let error_msg = publish_result
            .error_message
            .unwrap_or_else(|| "Unknown error during publish".to_string());

        error!("{}", error_msg);

        Ok(PublishCommandResult::failure(error_msg, warnings, false))
    }
}

/// Find the manifest file (gpack.yaml or gpack.toml) in the given directory
fn find_manifest_file(dir: &Path) -> Result<PathBuf> {
    let yaml_path = dir.join("gpack.yaml");
    if yaml_path.exists() {
        return Ok(yaml_path);
    }

    let yml_path = dir.join("gpack.yml");
    if yml_path.exists() {
        return Ok(yml_path);
    }

    Err(Error::ConfigError(format!(
        "No manifest file found in {}. Expected gpack.yaml or gpack.yml",
        dir.display()
    )))
}

/// Create a tarball of the package (placeholder implementation)
fn create_tarball(_dir: &Path, manifest: &CargoManifest) -> Result<Vec<u8>> {
    // In a real implementation, this would:
    // 1. Read all source files
    // 2. Apply include/exclude filters from manifest
    // 3. Create a compressed tarball
    // 4. Calculate and verify checksum

    // For now, just create a placeholder with the manifest
    let manifest_json = serde_json::to_vec(manifest).map_err(|e| {
        Error::ConfigError(format!("Failed to serialize manifest: {e}"))
    })?;

    Ok(manifest_json)
}

// ==================== TESTS ====================

#[cfg(test)]
mod tests {
    use super::*;
    use crate::publish::manifest::{GpackManifest, GpackPackage};
    use indexmap::IndexMap;
    use tempfile::TempDir;

    fn create_test_manifest() -> GpackManifest {
        GpackManifest {
            package: GpackPackage {
                name: "test-crate".to_string(),
                version: "1.0.0".to_string(),
                edition: "2021".to_string(),
                authors: vec!["Test <test@example.com>".to_string()],
                description: Some("A test crate".to_string()),
                license: Some("MIT".to_string()),
                license_file: None,
                repository: None,
                homepage: None,
                documentation: None,
                readme: None,
                keywords: vec![],
                categories: vec![],
                exclude: vec![],
                include: vec![],
                publish: true,
                fmea_documented: None,
            },
            dependencies: IndexMap::new(),
            dev_dependencies: IndexMap::new(),
            build_dependencies: IndexMap::new(),
            features: IndexMap::new(),
            target: IndexMap::new(),
            workspace: None,
        }
    }

    fn create_temp_package() -> (TempDir, PathBuf) {
        let temp_dir = TempDir::new().unwrap();
        let manifest_path = temp_dir.path().join("gpack.yaml");

        let manifest = create_test_manifest();
        let converter = ManifestConverter::new();
        converter.write_yaml_file(&manifest, &manifest_path).unwrap();

        (temp_dir, manifest_path)
    }

    #[test]
    fn test_publish_config_default() {
        let config = PublishConfig::default();
        assert_eq!(config.target, "crates.io");
        assert!(!config.dry_run);
        assert!(!config.require_fmea);
    }

    #[test]
    fn test_publish_config_builder() {
        let config = PublishConfig::new("/my/package")
            .with_target("custom-registry")
            .with_dry_run(true)
            .with_fmea_required(true)
            .with_allow_warnings(true);

        assert_eq!(config.package_path, PathBuf::from("/my/package"));
        assert_eq!(config.target, "custom-registry");
        assert!(config.dry_run);
        assert!(config.require_fmea);
        assert!(config.allow_warnings);
    }

    #[test]
    fn test_find_manifest_yaml() {
        let (temp_dir, _) = create_temp_package();
        let result = find_manifest_file(temp_dir.path());
        assert!(result.is_ok());
        assert!(result.unwrap().ends_with("gpack.yaml"));
    }

    #[test]
    fn test_find_manifest_not_found() {
        let temp_dir = TempDir::new().unwrap();
        let result = find_manifest_file(temp_dir.path());
        assert!(result.is_err());
    }

    #[test]
    fn test_publish_command_result_success() {
        let result = PublishCommandResult::success(
            "my-crate".to_string(),
            "1.0.0".to_string(),
            Some("https://crates.io/crates/my-crate".to_string()),
            vec![],
            false,
        );

        assert!(result.success);
        assert_eq!(result.crate_name, Some("my-crate".to_string()));
        assert_eq!(result.version, Some("1.0.0".to_string()));
        assert!(!result.dry_run);
    }

    #[test]
    fn test_publish_command_result_failure() {
        let result = PublishCommandResult::failure(
            "Validation failed".to_string(),
            vec!["Warning 1".to_string()],
            true,
        );

        assert!(!result.success);
        assert!(result.crate_name.is_none());
        assert_eq!(result.error, Some("Validation failed".to_string()));
        assert!(result.dry_run);
    }

    #[tokio::test]
    async fn test_execute_publish_dry_run() {
        let (temp_dir, _) = create_temp_package();

        let config = PublishConfig::new(temp_dir.path())
            .with_dry_run(true)
            .with_allow_warnings(true);

        let result = execute_publish(&config).await.unwrap();

        assert!(result.success);
        assert!(result.dry_run);
        assert_eq!(result.crate_name, Some("test-crate".to_string()));
        assert_eq!(result.version, Some("1.0.0".to_string()));
    }

    #[tokio::test]
    async fn test_execute_publish_validation_failure() {
        let temp_dir = TempDir::new().unwrap();
        let manifest_path = temp_dir.path().join("gpack.yaml");

        // Create an invalid manifest (missing required fields)
        let manifest = GpackManifest {
            package: GpackPackage {
                name: "".to_string(), // Invalid: empty name
                version: "1.0.0".to_string(),
                edition: "2021".to_string(),
                authors: vec![],
                description: None, // Missing required
                license: None,     // Missing required
                license_file: None,
                repository: None,
                homepage: None,
                documentation: None,
                readme: None,
                keywords: vec![],
                categories: vec![],
                exclude: vec![],
                include: vec![],
                publish: true,
                fmea_documented: None,
            },
            dependencies: IndexMap::new(),
            dev_dependencies: IndexMap::new(),
            build_dependencies: IndexMap::new(),
            features: IndexMap::new(),
            target: IndexMap::new(),
            workspace: None,
        };

        let converter = ManifestConverter::new();
        converter.write_yaml_file(&manifest, &manifest_path).unwrap();

        let config = PublishConfig::new(temp_dir.path()).with_dry_run(true);

        let result = execute_publish(&config).await.unwrap();

        assert!(!result.success);
        assert!(result.error.is_some());
        assert!(result.error.unwrap().contains("Validation failed"));
    }

    #[tokio::test]
    async fn test_execute_publish_fmea_required() {
        let (temp_dir, _) = create_temp_package();

        let config = PublishConfig::new(temp_dir.path())
            .with_dry_run(true)
            .with_fmea_required(true);

        let result = execute_publish(&config).await.unwrap();

        // Should fail because FMEA is required but not documented
        assert!(!result.success);
        assert!(result.error.is_some());
        assert!(result.error.unwrap().contains("FMEA"));
    }

    #[tokio::test]
    async fn test_execute_publish_missing_manifest() {
        let temp_dir = TempDir::new().unwrap();

        let config = PublishConfig::new(temp_dir.path()).with_dry_run(true);

        let result = execute_publish(&config).await;

        assert!(result.is_err());
    }

    #[test]
    fn test_create_tarball() {
        let temp_dir = TempDir::new().unwrap();
        let manifest = create_test_manifest();
        let converter = ManifestConverter::new();
        let cargo_manifest = converter.yaml_to_toml(&manifest).unwrap();

        let tarball = create_tarball(temp_dir.path(), &cargo_manifest);
        assert!(tarball.is_ok());
        assert!(!tarball.unwrap().is_empty());
    }
}
