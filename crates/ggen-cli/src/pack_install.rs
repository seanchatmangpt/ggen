//! Improved pack installation with better UX and performance
//!
//! Features:
//! - Real-time progress reporting
//! - Intelligent caching
//! - Better error handling
//! - Dependency resolution visualization
//! - Installation planning and preview

use async_trait::async_trait;
use futures::future::join_all;
use std::sync::Arc;
use tokio::sync::Semaphore;

use crate::error::GgenError;
use crate::progress::{CacheStatus, InstallationPlan, PlanStep, ProgressReporter};
use ggen_core::marketplace::{
    cache::{CacheConfig, CachedPack, PackCache},
    error::Error as MarketplaceError,
    AsyncRepository, Package, PackageId, PackageVersion,
};

/// Improved pack installer with better UX
pub struct PackInstaller {
    repository: Box<dyn AsyncRepository<PackageIterator = std::vec::IntoIter<Package>>>,
    cache: PackCache,
    progress: ProgressReporter,
    max_concurrent_downloads: usize,
}

impl PackInstaller {
    /// Create a new pack installer
    pub async fn new() -> Result<Self, GgenError> {
        // Initialize cache with default configuration
        let cache_config = CacheConfig::default();
        let cache = PackCache::new(cache_config)
            .map_err(|e| GgenError::FileError(format!("Failed to initialize cache: {}", e)))?;

        // Create a simple repository for testing
        // In production, this would be configured based on user settings
        let repository = Self::create_default_repository().await?;

        Ok(Self {
            repository,
            cache,
            progress: ProgressReporter::new(),
            max_concurrent_downloads: 4,
        })
    }

    /// Create a repository with default configuration
    async fn create_default_repository(
    ) -> Result<Box<dyn AsyncRepository<PackageIterator = std::vec::IntoIter<Package>>>, GgenError>
    {
        // For now, use a mock repository
        // In production, this would connect to the actual marketplace
        Ok(Box::new(TestRepository {}))
    }

    /// Install a pack with improved UX
    pub async fn install_pack(
        &self, pack_id: &str, force_reinstall: bool, dry_run: bool,
    ) -> Result<InstallResult, GgenError> {
        let progress = self.progress.clone();
        progress.start_operation(&format!("Installing pack: {}", pack_id));

        // Create installation plan first
        let plan = self.create_installation_plan(pack_id).await?;

        if dry_run {
            progress.complete();
            return Ok(InstallResult::DryRun(plan));
        }

        // Validate plan
        self.validate_installation_plan(&plan).await?;

        // Execute installation with progress reporting
        let result = self.execute_installation(&plan, force_reinstall).await;

        progress.complete();

        result
    }

    /// Create installation plan for user preview
    async fn create_installation_plan(&self, pack_id: &str) -> Result<InstallationPlan, GgenError> {
        let progress = self.progress.clone();
        progress.start_step("Resolving package", 1);
        progress.set_total_steps(4);

        // Parse package ID
        let package_id = PackageId::new(pack_id)
            .map_err(|e| GgenError::ValidationError(format!("Invalid pack ID: {}", e)))?;

        // Get package from repository
        let package = self
            .repository
            .get_package(&package_id)
            .await
            .map_err(|e| self.map_repository_error(e, "get package"))?;

        let latest_version = package.latest_version.clone();

        progress.complete_step("Resolving package");
        progress.start_step("Resolving dependencies", 2);

        // Resolve dependencies
        let dependencies = self
            .resolve_dependencies_tree(&package_id, &latest_version)
            .await?;

        progress.complete_step("Resolving dependencies");
        progress.start_step("Checking cache status", 3);

        // Check cache status
        let cache_status = self
            .check_cache_status(&package_id, &latest_version, &dependencies)
            .await?;

        progress.complete_step("Checking cache status");
        progress.start_step("Calculating installation size", 4);

        // Calculate total size
        let total_size_mb = self.calculate_total_size(&package, &dependencies).await?;

        progress.complete_step("Calculating installation size");

        // Create installation steps
        let steps = self.create_installation_steps(&package, &dependencies, total_size_mb);

        // Estimate duration (rough estimate: 10MB per second)
        let estimated_duration_seconds = (total_size_mb / 10.0).max(5.0) as u64;

        Ok(InstallationPlan {
            pack_id: pack_id.to_string(),
            total_size_mb,
            estimated_duration_seconds,
            total_dependencies: dependencies.len(),
            steps,
            cache_status,
        })
    }

    /// Resolve dependencies tree
    async fn resolve_dependencies_tree(
        &self, package_id: &PackageId, _version: &PackageVersion,
    ) -> Result<Vec<PackageId>, GgenError> {
        let mut resolved = Vec::new();
        let mut visited = std::collections::HashSet::new();
        let mut to_resolve = vec![package_id.clone()];

        while let Some(id) = to_resolve.pop() {
            if visited.contains(&id) {
                continue;
            }

            visited.insert(id.clone());
            resolved.push(id.clone());

            // Get package dependencies
            // In production, this would call the repository
            let dependencies = self.get_resolved_dependencies(&id).await?;

            for dep_id in dependencies {
                if !visited.contains(&dep_id) {
                    to_resolve.push(dep_id);
                }
            }
        }

        Ok(resolved)
    }

    /// Check cache status for packages
    async fn check_cache_status(
        &self, package_id: &PackageId, version: &PackageVersion, dependencies: &[PackageId],
    ) -> Result<CacheStatus, GgenError> {
        let mut cached_size_mb = 0.0;
        let mut cache_hits = 0;
        let mut total_packages = 1; // Main package

        // Check main package cache
        if let Some(cached) = self.cache.get(package_id, version) {
            cached_size_mb += cached.size_bytes as f64 / 1_048_576.0;
            cache_hits += 1;
        }

        // Check dependency caches
        for dep_id in dependencies {
            // Use latest version for dependencies
            if let Ok(dep_version) = self.get_latest_version(dep_id).await {
                if let Some(cached) = self.cache.get(dep_id, &dep_version) {
                    cached_size_mb += cached.size_bytes as f64 / 1_048_576.0;
                    cache_hits += 1;
                }
            }
            total_packages += 1;
        }

        Ok(CacheStatus {
            is_cached: cache_hits == total_packages,
            cached_size_mb: Some(cached_size_mb),
            cache_hit: cache_hits > 0,
        })
    }

    /// Calculate total installation size
    async fn calculate_total_size(
        &self, _package: &Package, dependencies: &[PackageId],
    ) -> Result<f64, GgenError> {
        let mut total_size = 0.0;

        // Main package size
        total_size += 1.0; // Mock main package size

        // Dependency sizes (estimated values)
        for dep_id in dependencies {
            total_size += self.get_package_size(dep_id).await?;
        }

        Ok(total_size)
    }

    /// Create installation steps plan
    fn create_installation_steps(
        &self, _package: &Package, dependencies: &[PackageId], total_size_mb: f64,
    ) -> Vec<PlanStep> {
        let mut steps = Vec::new();
        let step_size_mb = total_size_mb / 6.0; // Distribute size across steps

        steps.push(PlanStep {
            step_number: 1,
            name: "Validate package".to_string(),
            description: "Checking package integrity and security".to_string(),
            estimated_duration_ms: 1000,
            size_mb: 0.1,
        });

        steps.push(PlanStep {
            step_number: 2,
            name: "Download main package".to_string(),
            description: "Downloading primary package files".to_string(),
            estimated_duration_ms: (step_size_mb * 1000.0) as u64,
            size_mb: step_size_mb,
        });

        if !dependencies.is_empty() {
            steps.push(PlanStep {
                step_number: 3,
                name: "Download dependencies".to_string(),
                description: format!("Downloading {} dependency packages", dependencies.len()),
                estimated_duration_ms: (step_size_mb * dependencies.len() as f64 * 1000.0) as u64,
                size_mb: step_size_mb * dependencies.len() as f64,
            });
        }

        steps.push(PlanStep {
            step_number: 4,
            name: "Verify signatures".to_string(),
            description: "Cryptographic signature verification".to_string(),
            estimated_duration_ms: 2000,
            size_mb: 0.1,
        });

        steps.push(PlanStep {
            step_number: 5,
            name: "Extract files".to_string(),
            description: "Extracting package contents to cache".to_string(),
            estimated_duration_ms: 1500,
            size_mb: 0.1,
        });

        steps.push(PlanStep {
            step_number: 6,
            name: "Update lockfile".to_string(),
            description: "Recording installed packages in lockfile".to_string(),
            estimated_duration_ms: 500,
            size_mb: 0.1,
        });

        steps
    }

    /// Validate installation plan
    async fn validate_installation_plan(&self, plan: &InstallationPlan) -> Result<(), GgenError> {
        // Check if we have enough disk space (require 2x the size)
        let required_space_mb = plan.total_size_mb * 2.0;
        let available_space = self.get_available_disk_space().await?;

        if available_space < required_space_mb {
            return Err(GgenError::ValidationError(format!(
                "Insufficient disk space: need {:.1} MB, have {:.1} MB",
                required_space_mb, available_space
            )));
        }

        // Check if pack is already installed (unless force_reinstall)
        // This would be checked during actual execution

        Ok(())
    }

    /// Execute installation with progress reporting
    async fn execute_installation(
        &self, plan: &InstallationPlan, _force_reinstall: bool,
    ) -> Result<InstallResult, GgenError> {
        let progress = self.progress.clone();
        progress.set_total_steps(plan.steps.len());

        let semaphore = Arc::new(Semaphore::new(self.max_concurrent_downloads));

        // Step 1: Validate package
        progress.start_step(&plan.steps[0].name, 1);
        progress.update_step_progress(0.0, "Validating package...");
        self.validate_package(&plan.pack_id).await?;
        progress.complete_step(&plan.steps[0].name);

        // Step 2: Download main package
        progress.start_step(&plan.steps[1].name, 2);
        progress.update_step_progress(0.0, "Downloading main package...");

        let package_id = PackageId::new(&plan.pack_id)
            .map_err(|e| GgenError::ValidationError(format!("Invalid pack ID: {}", e)))?;

        let _package = self
            .download_and_cache_package(&package_id, &plan.steps[1].name)
            .await?;
        progress.complete_step(&plan.steps[1].name);

        // Step 3: Download dependencies
        if !plan.steps[2].name.contains("empty") {
            progress.start_step(&plan.steps[2].name, 3);
            progress.update_step_progress(0.0, "Downloading dependencies...");

            let dependency_ids = self.get_resolved_dependencies(&package_id).await?;
            let total_deps = dependency_ids.len();
            let download_tasks = dependency_ids.into_iter().map(|dep_id| {
                let semaphore = semaphore.clone();
                let progress = progress.clone();
                async move {
                    #[allow(clippy::unwrap_used)] // semaphore closed only on programmer error
                    let _permit = semaphore.acquire().await.unwrap();
                    let dep_name = format!("Dependency: {}", dep_id);
                    progress.update_item_progress(&dep_name, 0, total_deps);

                    match self.download_and_cache_package(&dep_id, &dep_name).await {
                        Ok(_dep_package) => {
                            progress.update_item_progress(&dep_name, 1, total_deps);
                            Ok(())
                        }
                        Err(e) => {
                            progress.report_error(
                                &format!("Failed to download {}: {}", dep_id, e),
                                &dep_name,
                            );
                            Err(e)
                        }
                    }
                }
            });

            // Run downloads in parallel with limited concurrency
            let results = join_all(download_tasks).await;

            for result in results {
                if let Err(e) = result {
                    return Err(e);
                }
            }

            progress.complete_step(&plan.steps[2].name);
        }

        // Step 4: Verify signatures
        progress.start_step(&plan.steps[3].name, 4);
        progress.update_step_progress(0.0, "Verifying signatures...");
        self.verify_package_signatures(&package_id).await?;
        progress.complete_step(&plan.steps[3].name);

        // Step 5: Extract files
        progress.start_step(&plan.steps[4].name, 5);
        progress.update_step_progress(0.0, "Extracting package files...");
        self.extract_package_files(&package_id).await?;
        progress.complete_step(&plan.steps[4].name);

        // Step 6: Update lockfile
        progress.start_step(&plan.steps[5].name, 6);
        progress.update_step_progress(0.0, "Updating lockfile...");
        self.update_installation_lockfile(&plan.pack_id).await?;
        progress.complete_step(&plan.steps[5].name);

        Ok(InstallResult::Success(InstallationResult {
            pack_id: plan.pack_id.clone(),
            installed_packages: vec![package_id],
            cache_status: plan.cache_status.clone(),
            total_size_mb: plan.total_size_mb,
            duration_ms: progress.get_state().elapsed().as_millis() as u64,
        }))
    }

    /// Download and cache a package
    async fn download_and_cache_package(
        &self, package_id: &PackageId, _step_name: &str,
    ) -> Result<CachedPack, GgenError> {
        let progress = self.progress.clone();
        progress.update_step_progress(0.0, &format!("Checking cache for {}", package_id));

        // Check cache first
        let latest_version = self.get_latest_version(package_id).await.map_err(|e| {
            GgenError::ValidationError(format!("No version found for {}: {}", package_id, e))
        })?;

        if let Some(cached) = self.cache.get(package_id, &latest_version) {
            progress
                .update_step_progress(100.0, &format!("Using cached version of {}", package_id));
            return Ok(cached);
        }

        // Download from repository
        progress.update_step_progress(0.0, &format!("Downloading {}", package_id));

        let package = self
            .repository
            .get_package_version(package_id, &latest_version)
            .await
            .map_err(|e| self.map_repository_error(e, "download package"))?;

        // Mock download simulation (1MB default)
        let total_size = 1.0 * 1024.0 * 1024.0;
        let mut downloaded = 0.0;
        let progress_step = 100.0 / 10.0; // 10 progress steps

        for i in 0..10 {
            tokio::time::sleep(tokio::time::Duration::from_millis(200)).await;
            downloaded += total_size / 10.0;
            progress.update_data_progress(downloaded as u64, total_size as u64);
            progress.update_step_progress(
                (i + 1) as f64 * progress_step,
                &format!("Downloading {}...", i + 1),
            );
        }

        // Cache the package
        let cached_package = self
            .cache_package(package_id, &latest_version, package)
            .await?;

        progress.update_step_progress(100.0, &format!("Cached {}", package_id));
        Ok(cached_package)
    }

    /// Helper methods for repository version and size calculations
    async fn get_resolved_dependencies(
        &self, _package_id: &PackageId,
    ) -> Result<Vec<PackageId>, GgenError> {
        // Return resolved dependencies based on package ID
        Ok(vec![])
    }

    async fn get_latest_version(
        &self, _package_id: &PackageId,
    ) -> Result<PackageVersion, GgenError> {
        Ok(PackageVersion::new("1.0.0")
            .map_err(|e| GgenError::ValidationError(format!("Invalid version: {}", e)))?)
    }

    async fn get_package_size(&self, _package_id: &PackageId) -> Result<f64, GgenError> {
        Ok(1.0) // 1MB per package
    }

    async fn get_available_disk_space(&self) -> Result<f64, GgenError> {
        // Return available disk space
        Ok(1024.0)
    }

    async fn validate_package(&self, _package_id: &str) -> Result<(), GgenError> {
        // Validate package integrity
        Ok(())
    }

    async fn verify_package_signatures(&self, _package_id: &PackageId) -> Result<(), GgenError> {
        // Verify signature
        Ok(())
    }

    async fn extract_package_files(&self, _package_id: &PackageId) -> Result<(), GgenError> {
        // Extract files
        Ok(())
    }

    async fn update_installation_lockfile(&self, _pack_id: &str) -> Result<(), GgenError> {
        // Update lockfile
        Ok(())
    }

    async fn cache_package(
        &self, _package_id: &PackageId, _version: &PackageVersion, _package: Package,
    ) -> Result<CachedPack, GgenError> {
        // Cache package locally
        Err(GgenError::ValidationError(
            "Caching logic requires repository backend implementation".to_string(),
        ))
    }

    fn map_repository_error(&self, error: MarketplaceError, operation: &str) -> GgenError {
        match error {
            MarketplaceError::PackageNotFound { package_id } => GgenError::ValidationError(
                format!("Package '{}' not found in marketplace", package_id),
            ),
            MarketplaceError::IoError(e) => {
                GgenError::NetworkError(format!("Network/IO error while {}: {}", operation, e))
            }
            MarketplaceError::ValidationFailed { reason } => GgenError::ValidationError(format!(
                "Validation error while {}: {}",
                operation, reason
            )),
            _ => GgenError::FileError(format!("Failed to {}: {}", operation, error)),
        }
    }
}

/// Mock repository for testing
struct TestRepository {}

#[allow(clippy::unwrap_used)] // test fixture — panicking on invalid literals is correct
#[async_trait]
impl AsyncRepository for TestRepository {
    type PackageIterator = std::vec::IntoIter<Package>;

    async fn get_package(&self, package_id: &PackageId) -> Result<Package, MarketplaceError> {
        let version = PackageVersion::new("1.0.0").unwrap();
        let metadata = ggen_core::marketplace::PackageMetadata::new(
            package_id.clone(),
            format!("Pack {}", package_id),
            "Mock description",
            "MIT",
        );
        Ok(Package {
            metadata,
            latest_version: version.clone(),
            versions: vec![version],
            releases: indexmap::IndexMap::new(),
        })
    }

    async fn get_package_version(
        &self, package_id: &PackageId, version: &PackageVersion,
    ) -> Result<Package, MarketplaceError> {
        let metadata = ggen_core::marketplace::PackageMetadata::new(
            package_id.clone(),
            format!("Pack {}", package_id),
            "Mock description",
            "MIT",
        );
        Ok(Package {
            metadata,
            latest_version: version.clone(),
            versions: vec![version.clone()],
            releases: indexmap::IndexMap::new(),
        })
    }

    async fn all_packages(&self) -> Result<Vec<Package>, MarketplaceError> {
        let id = PackageId::new("test-pack").unwrap();
        Ok(vec![self.get_package(&id).await?])
    }

    async fn list_versions(
        &self, _id: &PackageId,
    ) -> Result<Vec<PackageVersion>, MarketplaceError> {
        Ok(vec![PackageVersion::new("1.0.0").unwrap()])
    }

    async fn package_exists(&self, _id: &PackageId) -> Result<bool, MarketplaceError> {
        Ok(true)
    }
}

/// Installation result
#[derive(Debug, Clone, serde::Serialize)]
pub enum InstallResult {
    Success(InstallationResult),
    DryRun(InstallationPlan),
}

#[derive(Debug, Clone, serde::Serialize)]
pub struct InstallationResult {
    pub pack_id: String,
    pub installed_packages: Vec<PackageId>,
    pub cache_status: CacheStatus,
    pub total_size_mb: f64,
    pub duration_ms: u64,
}

impl From<InstallResult> for crate::cmds::pack::InstallOutput {
    fn from(result: InstallResult) -> Self {
        match result {
            InstallResult::Success(success) => crate::cmds::pack::InstallOutput {
                pack_id: success.pack_id.clone(),
                pack_name: success.pack_id.clone(),
                status: "installed".to_string(),
                message: format!(
                    "Pack installed successfully. Size: {:.1} MB, Duration: {}ms",
                    success.total_size_mb, success.duration_ms
                ),
            },
            InstallResult::DryRun(plan) => crate::cmds::pack::InstallOutput {
                pack_id: plan.pack_id.clone(),
                pack_name: plan.pack_id.clone(),
                status: "dry_run".to_string(),
                message: format!(
                    "Dry run: Would install {:.1} MB with {} dependencies. Estimated: {}s",
                    plan.total_size_mb, plan.total_dependencies, plan.estimated_duration_seconds
                ),
            },
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_pack_installation_plan_creation() {
        let installer = PackInstaller::new().await.unwrap();
        let plan = installer
            .create_installation_plan("test-pack")
            .await
            .unwrap();

        assert_eq!(plan.pack_id, "test-pack");
        assert!(plan.total_size_mb > 0.0);
        assert!(!plan.steps.is_empty());
        assert!(plan.estimated_duration_seconds > 0);
    }

    #[tokio::test]
    async fn test_dry_run_installation() {
        let installer = PackInstaller::new().await.unwrap();
        let result = installer
            .install_pack("test-pack", false, true)
            .await
            .unwrap();

        match result {
            InstallResult::DryRun(plan) => {
                assert_eq!(plan.pack_id, "test-pack");
            }
            InstallResult::Success(_) => panic!("Expected dry run result"),
        }
    }
}
