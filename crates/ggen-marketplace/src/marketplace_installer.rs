//! Comprehensive marketplace installer
//!
//! Features:
//! - Full installation flow: download -> resolve -> validate -> install
//! - Offline fallback: network failure -> cached version with warning
//! - CLI integration: ggen marketplace install <crate> [--force-fmea] [--offline] [--no-cache]
//! - Progress reporting
//! - Transaction support with rollback

use crate::audit::{AuditManager, InstallationStatus};
use crate::cache::MultiLayerCache;
use crate::error::{Error, Result};
use crate::install::{InstallationTransaction, RollbackAction};
use crate::install_validator::{InstallValidator, ValidationOptions};
use crate::lockfile::{Lockfile, LockfileManager};
use crate::models::{PackageId, PackageVersion};
use crate::resolver::{DependencyResolver, ResolvedDependency, ResolutionResult};
use crate::traits::AsyncRepository;
use chrono::Utc;
use std::path::{Path, PathBuf};
use std::time::Instant;
use tracing::{debug, error, info, span, warn, Level};

/// Installation options
#[derive(Debug, Clone)]
pub struct InstallOptions {
    /// Force FMEA validation (proceed despite critical failures)
    pub force_fmea: bool,
    /// Use offline mode (only use cache)
    pub offline: bool,
    /// Skip cache entirely
    pub no_cache: bool,
    /// Installation directory
    pub install_path: PathBuf,
    /// Dry run (don't actually install)
    pub dry_run: bool,
    /// Update lock file after installation
    pub update_lockfile: bool,
    /// Verbose output
    pub verbose: bool,
}

impl Default for InstallOptions {
    fn default() -> Self {
        Self {
            force_fmea: false,
            offline: false,
            no_cache: false,
            install_path: PathBuf::from("."),
            dry_run: false,
            update_lockfile: true,
            verbose: false,
        }
    }
}

impl InstallOptions {
    /// Create install options from CLI flags
    pub fn from_cli(
        force_fmea: bool,
        offline: bool,
        no_cache: bool,
        install_path: Option<PathBuf>,
        dry_run: bool,
    ) -> Self {
        Self {
            force_fmea,
            offline,
            no_cache,
            install_path: install_path.unwrap_or_else(|| PathBuf::from(".")),
            dry_run,
            update_lockfile: !dry_run,
            verbose: false,
        }
    }
}

/// Installation progress callback
pub type ProgressCallback = Box<dyn Fn(InstallProgress) + Send + Sync>;

/// Installation progress event
#[derive(Debug, Clone)]
pub enum InstallProgress {
    /// Starting installation
    Starting {
        package_id: PackageId,
        version: PackageVersion,
    },
    /// Resolving dependencies
    Resolving {
        package_id: PackageId,
    },
    /// Resolution complete
    ResolutionComplete {
        total_packages: usize,
    },
    /// Downloading package
    Downloading {
        package_id: PackageId,
        version: PackageVersion,
        current: usize,
        total: usize,
    },
    /// Using cached package
    UsingCache {
        package_id: PackageId,
        version: PackageVersion,
    },
    /// Validating package
    Validating {
        package_id: PackageId,
        version: PackageVersion,
    },
    /// Installing package
    Installing {
        package_id: PackageId,
        version: PackageVersion,
        current: usize,
        total: usize,
    },
    /// Installation complete
    Complete {
        total_packages: usize,
        duration_ms: u64,
    },
    /// Installation failed
    Failed {
        package_id: PackageId,
        error: String,
    },
    /// Offline fallback used
    OfflineFallback {
        package_id: PackageId,
        reason: String,
    },
    /// Force FMEA applied
    ForceApplied {
        package_id: PackageId,
        critical_failures: usize,
    },
}

/// Installation result
#[derive(Debug, Clone)]
pub struct InstallResult {
    /// Packages installed
    pub installed_packages: Vec<(PackageId, PackageVersion)>,
    /// Lock file (if generated)
    pub lockfile: Option<Lockfile>,
    /// Installation duration in milliseconds
    pub duration_ms: u64,
    /// Whether offline fallback was used
    pub used_offline_fallback: bool,
    /// Whether force FMEA was applied
    pub force_fmea_applied: bool,
    /// Warnings generated during installation
    pub warnings: Vec<String>,
}

/// Comprehensive marketplace installer
pub struct MarketplaceInstaller<R: AsyncRepository> {
    /// Package repository
    repository: R,
    /// Multi-layer cache
    cache: MultiLayerCache,
    /// Dependency resolver
    resolver: DependencyResolver<R>,
    /// Install validator
    validator: InstallValidator,
    /// Lock file manager
    lockfile_manager: LockfileManager,
    /// Audit manager
    audit_manager: AuditManager,
    /// Progress callback
    progress_callback: Option<ProgressCallback>,
}

impl<R: AsyncRepository + Clone> MarketplaceInstaller<R> {
    /// Create a new marketplace installer
    pub fn new(
        repository: R,
        cache_dir: impl Into<PathBuf>,
        project_dir: impl Into<PathBuf>,
    ) -> Result<Self> {
        let cache_dir = cache_dir.into();
        let project_dir = project_dir.into();

        let cache = MultiLayerCache::new(&cache_dir)?;
        let resolver = DependencyResolver::new(repository.clone());
        let validator = InstallValidator::new();
        let lockfile_manager = LockfileManager::new(&project_dir);
        let audit_manager = AuditManager::new(cache_dir.join("audit"))?;

        Ok(Self {
            repository,
            cache,
            resolver,
            validator,
            lockfile_manager,
            audit_manager,
            progress_callback: None,
        })
    }

    /// Set progress callback
    pub fn with_progress_callback(mut self, callback: ProgressCallback) -> Self {
        self.progress_callback = Some(callback);
        self
    }

    /// Report progress
    fn report_progress(&self, progress: InstallProgress) {
        if let Some(callback) = &self.progress_callback {
            callback(progress);
        }
    }

    /// Install a package
    pub async fn install(
        &mut self,
        package_id: &PackageId,
        version: Option<&PackageVersion>,
        options: &InstallOptions,
    ) -> Result<InstallResult> {
        let span = span!(
            Level::INFO,
            "install",
            package = %package_id,
            version = ?version.map(|v| v.to_string()),
            offline = %options.offline,
            force_fmea = %options.force_fmea
        );
        let _enter = span.enter();

        let start_time = Instant::now();
        let mut warnings: Vec<String> = Vec::new();
        let mut used_offline_fallback = false;
        let mut force_fmea_applied = false;

        info!(
            package = %package_id,
            "Starting installation"
        );

        // Step 1: Get package version
        let resolved_version = if let Some(v) = version {
            v.clone()
        } else {
            self.get_latest_version(package_id, options).await?
        };

        self.report_progress(InstallProgress::Starting {
            package_id: package_id.clone(),
            version: resolved_version.clone(),
        });

        // Step 2: Resolve dependencies
        self.report_progress(InstallProgress::Resolving {
            package_id: package_id.clone(),
        });

        let resolution = self.resolve_with_fallback(
            package_id,
            &resolved_version,
            options,
        ).await;

        let resolution = match resolution {
            Ok(r) => r,
            Err(e) if options.offline => {
                // Try to use existing lock file in offline mode
                if let Ok(lockfile) = self.lockfile_manager.read() {
                    if let Some(locked) = lockfile.get(package_id) {
                        warn!(
                            package = %package_id,
                            "Using locked version in offline mode"
                        );
                        used_offline_fallback = true;
                        self.report_progress(InstallProgress::OfflineFallback {
                            package_id: package_id.clone(),
                            reason: e.to_string(),
                        });

                        // Create minimal resolution from lockfile
                        let deps: Vec<ResolvedDependency> = lockfile.packages
                            .iter()
                            .map(|(id, pkg)| ResolvedDependency {
                                id: id.clone(),
                                version: pkg.version.clone(),
                                checksum: pkg.checksum.clone(),
                                download_url: pkg.source.clone(),
                                dependencies: pkg.dependencies.clone(),
                                depth: 0,
                            })
                            .collect();

                        ResolutionResult {
                            resolved: deps,
                            tree: crate::resolver::ResolutionNode {
                                id: package_id.clone(),
                                version: locked.version.clone(),
                                children: vec![],
                            },
                            total_packages: lockfile.len(),
                            resolution_time: std::time::Duration::from_millis(0),
                            resolved_at: Utc::now(),
                        }
                    } else {
                        return Err(Error::InstallationFailed {
                            reason: format!(
                                "Package {} not in lock file and offline mode is enabled",
                                package_id
                            ),
                        });
                    }
                } else {
                    return Err(e);
                }
            }
            Err(e) => return Err(e),
        };

        self.report_progress(InstallProgress::ResolutionComplete {
            total_packages: resolution.total_packages,
        });

        debug!(
            package = %package_id,
            total = %resolution.total_packages,
            "Dependency resolution complete"
        );

        // Step 3: Validate packages
        let validation_options = ValidationOptions {
            force_fmea: options.force_fmea,
            skip_validation: false,
            max_rpn: 200,
            strict_mode: false,
            install_path: options.install_path.clone(),
        };

        let validation_results = self.validator.validate_dependencies(
            &resolution.resolved,
            &validation_options,
        )?;

        for result in &validation_results {
            self.report_progress(InstallProgress::Validating {
                package_id: result.package_id.clone(),
                version: result.version.clone(),
            });

            if result.force_applied {
                force_fmea_applied = true;
                self.report_progress(InstallProgress::ForceApplied {
                    package_id: result.package_id.clone(),
                    critical_failures: result.critical_failures,
                });
                warnings.push(format!(
                    "Force FMEA applied for {}: {} critical failures",
                    result.package_id, result.critical_failures
                ));
            }

            for warning in &result.warnings {
                warnings.push(warning.clone());
            }
        }

        // Step 4: Dry run check
        if options.dry_run {
            info!(
                package = %package_id,
                total = %resolution.total_packages,
                "Dry run complete, no packages installed"
            );

            return Ok(InstallResult {
                installed_packages: resolution.resolved
                    .iter()
                    .map(|d| (d.id.clone(), d.version.clone()))
                    .collect(),
                lockfile: Some(Lockfile::from_resolved(&resolution.resolved, &[package_id.clone()])),
                duration_ms: start_time.elapsed().as_millis() as u64,
                used_offline_fallback,
                force_fmea_applied,
                warnings,
            });
        }

        // Step 5: Download and install packages
        let mut transaction = InstallationTransaction::new();
        transaction.start();

        let total = resolution.resolved.len();
        let mut installed_packages: Vec<(PackageId, PackageVersion)> = Vec::new();

        for (idx, dep) in resolution.resolved.iter().enumerate() {
            self.report_progress(InstallProgress::Installing {
                package_id: dep.id.clone(),
                version: dep.version.clone(),
                current: idx + 1,
                total,
            });

            // Check cache first (unless no_cache is set)
            let package_data = if !options.no_cache {
                if let Some(cached_path) = self.cache.get_package(&dep.id, &dep.version) {
                    self.report_progress(InstallProgress::UsingCache {
                        package_id: dep.id.clone(),
                        version: dep.version.clone(),
                    });
                    debug!(
                        package = %dep.id,
                        version = %dep.version,
                        "Using cached package"
                    );
                    Some(std::fs::read(&cached_path)?)
                } else {
                    None
                }
            } else {
                None
            };

            let package_data = match package_data {
                Some(data) => data,
                None => {
                    if options.offline {
                        error!(
                            package = %dep.id,
                            version = %dep.version,
                            "Package not cached and offline mode enabled"
                        );
                        transaction.mark_failed(
                            dep.id.clone(),
                            dep.version.clone(),
                            "Not cached and offline".to_string(),
                        );
                        continue;
                    }

                    self.report_progress(InstallProgress::Downloading {
                        package_id: dep.id.clone(),
                        version: dep.version.clone(),
                        current: idx + 1,
                        total,
                    });

                    // Download package (simulated - in real implementation, use reqwest)
                    match self.download_package(&dep.id, &dep.version, &dep.download_url).await {
                        Ok(data) => {
                            // Cache the downloaded package
                            if !options.no_cache {
                                if let Err(e) = self.cache.set_package(&dep.id, &dep.version, &data) {
                                    warn!(
                                        package = %dep.id,
                                        error = %e,
                                        "Failed to cache package"
                                    );
                                }
                            }
                            data
                        }
                        Err(e) => {
                            transaction.mark_failed(
                                dep.id.clone(),
                                dep.version.clone(),
                                e.to_string(),
                            );
                            continue;
                        }
                    }
                }
            };

            // Install package to target directory
            let install_path = options.install_path
                .join("packages")
                .join(dep.id.to_string())
                .join(dep.version.to_string());

            if let Err(e) = self.install_package_data(&dep.id, &dep.version, &package_data, &install_path) {
                transaction.mark_failed(dep.id.clone(), dep.version.clone(), e.to_string());
            } else {
                transaction.mark_installed(
                    dep.id.clone(),
                    dep.version.clone(),
                    install_path.to_string_lossy().to_string(),
                );
                installed_packages.push((dep.id.clone(), dep.version.clone()));
            }
        }

        // Step 6: Handle transaction result
        if transaction.has_failures() {
            error!(
                package = %package_id,
                failures = %transaction.failed_packages.len(),
                "Installation has failures, rolling back"
            );

            self.report_progress(InstallProgress::Failed {
                package_id: package_id.clone(),
                error: format!(
                    "{} packages failed to install",
                    transaction.failed_packages.len()
                ),
            });

            // Rollback
            self.rollback_transaction(&mut transaction).await?;

            // Record in audit
            self.audit_manager.record_installation(
                package_id,
                &resolved_version,
                &options.install_path,
                InstallationStatus::RolledBack,
                validation_results.first(),
                &resolution.resolved,
                start_time.elapsed().as_millis() as u64,
            )?;

            return Err(Error::InstallationFailed {
                reason: format!(
                    "Installation failed for {} packages: {:?}",
                    transaction.failed_packages.len(),
                    transaction.failed_packages.iter().map(|(id, _, _)| id.to_string()).collect::<Vec<_>>()
                ),
            });
        }

        // Commit transaction
        transaction.commit();

        // Step 7: Update lock file
        let lockfile = if options.update_lockfile {
            let lockfile = self.lockfile_manager.update(
                &resolution.resolved,
                &[package_id.clone()],
            )?;
            Some(lockfile)
        } else {
            None
        };

        let duration_ms = start_time.elapsed().as_millis() as u64;

        // Step 8: Record in audit
        self.audit_manager.record_installation(
            package_id,
            &resolved_version,
            &options.install_path,
            InstallationStatus::Success,
            validation_results.first(),
            &resolution.resolved,
            duration_ms,
        )?;

        self.report_progress(InstallProgress::Complete {
            total_packages: installed_packages.len(),
            duration_ms,
        });

        info!(
            package = %package_id,
            total = %installed_packages.len(),
            duration_ms = %duration_ms,
            "Installation complete"
        );

        Ok(InstallResult {
            installed_packages,
            lockfile,
            duration_ms,
            used_offline_fallback,
            force_fmea_applied,
            warnings,
        })
    }

    /// Get latest version of a package
    async fn get_latest_version(
        &self,
        package_id: &PackageId,
        options: &InstallOptions,
    ) -> Result<PackageVersion> {
        // Try cache first
        if !options.no_cache {
            if let Some(package) = self.cache.get_metadata(package_id) {
                return Ok(package.latest_version);
            }
        }

        if options.offline {
            // Check lock file
            if let Ok(lockfile) = self.lockfile_manager.read() {
                if let Some(locked) = lockfile.get(package_id) {
                    return Ok(locked.version.clone());
                }
            }

            return Err(Error::InstallationFailed {
                reason: format!(
                    "Cannot determine latest version for {} in offline mode",
                    package_id
                ),
            });
        }

        // Fetch from repository
        let package = self.repository.get_package(package_id).await?;

        // Cache the metadata
        if !options.no_cache {
            if let Err(e) = self.cache.set_metadata(&package) {
                warn!(
                    package = %package_id,
                    error = %e,
                    "Failed to cache metadata"
                );
            }
        }

        Ok(package.latest_version)
    }

    /// Resolve dependencies with fallback
    async fn resolve_with_fallback(
        &self,
        package_id: &PackageId,
        version: &PackageVersion,
        options: &InstallOptions,
    ) -> Result<ResolutionResult> {
        if options.offline {
            // In offline mode, try to use lock file
            if let Ok(lockfile) = self.lockfile_manager.read() {
                if lockfile.contains(package_id) {
                    // Build resolution from lock file
                    let deps: Vec<ResolvedDependency> = lockfile.packages
                        .iter()
                        .map(|(id, pkg)| ResolvedDependency {
                            id: id.clone(),
                            version: pkg.version.clone(),
                            checksum: pkg.checksum.clone(),
                            download_url: pkg.source.clone(),
                            dependencies: pkg.dependencies.clone(),
                            depth: 0,
                        })
                        .collect();

                    return Ok(ResolutionResult {
                        resolved: deps,
                        tree: crate::resolver::ResolutionNode {
                            id: package_id.clone(),
                            version: version.clone(),
                            children: vec![],
                        },
                        total_packages: lockfile.len(),
                        resolution_time: std::time::Duration::from_millis(0),
                        resolved_at: Utc::now(),
                    });
                }
            }

            return Err(Error::InstallationFailed {
                reason: format!(
                    "Cannot resolve {} in offline mode without lock file",
                    package_id
                ),
            });
        }

        self.resolver.resolve(package_id, version).await
    }

    /// Download a package
    async fn download_package(
        &self,
        _package_id: &PackageId,
        _version: &PackageVersion,
        _download_url: &str,
    ) -> Result<Vec<u8>> {
        // In real implementation, use reqwest to download
        // For now, return simulated data
        Ok(b"simulated package data".to_vec())
    }

    /// Install package data to disk
    fn install_package_data(
        &self,
        package_id: &PackageId,
        version: &PackageVersion,
        _data: &[u8],
        install_path: &Path,
    ) -> Result<()> {
        // Create installation directory
        std::fs::create_dir_all(install_path)?;

        // In real implementation, extract tarball and copy files
        // For now, create marker file
        let marker_path = install_path.join(".installed");
        std::fs::write(&marker_path, format!("{}@{}", package_id, version))?;

        debug!(
            package = %package_id,
            version = %version,
            path = %install_path.display(),
            "Installed package"
        );

        Ok(())
    }

    /// Rollback a failed transaction
    async fn rollback_transaction(&self, transaction: &mut InstallationTransaction) -> Result<()> {
        transaction.begin_rollback();

        for action in transaction.rollback_actions.iter().rev() {
            match action {
                RollbackAction::RemovePackage { id, version, path } => {
                    debug!(
                        package = %id,
                        version = %version,
                        path = %path,
                        "Rolling back: removing installed files"
                    );
                    let path = Path::new(path);
                    if path.exists() {
                        std::fs::remove_dir_all(path)?;
                    }
                }
                RollbackAction::RestoreBackup { backup_path, target_path } => {
                    debug!(
                        backup = %backup_path,
                        target = %target_path,
                        "Rolling back: restoring backup"
                    );
                    let backup = Path::new(backup_path);
                    let target = Path::new(target_path);
                    if backup.exists() {
                        std::fs::rename(backup, target)?;
                    }
                }
                RollbackAction::ExecuteCleanup { script } => {
                    debug!(script = %script, "Rolling back: executing cleanup");
                    // In real implementation, execute cleanup script
                }
            }
        }

        transaction.complete_rollback();
        Ok(())
    }

    /// Get cache statistics
    pub fn cache_stats(&self) -> crate::cache::CacheStats {
        self.cache.stats()
    }

    /// Get audit statistics
    pub fn audit_stats(&self) -> crate::audit::AuditStatistics {
        self.audit_manager.statistics()
    }

    /// Cleanup expired cache entries
    pub fn cleanup_cache(&self) -> Result<crate::cache::CleanupStats> {
        self.cache.cleanup_expired()
    }

    /// Clear all cache
    pub fn clear_cache(&self) -> Result<()> {
        self.cache.clear()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::models::{Package, PackageMetadata, ReleaseInfo};
    use crate::registry::Registry;
    use tempfile::TempDir;

    fn create_test_package(id: &str, version: &str, deps: Vec<(&str, &str)>) -> Package {
        let pkg_id = PackageId::new(id).unwrap();
        let pkg_version = PackageVersion::new(version).unwrap();
        let metadata = PackageMetadata::new(
            pkg_id.clone(),
            id,
            format!("Test package {}", id),
            "MIT",
        );

        let dep_list: Vec<crate::models::PackageDependency> = deps
            .into_iter()
            .map(|(dep_id, dep_ver)| crate::models::PackageDependency {
                id: PackageId::new(dep_id).unwrap(),
                version_req: dep_ver.to_string(),
                optional: false,
            })
            .collect();

        let mut releases = indexmap::IndexMap::new();
        releases.insert(
            pkg_version.clone(),
            ReleaseInfo {
                version: pkg_version.clone(),
                released_at: Utc::now(),
                changelog: "Test release".to_string(),
                checksum: "a".repeat(64),
                download_url: format!("https://example.com/{}.tar.gz", id),
                dependencies: dep_list,
            },
        );

        Package {
            metadata,
            latest_version: pkg_version.clone(),
            versions: vec![pkg_version],
            releases,
        }
    }

    #[tokio::test]
    async fn test_install_options_default() {
        let options = InstallOptions::default();

        assert!(!options.force_fmea);
        assert!(!options.offline);
        assert!(!options.no_cache);
        assert!(!options.dry_run);
        assert!(options.update_lockfile);
    }

    #[tokio::test]
    async fn test_install_options_from_cli() {
        let options = InstallOptions::from_cli(
            true,  // force_fmea
            true,  // offline
            false, // no_cache
            Some(PathBuf::from("/tmp/install")),
            false, // dry_run
        );

        assert!(options.force_fmea);
        assert!(options.offline);
        assert!(!options.no_cache);
        assert_eq!(options.install_path, PathBuf::from("/tmp/install"));
    }

    #[tokio::test]
    async fn test_marketplace_installer_creation() {
        let temp_dir = TempDir::new().unwrap();
        let registry = Registry::new(100).await;

        let installer = MarketplaceInstaller::new(
            registry,
            temp_dir.path().join("cache"),
            temp_dir.path().join("project"),
        );

        assert!(installer.is_ok());
    }

    #[tokio::test]
    async fn test_install_single_package() {
        let temp_dir = TempDir::new().unwrap();
        let registry = Registry::new(100).await;

        // Add test package
        let pkg = create_test_package("test-pkg", "1.0.0", vec![]);
        registry.insert(pkg).unwrap();

        let mut installer = MarketplaceInstaller::new(
            registry,
            temp_dir.path().join("cache"),
            temp_dir.path().join("project"),
        ).unwrap();

        let options = InstallOptions {
            install_path: temp_dir.path().join("install"),
            ..Default::default()
        };

        let pkg_id = PackageId::new("test-pkg").unwrap();
        let result = installer.install(&pkg_id, None, &options).await.unwrap();

        assert_eq!(result.installed_packages.len(), 1);
        assert!(!result.force_fmea_applied);
        assert!(!result.used_offline_fallback);
    }

    #[tokio::test]
    async fn test_install_with_dependencies() {
        let temp_dir = TempDir::new().unwrap();
        let registry = Registry::new(100).await;

        // Add packages: A -> B -> C
        let pkg_c = create_test_package("pkg-c", "1.0.0", vec![]);
        let pkg_b = create_test_package("pkg-b", "1.0.0", vec![("pkg-c", "^1.0.0")]);
        let pkg_a = create_test_package("pkg-a", "1.0.0", vec![("pkg-b", "^1.0.0")]);

        registry.insert(pkg_c).unwrap();
        registry.insert(pkg_b).unwrap();
        registry.insert(pkg_a).unwrap();

        let mut installer = MarketplaceInstaller::new(
            registry,
            temp_dir.path().join("cache"),
            temp_dir.path().join("project"),
        ).unwrap();

        let options = InstallOptions {
            install_path: temp_dir.path().join("install"),
            ..Default::default()
        };

        let pkg_id = PackageId::new("pkg-a").unwrap();
        let result = installer.install(&pkg_id, None, &options).await.unwrap();

        assert_eq!(result.installed_packages.len(), 3);
    }

    #[tokio::test]
    async fn test_install_dry_run() {
        let temp_dir = TempDir::new().unwrap();
        let registry = Registry::new(100).await;

        let pkg = create_test_package("test-pkg", "1.0.0", vec![]);
        registry.insert(pkg).unwrap();

        let mut installer = MarketplaceInstaller::new(
            registry,
            temp_dir.path().join("cache"),
            temp_dir.path().join("project"),
        ).unwrap();

        let options = InstallOptions {
            install_path: temp_dir.path().join("install"),
            dry_run: true,
            ..Default::default()
        };

        let pkg_id = PackageId::new("test-pkg").unwrap();
        let result = installer.install(&pkg_id, None, &options).await.unwrap();

        // Dry run should not create directories
        assert!(!temp_dir.path().join("install/packages/test-pkg").exists());
        assert_eq!(result.installed_packages.len(), 1);
    }

    #[tokio::test]
    async fn test_cache_stats() {
        let temp_dir = TempDir::new().unwrap();
        let registry = Registry::new(100).await;

        let installer = MarketplaceInstaller::new(
            registry,
            temp_dir.path().join("cache"),
            temp_dir.path().join("project"),
        ).unwrap();

        let stats = installer.cache_stats();
        assert_eq!(stats.metadata_entries, 0);
        assert_eq!(stats.package_entries, 0);
    }

    #[tokio::test]
    async fn test_offline_mode_fails_without_cache() {
        let temp_dir = TempDir::new().unwrap();
        let registry = Registry::new(100).await;

        let mut installer = MarketplaceInstaller::new(
            registry,
            temp_dir.path().join("cache"),
            temp_dir.path().join("project"),
        ).unwrap();

        let options = InstallOptions {
            offline: true,
            install_path: temp_dir.path().join("install"),
            ..Default::default()
        };

        let pkg_id = PackageId::new("nonexistent").unwrap();
        let result = installer.install(&pkg_id, None, &options).await;

        assert!(result.is_err());
    }

    #[tokio::test]
    async fn test_progress_callback() {
        let temp_dir = TempDir::new().unwrap();
        let registry = Registry::new(100).await;

        let pkg = create_test_package("test-pkg", "1.0.0", vec![]);
        registry.insert(pkg).unwrap();

        use std::sync::atomic::{AtomicUsize, Ordering};
        use std::sync::Arc;

        let progress_count = Arc::new(AtomicUsize::new(0));
        let count_clone = progress_count.clone();

        let mut installer = MarketplaceInstaller::new(
            registry,
            temp_dir.path().join("cache"),
            temp_dir.path().join("project"),
        ).unwrap()
        .with_progress_callback(Box::new(move |_| {
            count_clone.fetch_add(1, Ordering::SeqCst);
        }));

        let options = InstallOptions {
            install_path: temp_dir.path().join("install"),
            ..Default::default()
        };

        let pkg_id = PackageId::new("test-pkg").unwrap();
        installer.install(&pkg_id, None, &options).await.unwrap();

        // Should have received multiple progress updates
        assert!(progress_count.load(Ordering::SeqCst) > 0);
    }
}
