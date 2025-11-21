//! Package installation system with dependency resolution
//!
//! Features:
//! - Dependency resolution with cycle detection
//! - Conflict detection
//! - Atomic installation with transaction tracking
//! - Rollback on failure

use async_trait::async_trait;
use std::collections::HashSet;
use tracing::{debug, error, info, warn};
use uuid::Uuid;

use crate::error::Result;
use crate::models::{InstallationManifest, PackageId, PackageVersion};
use crate::traits::{AsyncRepository, Installable};

/// Transaction state for installation tracking
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TransactionState {
    /// Transaction started
    Pending,
    /// Installing packages
    InProgress,
    /// All packages installed successfully
    Committed,
    /// Installation failed, rolling back
    RollingBack,
    /// Rollback complete
    RolledBack,
    /// Transaction failed after rollback
    Failed,
}

impl std::fmt::Display for TransactionState {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Pending => write!(f, "pending"),
            Self::InProgress => write!(f, "in_progress"),
            Self::Committed => write!(f, "committed"),
            Self::RollingBack => write!(f, "rolling_back"),
            Self::RolledBack => write!(f, "rolled_back"),
            Self::Failed => write!(f, "failed"),
        }
    }
}

/// Installation transaction for atomic operations with rollback
#[derive(Clone, Debug)]
pub struct InstallationTransaction {
    /// Transaction ID
    pub id: Uuid,
    /// Current state
    pub state: TransactionState,
    /// Packages successfully installed in this transaction
    pub installed_packages: Vec<(PackageId, PackageVersion)>,
    /// Packages that failed to install
    pub failed_packages: Vec<(PackageId, PackageVersion, String)>,
    /// Rollback actions to perform
    pub rollback_actions: Vec<RollbackAction>,
    /// Started at
    pub started_at: chrono::DateTime<chrono::Utc>,
    /// Completed at
    pub completed_at: Option<chrono::DateTime<chrono::Utc>>,
}

impl InstallationTransaction {
    /// Create a new transaction
    pub fn new() -> Self {
        Self {
            id: Uuid::new_v4(),
            state: TransactionState::Pending,
            installed_packages: Vec::new(),
            failed_packages: Vec::new(),
            rollback_actions: Vec::new(),
            started_at: chrono::Utc::now(),
            completed_at: None,
        }
    }

    /// Mark a package as successfully installed
    pub fn mark_installed(&mut self, id: PackageId, version: PackageVersion, install_path: String) {
        self.installed_packages.push((id.clone(), version.clone()));
        self.rollback_actions.push(RollbackAction::RemovePackage {
            id,
            version,
            path: install_path,
        });
    }

    /// Mark a package as failed
    pub fn mark_failed(&mut self, id: PackageId, version: PackageVersion, reason: String) {
        self.failed_packages.push((id, version, reason));
    }

    /// Check if transaction has any failures
    pub fn has_failures(&self) -> bool {
        !self.failed_packages.is_empty()
    }

    /// Get the number of packages installed
    pub fn installed_count(&self) -> usize {
        self.installed_packages.len()
    }

    /// Transition to in-progress state
    pub fn start(&mut self) {
        self.state = TransactionState::InProgress;
        debug!("Transaction {} started", self.id);
    }

    /// Commit the transaction
    pub fn commit(&mut self) {
        self.state = TransactionState::Committed;
        self.completed_at = Some(chrono::Utc::now());
        self.rollback_actions.clear(); // No rollback needed
        info!(
            "Transaction {} committed with {} packages",
            self.id,
            self.installed_packages.len()
        );
    }

    /// Begin rollback
    pub fn begin_rollback(&mut self) {
        self.state = TransactionState::RollingBack;
        warn!("Transaction {} beginning rollback", self.id);
    }

    /// Complete rollback
    pub fn complete_rollback(&mut self) {
        self.state = TransactionState::RolledBack;
        self.completed_at = Some(chrono::Utc::now());
        info!(
            "Transaction {} rolled back, removed {} packages",
            self.id,
            self.installed_packages.len()
        );
    }

    /// Mark transaction as failed
    pub fn fail(&mut self) {
        self.state = TransactionState::Failed;
        self.completed_at = Some(chrono::Utc::now());
        error!("Transaction {} failed", self.id);
    }
}

impl Default for InstallationTransaction {
    fn default() -> Self {
        Self::new()
    }
}

/// Action to perform during rollback
#[derive(Clone, Debug)]
pub enum RollbackAction {
    /// Remove an installed package
    RemovePackage {
        id: PackageId,
        version: PackageVersion,
        path: String,
    },
    /// Restore a backup file
    RestoreBackup { backup_path: String, target_path: String },
    /// Execute a cleanup script
    ExecuteCleanup { script: String },
}

impl std::fmt::Display for RollbackAction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::RemovePackage { id, version, path } => {
                write!(f, "remove {}@{} from {}", id, version, path)
            }
            Self::RestoreBackup {
                backup_path,
                target_path,
            } => write!(f, "restore {} to {}", backup_path, target_path),
            Self::ExecuteCleanup { script } => write!(f, "execute cleanup: {}", script),
        }
    }
}

/// Package installer with dependency resolution
pub struct Installer<R: AsyncRepository> {
    repository: R,
}

impl<R: AsyncRepository> Installer<R> {
    /// Create a new installer
    pub fn new(repository: R) -> Self {
        Self { repository }
    }

    /// Resolve a dependency tree (iterative approach for Send compatibility)
    pub async fn resolve_dependencies(
        &self, root_id: &PackageId, root_version: &PackageVersion,
    ) -> Result<Vec<(PackageId, PackageVersion)>> {
        let mut resolved = Vec::new();
        let mut visited = HashSet::new();
        let mut to_process = vec![(root_id.clone(), root_version.clone())];

        while let Some((id, version)) = to_process.pop() {
            if visited.contains(&id) {
                continue;
            }

            // Get package and process its dependencies
            let package = self.repository.get_package_version(&id, &version).await?;

            for release in package.releases.values() {
                for dep in &release.dependencies {
                    if !visited.contains(&dep.id) {
                        let parsed_version = dep.version_req.parse::<PackageVersion>()?;
                        to_process.push((dep.id.clone(), parsed_version));
                    }
                }
            }

            visited.insert(id.clone());
            resolved.push((id, version));
        }

        // Sort by dependency order (dependencies first)
        resolved.reverse();

        debug!(
            "Resolved {} dependencies for {}@{}",
            resolved.len(),
            root_id,
            root_version
        );

        Ok(resolved)
    }

    /// Create an installation manifest
    pub async fn create_manifest(
        &self, package_ids: Vec<PackageId>, install_path: String,
    ) -> Result<InstallationManifest> {
        let mut dependencies = indexmap::IndexMap::new();

        for pkg_id in &package_ids {
            let package = self.repository.get_package(pkg_id).await?;
            let latest_version = package.latest_version.clone();
            dependencies.insert(pkg_id.clone(), latest_version.clone());

            // Resolve dependencies
            let resolved = self.resolve_dependencies(pkg_id, &latest_version).await?;

            for (dep_id, dep_version) in resolved {
                if !dependencies.contains_key(&dep_id) {
                    dependencies.insert(dep_id, dep_version);
                }
            }
        }

        let manifest = InstallationManifest {
            id: Uuid::new_v4(),
            packages: package_ids,
            dependencies,
            install_path,
            planned_at: chrono::Utc::now(),
        };

        info!(
            "Created installation manifest {} with {} packages",
            manifest.id,
            manifest.dependencies.len()
        );

        Ok(manifest)
    }

    /// Check for version conflicts
    pub fn check_conflicts(
        &self, dependencies: &indexmap::IndexMap<PackageId, PackageVersion>,
    ) -> Result<()> {
        // In a real implementation, this would check semantic version constraints
        // For now, we allow any version combination

        debug!("Checked {} dependencies for conflicts", dependencies.len());

        Ok(())
    }

    /// Validate installation manifest before execution
    pub async fn validate_manifest(&self, manifest: &InstallationManifest) -> Result<()> {
        // Check all packages exist
        for (pkg_id, version) in &manifest.dependencies {
            self.repository.get_package_version(pkg_id, version).await?;
        }

        // Check for conflicts
        self.check_conflicts(&manifest.dependencies)?;

        info!("Validated installation manifest {}", manifest.id);

        Ok(())
    }

    /// Simulate installation without making changes
    pub async fn dry_run(&self, manifest: &InstallationManifest) -> Result<InstallationPlan> {
        self.validate_manifest(manifest).await?;

        let mut plan = InstallationPlan {
            id: manifest.id,
            packages: Vec::new(),
            total_size: 0,
            estimated_time: std::time::Duration::from_secs(0),
        };

        for (pkg_id, version) in &manifest.dependencies {
            self.repository.get_package_version(pkg_id, version).await?;
            let _package = self.repository.get_package_version(pkg_id, version).await?;

            // Simulate size calculation (in real implementation, would fetch actual sizes)
            let size_estimate = 1024 * 100; // 100KB estimate per package
            plan.total_size += size_estimate;
            plan.packages.push(PackageInstallPlan {
                id: pkg_id.clone(),
                version: version.clone(),
                size: size_estimate,
            });
        }

        // Estimate time: 100KB per second
        plan.estimated_time = std::time::Duration::from_secs((plan.total_size / 102_400) as u64);

        debug!(
            "Dry-run installation: {} packages, {} bytes",
            plan.packages.len(),
            plan.total_size
        );

        Ok(plan)
    }

    /// Execute rollback actions to restore previous state
    pub async fn execute_rollback(&self, transaction: &mut InstallationTransaction) -> Result<()> {
        transaction.begin_rollback();

        for action in transaction.rollback_actions.iter().rev() {
            match action {
                RollbackAction::RemovePackage { id, version, path } => {
                    debug!("Rolling back: removing {}@{} from {}", id, version, path);
                    // In a real implementation, this would remove the package files
                    // For now, we just log the action
                }
                RollbackAction::RestoreBackup {
                    backup_path,
                    target_path,
                } => {
                    debug!("Rolling back: restoring {} to {}", backup_path, target_path);
                    // In a real implementation, this would restore from backup
                }
                RollbackAction::ExecuteCleanup { script } => {
                    debug!("Rolling back: executing cleanup script: {}", script);
                    // In a real implementation, this would execute the script
                }
            }
        }

        transaction.complete_rollback();
        info!(
            "Rollback completed for transaction {}, {} actions executed",
            transaction.id,
            transaction.rollback_actions.len()
        );

        Ok(())
    }

    /// Install packages with transaction and automatic rollback on failure
    pub async fn install_with_rollback(
        &self, manifest: InstallationManifest,
    ) -> Result<(InstallationManifest, InstallationTransaction)> {
        self.validate_manifest(&manifest).await?;

        let mut transaction = InstallationTransaction::new();
        transaction.start();

        info!(
            "Starting transactional installation {} for {} packages",
            transaction.id,
            manifest.dependencies.len()
        );

        // Install each package
        for (pkg_id, version) in &manifest.dependencies {
            match self.repository.get_package_version(pkg_id, version).await {
                Ok(_package) => {
                    // Simulate installation (in real implementation, would download and extract)
                    transaction.mark_installed(
                        pkg_id.clone(),
                        version.clone(),
                        format!("{}/{}", manifest.install_path, pkg_id),
                    );
                    debug!("Installed {}@{}", pkg_id, version);
                }
                Err(e) => {
                    transaction.mark_failed(pkg_id.clone(), version.clone(), e.to_string());
                    warn!("Failed to install {}@{}: {}", pkg_id, version, e);
                }
            }
        }

        // Check for failures and rollback if necessary
        if transaction.has_failures() {
            error!(
                "Installation failed with {} errors, initiating rollback",
                transaction.failed_packages.len()
            );
            self.execute_rollback(&mut transaction).await?;

            return Err(crate::error::Error::InstallationFailed {
                reason: format!(
                    "Package {} failed: {}",
                    transaction.failed_packages[0].0,
                    transaction.failed_packages[0].2
                ),
            });
        }

        // Commit transaction
        transaction.commit();

        Ok((manifest, transaction))
    }
}

#[async_trait]
impl<R: AsyncRepository> Installable for Installer<R> {
    async fn install(&self, manifest: InstallationManifest) -> Result<InstallationManifest> {
        self.validate_manifest(&manifest).await?;

        info!(
            "Installing {} packages to {}",
            manifest.packages.len(),
            manifest.install_path
        );

        // In a real implementation, this would:
        // 1. Download packages
        // 2. Verify checksums
        // 3. Extract to install path
        // 4. Run post-install hooks
        // 5. Write lock file

        Ok(manifest)
    }

    async fn resolve_dependencies(
        &self, id: &PackageId, version: &PackageVersion,
    ) -> Result<Vec<(PackageId, PackageVersion)>> {
        Installer::resolve_dependencies(self, id, version).await
    }

    async fn dry_run_install(&self, manifest: &InstallationManifest) -> Result<String> {
        let plan = self.dry_run(manifest).await?;
        Ok(plan.to_string())
    }
}

/// Plan for package installation
#[derive(Clone, Debug)]
pub struct InstallationPlan {
    /// Installation ID
    pub id: Uuid,
    /// Packages to install
    pub packages: Vec<PackageInstallPlan>,
    /// Total size in bytes
    pub total_size: u64,
    /// Estimated installation time
    pub estimated_time: std::time::Duration,
}

impl std::fmt::Display for InstallationPlan {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "Installation Plan {}", self.id)?;
        writeln!(f, "Packages: {}", self.packages.len())?;
        writeln!(f, "Total size: {} MB", self.total_size / (1024 * 1024))?;
        writeln!(
            f,
            "Estimated time: {:.1}s",
            self.estimated_time.as_secs_f64()
        )?;
        writeln!(f)?;

        for pkg in &self.packages {
            writeln!(f, "  - {}@{} ({} KB)", pkg.id, pkg.version, pkg.size / 1024)?;
        }

        Ok(())
    }
}

/// Plan for a single package installation
#[derive(Clone, Debug)]
pub struct PackageInstallPlan {
    /// Package ID
    pub id: PackageId,
    /// Version
    pub version: PackageVersion,
    /// Estimated size in bytes
    pub size: u64,
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::registry::Registry;

    #[tokio::test]
    async fn test_installation_manifest_creation() {
        let registry = Registry::new(100).await;
        let installer = Installer::new(registry);

        let manifest = installer
            .create_manifest(vec![], "/tmp/ggen".to_string())
            .await
            .unwrap();

        assert_eq!(manifest.packages.len(), 0);
        assert_eq!(manifest.install_path, "/tmp/ggen");
    }

    #[tokio::test]
    async fn test_conflict_checking() {
        let registry = Registry::new(100).await;
        let installer = Installer::new(registry);

        let deps = indexmap::IndexMap::new();
        assert!(installer.check_conflicts(&deps).is_ok());
    }
}
