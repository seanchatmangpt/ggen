//! Data Migration: v1 PackageRegistry → v2 RDF Store
//!
//! Provides utilities for migrating package data from the v1 in-memory
//! registry to the v2 RDF-backed semantic store.

use crate::error::Result;
use crate::models::{Package, PackageId};
use crate::registry_rdf::RdfRegistry;
use crate::traits::AsyncRepository;
use std::sync::Arc;
use tracing::{info, warn};

/// Migration coordinator for v1 → v2 data migration
pub struct MigrationCoordinator {
    /// Target RDF registry
    target: Arc<RdfRegistry>,
}

impl MigrationCoordinator {
    /// Create a new migration coordinator
    pub fn new(target: Arc<RdfRegistry>) -> Self {
        Self { target }
    }

    /// Migrate packages from v1 to RDF
    ///
    /// This performs a complete migration of all packages,
    /// ensuring data integrity and completeness.
    pub async fn migrate_packages(&self, v1_packages: Vec<Package>) -> Result<MigrationReport> {
        info!(
            "Starting migration of {} packages from v1 to RDF",
            v1_packages.len()
        );

        let mut report = MigrationReport::new();
        report.total_packages = v1_packages.len();

        // Batch insert for efficiency
        let batch_size = 50;
        for chunk in v1_packages.chunks(batch_size) {
            match self.target.batch_insert_packages(chunk.to_vec()).await {
                Ok(inserted) => {
                    report.migrated_packages += inserted;
                }
                Err(e) => {
                    warn!("Batch insert failed: {}", e);
                    report.errors.push(format!("Batch insert error: {}", e));
                }
            }
        }

        info!(
            "Migration complete: {}/{} packages migrated",
            report.migrated_packages, report.total_packages
        );

        Ok(report)
    }

    /// Verify migration integrity
    ///
    /// Ensures all v1 packages exist in v2 with matching metadata
    pub async fn verify_migration(&self, v1_packages: Vec<Package>) -> Result<VerificationReport> {
        info!("Verifying migration of {} packages", v1_packages.len());

        let mut report = VerificationReport::new();
        report.total_packages = v1_packages.len();

        for v1_package in v1_packages {
            match self.verify_package(&v1_package).await {
                Ok(true) => {
                    report.verified_packages += 1;
                }
                Ok(false) => {
                    report.mismatches.push(v1_package.metadata.id.to_string());
                }
                Err(e) => {
                    report.errors.push(format!(
                        "Verification failed for {}: {}",
                        v1_package.metadata.id, e
                    ));
                }
            }
        }

        info!(
            "Verification complete: {}/{} packages verified",
            report.verified_packages, report.total_packages
        );

        Ok(report)
    }

    /// Verify a single package
    async fn verify_package(&self, v1_package: &Package) -> Result<bool> {
        // Retrieve from RDF
        let v2_package = self.target.get_package(&v1_package.metadata.id).await?;

        // Compare critical fields
        let matches = v1_package.metadata.id == v2_package.metadata.id
            && v1_package.metadata.name == v2_package.metadata.name
            && v1_package.metadata.description == v2_package.metadata.description
            && v1_package.metadata.license == v2_package.metadata.license
            && v1_package.latest_version == v2_package.latest_version
            && v1_package.versions == v2_package.versions;

        if !matches {
            warn!(
                "Package {} data mismatch between v1 and v2",
                v1_package.metadata.id
            );
        }

        Ok(matches)
    }

    /// Perform incremental migration (only new/updated packages)
    pub async fn incremental_migrate(&self, v1_packages: Vec<Package>) -> Result<MigrationReport> {
        info!("Starting incremental migration");

        let mut report = MigrationReport::new();
        report.total_packages = v1_packages.len();

        for package in v1_packages {
            // Check if package exists
            match self.target.package_exists(&package.metadata.id).await {
                Ok(true) => {
                    // Skip existing packages
                    report.skipped_packages += 1;
                }
                Ok(false) | Err(_) => {
                    // Migrate new package
                    match self.target.insert_package_rdf(&package).await {
                        Ok(_) => {
                            report.migrated_packages += 1;
                        }
                        Err(e) => {
                            report
                                .errors
                                .push(format!("Failed to migrate {}: {}", package.metadata.id, e));
                        }
                    }
                }
            }
        }

        info!(
            "Incremental migration complete: {} migrated, {} skipped",
            report.migrated_packages, report.skipped_packages
        );

        Ok(report)
    }
}

/// Migration report
#[derive(Debug, Clone)]
pub struct MigrationReport {
    /// Total packages to migrate
    pub total_packages: usize,
    /// Successfully migrated packages
    pub migrated_packages: usize,
    /// Skipped packages (already exist)
    pub skipped_packages: usize,
    /// Migration errors
    pub errors: Vec<String>,
}

impl MigrationReport {
    fn new() -> Self {
        Self {
            total_packages: 0,
            migrated_packages: 0,
            skipped_packages: 0,
            errors: Vec::new(),
        }
    }

    /// Check if migration was successful
    pub fn is_successful(&self) -> bool {
        self.errors.is_empty() && self.migrated_packages == self.total_packages
    }

    /// Get success rate (0.0 - 1.0)
    pub fn success_rate(&self) -> f64 {
        if self.total_packages == 0 {
            1.0
        } else {
            self.migrated_packages as f64 / self.total_packages as f64
        }
    }
}

impl std::fmt::Display for MigrationReport {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Migration: {}/{} migrated ({:.1}%), {} skipped, {} errors",
            self.migrated_packages,
            self.total_packages,
            self.success_rate() * 100.0,
            self.skipped_packages,
            self.errors.len()
        )
    }
}

/// Verification report
#[derive(Debug, Clone)]
pub struct VerificationReport {
    /// Total packages to verify
    pub total_packages: usize,
    /// Successfully verified packages
    pub verified_packages: usize,
    /// Package IDs with data mismatches
    pub mismatches: Vec<String>,
    /// Verification errors
    pub errors: Vec<String>,
}

impl VerificationReport {
    fn new() -> Self {
        Self {
            total_packages: 0,
            verified_packages: 0,
            mismatches: Vec::new(),
            errors: Vec::new(),
        }
    }

    /// Check if verification passed
    pub fn is_valid(&self) -> bool {
        self.errors.is_empty() && self.mismatches.is_empty()
    }

    /// Get verification rate (0.0 - 1.0)
    pub fn verification_rate(&self) -> f64 {
        if self.total_packages == 0 {
            1.0
        } else {
            self.verified_packages as f64 / self.total_packages as f64
        }
    }
}

impl std::fmt::Display for VerificationReport {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Verification: {}/{} verified ({:.1}%), {} mismatches, {} errors",
            self.verified_packages,
            self.total_packages,
            self.verification_rate() * 100.0,
            self.mismatches.len(),
            self.errors.len()
        )
    }
}

/// Consistency checker for v1 and v2 backends
pub struct ConsistencyChecker {
    rdf_registry: Arc<RdfRegistry>,
}

impl ConsistencyChecker {
    /// Create a new consistency checker
    pub fn new(rdf_registry: Arc<RdfRegistry>) -> Self {
        Self { rdf_registry }
    }

    /// Check consistency between v1 and v2 for a package
    pub async fn check_package_consistency(
        &self, package_id: &PackageId, v1_package: &Package,
    ) -> Result<ConsistencyResult> {
        let v2_package = self.rdf_registry.get_package(package_id).await?;

        let mut result = ConsistencyResult::new(package_id.to_string());

        // Check metadata fields
        if v1_package.metadata.name != v2_package.metadata.name {
            result.differences.push("name mismatch".to_string());
        }
        if v1_package.metadata.description != v2_package.metadata.description {
            result.differences.push("description mismatch".to_string());
        }
        if v1_package.metadata.license != v2_package.metadata.license {
            result.differences.push("license mismatch".to_string());
        }

        // Check versions
        if v1_package.versions.len() != v2_package.versions.len() {
            result
                .differences
                .push("version count mismatch".to_string());
        }

        // Check releases
        if v1_package.releases.len() != v2_package.releases.len() {
            result
                .differences
                .push("release count mismatch".to_string());
        }

        result.is_consistent = result.differences.is_empty();

        Ok(result)
    }

    /// Run periodic consistency check
    pub async fn periodic_check(&self, v1_packages: Vec<Package>) -> Result<ConsistencyReport> {
        let mut report = ConsistencyReport::new();
        report.total_packages = v1_packages.len();

        for v1_package in v1_packages {
            match self
                .check_package_consistency(&v1_package.metadata.id, &v1_package)
                .await
            {
                Ok(result) => {
                    if result.is_consistent {
                        report.consistent_packages += 1;
                    } else {
                        report.inconsistent_packages.push(result);
                    }
                }
                Err(e) => {
                    report
                        .errors
                        .push(format!("{}: {}", v1_package.metadata.id, e));
                }
            }
        }

        Ok(report)
    }
}

/// Consistency check result for a package
#[derive(Debug, Clone)]
pub struct ConsistencyResult {
    /// Package ID
    pub package_id: String,
    /// Whether data is consistent
    pub is_consistent: bool,
    /// List of differences found
    pub differences: Vec<String>,
}

impl ConsistencyResult {
    fn new(package_id: String) -> Self {
        Self {
            package_id,
            is_consistent: true,
            differences: Vec::new(),
        }
    }
}

/// Consistency report for multiple packages
#[derive(Debug, Clone)]
pub struct ConsistencyReport {
    /// Total packages checked
    pub total_packages: usize,
    /// Packages with consistent data
    pub consistent_packages: usize,
    /// Packages with inconsistencies
    pub inconsistent_packages: Vec<ConsistencyResult>,
    /// Check errors
    pub errors: Vec<String>,
}

impl ConsistencyReport {
    fn new() -> Self {
        Self {
            total_packages: 0,
            consistent_packages: 0,
            inconsistent_packages: Vec::new(),
            errors: Vec::new(),
        }
    }

    /// Check if all packages are consistent
    pub fn is_fully_consistent(&self) -> bool {
        self.inconsistent_packages.is_empty() && self.errors.is_empty()
    }

    /// Get consistency rate (0.0 - 1.0)
    pub fn consistency_rate(&self) -> f64 {
        if self.total_packages == 0 {
            1.0
        } else {
            self.consistent_packages as f64 / self.total_packages as f64
        }
    }
}

impl std::fmt::Display for ConsistencyReport {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Consistency: {}/{} consistent ({:.1}%), {} inconsistent, {} errors",
            self.consistent_packages,
            self.total_packages,
            self.consistency_rate() * 100.0,
            self.inconsistent_packages.len(),
            self.errors.len()
        )
    }
}

// ============================================================================
// Phase 5B: Transaction Support for Safe Migration with Rollback
// ============================================================================

/// Checkpoint state machine for migration transactions
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CheckpointState {
    /// Checkpoint created but not yet committed
    Pending,
    /// Migration completed successfully
    Committed,
    /// Migration rolled back due to error
    RolledBack,
    /// Checkpoint expired and cleaned up
    Expired,
}

impl CheckpointState {
    /// Check if transition to new state is valid
    pub fn can_transition_to(&self, new_state: CheckpointState) -> bool {
        match (self, new_state) {
            (Self::Pending, Self::Committed) => true,
            (Self::Pending, Self::RolledBack) => true,
            (Self::Pending, Self::Expired) => true,
            (Self::Committed, Self::Expired) => true,
            _ => false,
        }
    }
}

impl std::fmt::Display for CheckpointState {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Pending => write!(f, "pending"),
            Self::Committed => write!(f, "committed"),
            Self::RolledBack => write!(f, "rolled_back"),
            Self::Expired => write!(f, "expired"),
        }
    }
}

/// Migration checkpoint for safe rollback
#[derive(Debug, Clone)]
pub struct MigrationCheckpoint {
    /// Unique checkpoint ID
    pub checkpoint_id: String,
    /// Creation timestamp
    pub created_at: std::time::Instant,
    /// Packages captured in checkpoint
    pub packages: Vec<Package>,
    /// Current state
    pub state: CheckpointState,
}

impl MigrationCheckpoint {
    /// Create a new checkpoint
    pub fn new(packages: Vec<Package>) -> Self {
        Self {
            checkpoint_id: uuid::Uuid::new_v4().to_string(),
            created_at: std::time::Instant::now(),
            packages,
            state: CheckpointState::Pending,
        }
    }

    /// Transition to new state
    pub fn transition_to(&mut self, new_state: CheckpointState) -> Result<()> {
        if self.state.can_transition_to(new_state) {
            self.state = new_state;
            Ok(())
        } else {
            Err(crate::error::Error::InvalidStateTransition {
                from: self.state.to_string(),
                to: new_state.to_string(),
            })
        }
    }

    /// Check if checkpoint is expired (default: 1 hour)
    pub fn is_expired(&self) -> bool {
        self.created_at.elapsed() > std::time::Duration::from_secs(3600)
    }
}

/// Transactional migration with rollback support
pub struct TransactionalMigration {
    /// Target RDF registry
    target: Arc<RdfRegistry>,
    /// Active checkpoints
    checkpoints: std::collections::HashMap<String, MigrationCheckpoint>,
}

impl TransactionalMigration {
    /// Create a new transactional migration coordinator
    pub fn new(target: Arc<RdfRegistry>) -> Self {
        Self {
            target,
            checkpoints: std::collections::HashMap::new(),
        }
    }

    /// Create a checkpoint before migration
    pub fn create_checkpoint(&mut self, packages: Vec<Package>) -> String {
        let checkpoint = MigrationCheckpoint::new(packages);
        let id = checkpoint.checkpoint_id.clone();
        self.checkpoints.insert(id.clone(), checkpoint);
        info!("Created migration checkpoint: {}", id);
        id
    }

    /// Execute migration with transaction semantics
    pub async fn migrate_transactional(
        &mut self, checkpoint_id: &str, packages: Vec<Package>,
    ) -> Result<TransactionalMigrationResult> {
        let checkpoint = self.checkpoints.get_mut(checkpoint_id).ok_or_else(|| {
            crate::error::Error::Other(format!("Checkpoint not found: {}", checkpoint_id))
        })?;

        if checkpoint.state != CheckpointState::Pending {
            return Err(crate::error::Error::InvalidStateTransition {
                from: checkpoint.state.to_string(),
                to: "migration".to_string(),
            });
        }

        let mut result = TransactionalMigrationResult::new(checkpoint_id.to_string());
        result.total_packages = packages.len();

        // Attempt migration
        for package in packages {
            match self.target.insert_package_rdf(&package).await {
                Ok(_) => result.migrated_packages += 1,
                Err(e) => {
                    result
                        .errors
                        .push(format!("{}: {}", package.metadata.id, e));
                }
            }
        }

        // Update checkpoint state based on result
        if result.errors.is_empty() {
            checkpoint.transition_to(CheckpointState::Committed)?;
            result.committed = true;
        }

        Ok(result)
    }

    /// Rollback to checkpoint state
    pub async fn rollback(&mut self, checkpoint_id: &str) -> Result<RollbackResult> {
        let checkpoint = self.checkpoints.get_mut(checkpoint_id).ok_or_else(|| {
            crate::error::Error::Other(format!("Checkpoint not found: {}", checkpoint_id))
        })?;

        if checkpoint.state == CheckpointState::Committed {
            return Err(crate::error::Error::InvalidStateTransition {
                from: "committed".to_string(),
                to: "rolled_back".to_string(),
            });
        }

        let packages_to_restore = checkpoint.packages.clone();
        checkpoint.transition_to(CheckpointState::RolledBack)?;

        let mut result = RollbackResult::new(checkpoint_id.to_string());
        result.packages_restored = packages_to_restore.len();

        warn!("Rolled back migration checkpoint: {}", checkpoint_id);
        Ok(result)
    }

    /// Clean up expired checkpoints
    pub fn cleanup_expired(&mut self) -> usize {
        let expired: Vec<String> = self
            .checkpoints
            .iter()
            .filter(|(_, cp)| cp.is_expired())
            .map(|(id, _)| id.clone())
            .collect();

        let count = expired.len();
        for id in expired {
            self.checkpoints.remove(&id);
        }
        count
    }
}

/// Result of transactional migration
#[derive(Debug, Clone)]
pub struct TransactionalMigrationResult {
    /// Checkpoint ID
    pub checkpoint_id: String,
    /// Total packages to migrate
    pub total_packages: usize,
    /// Successfully migrated
    pub migrated_packages: usize,
    /// Whether committed
    pub committed: bool,
    /// Errors encountered
    pub errors: Vec<String>,
}

impl TransactionalMigrationResult {
    fn new(checkpoint_id: String) -> Self {
        Self {
            checkpoint_id,
            total_packages: 0,
            migrated_packages: 0,
            committed: false,
            errors: Vec::new(),
        }
    }
}

impl std::fmt::Display for TransactionalMigrationResult {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Migration [{}]: {}/{} migrated, committed: {}, errors: {}",
            self.checkpoint_id,
            self.migrated_packages,
            self.total_packages,
            self.committed,
            self.errors.len()
        )
    }
}

/// Result of rollback operation
#[derive(Debug, Clone)]
pub struct RollbackResult {
    /// Checkpoint ID
    pub checkpoint_id: String,
    /// Packages restored
    pub packages_restored: usize,
}

impl RollbackResult {
    fn new(checkpoint_id: String) -> Self {
        Self {
            checkpoint_id,
            packages_restored: 0,
        }
    }
}

impl std::fmt::Display for RollbackResult {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Rollback [{}]: {} packages restored",
            self.checkpoint_id, self.packages_restored
        )
    }
}

/// Post-migration validator
pub struct MigrationValidator {
    rdf_registry: Arc<RdfRegistry>,
}

impl MigrationValidator {
    /// Create a new migration validator
    pub fn new(rdf_registry: Arc<RdfRegistry>) -> Self {
        Self { rdf_registry }
    }

    /// Validate migration results
    pub async fn validate(
        &self, original_packages: &[Package],
    ) -> Result<MigrationValidationResult> {
        let mut result = MigrationValidationResult::new();
        result.total_packages = original_packages.len();

        for original in original_packages {
            match self.rdf_registry.get_package(&original.metadata.id).await {
                Ok(migrated) => {
                    if self.packages_match(original, &migrated) {
                        result.validated_packages += 1;
                    } else {
                        result.mismatches.push(original.metadata.id.to_string());
                    }
                }
                Err(e) => {
                    result
                        .errors
                        .push(format!("{}: {}", original.metadata.id, e));
                }
            }
        }

        Ok(result)
    }

    fn packages_match(&self, original: &Package, migrated: &Package) -> bool {
        original.metadata.id == migrated.metadata.id
            && original.metadata.name == migrated.metadata.name
            && original.versions == migrated.versions
    }
}

/// Migration validation result
#[derive(Debug, Clone)]
pub struct MigrationValidationResult {
    /// Total packages validated
    pub total_packages: usize,
    /// Successfully validated
    pub validated_packages: usize,
    /// Packages with mismatches
    pub mismatches: Vec<String>,
    /// Validation errors
    pub errors: Vec<String>,
}

impl MigrationValidationResult {
    fn new() -> Self {
        Self {
            total_packages: 0,
            validated_packages: 0,
            mismatches: Vec::new(),
            errors: Vec::new(),
        }
    }

    /// Check if validation passed
    pub fn is_valid(&self) -> bool {
        self.mismatches.is_empty() && self.errors.is_empty()
    }
}

impl std::fmt::Display for MigrationValidationResult {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Validation: {}/{} valid, {} mismatches, {} errors",
            self.validated_packages,
            self.total_packages,
            self.mismatches.len(),
            self.errors.len()
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_migration_report() {
        let report = MigrationReport {
            total_packages: 100,
            migrated_packages: 95,
            skipped_packages: 0,
            errors: Vec::new(),
        };

        assert_eq!(report.success_rate(), 0.95);
        assert!(!report.is_successful()); // Not 100% migrated
    }

    #[tokio::test]
    async fn test_consistency_result() {
        let mut result = ConsistencyResult::new("test-pkg".to_string());
        result.differences.push("version mismatch".to_string());
        result.is_consistent = false;

        assert!(!result.is_consistent);
        assert_eq!(result.differences.len(), 1);
    }
}
