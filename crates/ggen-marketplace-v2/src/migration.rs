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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::models::{PackageMetadata, PackageVersion};

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
