//! Data Migration: v1 `PackageRegistry` → v2 RDF Store
//!
//! Provides utilities for migrating package data from the v1 in-memory
//! registry to the v2 RDF-backed semantic store.
//!
//! Also provides pack migration system for handling version upgrades with
//! compatibility checking, path computation, and rollback support.

#![allow(clippy::missing_errors_doc)]

use crate::error::Result;
use crate::models::{Package, PackageId, PackageVersion, ReleaseInfo};
use crate::registry_rdf::RdfRegistry;
use crate::traits::AsyncRepository;
use std::collections::{HashMap, VecDeque};
use std::sync::Arc;
use tracing::{info, warn};
use semver::Version;

// ============================================================================
// PACK UPGRADE MIGRATION SYSTEM
// ============================================================================

/// Represents a directed edge in the version upgrade graph
///
/// Allows transitions between versions with optional validation rules.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct UpgradeEdge {
    /// From version
    pub from: PackageVersion,
    /// To version
    pub to: PackageVersion,
    /// Whether direct upgrade is allowed (false = requires intermediate steps)
    pub is_direct: bool,
}

impl UpgradeEdge {
    /// Create a new upgrade edge
    pub fn new(from: PackageVersion, to: PackageVersion) -> Self {
        Self {
            from,
            to,
            is_direct: true,
        }
    }

    /// Mark this edge as requiring intermediate steps
    pub fn indirect(mut self) -> Self {
        self.is_direct = false;
        self
    }
}

/// Handles pack version migrations with upgrade path computation
///
/// Manages upgrade graphs, path finding, and migration execution.
/// Supports:
/// - Linear upgrades (v1 → v2 → v3)
/// - Branching paths (v1 → v2a or v2b)
/// - Incompatible upgrades (errors)
/// - Rollback on migration failure
pub struct Migrator {
    /// Upgrade graph edges: from_version → [(to_version, is_direct)]
    upgrade_graph: HashMap<PackageVersion, Vec<UpgradeEdge>>,
    /// Rollback state: saved package snapshots
    rollback_states: HashMap<String, Package>,
}

impl Migrator {
    /// Create a new migrator with an empty upgrade graph
    pub fn new() -> Self {
        Self {
            upgrade_graph: HashMap::new(),
            rollback_states: HashMap::new(),
        }
    }

    /// Add an upgrade edge to the graph
    pub fn add_upgrade_edge(&mut self, edge: UpgradeEdge) {
        self.upgrade_graph
            .entry(edge.from.clone())
            .or_insert_with(Vec::new)
            .push(edge);
    }

    /// Add multiple upgrade edges from a version
    pub fn add_upgrade_edges<I>(&mut self, edges: I)
    where
        I: IntoIterator<Item = UpgradeEdge>,
    {
        for edge in edges {
            self.add_upgrade_edge(edge);
        }
    }

    /// Define a linear upgrade path (v1 → v2 → v3 → ...)
    pub fn add_linear_path(&mut self, versions: &[PackageVersion]) {
        for i in 0..versions.len().saturating_sub(1) {
            self.add_upgrade_edge(UpgradeEdge::new(
                versions[i].clone(),
                versions[i + 1].clone(),
            ));
        }
    }

    /// Compute upgrade path from one version to another
    ///
    /// Uses BFS to find the shortest path through the upgrade graph.
    /// Returns the sequence of versions to upgrade through (inclusive).
    ///
    /// # Errors
    ///
    /// * `Error::ValidationFailed` - When no upgrade path exists or from/to are same
    pub fn compute_upgrade_path(
        &self, from: &PackageVersion, to: &PackageVersion,
    ) -> Result<Vec<PackageVersion>> {
        if from == to {
            return Ok(vec![from.clone()]);
        }

        // BFS to find shortest path
        let mut queue: VecDeque<(PackageVersion, Vec<PackageVersion>)> = VecDeque::new();
        let mut visited = std::collections::HashSet::new();

        queue.push_back((from.clone(), vec![from.clone()]));
        visited.insert(from.clone());

        while let Some((current, path)) = queue.pop_front() {
            if let Some(edges) = self.upgrade_graph.get(&current) {
                for edge in edges {
                    if edge.to == *to {
                        let mut final_path = path.clone();
                        final_path.push(to.clone());
                        return Ok(final_path);
                    }

                    if !visited.contains(&edge.to) {
                        visited.insert(edge.to.clone());
                        let mut new_path = path.clone();
                        new_path.push(edge.to.clone());
                        queue.push_back((edge.to.clone(), new_path));
                    }
                }
            }
        }

        Err(crate::error::Error::ValidationFailed {
            reason: format!("No upgrade path found from {} to {}", from, to),
        })
    }

    /// Check if a direct upgrade is possible between versions
    pub fn can_upgrade_directly(&self, from: &PackageVersion, to: &PackageVersion) -> bool {
        if let Some(edges) = self.upgrade_graph.get(from) {
            edges.iter().any(|e| &e.to == to && e.is_direct)
        } else {
            false
        }
    }

    /// Check if any upgrade path exists between versions
    pub fn can_upgrade(&self, from: &PackageVersion, to: &PackageVersion) -> bool {
        self.compute_upgrade_path(from, to).is_ok()
    }

    /// Get all possible target versions from a given source version
    pub fn get_upgrade_targets(&self, from: &PackageVersion) -> Vec<PackageVersion> {
        self.upgrade_graph
            .get(from)
            .map(|edges| edges.iter().map(|e| e.to.clone()).collect())
            .unwrap_or_default()
    }

    /// Save a package state for rollback
    fn save_rollback_state(&mut self, package: &Package) {
        let key = format!(
            "{}_{}@{}",
            package.metadata.id, "rollback", package.latest_version
        );
        self.rollback_states.insert(key, package.clone());
    }

    /// Restore a package from rollback state
    fn restore_rollback_state(&mut self, package: &Package) -> Result<Package> {
        let key = format!(
            "{}_{}@{}",
            package.metadata.id, "rollback", package.latest_version
        );
        self.rollback_states
            .remove(&key)
            .ok_or_else(|| crate::error::Error::ValidationFailed {
                reason: format!("No rollback state available for {}", package.metadata.id),
            })
    }

    /// Migrate a package from one version to another
    ///
    /// Executes the migration following the upgrade path, applying
    /// version-specific transformations. Saves rollback state before
    /// executing migration.
    ///
    /// # Errors
    ///
    /// * `Error::ValidationFailed` - When no upgrade path exists
    /// * `Error::ValidationFailed` - When migration transform fails
    pub fn migrate(
        &mut self, mut package: Package, from: &PackageVersion, to: &PackageVersion,
    ) -> Result<Package> {
        info!(
            "Starting migration of {} from {} to {}",
            package.metadata.id, from, to
        );

        // Validate upgrade path exists
        let path = self.compute_upgrade_path(from, to)?;

        // Save rollback state
        self.save_rollback_state(&package);

        // Execute migrations for each step in the path
        for i in 0..path.len().saturating_sub(1) {
            let from_v = &path[i];
            let to_v = &path[i + 1];

            match self.apply_migration_transform(&mut package, from_v, to_v) {
                Ok(()) => {
                    info!("Migration step {}: {} -> {} completed", i + 1, from_v, to_v);
                }
                Err(e) => {
                    warn!("Migration step failed: {}, attempting rollback", e);
                    if let Ok(_rolled_back) = self.restore_rollback_state(&package) {
                        return Err(crate::error::Error::ValidationFailed {
                            reason: format!(
                                "Migration failed at step {} -> {}: {}. Rolled back to original state.",
                                from_v, to_v, e
                            ),
                        });
                    }
                    return Err(e);
                }
            }
        }

        // Update package version to target
        package.latest_version = to.clone();
        if !package.versions.contains(to) {
            package.versions.push(to.clone());
            package.versions.sort();
        }

        info!(
            "Successfully migrated {} to version {}",
            package.metadata.id, to
        );
        Ok(package)
    }

    /// Apply version-specific migration transformations
    ///
    /// Implements transform rules for each version transition.
    /// Extended by adding cases for real version transitions.
    fn apply_migration_transform(
        &self, package: &mut Package, from: &PackageVersion, to: &PackageVersion,
    ) -> Result<()> {
        info!("Applying migration transform: {} -> {}", from, to);

        // Parse versions as semver for comparison
        let from_semver = Version::parse(from.as_str()).map_err(|_| {
            crate::error::Error::ValidationFailed {
                reason: format!("Invalid semver version: {}", from),
            }
        })?;

        let to_semver = Version::parse(to.as_str()).map_err(|_| {
            crate::error::Error::ValidationFailed {
                reason: format!("Invalid semver version: {}", to),
            }
        })?;

        // Reject downgrades (from_version >= to_version)
        if from_semver >= to_semver {
            return Err(crate::error::Error::ValidationFailed {
                reason: format!(
                    "Cannot downgrade from {} to {}. Migrations only support upgrades.",
                    from, to
                ),
            });
        }

        // Ensure package metadata is valid
        if package.metadata.id.as_str().is_empty() {
            return Err(crate::error::Error::ValidationFailed {
                reason: "Package ID cannot be empty during migration".to_string(),
            });
        }

        // Create ReleaseInfo for the target version if it doesn't exist
        if !package.releases.contains_key(to) {
            use crate::trust::TrustTier;
            let release_info = ReleaseInfo {
                version: to.clone(),
                released_at: chrono::Utc::now(),
                changelog: String::new(),
                checksum: String::new(),
                signature: None,
                download_url: String::new(),
                dependencies: Vec::new(),
                trust_tier: TrustTier::Experimental,
                registry_class: crate::models::default_registry_class(),
            };
            package.releases.insert(to.clone(), release_info);
        }

        // Update the package's updated_at timestamp to current time
        package.metadata.updated_at = chrono::Utc::now();

        info!(
            "Migration transform complete: {} -> {} for package {}",
            from, to, package.metadata.id
        );

        Ok(())
    }

    /// Get all registered versions in the upgrade graph
    pub fn get_all_versions(&self) -> Vec<PackageVersion> {
        let mut versions = std::collections::HashSet::new();
        for (from, edges) in &self.upgrade_graph {
            versions.insert(from.clone());
            for edge in edges {
                versions.insert(edge.to.clone());
            }
        }
        let mut v: Vec<_> = versions.into_iter().collect();
        v.sort();
        v
    }

    /// Get upgrade compatibility matrix
    ///
    /// Returns a matrix showing which versions can upgrade to which.
    pub fn get_compatibility_matrix(&self) -> HashMap<PackageVersion, Vec<PackageVersion>> {
        let mut matrix = HashMap::new();
        for from in self.get_all_versions() {
            matrix.insert(from.clone(), self.get_upgrade_targets(&from));
        }
        matrix
    }
}

impl Default for Migrator {
    fn default() -> Self {
        Self::new()
    }
}

// ============================================================================
// LEGACY v1-v2 DATA MIGRATION
// ============================================================================

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
    ///
    /// # Errors
    ///
    /// * [`Error::RdfStoreError`] - When batch insertion fails for any package batch
    /// * [`Error::RegistryError`] - When the target registry is unavailable
    pub fn migrate_packages(&self, v1_packages: &[Package]) -> Result<MigrationReport> {
        info!(
            "Starting migration of {} packages from v1 to RDF",
            v1_packages.len()
        );

        let mut report = MigrationReport::new();
        report.total_packages = v1_packages.len();

        // Batch insert for efficiency
        let batch_size = 50;
        for chunk in v1_packages.chunks(batch_size) {
            match self.target.batch_insert_packages(chunk.to_vec()) {
                Ok(inserted) => {
                    report.migrated_packages += inserted;
                }
                Err(e) => {
                    warn!("Batch insert failed: {e}");
                    report.errors.push(format!("Batch insert error: {e}"));
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
    ///
    /// # Errors
    ///
    /// * [`Error::PackageNotFound`] - When a v1 package does not exist in the v2 registry
    /// * [`Error::RdfStoreError`] - When querying the RDF store fails
    pub async fn verify_migration(&self, v1_packages: &[Package]) -> Result<VerificationReport> {
        info!("Verifying migration of {} packages", v1_packages.len());

        let mut report = VerificationReport::new();
        report.total_packages = v1_packages.len();

        for v1_package in v1_packages {
            match self.verify_package(v1_package).await {
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
    ///
    /// # Errors
    ///
    /// * [`Error::PackageNotFound`] - When the package does not exist in the v2 registry
    /// * [`Error::RdfStoreError`] - When querying the RDF store fails
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
    ///
    /// # Errors
    ///
    /// * [`Error::RdfStoreError`] - When inserting a package into the RDF store fails
    /// * [`Error::RegistryError`] - When checking package existence fails
    pub async fn incremental_migrate(&self, v1_packages: &[Package]) -> Result<MigrationReport> {
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
                    match self.target.insert_package_rdf(package) {
                        Ok(()) => {
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
    #[must_use]
    pub fn is_successful(&self) -> bool {
        self.errors.is_empty() && self.migrated_packages == self.total_packages
    }

    /// Get success rate (0.0 - 1.0)
    #[must_use]
    #[allow(clippy::cast_precision_loss)]
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
    #[must_use]
    pub fn is_valid(&self) -> bool {
        self.errors.is_empty() && self.mismatches.is_empty()
    }

    /// Get verification rate (0.0 - 1.0)
    #[must_use]
    #[allow(clippy::cast_precision_loss)]
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
    ///
    /// # Errors
    ///
    /// * [`Error::PackageNotFound`] - When the package does not exist in the v2 registry
    /// * [`Error::RdfStoreError`] - When querying the RDF store fails
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
    ///
    /// # Errors
    ///
    /// * [`Error::PackageNotFound`] - When a package does not exist in the v2 registry
    /// * [`Error::RdfStoreError`] - When querying the RDF store fails
    pub async fn periodic_check(&self, v1_packages: &[Package]) -> Result<ConsistencyReport> {
        let mut report = ConsistencyReport::new();
        report.total_packages = v1_packages.len();

        for v1_package in v1_packages {
            match self
                .check_package_consistency(&v1_package.metadata.id, v1_package)
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
    #[must_use]
    pub fn is_fully_consistent(&self) -> bool {
        self.inconsistent_packages.is_empty() && self.errors.is_empty()
    }

    /// Get consistency rate (0.0 - 1.0)
    #[must_use]
    #[allow(clippy::cast_precision_loss)]
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
    use crate::models::PackageMetadata;

    // ========================================================================
    // PACK MIGRATION SYSTEM TESTS
    // ========================================================================

    #[test]
    fn test_upgrade_edge_creation() {
        let edge = UpgradeEdge::new(
            PackageVersion::new("1.0.0").unwrap(),
            PackageVersion::new("2.0.0").unwrap(),
        );

        assert_eq!(edge.from, PackageVersion::new("1.0.0").unwrap());
        assert_eq!(edge.to, PackageVersion::new("2.0.0").unwrap());
        assert!(edge.is_direct);
    }

    #[test]
    fn test_migrator_creation() {
        let migrator = Migrator::new();
        assert!(migrator.upgrade_graph.is_empty());
        assert!(migrator.rollback_states.is_empty());
    }

    #[test]
    fn test_linear_upgrade_path() {
        let mut migrator = Migrator::new();
        let versions = vec![
            PackageVersion::new("1.0.0").unwrap(),
            PackageVersion::new("2.0.0").unwrap(),
            PackageVersion::new("3.0.0").unwrap(),
        ];

        migrator.add_linear_path(&versions);

        // Verify edges were added
        assert_eq!(migrator.upgrade_graph.len(), 2);
    }

    #[test]
    fn test_compute_upgrade_path_linear() {
        let mut migrator = Migrator::new();
        let v1 = PackageVersion::new("1.0.0").unwrap();
        let v2 = PackageVersion::new("2.0.0").unwrap();
        let v3 = PackageVersion::new("3.0.0").unwrap();

        migrator.add_linear_path(&[v1.clone(), v2.clone(), v3.clone()]);

        let path = migrator.compute_upgrade_path(&v1, &v3).unwrap();
        assert_eq!(path, vec![v1, v2, v3]);
    }

    #[test]
    fn test_compute_upgrade_path_same_version() {
        let migrator = Migrator::new();
        let v1 = PackageVersion::new("1.0.0").unwrap();

        let path = migrator.compute_upgrade_path(&v1, &v1).unwrap();
        assert_eq!(path, vec![v1]);
    }

    #[test]
    fn test_compute_upgrade_path_no_path() {
        let migrator = Migrator::new();
        let v1 = PackageVersion::new("1.0.0").unwrap();
        let v2 = PackageVersion::new("2.0.0").unwrap();

        let result = migrator.compute_upgrade_path(&v1, &v2);
        assert!(result.is_err());
    }

    #[test]
    fn test_direct_upgrade_check() {
        let mut migrator = Migrator::new();
        let v1 = PackageVersion::new("1.0.0").unwrap();
        let v2 = PackageVersion::new("2.0.0").unwrap();

        migrator.add_upgrade_edge(UpgradeEdge::new(v1.clone(), v2.clone()));

        assert!(migrator.can_upgrade_directly(&v1, &v2));
        assert!(!migrator.can_upgrade_directly(&v2, &v1));
    }

    #[test]
    fn test_branching_upgrade_paths() {
        let mut migrator = Migrator::new();
        let v1 = PackageVersion::new("1.0.0").unwrap();
        let v2a = PackageVersion::new("2.0.0").unwrap();
        let v2b = PackageVersion::new("2.1.0").unwrap();

        // v1 can upgrade to either v2a or v2b
        migrator.add_upgrade_edge(UpgradeEdge::new(v1.clone(), v2a.clone()));
        migrator.add_upgrade_edge(UpgradeEdge::new(v1.clone(), v2b.clone()));

        assert!(migrator.can_upgrade(&v1, &v2a));
        assert!(migrator.can_upgrade(&v1, &v2b));

        let targets = migrator.get_upgrade_targets(&v1);
        assert_eq!(targets.len(), 2);
    }

    #[test]
    fn test_get_all_versions() {
        let mut migrator = Migrator::new();
        let v1 = PackageVersion::new("1.0.0").unwrap();
        let v2 = PackageVersion::new("2.0.0").unwrap();
        let v3 = PackageVersion::new("3.0.0").unwrap();

        migrator.add_linear_path(&[v1.clone(), v2.clone(), v3.clone()]);

        let versions = migrator.get_all_versions();
        assert_eq!(versions.len(), 3);
    }

    #[test]
    fn test_compatibility_matrix() {
        let mut migrator = Migrator::new();
        let v1 = PackageVersion::new("1.0.0").unwrap();
        let v2 = PackageVersion::new("2.0.0").unwrap();

        migrator.add_linear_path(&[v1.clone(), v2.clone()]);

        let matrix = migrator.get_compatibility_matrix();
        assert_eq!(matrix.len(), 2);
        assert_eq!(matrix.get(&v1).unwrap().len(), 1);
        assert_eq!(matrix.get(&v2).unwrap().len(), 0);
    }

    #[test]
    fn test_migrate_linear_upgrade() {
        let mut migrator = Migrator::new();
        let v1 = PackageVersion::new("1.0.0").unwrap();
        let v2 = PackageVersion::new("2.0.0").unwrap();

        migrator.add_linear_path(&[v1.clone(), v2.clone()]);

        let pkg_id = crate::models::PackageId::new("test-pkg").unwrap();
        let package = Package {
            metadata: PackageMetadata::new(pkg_id, "Test Package", "A test package", "MIT"),
            latest_version: v1.clone(),
            versions: vec![v1.clone()],
            releases: indexmap::IndexMap::new(),
        };

        let result = migrator.migrate(package.clone(), &v1, &v2);
        assert!(result.is_ok());

        let migrated = result.unwrap();
        assert_eq!(migrated.latest_version, v2);
    }

    #[test]
    fn test_indirect_upgrade_edge() {
        let edge = UpgradeEdge::new(
            PackageVersion::new("1.0.0").unwrap(),
            PackageVersion::new("3.0.0").unwrap(),
        )
        .indirect();

        assert!(!edge.is_direct);
    }

    #[test]
    fn test_migrator_default() {
        let _migrator = Migrator::default();
        // Should not panic and should be empty
        assert!(_migrator.upgrade_graph.is_empty());
    }

    // ========================================================================
    // LEGACY v1-v2 DATA MIGRATION TESTS
    // ========================================================================

    #[test]
    fn test_migration_report() {
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
